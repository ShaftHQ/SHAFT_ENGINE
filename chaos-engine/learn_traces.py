#!/usr/bin/env python3
"""Portable learn-traces collector. Map-reduce stays with isolated host agents."""

from __future__ import annotations

import argparse
import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

SECRET = re.compile(
    r"(?i)(api[_-]?key|token|password|secret|authorization|bearer)\s*[:=]\s*\S+"
)
BEARER = re.compile(r"(?i)\bBearer\s+[A-Za-z0-9._\-]+")
SK_TOKEN = re.compile(r"\b(?:sk|ghp|gho|ghu|ghs|ghr)_[A-Za-z0-9]{8,}\b")


def redact(text: str) -> str:
    out = SECRET.sub(r"\1=<redacted>", text)
    out = BEARER.sub("Bearer <redacted>", out)
    out = SK_TOKEN.sub("<redacted-token>", out)
    return out


def _read_jsonl(path: Path) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    try:
        for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
            if not line.strip():
                continue
            try:
                parsed = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(parsed, dict):
                rows.append(parsed)
    except OSError:
        return []
    return rows


def _read_json_or_jsonl(path: Path) -> list[dict[str, object]]:
    if path.suffix.casefold() == ".jsonl":
        return _read_jsonl(path)
    try:
        payload = json.loads(path.read_text(encoding="utf-8", errors="replace"))
    except (OSError, json.JSONDecodeError):
        return _read_jsonl(path)
    if isinstance(payload, dict):
        for key in ("messages", "items", "events", "turns"):
            value = payload.get(key)
            if isinstance(value, list):
                return [item for item in value if isinstance(item, dict)]
        return [payload]
    if isinstance(payload, list):
        return [item for item in payload if isinstance(item, dict)]
    return []


def _text_blobs(row: dict[str, object]) -> list[str]:
    blobs: list[str] = []
    for key in ("content", "text", "message", "prompt", "input"):
        value = row.get(key)
        if isinstance(value, str):
            blobs.append(value)
        elif isinstance(value, dict):
            for nested in ("content", "text", "role"):
                item = value.get(nested)
                if isinstance(item, str):
                    blobs.append(item)
            content = value.get("content")
            if isinstance(content, list):
                for block in content:
                    if isinstance(block, dict):
                        text = block.get("text")
                        if isinstance(text, str):
                            blobs.append(text)
                    elif isinstance(block, str):
                        blobs.append(block)
        elif isinstance(value, list):
            for block in value:
                if isinstance(block, str):
                    blobs.append(block)
                elif isinstance(block, dict):
                    text = block.get("text")
                    if isinstance(text, str):
                        blobs.append(text)
    return blobs


def _row_role(row: dict[str, object]) -> str:
    for key in ("role", "type", "kind"):
        value = row.get(key)
        if isinstance(value, str) and value.strip():
            return value.strip().casefold()
    message = row.get("message")
    if isinstance(message, dict):
        role = message.get("role")
        if isinstance(role, str):
            return role.strip().casefold()
    return ""


def classify_session(rows: list[dict[str, object]], *, source: str) -> tuple[str, str]:
    """Return (keep|drop, reason)."""
    if not rows:
        return "drop", "empty"
    roles = {_row_role(row) for row in rows}
    text = " ".join(blob for row in rows for blob in _text_blobs(row)).casefold()
    if any(
        marker in text
        for marker in ("isSidechain\": true", "issubagent", "\"subagent\"")
    ) or any("sidechain" in role or "subagent" in role for role in roles):
        # Explicit sidechain flags on rows.
        for row in rows:
            if row.get("isSidechain") is True or row.get("isSubagent") is True:
                return "drop", "subagent"
            if str(row.get("type", "")).casefold() in {"subagent", "sidechain"}:
                return "drop", "subagent"
    for row in rows:
        if row.get("isSidechain") is True or row.get("isSubagent") is True:
            return "drop", "subagent"
        agent = str(row.get("agentId") or row.get("parentSessionId") or "").strip()
        if agent and str(row.get("type", "")).casefold() in {"subagent", "sidechain"}:
            return "drop", "subagent"
    if "synthetic" in roles or any(
        str(row.get("source", "")).casefold() == "synthetic" for row in rows
    ):
        return "drop", "synthetic"
    if any(str(row.get("headless")).casefold() in {"1", "true", "yes"} for row in rows):
        return "drop", "headless"
    humanish = {"user", "human", "prompt", "user_message", "userpromptsubmit"}
    if roles & humanish or any(_row_role(row) == "user" for row in rows):
        return "keep", "human_session"
    # Claude often uses type=user rather than role.
    if any(str(row.get("type", "")).casefold() == "user" for row in rows):
        return "keep", "human_session"
    # Codex rollouts often nest under type=event_msg / payload
    for row in rows:
        payload = row.get("payload")
        if isinstance(payload, dict) and str(payload.get("type", "")).casefold() in {
            "user_message",
            "message",
        }:
            return "keep", "human_session"
        if str(row.get("type", "")).casefold() in {"response_item", "event_msg"}:
            role = ""
            if isinstance(payload, dict):
                role = str(payload.get("role") or payload.get("type") or "").casefold()
            if role in humanish or role == "user_message":
                return "keep", "human_session"
    del source
    return "drop", "no_human_turn"


def session_id_for(path: Path, host: str) -> str:
    stem = path.stem
    safe = re.sub(r"[^A-Za-z0-9._-]+", "_", f"{host}_{stem}")
    return safe[:120] or f"{host}_session"


def _redact_value(value: object) -> object:
    if isinstance(value, str):
        return redact(value)
    if isinstance(value, list):
        return [_redact_value(item) for item in value]
    if isinstance(value, dict):
        return {str(key): _redact_value(item) for key, item in value.items()}
    return value


def redact_rows(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    return [_redact_value(row) for row in rows if isinstance(row, dict)]  # type: ignore[misc]


def iter_session_files(root: Path) -> list[Path]:
    if not root.is_dir():
        return []
    files: list[Path] = []
    for path in sorted(root.rglob("*")):
        if not path.is_file():
            continue
        if path.name.casefold() in {"manifest.json", "config.json"}:
            continue
        if path.suffix.casefold() in {".jsonl", ".json"}:
            # Skip obvious non-session trees.
            parts = {part.casefold() for part in path.parts}
            if parts & {"skills", "plugins", "node_modules", ".git"}:
                continue
            files.append(path)
    return files


def host_roots(home: Path) -> list[tuple[str, Path]]:
    return [
        ("grok", Path(os_environ_home(home, "GROK_HOME", ".grok")) / "sessions"),
        ("claude", Path(os_environ_home(home, "CLAUDE_CONFIG_DIR", ".claude")) / "projects"),
        ("codex", Path(os_environ_home(home, "CODEX_HOME", ".codex")) / "sessions"),
        ("gemini", Path(os_environ_home(home, "GEMINI_HOME", ".gemini")) / "tmp"),
        ("gemini", Path(os_environ_home(home, "GEMINI_HOME", ".gemini")) / "sessions"),
    ]


def os_environ_home(home: Path, env_name: str, default_name: str) -> Path:
    import os

    configured = os.environ.get(env_name)
    if configured:
        return Path(configured)
    # CLAUDE_CONFIG is a file path in mcp_policy; prefer CLAUDE_CONFIG_DIR / .claude dir.
    if env_name == "CLAUDE_CONFIG_DIR":
        return home / ".claude"
    return home / default_name


def collect(home: Path, out: Path) -> dict[str, object]:
    out.mkdir(parents=True, exist_ok=True)
    sessions_dir = out / "sessions"
    sessions_dir.mkdir(exist_ok=True)
    sessions: list[dict[str, object]] = []
    dropped: dict[str, int] = {
        "missing_roots": 0,
        "empty": 0,
        "subagent": 0,
        "synthetic": 0,
        "headless": 0,
        "no_human_turn": 0,
        "unreadable": 0,
    }
    seen_roots: set[Path] = set()
    for host, root in host_roots(home):
        resolved = root
        if resolved in seen_roots:
            continue
        seen_roots.add(resolved)
        if not resolved.is_dir():
            dropped["missing_roots"] += 1
            continue
        for path in iter_session_files(resolved):
            rows = _read_json_or_jsonl(path)
            if not rows:
                dropped["unreadable"] = dropped.get("unreadable", 0) + 1
                continue
            decision, reason = classify_session(rows, source=host)
            if decision != "keep":
                dropped[reason] = dropped.get(reason, 0) + 1
                continue
            sid = session_id_for(path, host)
            payload = {
                "id": sid,
                "host": host,
                "sourcePath": str(path),
                "keptReason": reason,
                "messages": redact_rows(rows),
            }
            target = sessions_dir / f"{sid}.json"
            target.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
            # Guard: credentials must not survive redaction in written output.
            written = target.read_text(encoding="utf-8")
            if SECRET.search(written) or BEARER.search(written) or SK_TOKEN.search(written):
                # Re-redact whole file as a belt-and-suspenders pass.
                target.write_text(redact(written), encoding="utf-8")
            sessions.append(
                {
                    "id": sid,
                    "host": host,
                    "path": str(target.relative_to(out)),
                    "sourcePath": str(path),
                }
            )
    manifest = {
        "schemaVersion": 1,
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "home": str(home),
        "sessions_kept": len(sessions),
        "sessions": sessions,
        "dropped": dropped,
        "next": "map-reduce-verify via isolated subagents; see references/learn-traces.md",
    }
    (out / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    return manifest


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="learn_traces.py")
    sub = parser.add_subparsers(dest="cmd", required=True)
    collect_p = sub.add_parser("collect")
    collect_p.add_argument("--home", type=Path, default=Path.home())
    collect_p.add_argument("--out", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.cmd == "collect":
        manifest = collect(args.home.expanduser(), args.out)
        print(json.dumps({"run_dir": str(args.out), **manifest}))
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
