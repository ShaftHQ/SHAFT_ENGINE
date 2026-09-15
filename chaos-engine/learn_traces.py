#!/usr/bin/env python3
"""Portable learn-traces collect + map-reduce-verify (#5784 / #5847).

Collect remains host-agnostic. Map-reduce-verify uses an isolated file
contract (prompts + batch notes) so every host can run /learn without a
Grok TUI workflow. Offline mode fills that contract deterministically for
scripts/tests; hosts may instead write the same files via Task/subagents.
Actions target git-tracked overlay paths only — never ~/.grok/skills.
"""

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
        "next": "python3 .chaos-engine/learn_traces.py learn --out <run-dir> (or prepare/offline/finalize); see references/learn-traces.md",
    }
    (out / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    return manifest



# --- map-reduce-verify file contract (#5847) ---------------------------------

BATCH_SIZE = 8
FORBIDDEN_TARGET_MARKERS = (
    "~/.grok/skills",
    "/.grok/skills/",
    "GROK_HOME/skills",
    "\\grok\\skills\\",
)
OVERLAY_TARGET_PREFIXES = (
    "chaos-engine/",
    ".chaos-engine/",
    "patches/",
)
PHRASE_TOKEN = re.compile(r"[A-Za-z][A-Za-z0-9_./-]{2,}")
STOPWORDS = frozenset(
    {
        "the",
        "and",
        "for",
        "that",
        "with",
        "this",
        "from",
        "have",
        "were",
        "been",
        "will",
        "your",
        "into",
        "about",
        "there",
        "their",
        "what",
        "when",
        "where",
        "which",
        "while",
        "would",
        "could",
        "should",
        "please",
        "thanks",
        "hello",
        "just",
        "like",
        "want",
        "need",
        "make",
        "sure",
        "also",
        "then",
        "than",
        "them",
        "they",
        "you",
        "are",
        "was",
        "not",
        "but",
        "all",
        "any",
        "can",
        "our",
        "out",
        "use",
        "using",
        "used",
        "run",
        "running",
        "file",
        "files",
        "path",
        "code",
        "test",
        "tests",
        "okay",
        "yes",
        "no",
    }
)

MAP_PROMPT = """# Map batch (isolated)

Read only the session JSON files listed in this batch. Extract:
- repeated human phrases worth encoding as overlay policy
- stale skill lines / unused loaded surfaces
- gaps (jobs humans re-explained)

Cite session `id` values. Invent nothing. Write JSON to the batch path.
Do not edit ~/.grok/skills. Do not open other batches.
"""

REDUCE_PROMPT = """# Reduce (isolated)

Fold every map/batch-*.json note into one synthesis.json.
Open candidate overlay files under chaos-engine/ (or installed .chaos-engine/
source pointers) as context only. Invent nothing.
Targets must be git-tracked overlay paths (chaos-engine/... or patches/*.diff).
Never propose ~/.grok/skills/... as the primary write.
"""

VERIFY_PROMPT = """# Verify skeptics (isolated)

Three independent checks: phrases, stale lines, deletes.
Keep or drop candidates from reduce/synthesis.json. Add nothing.
Fail closed when evidence (session ids) is missing.
First-ever run: step/curate deletes — never auto-delete.
"""


def load_manifest(run_dir: Path) -> dict[str, object]:
    path = run_dir / "manifest.json"
    if not path.is_file():
        raise FileNotFoundError(f"missing manifest.json under {run_dir}")
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError("manifest.json must be an object")
    return payload


def load_session(run_dir: Path, relative: str) -> dict[str, object]:
    path = run_dir / relative
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"session {relative} must be an object")
    return payload


def _session_user_text(session: dict[str, object]) -> str:
    messages = session.get("messages")
    blobs: list[str] = []
    if isinstance(messages, list):
        for row in messages:
            if not isinstance(row, dict):
                continue
            role = _row_role(row)
            row_type = str(row.get("type", "")).casefold()
            if role in {"user", "human", "prompt", "user_message"} or row_type == "user":
                blobs.extend(_text_blobs(row))
            payload = row.get("payload")
            if isinstance(payload, dict):
                ptype = str(payload.get("type") or payload.get("role") or "").casefold()
                if ptype in {"user_message", "user", "message"}:
                    message = payload.get("message")
                    if isinstance(message, str):
                        blobs.append(message)
                    blobs.extend(
                        str(payload.get(key))
                        for key in ("text", "content")
                        if isinstance(payload.get(key), str)
                    )
    return "\n".join(blobs)


def _phrase_counts(text: str) -> dict[str, int]:
    counts: dict[str, int] = {}
    tokens = [tok.casefold() for tok in PHRASE_TOKEN.findall(text)]
    tokens = [tok for tok in tokens if tok not in STOPWORDS and not tok.isdigit()]
    for index in range(len(tokens)):
        unigram = tokens[index]
        counts[unigram] = counts.get(unigram, 0) + 1
        if index + 1 < len(tokens):
            bigram = f"{tokens[index]} {tokens[index + 1]}"
            counts[bigram] = counts.get(bigram, 0) + 1
    return counts


def is_forbidden_target(target: str) -> bool:
    normalized = target.strip().replace("\\", "/")
    lowered = normalized.casefold()
    if any(marker.casefold() in lowered for marker in FORBIDDEN_TARGET_MARKERS):
        return True
    if lowered.startswith("~/.grok/") or "/.grok/skills" in lowered:
        return True
    return False


def is_git_tracked_overlay_target(target: str) -> bool:
    normalized = target.strip().replace("\\", "/").lstrip("./")
    if is_forbidden_target(normalized):
        return False
    return any(normalized.startswith(prefix) for prefix in OVERLAY_TARGET_PREFIXES)


def prepare(run_dir: Path) -> dict[str, object]:
    """Write isolated prompts + empty map/reduce/verify layout for host agents."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    sessions = manifest.get("sessions")
    if not isinstance(sessions, list):
        raise ValueError("manifest.sessions must be a list")
    prompts = run_dir / "prompts"
    map_dir = run_dir / "map"
    reduce_dir = run_dir / "reduce"
    verify_dir = run_dir / "verify"
    for directory in (prompts, map_dir, reduce_dir, verify_dir):
        directory.mkdir(parents=True, exist_ok=True)
    (prompts / "map.md").write_text(MAP_PROMPT.strip() + "\n", encoding="utf-8")
    (prompts / "reduce.md").write_text(REDUCE_PROMPT.strip() + "\n", encoding="utf-8")
    (prompts / "verify.md").write_text(VERIFY_PROMPT.strip() + "\n", encoding="utf-8")
    batches: list[dict[str, object]] = []
    kept = [item for item in sessions if isinstance(item, dict)]
    for start in range(0, max(len(kept), 1), BATCH_SIZE):
        chunk = kept[start : start + BATCH_SIZE]
        batch_id = f"batch-{start // BATCH_SIZE:03d}"
        batch = {
            "id": batch_id,
            "schemaVersion": 1,
            "sessions": [
                {
                    "id": item.get("id"),
                    "path": item.get("path"),
                    "host": item.get("host"),
                }
                for item in chunk
            ],
            "notes": [],
            "status": "pending",
        }
        target = map_dir / f"{batch_id}.json"
        if not target.is_file():
            target.write_text(json.dumps(batch, indent=2) + "\n", encoding="utf-8")
        batches.append({"id": batch_id, "path": f"map/{batch_id}.json", "size": len(chunk)})
    layout = {
        "schemaVersion": 1,
        "kind": "learn-traces-layout",
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "sessions_kept": int(manifest.get("sessions_kept") or len(kept)),
        "batches": batches,
        "prompts": ["prompts/map.md", "prompts/reduce.md", "prompts/verify.md"],
        "next": (
            "Fill map/batch-*.json via isolated subagents (or `learn --offline`), "
            "then reduce/verify; finalize writes report.md + actions.json. "
            "Never write ~/.grok/skills."
        ),
    }
    (run_dir / "layout.json").write_text(json.dumps(layout, indent=2) + "\n", encoding="utf-8")
    return layout


def offline_map_reduce_verify(run_dir: Path) -> dict[str, object]:
    """Deterministic zero-LLM fill of the map/reduce/verify file contract."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    if not (run_dir / "layout.json").is_file():
        prepare(run_dir)
    sessions_meta = [
        item for item in (manifest.get("sessions") or []) if isinstance(item, dict)
    ]
    phrase_sessions: dict[str, set[str]] = {}
    for item in sessions_meta:
        relative = str(item.get("path") or "")
        sid = str(item.get("id") or "")
        if not relative or not sid:
            continue
        try:
            session = load_session(run_dir, relative)
        except (OSError, json.JSONDecodeError, ValueError):
            continue
        text = _session_user_text(session)
        for phrase, count in _phrase_counts(text).items():
            if count < 1:
                continue
            phrase_sessions.setdefault(phrase, set()).add(sid)
    # Prefer phrases seen across >=2 sessions, else top unigrams from any session.
    repeated = [
        (phrase, sorted(sids))
        for phrase, sids in phrase_sessions.items()
        if len(sids) >= 2 and " " in phrase
    ]
    repeated.sort(key=lambda item: (-len(item[1]), item[0]))
    if not repeated:
        repeated = [
            (phrase, sorted(sids))
            for phrase, sids in phrase_sessions.items()
            if len(sids) >= 1 and " " not in phrase and len(phrase) >= 6
        ]
        repeated.sort(key=lambda item: (-len(item[1]), item[0]))
    repeated = repeated[:12]

    map_dir = run_dir / "map"
    map_dir.mkdir(exist_ok=True)
    # Rebuild batches from layout or sessions.
    batch_files = sorted(map_dir.glob("batch-*.json"))
    if not batch_files:
        prepare(run_dir)
        batch_files = sorted(map_dir.glob("batch-*.json"))
    for batch_path in batch_files:
        batch = json.loads(batch_path.read_text(encoding="utf-8"))
        batch_sids = {
            str(entry.get("id"))
            for entry in (batch.get("sessions") or [])
            if isinstance(entry, dict)
        }
        notes: list[dict[str, object]] = []
        for phrase, sids in repeated:
            hit = sorted(batch_sids & set(sids))
            if not hit:
                continue
            notes.append(
                {
                    "kind": "repeated_phrase",
                    "phrase": phrase,
                    "sessions": hit,
                    "proposedTarget": "chaos-engine/references/harness-learn.md",
                    "summary": f"Encode repeated guidance: {phrase}",
                }
            )
        if not notes and batch_sids:
            # Coverage: still cite every session in the batch so finalize can match.
            notes.append(
                {
                    "kind": "coverage_ack",
                    "phrase": "",
                    "sessions": sorted(batch_sids),
                    "proposedTarget": "chaos-engine/references/learn-traces.md",
                    "summary": "No repeated phrase; keep portable learn contract documented",
                }
            )
        batch["notes"] = notes
        batch["status"] = "mapped"
        batch_path.write_text(json.dumps(batch, indent=2) + "\n", encoding="utf-8")

    # Reduce
    all_notes: list[dict[str, object]] = []
    cited: set[str] = set()
    for batch_path in sorted(map_dir.glob("batch-*.json")):
        batch = json.loads(batch_path.read_text(encoding="utf-8"))
        for note in batch.get("notes") or []:
            if isinstance(note, dict):
                all_notes.append(note)
                for sid in note.get("sessions") or []:
                    cited.add(str(sid))
    candidates: list[dict[str, object]] = []
    seen_targets: set[str] = set()
    for note in all_notes:
        target = str(note.get("proposedTarget") or "").strip()
        if not target or is_forbidden_target(target):
            continue
        if not is_git_tracked_overlay_target(target):
            # Rewrite home-skill mistakes into overlay patches path.
            target = "patches/learn-overlay.diff"
        key = f"{note.get('kind')}:{target}:{note.get('phrase')}"
        if key in seen_targets:
            # Merge session evidence.
            for existing in candidates:
                if (
                    existing.get("kind") == note.get("kind")
                    and existing.get("target") == target
                    and existing.get("phrase") == note.get("phrase")
                ):
                    merged = set(str(s) for s in (existing.get("evidenceSessions") or []))
                    merged.update(str(s) for s in (note.get("sessions") or []))
                    existing["evidenceSessions"] = sorted(merged)
                    break
            continue
        seen_targets.add(key)
        candidates.append(
            {
                "id": f"cand-{len(candidates) + 1:03d}",
                "kind": "overlay-edit" if note.get("kind") != "delete" else "curate-delete",
                "target": target,
                "phrase": note.get("phrase") or "",
                "summary": note.get("summary") or f"Overlay edit for {target}",
                "evidenceSessions": sorted(str(s) for s in (note.get("sessions") or [])),
                "delivery": "git-pr",
                "autoApply": False,
                "autoDelete": False,
            }
        )
    synthesis = {
        "schemaVersion": 1,
        "kind": "learn-traces-reduce",
        "sessionsCited": sorted(cited),
        "candidates": candidates,
        "policy": {
            "forbidHomeSkills": True,
            "firstRunCurateOnly": True,
            "delivery": "git-tracked PR or patches/*.diff",
        },
    }
    reduce_dir = run_dir / "reduce"
    reduce_dir.mkdir(exist_ok=True)
    (reduce_dir / "synthesis.json").write_text(
        json.dumps(synthesis, indent=2) + "\n", encoding="utf-8"
    )

    # Verify: three skeptic files — keep only evidenced, non-home targets.
    kept_actions: list[dict[str, object]] = []
    dropped: list[dict[str, object]] = []
    for candidate in candidates:
        evidence = candidate.get("evidenceSessions") or []
        target = str(candidate.get("target") or "")
        reason = ""
        if not evidence:
            reason = "missing_evidence"
        elif is_forbidden_target(target):
            reason = "forbidden_home_skill"
        elif not is_git_tracked_overlay_target(target):
            reason = "not_git_tracked_overlay"
        if reason:
            dropped.append({**candidate, "dropReason": reason})
            continue
        action = dict(candidate)
        if action.get("kind") == "curate-delete":
            action["autoDelete"] = False
            action["summary"] = (
                str(action.get("summary") or "") + " (first-run: curate only, no auto-delete)"
            ).strip()
        kept_actions.append(action)

    verify_dir = run_dir / "verify"
    verify_dir.mkdir(exist_ok=True)
    phrases = {
        "schemaVersion": 1,
        "skeptic": "phrases",
        "kept": [a for a in kept_actions if a.get("phrase")],
        "dropped": [d for d in dropped if d.get("phrase")],
    }
    stale = {
        "schemaVersion": 1,
        "skeptic": "stale_lines",
        "kept": [a for a in kept_actions if a.get("kind") == "overlay-edit"],
        "dropped": [d for d in dropped if d.get("kind") == "overlay-edit"],
    }
    deletes = {
        "schemaVersion": 1,
        "skeptic": "deletes",
        "kept": [],
        "dropped": [
            d
            for d in dropped
            if d.get("kind") == "curate-delete"
        ]
        + [
            {**a, "dropReason": "first_run_curate_only"}
            for a in kept_actions
            if a.get("kind") == "curate-delete"
        ],
        "policy": "first-ever run is step/curate, never auto-delete",
    }
    # Deletes never stay in kept_actions for first-run offline path.
    kept_actions = [a for a in kept_actions if a.get("kind") != "curate-delete"]
    (verify_dir / "phrases.json").write_text(
        json.dumps(phrases, indent=2) + "\n", encoding="utf-8"
    )
    (verify_dir / "stale.json").write_text(
        json.dumps(stale, indent=2) + "\n", encoding="utf-8"
    )
    (verify_dir / "deletes.json").write_text(
        json.dumps(deletes, indent=2) + "\n", encoding="utf-8"
    )
    verdict = {
        "schemaVersion": 1,
        "kind": "learn-traces-verify",
        "kept": kept_actions,
        "dropped": dropped,
        "sessionsCited": sorted(cited),
    }
    (verify_dir / "verdict.json").write_text(
        json.dumps(verdict, indent=2) + "\n", encoding="utf-8"
    )
    return verdict


def _require_verify_artifacts(run_dir: Path) -> dict[str, object]:
    verdict_path = run_dir / "verify" / "verdict.json"
    if verdict_path.is_file():
        payload = json.loads(verdict_path.read_text(encoding="utf-8"))
        if isinstance(payload, dict):
            return payload
    # Assemble from skeptic files if present.
    kept: list[dict[str, object]] = []
    dropped: list[dict[str, object]] = []
    cited: set[str] = set()
    for name in ("phrases.json", "stale.json", "deletes.json"):
        path = run_dir / "verify" / name
        if not path.is_file():
            continue
        payload = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(payload, dict):
            continue
        for item in payload.get("kept") or []:
            if isinstance(item, dict):
                kept.append(item)
                for sid in item.get("evidenceSessions") or item.get("sessions") or []:
                    cited.add(str(sid))
        for item in payload.get("dropped") or []:
            if isinstance(item, dict):
                dropped.append(item)
    if not (run_dir / "verify").is_dir():
        raise FileNotFoundError("verify/ missing; run offline map-reduce or host skeptics first")
    if not any((run_dir / "verify" / name).is_file() for name in (
        "verdict.json",
        "phrases.json",
        "stale.json",
        "deletes.json",
    )):
        raise FileNotFoundError("verify artifacts missing; fail closed")
    return {
        "schemaVersion": 1,
        "kind": "learn-traces-verify",
        "kept": kept,
        "dropped": dropped,
        "sessionsCited": sorted(cited),
    }


def finalize(run_dir: Path) -> dict[str, object]:
    """Fail-closed report.md + actions.json from verify artifacts."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    kept_count = int(manifest.get("sessions_kept") or 0)
    verdict = _require_verify_artifacts(run_dir)
    raw_actions = [
        item for item in (verdict.get("kept") or []) if isinstance(item, dict)
    ]
    actions: list[dict[str, object]] = []
    rejected: list[dict[str, object]] = []
    cited: set[str] = set(str(s) for s in (verdict.get("sessionsCited") or []))
    for item in raw_actions:
        target = str(item.get("target") or "").strip()
        evidence = [str(s) for s in (item.get("evidenceSessions") or [])]
        cited.update(evidence)
        if not evidence:
            rejected.append({**item, "rejectReason": "missing_evidence"})
            continue
        if is_forbidden_target(target) or not is_git_tracked_overlay_target(target):
            rejected.append({**item, "rejectReason": "non_git_tracked_or_home"})
            continue
        actions.append(
            {
                "id": item.get("id") or f"act-{len(actions) + 1:03d}",
                "kind": item.get("kind") or "overlay-edit",
                "target": target,
                "summary": item.get("summary") or "",
                "phrase": item.get("phrase") or "",
                "evidenceSessions": evidence,
                "delivery": "git-pr",
                "autoApply": False,
                "autoDelete": False,
            }
        )
    coverage_line = (
        f"Coverage: {kept_count} sessions kept in manifest; "
        f"{len(cited)} cited in map-reduce-verify."
    )
    actions_doc = {
        "schemaVersion": 1,
        "kind": "learn-traces-actions",
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "sessions_kept": kept_count,
        "sessions_cited": sorted(cited),
        "coverageLine": coverage_line,
        "actions": actions,
        "rejected": rejected,
        "policy": {
            "forbidHomeSkills": True,
            "forbiddenPrefixes": list(FORBIDDEN_TARGET_MARKERS),
            "overlayPrefixes": list(OVERLAY_TARGET_PREFIXES),
            "delivery": "git-tracked overlay PR or patches/*.diff — never ~/.grok/skills",
            "firstRunCurateOnly": True,
        },
    }
    (run_dir / "actions.json").write_text(
        json.dumps(actions_doc, indent=2) + "\n", encoding="utf-8"
    )
    lines = [
        "# Learn traces report",
        "",
        coverage_line,
        "",
        "## Overview",
        "",
        f"- Sessions kept: {kept_count}",
        f"- Sessions cited: {len(cited)}",
        f"- Actions proposed: {len(actions)}",
        f"- Rejected (fail-closed): {len(rejected)}",
        "",
        "## Actions (git-tracked overlay only)",
        "",
    ]
    if not actions:
        lines.append("_No overlay actions survived verify._")
        lines.append("")
    else:
        lines.append("| id | target | summary | evidence |")
        lines.append("| --- | --- | --- | --- |")
        for action in actions:
            evidence = ", ".join(action.get("evidenceSessions") or [])  # type: ignore[arg-type]
            lines.append(
                f"| {action.get('id')} | `{action.get('target')}` | "
                f"{action.get('summary')} | {evidence} |"
            )
        lines.append("")
    lines.extend(
        [
            "## Policy",
            "",
            "- Targets are git-tracked `chaos-engine/` / `.chaos-engine/` source PR paths or `patches/*.diff`.",
            "- Never write learned skills under `~/.grok/skills`.",
            "- First-ever run is step/curate, never auto-delete.",
            "- Host TUI `/learn` is not required; this runner owns the portable contract.",
            "",
        ]
    )
    (run_dir / "report.md").write_text("\n".join(lines), encoding="utf-8")
    return actions_doc


def learn(
    run_dir: Path,
    *,
    home: Path | None = None,
    collect_first: bool = True,
    offline: bool = True,
) -> dict[str, object]:
    """One portable /learn entry: collect → prepare → map-reduce-verify → report."""
    run_dir = run_dir.resolve()
    run_dir.mkdir(parents=True, exist_ok=True)
    manifest_path = run_dir / "manifest.json"
    if collect_first or not manifest_path.is_file():
        collect(home or Path.home(), run_dir)
    layout = prepare(run_dir)
    if offline:
        offline_map_reduce_verify(run_dir)
    elif not (run_dir / "verify" / "verdict.json").is_file():
        # Host agents must fill the contract; do not invent verify.
        return {
            "schemaVersion": 1,
            "kind": "learn-traces-learn",
            "run_dir": str(run_dir),
            "layout": layout,
            "status": "awaiting_host_map_reduce_verify",
            "next": (
                "Run isolated subagents per prompts/; then "
                "`python3 .chaos-engine/learn_traces.py finalize --run-dir "
                f"{run_dir}`"
            ),
        }
    actions = finalize(run_dir)
    return {
        "schemaVersion": 1,
        "kind": "learn-traces-learn",
        "run_dir": str(run_dir),
        "status": "complete",
        "sessions_kept": actions.get("sessions_kept"),
        "actions": len(actions.get("actions") or []),
        "report": "report.md",
        "actionsPath": "actions.json",
        "coverageLine": actions.get("coverageLine"),
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="learn_traces.py")
    sub = parser.add_subparsers(dest="cmd", required=True)
    collect_p = sub.add_parser("collect", help="collect redacted host sessions")
    collect_p.add_argument("--home", type=Path, default=Path.home())
    collect_p.add_argument("--out", type=Path, required=True)
    prepare_p = sub.add_parser(
        "prepare", help="write map/reduce/verify file layout + prompts"
    )
    prepare_p.add_argument("--run-dir", type=Path, required=True)
    offline_p = sub.add_parser(
        "offline",
        help="deterministic map-reduce-verify fill (scripts/tests; no host TUI)",
    )
    offline_p.add_argument("--run-dir", type=Path, required=True)
    finalize_p = sub.add_parser(
        "finalize", help="fail-closed report.md + actions.json from verify/"
    )
    finalize_p.add_argument("--run-dir", type=Path, required=True)
    learn_p = sub.add_parser(
        "learn",
        help="portable /learn: collect + map-reduce-verify + report (default offline)",
    )
    learn_p.add_argument("--out", type=Path, required=True)
    learn_p.add_argument("--home", type=Path, default=Path.home())
    learn_p.add_argument(
        "--no-collect",
        action="store_true",
        help="reuse existing run-dir manifest/sessions",
    )
    learn_p.add_argument(
        "--host-agents",
        action="store_true",
        help="prepare only; do not run offline map-reduce (await subagents)",
    )
    args = parser.parse_args(argv)
    if args.cmd == "collect":
        manifest = collect(args.home.expanduser(), args.out)
        print(json.dumps({"run_dir": str(args.out), **manifest}))
        return 0
    if args.cmd == "prepare":
        layout = prepare(args.run_dir)
        print(json.dumps({"run_dir": str(args.run_dir), **layout}))
        return 0
    if args.cmd == "offline":
        verdict = offline_map_reduce_verify(args.run_dir)
        print(json.dumps({"run_dir": str(args.run_dir), **verdict}))
        return 0
    if args.cmd == "finalize":
        try:
            actions = finalize(args.run_dir)
        except FileNotFoundError as error:
            print(json.dumps({"ok": False, "error": str(error)}), file=sys.stderr)
            return 2
        print(json.dumps({"run_dir": str(args.run_dir), **actions}))
        return 0
    if args.cmd == "learn":
        result = learn(
            args.out,
            home=args.home.expanduser(),
            collect_first=not args.no_collect,
            offline=not args.host_agents,
        )
        print(json.dumps(result))
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
