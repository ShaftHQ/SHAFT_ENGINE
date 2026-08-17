"""Workstation local coding-agent report and preflight helpers (#5017)."""

from __future__ import annotations

import re

REQUIRED_REPORT_KEYS = (
    "ok",
    "model",
    "worktree",
    "files_allowed",
    "files_changed",
    "commit",
    "test_command",
    "test_exit",
    "elapsed_ms",
    "loopback",
    "blockers",
)


def preflight(worktree: str, spec_path: str, files_allowed: list[str], push: bool = False) -> list[str]:
    """Return blockers. Empty list means the run may start."""
    blockers: list[str] = []
    if not str(worktree or "").strip():
        blockers.append("worktree is required")
    if not str(spec_path or "").strip():
        blockers.append("spec path is required")
    if not files_allowed:
        blockers.append("file allowlist is required")
    if push:
        blockers.append("push is forbidden")
    return blockers


def surefire_failed(text: str) -> bool:
    """True when SHAFT/TestNG output shows a failed or empty test run."""
    lowered = text.lower()
    if "status: failed" in lowered or "<<< failure" in lowered:
        return True
    fail_counts = [int(value) for value in re.findall(r"failures:\s*(\d+)", lowered)]
    error_counts = [int(value) for value in re.findall(r"errors:\s*(\d+)", lowered)]
    if any(count > 0 for count in fail_counts + error_counts):
        return True
    test_counts = [int(value) for value in re.findall(r"tests run:\s*(\d+)", lowered)]
    return bool(test_counts) and all(count == 0 for count in test_counts)


def _normalize_path(path: str) -> str:
    return str(path).replace("\\", "/").lower()


def as_path_list(value: object) -> list[str]:
    """PowerShell ConvertTo-Json unwraps a one-element array into a string."""
    if value is None:
        return []
    if isinstance(value, list):
        return [str(item) for item in value]
    if isinstance(value, str):
        text = value.strip()
        return [text] if text else []
    return [str(value)]


def changed_paths_from_git_status(status_text: str) -> list[str]:
    """Parse `git status --porcelain` paths, including the new side of renames."""
    paths: list[str] = []
    for raw_line in str(status_text or "").splitlines():
        line = raw_line.rstrip("\n")
        if len(line) < 4:
            continue
        payload = line[3:]
        if " -> " in payload:
            payload = payload.split(" -> ", 1)[1]
        cleaned = payload.strip().strip('"')
        normalized = cleaned.replace("\\", "/").lower()
        name = normalized.rsplit("/", 1)[-1]
        if name.startswith(".aider") or "/.aider" in f"/{normalized}":
            continue
        paths.append(cleaned)
    return paths


def allowlist_violations(changed: list[str], allowed: list[str]) -> list[str]:
    allowed_set = {_normalize_path(item) for item in allowed}
    blockers: list[str] = []
    for item in changed:
        if _normalize_path(item) not in allowed_set:
            blockers.append(f"changed file outside allowlist: {item}")
    return blockers


def loopback_valid(value: str) -> bool:
    return bool(re.fullmatch(r"127\.0\.0\.1:[0-9]+", str(value or "")))


def validate_report(data: dict) -> list[str]:
    """Return blockers for a report payload."""
    blockers: list[str] = []
    if not isinstance(data, dict):
        return ["report must be an object"]
    for key in REQUIRED_REPORT_KEYS:
        if key not in data:
            blockers.append(f"missing key {key}")
    allowed = as_path_list(data.get("files_allowed"))
    if not allowed:
        blockers.append("file allowlist is required")
    changed = as_path_list(data.get("files_changed"))
    if allowed:
        blockers.extend(allowlist_violations(changed, allowed))
    loopback = str(data.get("loopback") or "")
    if not loopback_valid(loopback):
        blockers.append("loopback must be 127.0.0.1:<port>")
    return blockers


def write_report(path: str | object, data: dict) -> list[str]:
    """Normalize arrays, persist report.json, and return validation blockers."""
    from pathlib import Path
    import json

    if not isinstance(data, dict):
        raise TypeError("report must be an object")
    normalized = dict(data)
    normalized["files_allowed"] = as_path_list(data.get("files_allowed"))
    normalized["files_changed"] = as_path_list(data.get("files_changed"))
    blockers = [str(item) for item in as_path_list(data.get("blockers"))]
    extra = validate_report(normalized)
    if extra:
        normalized["ok"] = False
        for item in extra:
            if item not in blockers:
                blockers.append(item)
    normalized["blockers"] = blockers
    Path(path).write_text(json.dumps(normalized, indent=2) + "\n", encoding="utf-8")
    return extra


def main(argv: list[str] | None = None) -> int:
    import argparse
    import json
    from pathlib import Path

    parser = argparse.ArgumentParser(description="Local coding-agent preflight and report checks")
    sub = parser.add_subparsers(dest="cmd", required=True)
    pre = sub.add_parser("preflight")
    pre.add_argument("--worktree", default="")
    pre.add_argument("--spec", default="")
    pre.add_argument("--allowlist-json", default="[]")
    pre.add_argument("--push", action="store_true")
    val = sub.add_parser("validate")
    val.add_argument("--report", required=True)
    fire = sub.add_parser("surefire")
    fire.add_argument("--file", required=True)
    chg = sub.add_parser("changed")
    chg.add_argument("--status-file", default="")
    chg.add_argument("--head-file", default="")
    wrt = sub.add_parser("write")
    wrt.add_argument("--input", required=True)
    wrt.add_argument("--out", required=True)
    args = parser.parse_args(argv)

    if args.cmd == "preflight":
        allowed = json.loads(args.allowlist_json)
        if isinstance(allowed, str):
            allowed = [allowed]
        blockers = preflight(args.worktree, args.spec, list(allowed), push=args.push)
        if blockers:
            print("\n".join(blockers))
            return 2
        print("ok")
        return 0

    if args.cmd == "surefire":
        text = Path(args.file).read_text(encoding="utf-8", errors="replace")
        if surefire_failed(text):
            print("failed")
            return 1
        print("passed")
        return 0

    if args.cmd == "changed":
        paths: list[str] = []
        if args.status_file:
            paths.extend(
                changed_paths_from_git_status(
                    Path(args.status_file).read_text(encoding="utf-8", errors="replace")
                )
            )
        if args.head_file:
            for line in Path(args.head_file).read_text(encoding="utf-8", errors="replace").splitlines():
                item = line.strip()
                if not item:
                    continue
                normalized = item.replace("\\", "/").lower()
                name = normalized.rsplit("/", 1)[-1]
                if name.startswith(".aider") or "/.aider" in f"/{normalized}":
                    continue
                paths.append(item)
        seen: set[str] = set()
        for item in paths:
            key = _normalize_path(item)
            if key in seen:
                continue
            seen.add(key)
            print(item)
        return 0

    if args.cmd == "write":
        payload = json.loads(Path(args.input).read_text(encoding="utf-8"))
        extra = write_report(args.out, payload)
        if extra:
            print("\n".join(extra))
            return 2
        print("ok")
        return 0

    payload = json.loads(Path(args.report).read_text(encoding="utf-8"))
    blockers = validate_report(payload)
    if blockers:
        print("\n".join(blockers))
        return 2
    print("ok")
    return 0


def build_report(
    *,
    ok: bool,
    model: str,
    worktree: str,
    files_allowed: list[str],
    files_changed: list[str],
    commit: str,
    test_command: str,
    test_exit: int,
    elapsed_ms: int,
    loopback: str,
    blockers: list[str],
) -> dict:
    return {
        "ok": ok,
        "model": model,
        "worktree": worktree,
        "files_allowed": list(files_allowed),
        "files_changed": list(files_changed),
        "commit": commit,
        "test_command": test_command,
        "test_exit": test_exit,
        "elapsed_ms": elapsed_ms,
        "loopback": loopback,
        "blockers": list(blockers),
    }


if __name__ == "__main__":
    raise SystemExit(main())
