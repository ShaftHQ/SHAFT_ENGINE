"""Workstation local coding-agent report and preflight helpers (#5017)."""

from __future__ import annotations

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
    """True when SHAFT/TestNG output shows a failed test despite Maven exit 0."""
    lowered = text.lower()
    if "failures: 0" in lowered and "errors: 0" in lowered:
        return False
    if "status: failed" in lowered:
        return True
    if "failures:" in lowered:
        for token in lowered.replace(",", " ").split():
            if token.startswith("failures:"):
                continue
        if "failures: 0" not in lowered and "<<< failure" in lowered:
            return True
    return "<<< failure" in lowered or "status: failed" in lowered


def validate_report(data: dict) -> list[str]:
    """Return blockers for a report payload."""
    blockers: list[str] = []
    if not isinstance(data, dict):
        return ["report must be an object"]
    for key in REQUIRED_REPORT_KEYS:
        if key not in data:
            blockers.append(f"missing key {key}")
    allowed = data.get("files_allowed")
    if not isinstance(allowed, list) or not allowed:
        blockers.append("file allowlist is required")
    changed = data.get("files_changed")
    if isinstance(allowed, list) and isinstance(changed, list):
        allowed_set = {str(item).replace("\\", "/").lower() for item in allowed}
        for item in changed:
            normalized = str(item).replace("\\", "/").lower()
            if normalized not in allowed_set:
                blockers.append(f"changed file outside allowlist: {item}")
    loopback = str(data.get("loopback") or "")
    if loopback and not loopback.startswith("127.0.0.1"):
        blockers.append("loopback must be 127.0.0.1")
    return blockers


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
