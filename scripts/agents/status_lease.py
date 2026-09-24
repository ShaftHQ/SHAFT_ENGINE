#!/usr/bin/env python3
"""One status channel per in-flight PR (#6163).

The unattended watch (``watch_pr_checks.py --status-lease``) owns CI status
for PR N and records ``.chaos-engine/runtime/status-lease-<N>.json``. While
that lease is live, scheduled process-owner status routines and parent
re-entry narration stay silent unless the watch reached ``red`` / ``merged``
or the owner explicitly asked:

    python3 scripts/agents/status_lease.py routine --pr N [--last-reported red] [--owner-asked]

prints nothing (exit 0) while suppressed. Stdlib only; same on every host.
"""

from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path

LEASE_MAX_AGE_SECONDS = 6 * 60 * 60
REPORTABLE_STATES = frozenset({"red", "merged"})


def lease_path(root: Path, pr: int) -> Path:
    """Shared status lease for one in-flight PR."""
    return root / ".chaos-engine" / "runtime" / f"status-lease-{pr}.json"


def write_lease(root: Path, pr: int, *, pid: int, state: str, digest_path: str | None = None, now: float | None = None) -> Path:
    """Register (or update) the single status channel for PR `pr`."""
    path = lease_path(root, pr)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"pr": pr, "pid": pid, "state": state, "digest_path": digest_path, "updated_at": time.time() if now is None else now}
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")
    return path


def read_lease(root: Path, pr: int) -> dict | None:
    """Return the lease payload, or None when absent or unreadable."""
    path = lease_path(root, pr)
    if not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    return payload if isinstance(payload, dict) else None


def pid_alive(pid: int) -> bool:
    """Best-effort liveness probe for the watch process."""
    if pid <= 0:
        return False
    if os.name == "nt":
        return True
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True


def lease_is_live(lease: dict | None, *, now: float | None = None, alive=pid_alive) -> bool:
    """A lease is live while its watch pid runs and it is younger than LEASE_MAX_AGE_SECONDS."""
    if not lease:
        return False
    current = time.time() if now is None else now
    age = current - float(lease.get("updated_at") or 0)
    return age <= LEASE_MAX_AGE_SECONDS and alive(int(lease.get("pid") or 0))


def routine_status_line(root: Path, pr: int, *, last_reported: str | None = None, owner_asked: bool = False, now: float | None = None, alive=pid_alive) -> str:
    """Scheduled status routine body: '' while a live watch owns status and nothing reportable changed."""
    lease = read_lease(root, pr)
    if owner_asked or not lease_is_live(lease, now=now, alive=alive):
        return f"status due for PR #{pr}: no live watch lease; read the watch_pr_checks digest once"
    state = str(lease.get("state") or "pending")
    if state in REPORTABLE_STATES and state != last_reported:
        return f"PR #{pr} {state} (watch digest: {lease.get('digest_path') or 'n/a'})"
    return ""


def main(argv: list[str] | None = None) -> int:
    """CLI: acquire / release / routine. The routine prints nothing while suppressed."""
    parser = argparse.ArgumentParser(description="CLI for managing PR status leases")
    subparsers = parser.add_subparsers(dest="command", required=True)
    acquire_parser = subparsers.add_parser("acquire", help="Acquire a lease")
    acquire_parser.add_argument("--pr", type=int, required=True)
    acquire_parser.add_argument("--root", type=Path, default=Path.cwd())
    acquire_parser.add_argument("--pid", type=int, default=os.getppid())
    acquire_parser.add_argument("--state", type=str, default="pending")
    acquire_parser.add_argument("--digest-path", type=str, default=None)

    release_parser = subparsers.add_parser("release", help="Release a lease")
    release_parser.add_argument("--pr", type=int, required=True)
    release_parser.add_argument("--root", type=Path, default=Path.cwd())

    routine_parser = subparsers.add_parser("routine", help="Run the status routine")
    routine_parser.add_argument("--pr", type=int, required=True)
    routine_parser.add_argument("--root", type=Path, default=Path.cwd())
    routine_parser.add_argument("--last-reported", type=str, default=None)
    routine_parser.add_argument("--owner-asked", action="store_true")

    args = parser.parse_args(argv)
    if args.command == "acquire":
        write_lease(args.root, args.pr, pid=args.pid, state=args.state, digest_path=args.digest_path)
        return 0
    if args.command == "release":
        lease_path(args.root, args.pr).unlink(missing_ok=True)
        return 0
    if args.command == "routine":
        line = routine_status_line(args.root, args.pr, last_reported=args.last_reported, owner_asked=args.owner_asked)
        if line:
            print(line)
        return 0
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
