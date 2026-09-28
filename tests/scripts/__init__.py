"""Arm the repository guard for every tests.scripts run (#6239, #6249).

Some tests drive SessionStart, whose session-worktree setup checks out the
default branch, hard-resets it, and cleans it. Against the real checkout that
destroys uncommitted work. Importing this package (unittest and pytest both do)
does two things:

1. Sets CHAOS_ENGINE_TEST_REPO_GUARD to this repository's git common dir, so
   scripts/agents/session_worktree.py refuses checkout/reset/clean/worktree
   changes anywhere except temp fixtures. Child processes inherit it. Store
   refresh treats that variable as a test run and does not spawn.
2. Records branch, HEAD, porcelain status, and registered worktrees, and at
   exit fails the run (exit 70) if any of them changed.
"""

from __future__ import annotations

import atexit
import os
import subprocess  # nosec B404 - fixed read-only git queries.
import sys
from pathlib import Path

from . import repo_state_guard

GUARD_ENV = "CHAOS_ENGINE_TEST_REPO_GUARD"
REPOSITORY = Path(__file__).resolve().parents[2]


def _git(*arguments: str) -> str | None:
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git argv.
            ["git", *arguments],
            cwd=str(REPOSITORY),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return completed.stdout if completed.returncode == 0 else None


def repository_state() -> dict | None:
    head, branch, status = (
        _git("rev-parse", "HEAD"),
        _git("rev-parse", "--abbrev-ref", "HEAD"),
        _git("status", "--porcelain"),
    )
    if head is None or branch is None or status is None:
        return None
    return {
        "head": head.strip(),
        "branch": branch.strip(),
        "dirty": frozenset(line for line in status.splitlines() if line.strip()),
    }


def destroyed_work(before: dict, after: dict) -> list[str]:
    """Name what the run destroyed. New files are fine; lost edits are not."""
    reasons = []
    if before["branch"] != after["branch"]:
        reasons.append(f"branch moved {before['branch']} -> {after['branch']}")
    if before["head"] != after["head"]:
        reasons.append(f"HEAD moved {before['head'][:12]} -> {after['head'][:12]}")
    lost = sorted(before["dirty"] - after["dirty"])
    if lost:
        reasons.append("local edits discarded: " + ", ".join(lost[:10]))
    return reasons


def _arm() -> None:
    common = _git("rev-parse", "--git-common-dir")
    if common is None:
        return
    path = Path(common.strip())
    if not path.is_absolute():
        path = REPOSITORY / path
    protected = [item for item in os.environ.get(GUARD_ENV, "").split(os.pathsep) if item]
    resolved = str(path.resolve())
    if resolved not in protected:
        protected.append(resolved)
    os.environ[GUARD_ENV] = os.pathsep.join(protected)
    roots = repo_state_guard.checkout_roots(REPOSITORY) or [REPOSITORY]
    before = {}
    for root in roots:
        earlier = repo_state_guard.snapshot(root)
        if earlier.get("head"):
            before[root] = earlier
    if not before:
        return
    owner = os.getpid()

    def verify() -> None:
        if os.getpid() != owner:
            return
        reasons = []
        for root, earlier in before.items():
            reasons.extend(
                f"{root}: {item}"
                for item in repo_state_guard.differences(earlier, repo_state_guard.snapshot(root))
            )
        if reasons:
            print(
                "tests.scripts changed the repository under test (#6239): "
                + "; ".join(reasons),
                file=sys.stderr,
            )
            sys.stderr.flush()
            os._exit(70)

    atexit.register(verify)


_arm()
