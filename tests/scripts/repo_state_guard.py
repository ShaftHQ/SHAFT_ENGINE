"""Snapshot the invoking checkout so the suite can prove it never mutated it (#6239)."""

from __future__ import annotations

import os
import subprocess  # nosec B404 - fixed git argv, never a shell.
from pathlib import Path

PROTECTED_CHECKOUTS_ENV = "CHAOS_ENGINE_PROTECTED_CHECKOUTS"


def _git(root: Path, *arguments: str) -> str | None:
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed git argv.
            ["git", "-c", "core.longpaths=true", *arguments],
            cwd=str(root),
            capture_output=True,
            text=True,
            timeout=120,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return completed.stdout if completed.returncode == 0 else None


def checkout_roots(start: Path) -> list[Path]:
    """Return the invoking worktree and its primary checkout, deduplicated."""
    top = _git(start, "rev-parse", "--show-toplevel")
    if top is None:
        return []
    roots = [Path(top.strip()).resolve()]
    common = _git(start, "rev-parse", "--path-format=absolute", "--git-common-dir")
    if common is not None:
        primary = Path(common.strip()).resolve().parent
        if primary not in roots and (primary / ".git").exists():
            roots.append(primary)
    return roots


def protect(roots: list[Path]) -> None:
    """Tell ChaosEngine session-worktree code never to mutate these checkouts."""
    existing = [item for item in os.environ.get(PROTECTED_CHECKOUTS_ENV, "").split(os.pathsep) if item]
    merged = list(dict.fromkeys([*existing, *(str(root) for root in roots)]))
    os.environ[PROTECTED_CHECKOUTS_ENV] = os.pathsep.join(merged)


def snapshot(root: Path) -> dict[str, str | None]:
    """Branch, HEAD, porcelain status, and registered worktrees of one checkout."""
    branch = _git(root, "symbolic-ref", "-q", "--short", "HEAD")
    worktrees = _git(root, "worktree", "list", "--porcelain")
    return {
        "branch": branch.strip() if branch is not None else "(detached)",
        "head": (_git(root, "rev-parse", "HEAD") or "").strip() or None,
        "status": _git(root, "status", "--porcelain=v1", "--untracked-files=normal"),
        "worktrees": "\n".join(
            line for line in (worktrees or "").splitlines() if line.startswith("worktree ")
        ),
    }


def differences(before: dict[str, str | None], after: dict[str, str | None]) -> list[str]:
    """Human-readable list of every snapshot field the run changed."""
    changed = []
    for key in ("branch", "head", "status", "worktrees"):
        if before.get(key) != after.get(key):
            changed.append(f"{key}: {before.get(key)!r} -> {after.get(key)!r}")
    return changed
