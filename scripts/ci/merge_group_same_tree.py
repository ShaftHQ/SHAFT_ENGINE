#!/usr/bin/env python3
"""Print true when HEAD is a two-parent merge whose tree equals the second parent.

The merge queue rebuilds an up-to-date pull request as a merge commit. That
commit's tree matches the pull-request head (the second parent) exactly when
the base branch has not moved, so the pull-request gate already proved it.
Anything else, including a missing parent, prints false.
"""

from __future__ import annotations

import shutil
import subprocess
import sys


def git_output(*args: str) -> str | None:
    git = shutil.which("git")
    if git is None:
        return None
    result = subprocess.run(  # nosec B603 - fixed git argv, no shell.
        [git, *args],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout


def same_tree() -> bool:
    """True only for a two-parent merge whose tree is the second parent's tree."""
    listed = git_output("rev-list", "--parents", "-n", "1", "HEAD")
    if listed is None:
        return False
    # rev-list prints HEAD, then the first parent, then the second parent.
    if len(listed.split()) != 3:
        return False
    head_tree = git_output("rev-parse", "--verify", "--quiet", "HEAD^{tree}")
    second_tree = git_output("rev-parse", "--verify", "--quiet", "HEAD^2^{tree}")
    if head_tree is None or second_tree is None:
        return False
    return head_tree.strip() == second_tree.strip()


def main() -> int:
    print("true" if same_tree() else "false")
    return 0


if __name__ == "__main__":
    sys.exit(main())
