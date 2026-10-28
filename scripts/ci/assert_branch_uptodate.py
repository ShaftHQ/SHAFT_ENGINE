#!/usr/bin/env python3
"""Exit 0 when HEAD already contains the base tip (merge-queue same_tree can skip).

A pull request that is behind main builds a two-parent merge whose tree differs
from the PR head, so the merge queue re-runs the full PR Gate (#7439 / #7184).
Agents should rebase or merge the base, push, and wait for a green PR Gate
before ``gh pr merge --auto`` / enqueue when this exits 1.

Always fetch the base tip before ``merge-base --is-ancestor`` so forks and
shallow/stale clones cannot false-pass or false-fail. Pass ``--fetch-url`` for
fork PRs (typically ``github.event.pull_request.base.repo.clone_url``).

Usage:
  python3 scripts/ci/assert_branch_uptodate.py
  python3 scripts/ci/assert_branch_uptodate.py --base origin/main
  python3 scripts/ci/assert_branch_uptodate.py --base origin/main --fetch-url https://github.com/ShaftHQ/SHAFT_ENGINE.git
"""

from __future__ import annotations

import argparse
import shutil
import subprocess  # nosec B404 - fixed git argv, never a shell.
import sys


def _git(args: list[str]) -> subprocess.CompletedProcess[str]:
    git = shutil.which("git")
    if git is None:
        raise SystemExit("git not found")
    return subprocess.run(  # nosec B603 - fixed argv, resolved executable
        [git, *args],
        check=False,
        capture_output=True,
        text=True,
    )


def fetch_base(base_ref: str, fetch_url: str | None = None) -> None:
    """Refresh ``base_ref`` from origin (or ``fetch_url``) before the ancestor check.

    For fork PRs, Actions checkouts often lack the base remote tip; fetching from
    the base repository clone URL (``--fetch-url``) avoids false behind/uptodate.
    """
    # origin/main -> refspec main:refs/remotes/origin/main
    if "/" in base_ref:
        remote, _, branch = base_ref.partition("/")
    else:
        remote, branch = "origin", base_ref
    refspec = f"+refs/heads/{branch}:refs/remotes/{remote}/{branch}"
    if fetch_url:
        args = ["fetch", "--no-tags", "--depth", "1", fetch_url, refspec]
    else:
        args = ["fetch", "--no-tags", "--depth", "1", remote, f"+refs/heads/{branch}:refs/remotes/{remote}/{branch}"]
    result = _git(args)
    if result.returncode != 0:
        err = (result.stderr or result.stdout or "").strip()
        raise SystemExit(f"git fetch failed for {base_ref}: {err or result.returncode}")


def is_uptodate(base_ref: str, head_ref: str = "HEAD") -> bool:
    """True when every commit reachable from base_ref is reachable from head_ref."""
    probe = _git(["merge-base", "--is-ancestor", base_ref, head_ref])
    return probe.returncode == 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", default="origin/main", help="Base tip the branch must contain")
    parser.add_argument("--head", default="HEAD", help="Branch tip to check")
    parser.add_argument(
        "--fetch-url",
        default=None,
        help="Optional base-repo clone URL (fork PRs); fetch before merge-base",
    )
    parser.add_argument(
        "--skip-fetch",
        action="store_true",
        help="Skip the pre-check fetch (tests / already-freshened remotes only)",
    )
    args = parser.parse_args(argv)
    if not args.skip_fetch:
        fetch_base(args.base, args.fetch_url)
    if is_uptodate(args.base, args.head):
        print(f"uptodate: {args.head} contains {args.base}")
        return 0
    print(
        f"behind: {args.head} does not contain {args.base}; "
        "rebase/merge the base and re-run the PR Gate before enqueueing "
        "so the merge queue can same_tree-skip (#7439).",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
