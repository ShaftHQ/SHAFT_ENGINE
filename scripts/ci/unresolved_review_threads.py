#!/usr/bin/env python3
"""List unresolved PR review threads that block merge-queue enqueue (#7736).

Even when every check is green, GitHub refuses enqueuePullRequest while any
review conversation stays unresolved (Codacy / code-quality bots included).
Call this before ``gh pr merge --auto`` / enqueue. Exit 1 when any thread is
open so babysitters fail closed.

Usage:
  python3 scripts/ci/unresolved_review_threads.py --repo OWNER/NAME --pr N
  python3 scripts/ci/unresolved_review_threads.py --fixture path.json
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess  # nosec B404 - fixed gh argv only.
import sys
from pathlib import Path
from typing import Any, Mapping, Sequence


QUERY = """
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100) {
        nodes {
          isResolved
          isOutdated
          path
          comments(first: 1) {
            nodes { body author { login } }
          }
        }
      }
    }
  }
}
"""


def unresolved_threads(payload: Mapping[str, Any]) -> list[dict[str, Any]]:
    """Return unresolved thread summaries from a GraphQL pullRequest payload."""
    root = payload.get("data") or payload
    repository = root.get("repository") or {}
    pull = repository.get("pullRequest") or root.get("pullRequest") or {}
    nodes = ((pull.get("reviewThreads") or {}).get("nodes")) or []
    open_threads: list[dict[str, Any]] = []
    for node in nodes:
        if node.get("isResolved"):
            continue
        comments = ((node.get("comments") or {}).get("nodes")) or []
        first = comments[0] if comments else {}
        author = (first.get("author") or {}).get("login") or ""
        body = str(first.get("body") or "").strip().splitlines()[:1]
        open_threads.append(
            {
                "path": node.get("path") or "",
                "outdated": bool(node.get("isOutdated")),
                "author": author,
                "summary": body[0][:160] if body else "",
            }
        )
    return open_threads


def _run_gh(args: Sequence[str]) -> str:
    gh = shutil.which("gh")
    if gh is None:
        raise SystemExit("gh CLI not found")
    return subprocess.run(  # nosec B603 - absolute gh, fixed argv
        [gh, *args],
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def fetch_threads(repo: str, number: int) -> list[dict[str, Any]]:
    owner, _, name = repo.partition("/")
    raw = _run_gh(
        [
            "api",
            "graphql",
            "-f",
            f"query={QUERY}",
            "-F",
            f"owner={owner}",
            "-F",
            f"name={name}",
            "-F",
            f"number={number}",
        ]
    )
    return unresolved_threads(json.loads(raw))


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--repo", help="OWNER/NAME")
    parser.add_argument("--pr", type=int, help="pull request number")
    parser.add_argument("--fixture", type=Path, help="GraphQL JSON fixture (tests)")
    args = parser.parse_args(argv)

    if args.fixture:
        payload = json.loads(args.fixture.read_text(encoding="utf-8"))
        open_threads = unresolved_threads(payload)
    else:
        if not args.repo or not args.pr:
            parser.error("--repo and --pr are required unless --fixture is set")
        open_threads = fetch_threads(args.repo, args.pr)

    if not open_threads:
        print("unresolved-review-threads: none")
        return 0
    print(f"unresolved-review-threads: {len(open_threads)}")
    for thread in open_threads:
        print(
            f"- {thread['path'] or '(no path)'} "
            f"[{thread['author'] or 'unknown'}] {thread['summary']}"
        )
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
