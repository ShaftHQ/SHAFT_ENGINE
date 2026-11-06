#!/usr/bin/env python3
"""List unresolved review threads by author before calling a PR "waiting on CI".

Merge commands in this repo use ``--match-head-commit`` so a stale approval
cannot merge a newer head. Generic only: no product auto-merge config.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import defaultdict
from typing import Any


def unresolved_threads_by_author(threads: list[dict[str, Any]] | None) -> dict[str, list[str]]:
    """Group unresolved threads, including code-quality bots, by author login."""
    grouped: dict[str, list[str]] = defaultdict(list)
    for thread in threads or []:
        if not isinstance(thread, dict):
            continue
        if thread.get("isResolved") or thread.get("resolved"):
            continue
        author = str(thread.get("author") or thread.get("authorLogin") or "unknown")
        label = str(thread.get("title") or thread.get("path") or thread.get("id") or "thread")
        grouped[author].append(label)
    return dict(grouped)


def format_unresolved(grouped: dict[str, list[str]]) -> str:
    if not grouped:
        return "no unresolved review threads"
    lines = ["unresolved review threads (do not report waiting-on-CI while these are open):"]
    for author in sorted(grouped):
        lines.append(f"- {author}: {len(grouped[author])}")
    return "\n".join(lines)


def merge_command(pr: int | str, head_sha: str) -> str:
    """Documented merge command. Refuses an empty head."""
    sha = (head_sha or "").strip()
    if not sha:
        raise ValueError("merge command requires the head sha (--match-head-commit)")
    return f"gh pr merge {pr} --merge --match-head-commit {sha}"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--threads-json", help="JSON list of review threads")
    parser.add_argument("--pr", help="PR number for the documented merge command")
    parser.add_argument("--head", help="head sha for --match-head-commit")
    args = parser.parse_args(argv)
    if args.threads_json:
        payload = json.loads(args.threads_json)
        print(format_unresolved(unresolved_threads_by_author(payload)))
    if args.pr:
        print(merge_command(args.pr, args.head or ""))
    if not args.threads_json and not args.pr:
        parser.error("pass --threads-json and/or --pr --head")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, json.JSONDecodeError) as exc:
        print(f"merge_ready: {exc}", file=sys.stderr)
        raise SystemExit(2)
