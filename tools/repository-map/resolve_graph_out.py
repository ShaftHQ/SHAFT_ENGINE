#!/usr/bin/env python3
"""Resolve the shared graphify-out/ cache path from any worktree."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


def find_shared_graph_out(cwd: Path) -> Path:
    """Return the shared graphify-out/ path under the main checkout root."""
    completed = subprocess.run(
        ["git", "rev-parse", "--git-common-dir"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    common_dir = Path(completed.stdout.strip())
    if not common_dir.is_absolute():
        common_dir = (cwd / common_dir).resolve()
    return common_dir.parent / "graphify-out"


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Exit 0 only if the shared cache exists and is non-empty.",
    )
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    graph_out = find_shared_graph_out(Path.cwd())
    if args.check:
        if graph_out.is_dir() and any(graph_out.iterdir()):
            print(graph_out)
            return 0
        print(
            "absent - build it from the main checkout "
            "(see tools/repository-map/README.md) or fall back to rg/.memory",
            file=sys.stderr,
        )
        return 1
    print(graph_out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
