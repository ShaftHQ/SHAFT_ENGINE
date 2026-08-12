#!/usr/bin/env python3
"""Compatibility adapter for the canonical repository-aware PR watcher."""

from pathlib import Path
import sys


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
if str(REPOSITORY_ROOT) not in sys.path:
    sys.path.insert(0, str(REPOSITORY_ROOT))

from scripts.agents.watch_pr_checks import main  # noqa: E402


if __name__ == "__main__":
    raise SystemExit(main())
