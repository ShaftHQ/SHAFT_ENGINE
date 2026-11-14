#!/usr/bin/env python3
"""Run unittest from a named worktree when cwd is another checkout.

The empty cwd entry (``sys.path[0]``) is searched before ``PYTHONPATH`` and
is resolved at import time. ``PYTHONPATH`` alone is not sufficient while cwd
is another checkout. This process changes to ``--worktree`` before unittest
imports tests.

Usage:
  python3 scripts/ci/worktree_unittest.py --worktree <worktree> <tests>
"""

from __future__ import annotations

import os
import sys
import unittest
from pathlib import Path


def _worktree_and_labels(argv: list[str]) -> tuple[Path | None, list[str], str | None]:
    worktree: str | None = None
    labels: list[str] = []
    index = 0
    while index < len(argv):
        arg = argv[index]
        if arg == "--worktree":
            if index + 1 >= len(argv):
                return None, labels, "missing --worktree PATH"
            worktree = argv[index + 1]
            index += 2
            continue
        if arg.startswith("--worktree="):
            worktree = arg.split("=", 1)[1]
            index += 1
            continue
        labels.append(arg)
        index += 1
    if not worktree:
        return None, labels, "missing --worktree PATH"
    return Path(worktree), labels, None


def _unittest_status(program: unittest.TestProgram) -> int:
    result = program.result
    if result.testsRun == 0 and len(result.skipped) == 0:
        return 5
    return 0 if result.wasSuccessful() else 1


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    worktree, labels, error = _worktree_and_labels(args)
    if error is not None or worktree is None:
        print(error or "missing --worktree PATH", file=sys.stderr)
        return 2
    path = worktree.expanduser()
    if not path.is_absolute():
        path = Path.cwd() / path
    path = path.resolve()
    if not path.is_dir():
        print(f"worktree is not a directory: {path}", file=sys.stderr)
        return 2
    os.chdir(path)
    # Script-path execution stores this file's directory in sys.path[0].
    # The empty cwd entry is what beats PYTHONPATH, so it has to lead.
    if not sys.path or sys.path[0] != "":
        sys.path.insert(0, "")
    try:
        program = unittest.main(module=None, argv=["unittest", *labels], exit=False)
    except SystemExit as exc:
        code = exc.code
        if code is None:
            return 0
        return code if isinstance(code, int) else 1
    return _unittest_status(program)


if __name__ == "__main__":
    raise SystemExit(main())
