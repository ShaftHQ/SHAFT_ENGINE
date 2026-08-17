#!/usr/bin/env python3
"""Resolve the shared MemPalace path from any worktree."""

from __future__ import annotations

import argparse
import os
import shutil
# Only used to run one fixed git command (list-args, no shell) with the
# executable resolved to an absolute path below.
import subprocess  # nosec B404
import sys
from pathlib import Path


def find_shared_mempalace(cwd: Path) -> Path:
    """Return the shared palace path under the main checkout git directory."""
    if "SHAFT_MEMPALACE" in os.environ:
        configured = os.environ["SHAFT_MEMPALACE"].strip()
        if not configured:
            raise RuntimeError("SHAFT_MEMPALACE must not be blank")
        palace = Path(configured).expanduser()
        if not palace.is_absolute():
            raise RuntimeError("SHAFT_MEMPALACE must be absolute")
        return palace.resolve()
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    # Absolute executable path, fixed internal arguments, no shell.
    completed = subprocess.run(  # nosec B603
        [git_executable, "rev-parse", "--git-common-dir"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    common_dir = Path(completed.stdout.strip())
    if not common_dir.is_absolute():
        common_dir = (cwd / common_dir).resolve()
    return common_dir / "chaos-engine" / "mempalace"


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    return argparse.ArgumentParser(description=__doc__)


def main(argv: list[str] | None = None, cwd: Path | None = None) -> int:
    """Run the CLI."""
    build_parser().parse_args(argv)
    working_directory = cwd or Path.cwd()
    try:
        print(find_shared_mempalace(working_directory))
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
