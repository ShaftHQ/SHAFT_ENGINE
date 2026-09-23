#!/usr/bin/env python3
"""Resolve the shared MemPalace path from any worktree.

SHAFT_MEMPALACE must be absolute. The palace lives under git-common-dir at
``chaos-engine/mempalace``. ``CHAOS_ENGINE_MEMPALACE`` is the portable alias.
"""

from __future__ import annotations

import argparse
import importlib.util
import subprocess  # nosec B404 - imported for the CLI error type only.
import sys
from pathlib import Path


def _stores():
    path = Path(__file__).resolve().parents[2] / "chaos-engine" / "stores.py"
    spec = importlib.util.spec_from_file_location("chaos_engine_stores", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"ChaosEngine store resolver is absent: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def find_shared_mempalace(cwd: Path) -> Path:
    """Return the shared palace path under the main checkout git directory."""
    return _stores().resolve_palace(cwd)


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
