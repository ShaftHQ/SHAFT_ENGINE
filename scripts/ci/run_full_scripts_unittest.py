#!/usr/bin/env python3
"""Sanctioned entrypoint for a full ``tests/scripts`` unittest discover.

Never invoke ``python3 -m unittest discover -s tests/scripts`` directly on this
box: two discovers can overlap (~40 min each), blow the 35-minute session cap,
and thrash swap. This wrapper holds ``full_scripts_unittest_lock`` for the
duration of the suite. Prefer targeted modules; when a full discover is
required, start this script detached (``nohup`` / ``setsid``) and never in the
foreground of an interactive agent turn.

Usage:
  python3 scripts/ci/run_full_scripts_unittest.py [unittest args...]

Default args: discover -s tests/scripts -v
Extra args replace the default discover invocation after ``python3 -m unittest``.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci.overlay_pre_push import full_scripts_unittest_lock  # noqa: E402


def main(argv: list[str] | None = None) -> int:
    args = list(argv if argv is not None else sys.argv[1:])
    if not args:
        args = ["discover", "-s", "tests/scripts", "-v"]
    env = os.environ.copy()
    env.setdefault("PYTHONUNBUFFERED", "1")
    if sys.stdin is not None and sys.stdin.isatty():
        print(
            "warning: full suite in the foreground; start it detached (nohup or setsid)",
            file=sys.stderr,
        )
    try:
        lock = full_scripts_unittest_lock(ROOT, blocking=False)
    except BlockingIOError:
        print("refusing a second full-suite run; one copy at a time", file=sys.stderr)
        return 2
    with lock:
        # Re-exec under the same interpreter so unittest sees a clean argv.
        # The lock fd is inheritable, so it is held across os.execvpe.
        os.execvpe(
            sys.executable,
            [sys.executable, "-m", "unittest", *args],
            env,
        )
    return 0  # pragma: no cover — execvpe does not return


if __name__ == "__main__":
    raise SystemExit(main())
