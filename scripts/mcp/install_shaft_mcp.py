#!/usr/bin/env python3
"""Deprecated shim. Delegates to install_shaft_agentic_tools.py."""

from __future__ import annotations

import runpy
import sys
from pathlib import Path

_TARGET = Path(__file__).resolve().with_name("install_shaft_agentic_tools.py")


def _warn() -> None:
    print(
        "install_shaft_mcp.py is deprecated; delegating to install_shaft_agentic_tools.py",
        file=sys.stderr,
    )


if __name__ == "__main__":
    _warn()
    runpy.run_path(str(_TARGET), run_name="__main__")
else:
    loaded = runpy.run_path(str(_TARGET), run_name=__name__)
    globals().update({key: value for key, value in loaded.items() if key != "__name__"})
