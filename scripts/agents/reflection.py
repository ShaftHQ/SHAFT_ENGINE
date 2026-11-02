#!/usr/bin/env python3
"""Repository CLI adapter for the canonical portable reflection controller."""

from __future__ import annotations

import importlib.util
from pathlib import Path


_SOURCE = Path(__file__).resolve().parents[2] / "chaos-engine/hooks/reflection.py"
_SPEC = importlib.util.spec_from_file_location("shaft_portable_reflection", _SOURCE)
if _SPEC is None or _SPEC.loader is None:
    raise ImportError(f"Cannot load reflection controller from {_SOURCE}")
_CONTROLLER = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_CONTROLLER)

for _name in dir(_CONTROLLER):
    if not _name.startswith("_"):
        globals()[_name] = getattr(_CONTROLLER, _name)


if __name__ == "__main__":
    raise SystemExit(_CONTROLLER.main())
