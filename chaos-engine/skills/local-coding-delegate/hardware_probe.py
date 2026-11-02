"""Re-export the single hardware probe home."""

from __future__ import annotations

import importlib.util
from pathlib import Path

_PATH = Path(__file__).resolve().parent / "scripts" / "probe_hardware.py"
_SPEC = importlib.util.spec_from_file_location("probe_hardware_home", _PATH)
if _SPEC is None or _SPEC.loader is None:
    raise ImportError(_PATH)
_MODULE = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_MODULE)

classify = _MODULE.classify
probe = _MODULE.probe
main = _MODULE.main
