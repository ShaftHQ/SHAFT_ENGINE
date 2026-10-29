#!/usr/bin/env python3
"""
Thin CLI wrapper over mobile_retro_runtime (operators / local demos).

Production live paths call ``mobile_retro_runtime`` from assemble_shard_blob,
rerun-failures.sh, flutter e2e, overlay_pre_push, extract_allure_failures, and
hooks/guard PostToolUse — not this file alone.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path

_RUNTIME = Path(__file__).resolve().parent / "mobile_retro_runtime.py"
_SPEC = importlib.util.spec_from_file_location("mobile_retro_runtime", _RUNTIME)
if _SPEC is None or _SPEC.loader is None:
    raise SystemExit("mobile_retro_runtime unavailable")
_MOD = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_MOD)

if __name__ == "__main__":
    raise SystemExit(_MOD.main())
