"""#7964: a missing `.chaos-engine/tool.py` fails doctor and the next hook denial."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import os
import subprocess  # nosec B404 - the repair test launches the repo interpreter on install.py
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
REPAIR = "python3 .chaos-engine/install.py repair --project ."

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

class GateSelectionTest(unittest.TestCase):
    def test_changed_tool_paths_select_this_module(self) -> None:
        from scripts.ci.harness_pr_gate import classify_paths

        module = "tests.scripts.test_ce_tool_missing_7964"
        for path in (
            "tests/scripts/test_ce_tool_missing_7964.py",
            "scripts/agents/guard.py",
        ):
            plan = classify_paths([path])
            self.assertIn(module, plan.test_modules, path)
        for path in (
            "chaos-engine/delete_guard.py",
            "chaos-engine/install.py",
            "chaos-engine/hooks/guard.py",
            "chaos-engine/overlay_match.py",
            "chaos-engine/worktree_overlay.py",
        ):
            plan = classify_paths([path])
            self.assertNotIn(module, plan.test_modules, path)

if __name__ == "__main__":
    unittest.main()
