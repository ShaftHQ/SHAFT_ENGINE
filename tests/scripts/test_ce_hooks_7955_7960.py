"""Regressions for #7955-#7960. Calls the guards; does not reimplement them."""

from __future__ import annotations

import base64
import fnmatch
import importlib.util
import os
import subprocess
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

def _git(root: Path, *args: str) -> None:
    subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
        ["git", "-C", str(root), *args], check=True, capture_output=True, text=True
    )

class GateSelectionTest(unittest.TestCase):
    def test_changed_hook_paths_select_this_module(self) -> None:
        from scripts.ci.harness_pr_gate import classify_paths

        module = "tests.scripts.test_ce_hooks_7955_7960"
        for path in (
            "tests/scripts/test_ce_hooks_7955_7960.py",
            "scripts/agents/guard.py",
        ):
            plan = classify_paths([path])
            self.assertIn(module, plan.test_modules, path)
        # Those production files moved with the harness. A leftover chaos-engine/
        # path must not keep selecting this SHAFT module.
        for path in (
            "chaos-engine/rules/operation_boundary.py",
            "chaos-engine/hooks/retrieve_justification.py",
            "chaos-engine/force_push_approve.py",
        ):
            plan = classify_paths([path])
            self.assertNotIn(module, plan.test_modules, path)

if __name__ == "__main__":
    unittest.main()
