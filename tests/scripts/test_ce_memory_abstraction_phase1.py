"""Phase 1 CE memory abstraction (no overlap with sibling video sign-off gates)."""

from __future__ import annotations

import importlib.util
import json
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DESIGN = CE / "addons/design-skills"
LESSONS = DESIGN / "references/lessons"
SCOPE = DESIGN / "scripts/title_scope_gate.py"
WORKER = CE / "skills/local-agency/scripts/worker_ce_entry.py"

def _text(path: Path) -> str:
    return " ".join(path.read_text(encoding="utf-8").split())

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod

class NoVideoGateOverlapTest(unittest.TestCase):
    def test_no_viewer_video_qc_script(self) -> None:
        self.assertFalse((DESIGN / "scripts/viewer_video_qc.py").exists())

        # Do NOT assert absence of cmd_bidisolate: sibling video tip may land first.

if __name__ == "__main__":
    unittest.main()
