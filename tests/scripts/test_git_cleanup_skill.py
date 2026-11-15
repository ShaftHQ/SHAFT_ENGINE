"""The shipped git-cleanup skill is the portable cleanup procedure."""

from __future__ import annotations

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/git-cleanup/SKILL.md"

def _load_guard():
    path = ROOT / "chaos-engine/hooks/guard.py"
    spec = importlib.util.spec_from_file_location("ce_guard_6299", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

class OwnerCommitAttributionGuidanceTest(unittest.TestCase):
    def test_profile_does_not_treat_owner_email_as_unattributed_without_a_reviewer(self) -> None:
        text = (ROOT / "shaft-skills/ce-pack/entrypoint.md").read_text(encoding="utf-8")
        # Epic #6342 / CE-12: the key and owner email live in user-level config.
        self.assertNotIn("F79E3F65DB762CE1", text)
        self.assertNotIn("Mohab.MohieElDeen@outlook.com", text)
        self.assertIn("owner email attribution", text)
