"""P0 ce_brief locator-only builder (#6067)."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CeBriefTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/ce_brief.py", "ce_brief_p0")

    def test_under_budget_includes_skill_locator(self):
        brief = self.mod.build_brief(project=ROOT, max_bytes=4096)
        self.assertEqual(brief["schemaVersion"], 1)
        self.assertFalse(brief["truncated"])
        self.assertIn("chaos-engine/skills/chaos-engine/SKILL.md", brief["text"])
        self.assertIn("level-1-catalog.md", brief["text"])
        self.assertIn("zero-llm-catalog.md", brief["text"])
        self.assertIn(".chaos-engine-state/wake-pack.md", brief["text"])
        self.assertGreaterEqual(len(brief["used"]), 3)
        self.assertEqual(brief["bytes"], len(brief["text"].encode("utf-8")))

    def test_truncate_utf8_bytes(self):
        brief = self.mod.build_brief(project=ROOT, max_bytes=32)
        self.assertTrue(brief["truncated"])
        self.assertLessEqual(brief["bytes"], 32)
        self.assertLessEqual(len(brief["text"].encode("utf-8")), 32)

    def test_max_bytes_clamped(self):
        brief = self.mod.build_brief(project=ROOT, max_bytes=99999)
        self.assertLessEqual(brief["bytes"], 6144)


if __name__ == "__main__":
    unittest.main()
