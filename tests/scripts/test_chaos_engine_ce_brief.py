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



    def test_brief_text_is_locator_only_paths(self):
        """#6072: brief text is path locators, not skill-body prose."""
        brief = self.mod.build_brief(project=ROOT, max_bytes=4096)
        lines = [ln.strip() for ln in brief["text"].splitlines() if ln.strip()]
        self.assertGreaterEqual(len(lines), 3)
        for line in lines:
            self.assertFalse(line.startswith("#"), f"markdown heading leaked: {line!r}")
            self.assertNotIn("```", line)
            # locator paths use / or end with .md/.py
            self.assertTrue(
                "/" in line or line.endswith(".md") or line.endswith(".py"),
                f"non-locator line: {line!r}",
            )
        # must not dump skill body markers
        blob = brief["text"].lower()
        for banned in ("never regress", "hard rails", "api_key", "bearer ", "ghp_", "sk-"):
            self.assertNotIn(banned, blob)

    def test_brief_contains_no_secret_material(self):
        """#6072: no-secret — brief must not embed tokens/credentials."""
        brief = self.mod.build_brief(project=ROOT, max_bytes=6144)
        blob = brief["text"]
        for needle in ("BEGIN PRIVATE", "AKIA", "xoxb-", "password=", "Authorization:"):
            self.assertNotIn(needle, blob)
        # used/skipped are path strings only
        for item in brief["used"] + brief["skipped"]:
            self.assertIsInstance(item, str)
            self.assertNotRegex(item, r"(?i)(token|secret|password|api[_-]?key)\s*=")

if __name__ == "__main__":
    unittest.main()
