"""Dated ChaosEngine research/adoption matrix tests (#4797)."""

from __future__ import annotations

import re
import json
import importlib
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
MATRIX = ROOT / "chaos-engine/RESEARCH.md"


class ChaosEngineResearchTest(unittest.TestCase):
    def test_deepseek_harness_adoption_is_pinned_selective_and_dated(self):
        content = MATRIX.read_text(encoding="utf-8")
        self.assertIn("Accessed: 2026-08-15", content)
        self.assertIn("47f943859bef60e4160492346772ded9b24f765a", content)
        for adopted in (
            "capability ownership",
            "declarative composition",
            "orthogonal outcomes",
            "bounded asynchronous behavior",
            "quiescent teardown",
        ):
            self.assertIn(adopted, content.lower())
        for rejected in (
            "node runtime",
            "agent loop",
            "session log",
            "goals/todos",
            "everything is a plugin",
        ):
            self.assertIn(rejected, content.lower())

    def test_top_ten_matrix_is_dated_primary_sourced_and_actionable(self):
        content = MATRIX.read_text(encoding="utf-8")
        self.assertIn("Accessed: 2026-08-12", content)
        rows = [line for line in content.splitlines() if re.match(r"^\| [1-9][0-9]? \|", line)]
        self.assertEqual(10, len(rows))
        for row in rows:
            self.assertRegex(row, r"https://")
            self.assertRegex(row, r"\| (Adopted|Retained|Rejected) \|")
            self.assertIn("`", row)

    def test_matrix_names_the_selected_and_rejected_complete_approaches(self):
        content = MATRIX.read_text(encoding="utf-8")
        self.assertIn("Selected approach", content)
        self.assertIn("Marketplace-only alternative", content)
        self.assertIn("Global-tool alternative", content)
        self.assertIn("latest-main", content)

    def test_versioned_release_keeps_shaft_skills_but_retires_act_as_mohab(self):
        release = json.loads(
            (ROOT / "agent-plugins/release.json").read_text(encoding="utf-8")
        )
        self.assertEqual(["shaft-skills"], [item["name"] for item in release["packages"]])
        builder = importlib.import_module("scripts.ci.agent_plugin_release")
        self.assertEqual(("shaft-skills",), builder.REQUIRED_PACKAGES)
        workflow = (ROOT / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8")
        self.assertNotIn("act-as-mohab-*.zip", workflow)


if __name__ == "__main__":
    unittest.main()
