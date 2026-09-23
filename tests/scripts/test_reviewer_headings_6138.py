"""Reviewer verification-gap headings stay in roles and delegation (#6138)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HEADINGS = ("regression", "missing-adoption", "broken-verification", "unbound-check", "verdict")


class ReviewerHeadingsTest(unittest.TestCase):
    def test_headings_are_required_and_iron_law_6_stays(self):
        roles = (ROOT / "chaos-engine/references/roles.md").read_text(encoding="utf-8")
        delegation = (ROOT / "chaos-engine/references/delegation.md").read_text(encoding="utf-8")
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        for heading in HEADINGS:
            self.assertIn(f"### {heading}", roles)
            self.assertIn(heading, delegation)
        self.assertIn("Terminal adversarial review is on.", skill)


if __name__ == "__main__":
    unittest.main()
