"""Large deliveries open as drafts; studied repos and exact briefs stay bounded."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
STUDIED = ROOT / "chaos-engine/references/studied-repos-not-adopted.md"
ROLES = ROOT / "chaos-engine/references/roles.md"

PLAYBOOK_SENTENCES = (
    "draft pull request on the first commit",
    "pushes again after each locally green slice",
    "does not arm auto-merge",
    "one-file reversible fix is not a large delivery",
)

ABSENT_DIRS = (
    "chaos-engine/skills/voice-clone",
    "chaos-engine/skills/game-studio",
    "chaos-engine/skills/spatial-globe",
    "chaos-engine/vendor/VoiceStudio",
    "chaos-engine/vendor/gods-eye-view",
    "chaos-engine/vendor/Claude-Code-Game-Studios",
)


class LargeDeliveryDraftPrTest(unittest.TestCase):
    def test_playbook_requires_a_draft_for_a_large_delivery(self) -> None:
        text = PLAYBOOK.read_text(encoding="utf-8")
        for sentence in PLAYBOOK_SENTENCES:
            self.assertIn(sentence, text)
        self.assertLessEqual(PLAYBOOK.stat().st_size, 15360)
        self.assertEqual(0, text.count("This rule does not auto-merge."))

    def test_studied_repos_are_not_adopted(self) -> None:
        text = STUDIED.read_text(encoding="utf-8")
        self.assertIn("No skill until a later owner ask plus a fresh license check.", text)
        for relative in ABSENT_DIRS:
            self.assertFalse((ROOT / relative).exists(), relative)

    def test_implementer_applies_an_exact_brief_before_another_read(self) -> None:
        text = ROLES.read_text(encoding="utf-8")
        self.assertIn(
            "When the brief already contains the exact edit, apply that edit before any other read. "
            "If several minutes pass with no diff, stop and report that instead of another survey.",
            text,
        )


if __name__ == "__main__":
    unittest.main()
