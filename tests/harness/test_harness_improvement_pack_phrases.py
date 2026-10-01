"""Phrase-lock for harness/OpenCode improvement pack (#5998–#5996, #5992–#5986, #5994, #6011, #6015, #6016)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _read(rel: str) -> str:
    return (ROOT / rel).read_text(encoding="utf-8")


class HarnessImprovementPackPhrasesTest(unittest.TestCase):
    def test_local_agency_names_opencode_120s_and_one_command_prompt(self) -> None:
        skill = _read("chaos-engine/skills/local-agency/SKILL.md")
        guide = _read("chaos-engine/guides/local-agency.md")
        for text in (skill, guide):
            self.assertIn("120s", text)
            self.assertIn("product files", text)
            self.assertTrue("apply" in text.lower())

    def test_work_github_playbook_auto_merge_safety(self) -> None:
        playbook = _read("chaos-engine/references/work-github-playbook.md")
        self.assertIn("overlapping", playbook.lower())

    def test_process_owner_distinguishes_rog_vs_box(self) -> None:
        owner = _read("chaos-engine/references/process-owner-scrum-master.md")
        self.assertIn("Work machine", owner)
        self.assertIn("FreeToken probe host", owner)

    def test_identity_learning_session_novel_success_finalize(self) -> None:
        # Epic #6342: the rule lives with the Learning Session protocol, not identity.
        skill = _read("chaos-engine/skills/self-improve/SKILL.md")
        self.assertIn("novel_success", skill)
        self.assertIn("without inventing a second incident", skill)


if __name__ == "__main__":
    unittest.main()
