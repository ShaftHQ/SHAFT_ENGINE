"""Contract tests for ChaosEngine learning tickets #5767–#5770."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
PROCESS = ROOT / "chaos-engine/skills/omniroute/references/process-lifecycle.md"
SKILL = ROOT / "chaos-engine/skills/local-runtimes/references/omniroute.md"
GOTCHAS = ROOT / ".memory/memory/gotchas"


class Learning5767to5770Test(unittest.TestCase):
    def test_playbook_requires_release_note_label_on_every_human_pr(self) -> None:
        text = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("Human PR open checklist", text)
        self.assertIn("exactly one** release-note classification label", text)
        self.assertIn("gh pr create", text)
        self.assertIn("skill-md byte budget", text)
        self.assertIn("validate_agent_setup.py --skip-external", text)

    def test_process_lifecycle_forbids_broad_pkill(self) -> None:
        text = PROCESS.read_text(encoding="utf-8")
        self.assertIn("nohup", text)
        self.assertIn("pkill -f", text)
        self.assertIn("recorded", text.casefold())
        skill = SKILL.read_text(encoding="utf-8")
        self.assertIn("process-lifecycle.md", skill)
        self.assertLessEqual(len(skill.encode("utf-8")), 20000)

    def test_learned_gotchas_exist_for_each_issue(self) -> None:
        expected = {
            5767: "omniroute-skill-expansions-must-stay-under-skill-md-byte-budget",
            5768: "human-prs-need-exactly-one-release-note-classification-label",
            5769: "omniroute-verify-live-provider-ids-and-cli-target-matrix-before-long-run",
            5770: "do-not-broad-kill-omniroute-launcher-patterns-that-match-the-parent-shell",
        }
        for issue, slug in expected.items():
            md = GOTCHAS / f"{slug}.md"
            js = GOTCHAS / f"{slug}.json"
            self.assertTrue(md.is_file(), md)
            self.assertTrue(js.is_file(), js)
            body = md.read_text(encoding="utf-8")
            self.assertIn(f"#{issue}", body)


if __name__ == "__main__":
    unittest.main()
