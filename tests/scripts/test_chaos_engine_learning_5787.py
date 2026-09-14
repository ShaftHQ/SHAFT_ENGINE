"""Contract tests for ChaosEngine learning ticket #5787."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
GOTCHAS = ROOT / ".memory/memory/gotchas"
SLUG = "absorb-open-scaffold-prs-before-stacking-host-parity-follow-ons"


class Learning5787Test(unittest.TestCase):
    def test_playbook_requires_absorb_or_merge_scaffold_before_stacking(self) -> None:
        text = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("Human PR open checklist", text)
        self.assertIn("host-parity", text)
        self.assertIn("unmerged scaffold", text)
        self.assertIn("absorb it into the follow-on PR", text)
        self.assertIn("#5787", text)

    def test_gotcha_md_and_json_exist_and_cite_issue(self) -> None:
        md = GOTCHAS / f"{SLUG}.md"
        js = GOTCHAS / f"{SLUG}.json"
        self.assertTrue(md.is_file(), md)
        self.assertTrue(js.is_file(), js)
        body = md.read_text(encoding="utf-8")
        self.assertIn("#5787", body)
        sidecar = js.read_text(encoding="utf-8")
        self.assertIn("#5787", sidecar)
        self.assertIn(f"gotcha.{SLUG}", sidecar)


if __name__ == "__main__":
    unittest.main()
