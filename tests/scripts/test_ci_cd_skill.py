"""The shipped ci-cd skill carries the #6990 learnings (#6991)."""

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/ci-cd/SKILL.md"


class CiCdSkillTest(unittest.TestCase):
    def test_skill_states_targets_tiers_and_roll_forward(self) -> None:
        text = SKILL.read_text(encoding="utf-8")
        for token in ("name: ci-cd", "10 min", "15 min", "Push to main", "Nightly",
                      "Never auto-revert", "merge_group", "ci:full", "Release"):
            self.assertIn(token, text)

    def test_skill_is_indexed(self) -> None:
        index = (ROOT / "chaos-engine/harness-index.json").read_text(encoding="utf-8")
        self.assertIn("skills/ci-cd/SKILL.md", index)


if __name__ == "__main__":
    unittest.main()
