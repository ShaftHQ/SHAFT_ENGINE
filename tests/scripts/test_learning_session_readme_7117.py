"""Learning Session lessons from the README redesign (#7117) and the doctor fix (#7112)."""

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def read(relative: str) -> str:
    return (CE / relative).read_text(encoding="utf-8")


class LearningSessionReadme7117Test(unittest.TestCase):
    def test_bot_entry_requires_one_worktree_per_worker(self):
        text = read("references/bot-entry.md")
        self.assertIn("One worktree per worker", text)
        self.assertIn("git worktree add", text)

    def test_ci_cd_checks_pushed_sha_before_babysitting(self):
        text = read("skills/ci-cd/SKILL.md")
        self.assertIn("git ls-remote origin", text)
        self.assertIn("git rev-parse HEAD", text)

    def test_harness_maintenance_reinstalls_before_new_repair_component(self):
        text = read("references/harness-maintenance.md")
        self.assertIn("invalid choice", text)
        self.assertIn("reinstall", text)

    def test_maintain_recovery_does_not_force_portable_distribution(self):
        text = read("references/harness-maintenance.md")
        maintain = text.split("## Maintain", 1)[1].split("## Deletes", 1)[0]
        recovery = [line for line in maintain.splitlines() if "bootstrap.py" in line]
        self.assertEqual(["`python3 .chaos-engine/bootstrap.py --project . --repository OWNER/REPO --branch BRANCH`;"], recovery)
        self.assertNotIn("--distribution portable", text)

    def test_web_design_card_covers_readme_rendering_traps(self):
        text = read("addons/design-skills/references/craft/web-design.md")
        self.assertIn('"mode": "markdown"', text)
        self.assertIn("<a><picture>", text)
        self.assertIn("--force-prefers-reduced-motion", text)
        self.assertIn("README CTAs", text)
        self.assertLessEqual(len(text.splitlines()), 120)


if __name__ == "__main__":
    unittest.main()
