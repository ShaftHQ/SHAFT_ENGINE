"""The shipped git-cleanup skill is the portable cleanup procedure."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/git-cleanup/SKILL.md"


class GitCleanupSkillTest(unittest.TestCase):
    def test_procedure_and_limits(self) -> None:
        text = SKILL.read_text(encoding="utf-8")
        self.assertIn("configured default branch", text)
        self.assertIn("`HEAD` equals that remote tip", text)
        self.assertIn("`git status` is clean", text)
        self.assertIn("land (commit, push, pull request, merge), delete, or gitignore", text)
        self.assertIn("only after that classification", text)
        self.assertIn("Do not stash.", text)
        self.assertIn("Do not `reset --hard`.", text)
        self.assertIn("Do not force-push.", text)
        self.assertIn("Do not delete remote branches.", text)
        self.assertIn("Do not discard unique commits without explicit authorization.", text)
        self.assertIn("Attended: ask first", text)
        self.assertIn("Unattended: decide and run it.", text)
        self.assertNotIn("`main`", text)
