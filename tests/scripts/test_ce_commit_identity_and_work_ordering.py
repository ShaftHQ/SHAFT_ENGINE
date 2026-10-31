"""Commit-identity scope + work-ordering process cards."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load_commit_identity():
    path = CE / "commit_identity.py"
    spec = importlib.util.spec_from_file_location("ce_commit_identity", path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CommitIdentityTest(unittest.TestCase):
    def test_allowlist_and_normalize(self) -> None:
        ci = load_commit_identity()
        self.assertTrue(
            ci.chaosengine_author_allowed("https://github.com/ShaftHQ/SHAFT_ENGINE.git")
        )
        self.assertTrue(
            ci.chaosengine_author_allowed("git@github.com:ShaftHQ/shafthq.github.io.git")
        )
        self.assertFalse(
            ci.chaosengine_author_allowed("https://github.com/MohabMohie/automatest-coach-brain")
        )
        self.assertFalse(ci.chaosengine_author_allowed(""))
        self.assertEqual(
            "ShaftHQ/SHAFT_ENGINE",
            ci.normalize_remote("https://github.com/ShaftHQ/SHAFT_ENGINE.git"),
        )

    def test_assert_refuses_foreign_remote(self) -> None:
        ci = load_commit_identity()
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / ".git").mkdir()
            # no origin → refuse
            with self.assertRaises(PermissionError):
                ci.assert_chaosengine_author_allowed(root)

    def test_card_shape(self) -> None:
        raw = (CE / "references/commit-identity.md").read_text(encoding="utf-8")
        for section in ("## Rule", "## Why", "## Check"):
            self.assertIn(section, raw)
        self.assertIn("allowed_remotes", raw)
        self.assertIn("assert_chaosengine_author_allowed", raw)
        self.assertIn("user-guide", raw)
        self.assertIn("ShaftHQ/SHAFT_ENGINE", raw)
        self.assertIn("ShaftHQ/shafthq.github.io", raw)
        self.assertIn("CE_OWNER_IDENTITY", raw)


class WorkOrderingCardTest(unittest.TestCase):
    def test_card_shape(self) -> None:
        raw = (CE / "references/work-ordering.md").read_text(encoding="utf-8")
        for section in ("## Rule", "## Why", "## Check"):
            self.assertIn(section, raw)
        for needle in ("Open PRs", "Dependabot", "bugs", "features", "epics", "fold"):
            self.assertIn(needle, raw)
        tip = (CE / "references/tip-churn-preflight.md").read_text(encoding="utf-8")
        self.assertIn("work-ordering.md", tip)
        bot = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("work-ordering.md", bot)



class PreCommitIdentityGuardTest(unittest.TestCase):
    def test_blocks_chaosengine_on_foreign_repo(self) -> None:
        ci = load_commit_identity()
        hits = ci.pre_commit_identity_failures(
            name="ChaosEngine",
            email="chaosengine@mail.grokbot.com",
            remote_url="https://github.com/example/designer-workspace.git",
        )
        self.assertTrue(hits)
        self.assertIn("commit-identity", hits[0])

    def test_allows_chaosengine_on_engine(self) -> None:
        ci = load_commit_identity()
        hits = ci.pre_commit_identity_failures(
            name="ChaosEngine",
            email="chaosengine@mail.grokbot.com",
            remote_url="https://github.com/ShaftHQ/SHAFT_ENGINE.git",
        )
        self.assertEqual([], hits)

    def test_owner_identity_ok_anywhere(self) -> None:
        ci = load_commit_identity()
        hits = ci.pre_commit_identity_failures(
            name="Owner",
            email="owner@example.com",
            remote_url="https://github.com/example/designer-workspace.git",
        )
        self.assertEqual([], hits)

    def test_repo_identity_map_keys(self) -> None:
        ci = load_commit_identity()
        mapping = ci.repo_identity_map()
        self.assertIn("ShaftHQ/SHAFT_ENGINE", mapping)
        self.assertIn("ShaftHQ/shafthq.github.io", mapping)
        self.assertEqual(ci.CHAOSENGINE_EMAIL, mapping["ShaftHQ/SHAFT_ENGINE"])


if __name__ == "__main__":
    unittest.main()
