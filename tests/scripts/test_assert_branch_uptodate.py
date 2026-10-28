"""Tests for scripts/ci/assert_branch_uptodate.py (#7184)."""

import importlib.util
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "assert_branch_uptodate", ROOT / "scripts/ci/assert_branch_uptodate.py"
)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


class AssertBranchUptodateTest(unittest.TestCase):
    def test_is_uptodate_true_when_ancestor(self) -> None:
        with mock.patch.object(mod, "_git") as git:
            git.return_value = mock.Mock(returncode=0)
            self.assertTrue(mod.is_uptodate("origin/main", "HEAD"))
            git.assert_called_once_with(["merge-base", "--is-ancestor", "origin/main", "HEAD"])

    def test_is_uptodate_false_when_behind(self) -> None:
        with mock.patch.object(mod, "_git") as git:
            git.return_value = mock.Mock(returncode=1)
            self.assertFalse(mod.is_uptodate("origin/main", "HEAD"))

    def test_main_exits_1_when_behind(self) -> None:
        with mock.patch.object(mod, "is_uptodate", return_value=False):
            self.assertEqual(1, mod.main(["--base", "origin/main"]))

    def test_main_exits_0_when_uptodate(self) -> None:
        with mock.patch.object(mod, "is_uptodate", return_value=True):
            self.assertEqual(0, mod.main([]))


if __name__ == "__main__":
    unittest.main()
