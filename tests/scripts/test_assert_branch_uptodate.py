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

    def test_fetch_base_uses_origin_refspec(self) -> None:
        with mock.patch.object(mod, "_git") as git:
            git.return_value = mock.Mock(returncode=0, stdout="", stderr="")
            mod.fetch_base("origin/main")
            git.assert_called_once_with(
                ["fetch", "--no-tags", "--depth", "1", "origin", "+refs/heads/main:refs/remotes/origin/main"]
            )

    def test_fetch_base_uses_fetch_url_for_forks(self) -> None:
        with mock.patch.object(mod, "_git") as git:
            git.return_value = mock.Mock(returncode=0, stdout="", stderr="")
            mod.fetch_base("origin/main", "https://github.com/ShaftHQ/SHAFT_ENGINE.git")
            git.assert_called_once_with(
                [
                    "fetch",
                    "--no-tags",
                    "--depth",
                    "1",
                    "https://github.com/ShaftHQ/SHAFT_ENGINE.git",
                    "+refs/heads/main:refs/remotes/origin/main",
                ]
            )

    def test_main_fetches_then_checks(self) -> None:
        with mock.patch.object(mod, "fetch_base") as fetch, mock.patch.object(
            mod, "is_uptodate", return_value=True
        ) as probe:
            self.assertEqual(0, mod.main(["--base", "origin/main", "--fetch-url", "https://example.test/r.git"]))
            fetch.assert_called_once_with("origin/main", "https://example.test/r.git")
            probe.assert_called_once_with("origin/main", "HEAD")

    def test_main_exits_1_when_behind(self) -> None:
        with mock.patch.object(mod, "fetch_base"), mock.patch.object(mod, "is_uptodate", return_value=False):
            self.assertEqual(1, mod.main(["--base", "origin/main"]))

    def test_main_exits_0_when_uptodate(self) -> None:
        with mock.patch.object(mod, "fetch_base"), mock.patch.object(mod, "is_uptodate", return_value=True):
            self.assertEqual(0, mod.main([]))

    def test_skip_fetch_bypasses_network(self) -> None:
        with mock.patch.object(mod, "fetch_base") as fetch, mock.patch.object(
            mod, "is_uptodate", return_value=True
        ):
            self.assertEqual(0, mod.main(["--skip-fetch"]))
            fetch.assert_not_called()

    def test_workflow_wires_branch_uptodate_job(self) -> None:
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("branch-uptodate:", workflow)
        self.assertIn("scripts/ci/assert_branch_uptodate.py", workflow)
        self.assertIn("--fetch-url", workflow)
        job = workflow.split("branch-uptodate:", 1)[1].split("summary:", 1)[0]
        self.assertIn("fetch-depth: 0", job)
        self.assertIn("github.event.pull_request.head.sha", job)
        # Required by summary so it cannot be bypassed.
        summary = workflow.split("summary:", 1)[1].split("main-red-reaction:", 1)[0]
        self.assertIn("branch-uptodate", summary)


if __name__ == "__main__":
    unittest.main()
