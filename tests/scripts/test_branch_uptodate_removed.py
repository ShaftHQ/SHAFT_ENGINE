"""Assert Branch-contains-base-tip check is gone (Mohab t335u / t336u).

Merge conflicts + merge queue (tests against latest main) are enough; forcing
every open PR to re-sync after each merge was a bottleneck. Riskiest assumption:
PR Gate Summary still aggregates required legs without branch-uptodate.
"""

from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]


class BranchUptodateRemovedTest(unittest.TestCase):
    def test_workflow_has_no_branch_uptodate_job(self) -> None:
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertNotIn("branch-uptodate:", workflow)
        self.assertNotIn("Branch contains base tip", workflow)
        self.assertNotIn("assert_branch_uptodate.py", workflow)
        self.assertNotIn("BRANCH_UPTODATE_RESULT", workflow)

    def test_script_and_old_tests_deleted(self) -> None:
        self.assertFalse((ROOT / "scripts/ci/assert_branch_uptodate.py").exists())
        self.assertFalse((ROOT / "tests/scripts/test_assert_branch_uptodate.py").exists())

    def test_summary_does_not_depend_on_branch_uptodate(self) -> None:
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        summary = workflow.split("summary:", 1)[1].split("main-red-reaction:", 1)[0]
        self.assertIn("name: PR Gate Summary", summary)
        self.assertNotIn("branch-uptodate", summary)
        # Still requires the other required-check producer legs.
        self.assertIn("codecov-gate", summary)
        self.assertIn("unit-tests", summary)


if __name__ == "__main__":
    unittest.main()
