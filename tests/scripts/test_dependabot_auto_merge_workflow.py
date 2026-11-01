"""Contract: Dependabot auto-merge schedule path names the repo for gh."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "dependabot-auto-merge.yml"
REFERENCE = ROOT / "chaos-engine" / "references" / "automerge-rearm.md"


class DependabotAutoMergeWorkflowTest(unittest.TestCase):
    def test_schedule_rearm_passes_repo_flag_to_gh_pr_list(self) -> None:
        text = WORKFLOW.read_text(encoding="utf-8")
        self.assertIn("gh pr list -R", text)
        self.assertIn('GITHUB_REPOSITORY: ${{ github.repository }}', text)
        self.assertIn('repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}"', text)
        # No checkout on this workflow (pull_request_target must not run PR code;
        # schedule must not rely on ambient git remotes).
        self.assertNotIn("actions/checkout", text)

    def test_automerge_rearm_reference_requires_repo_flag(self) -> None:
        text = REFERENCE.read_text(encoding="utf-8")
        self.assertIn("-R", text)
        self.assertIn("GITHUB_REPOSITORY", text)
        self.assertIn("empty list", text.lower())


if __name__ == "__main__":
    unittest.main()
