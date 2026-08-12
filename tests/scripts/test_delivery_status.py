"""Owned pull-request delivery completion tests (#4766)."""

import copy
import subprocess
import tempfile
import unittest

from pathlib import Path

from scripts.agents.delivery_status import evaluate_delivery, inspect_cleanup


def manifest() -> dict:
    return {
        "ownedPullRequests": [{
            "repository": "consumer/project", "number": 7, "headOid": "abc",
            "mergeAuthorized": True, "authorityEvidence": {
                "source": "user-instruction", "locator": "thread:implementation-request",
                "recordedAt": "2026-08-12T10:00:00+00:00", "repositories": ["consumer/project"],
            },
            "dependsOn": [],
        }],
        "cleanup": {"repositories": [{
            "root": "C:/repo", "defaultBranch": "main", "taskWorktrees": ["C:/task"],
            "taskBranches": ["ChaosEngine/task"], "unrelatedDirtyWorktrees": ["C:/other"],
        }]},
    }


def merged() -> dict:
    return {"repository": "consumer/project", "number": 7, "headOid": "abc", "state": "CLOSED", "isDraft": False, "autoMergeRequest": None, "mergeStateStatus": "UNKNOWN", "mergedAt": "2026-08-12T12:00:00Z", "auditDecision": "allow"}


class DeliveryStatusTest(unittest.TestCase):
    cleanup = {"primarySynced": True, "taskWorktreesAbsent": True, "taskBranchesAbsent": True, "unrelatedDirtyPreserved": True, "repositories": []}

    def test_authorized_merged_pr_and_scoped_cleanup_allow_completion(self):
        receipt = evaluate_delivery(manifest(), [merged()], self.cleanup, execution_repository="consumer/project", execution_head="abc")
        self.assertEqual("allow", receipt["decision"])
        self.assertEqual(1, receipt["mergedCount"])

    def test_open_draft_armed_green_or_missing_merged_at_never_counts_as_delivered(self):
        for field, value in (("state", "OPEN"), ("isDraft", True), ("autoMergeRequest", {"enabledAt": "now"}), ("mergedAt", None)):
            status = merged()
            status[field] = value
            if field == "autoMergeRequest":
                status["mergedAt"] = None
            with self.subTest(field=field):
                receipt = evaluate_delivery(manifest(), [status], self.cleanup)
                self.assertEqual("block", receipt["decision"])
                self.assertTrue(any("mergedat" in reason.lower() for reason in receipt["reasons"]))

    def test_missing_authority_blocks_with_precise_constraint(self):
        plan = manifest()
        plan["ownedPullRequests"][0]["mergeAuthorized"] = False
        receipt = evaluate_delivery(plan, [merged()], self.cleanup)
        self.assertEqual("block", receipt["decision"])
        self.assertTrue(any("authority" in reason.lower() for reason in receipt["reasons"]))

    def test_companion_and_dependency_order_are_all_required(self):
        plan = manifest()
        plan["ownedPullRequests"].append({
            "repository": "consumer/docs", "number": 8, "headOid": "def",
            "mergeAuthorized": True, "authorityEvidence": {
                "source": "native-memory", "locator": "decision.companion-authority",
                "recordedAt": "2026-08-12T10:00:00+00:00",
                "repositories": ["consumer/docs"],
            },
            "dependsOn": ["consumer/project#7"],
        })
        engine = merged()
        docs = {**merged(), "repository": "consumer/docs", "number": 8, "headOid": "def"}
        self.assertEqual("allow", evaluate_delivery(plan, [engine, docs], self.cleanup)["decision"])
        self.assertEqual("unavailable", evaluate_delivery(plan, [docs], self.cleanup)["decision"])

    def test_failed_audit_or_incomplete_cleanup_blocks(self):
        for mutate in (
            lambda plan, status, cleanup: status.update(auditDecision="block"),
            lambda plan, status, cleanup: cleanup.update(primarySynced=False),
            lambda plan, status, cleanup: cleanup.update(unrelatedDirtyPreserved=False),
        ):
            plan, status, cleanup = manifest(), merged(), copy.deepcopy(self.cleanup)
            mutate(plan, status, cleanup)
            self.assertEqual("block", evaluate_delivery(plan, [status], cleanup)["decision"])

    def test_non_object_manifest_is_unavailable(self):
        self.assertEqual("unavailable", evaluate_delivery([], [], self.cleanup)["decision"])

    def test_cleanup_is_observed_from_git_not_manifest_claims(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            other = root / "other"
            other.mkdir()
            plan = manifest()
            plan["cleanup"]["repositories"][0].update(
                root=str(root), taskWorktrees=[str(root / "task")],
                unrelatedDirtyWorktrees=[str(other)],
            )
            def runner(command, **kwargs):
                joined = " ".join(command)
                if "rev-parse" in joined:
                    return subprocess.CompletedProcess(command, 0, "same\n", "")
                if "worktree list" in joined:
                    return subprocess.CompletedProcess(command, 0, f"worktree {root}\n", "")
                if "branch --format" in joined:
                    return subprocess.CompletedProcess(command, 0, "main\n", "")
                if "status --porcelain" in joined:
                    return subprocess.CompletedProcess(command, 0, " M user.txt\n", "")
                raise AssertionError(command)
            observed = inspect_cleanup(plan, runner=runner, executable="git")
            self.assertTrue(all(observed[field] for field in (
                "primarySynced", "taskWorktreesAbsent", "taskBranchesAbsent",
                "unrelatedDirtyPreserved",
            )))


if __name__ == "__main__":
    unittest.main()
