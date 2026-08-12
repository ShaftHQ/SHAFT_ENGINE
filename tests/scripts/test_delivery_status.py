"""Owned pull-request delivery completion tests (#4766)."""

import copy
import json
import subprocess  # nosec B404 - fixed local test doubles and commands.
import tempfile
import unittest

from pathlib import Path

from scripts.agents.delivery_status import evaluate_delivery, inspect_cleanup, validate_authority


def manifest() -> dict:
    return {
        "ownedPullRequests": [{
            "repository": "ShaftHQ/SHAFT_ENGINE", "number": 7, "headOid": "abc",
            "mergeAuthorized": True, "authorityEvidence": {
                "source": "native-memory", "locator": "decision.autonomous-merge-authority-extends-to-shafthq-github-io-docs-repo",
                "recordedAt": "2026-08-10T15:11:29+03:00", "repositories": ["ShaftHQ/SHAFT_ENGINE"],
            },
            "dependsOn": [],
        }],
        "cleanup": {"repositories": [{
            "root": "C:/repo", "defaultBranch": "main", "taskWorktrees": ["C:/task"],
            "taskBranches": ["ChaosEngine/task"], "unrelatedDirtyWorktrees": ["C:/other"],
        }]},
    }


def merged() -> dict:
    return {"repository": "ShaftHQ/SHAFT_ENGINE", "number": 7, "headOid": "abc", "state": "CLOSED", "isDraft": False, "autoMergeRequest": None, "mergeStateStatus": "UNKNOWN", "mergedAt": "2026-08-12T12:00:00Z", "auditDecision": "allow"}


class DeliveryStatusTest(unittest.TestCase):
    cleanup = {"primarySynced": True, "taskWorktreesAbsent": True, "taskBranchesAbsent": True, "unrelatedDirtyPreserved": True, "repositories": []}

    def test_authorized_merged_pr_and_scoped_cleanup_allow_completion(self):
        receipt = evaluate_delivery(manifest(), [merged()], self.cleanup, execution_repository="ShaftHQ/SHAFT_ENGINE", execution_head="abc")
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
            "repository": "ShaftHQ/shafthq.github.io", "number": 8, "headOid": "def",
            "mergeAuthorized": True, "authorityEvidence": {
                "source": "native-memory", "locator": "decision.autonomous-merge-authority-extends-to-shafthq-github-io-docs-repo",
                "recordedAt": "2026-08-10T15:11:29+03:00",
                "repositories": ["ShaftHQ/shafthq.github.io"],
            },
            "dependsOn": ["ShaftHQ/SHAFT_ENGINE#7"],
        })
        engine = merged()
        docs = {**merged(), "repository": "ShaftHQ/shafthq.github.io", "number": 8, "headOid": "def"}
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

    def test_malformed_authority_fails_closed_without_raising(self):
        plan = manifest()
        item = plan["ownedPullRequests"][0]
        item.update(number=True)
        item["authorityEvidence"].update(recordedAt="not-a-date", repositories="consumer/project")
        self.assertEqual("block", validate_authority(plan, "ShaftHQ/SHAFT_ENGINE", True, "abc")["decision"])

    def test_conditional_memory_prose_and_fabricated_timestamp_do_not_grant_authority(self):
        plan = manifest()
        evidence = plan["ownedPullRequests"][0]["authorityEvidence"]
        evidence.update(
            locator="constraint.own-each-pr-through-to-merge-babysit-ci-failures-and-review-comments-human-or-bot-until-green-then-merge-autonomously",
            recordedAt="2099-01-01T00:00:00+00:00",
        )
        receipt = validate_authority(plan, "ShaftHQ/SHAFT_ENGINE", 7, "abc", root=Path("."))
        self.assertEqual("block", receipt["decision"])

    def test_current_user_receipt_grants_or_revokes_authority(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            receipt_dir = root / ".git" / "act-as-mohab"
            receipt_dir.mkdir(parents=True)
            evidence = manifest()
            evidence["ownedPullRequests"][0]["authorityEvidence"] = None
            path = receipt_dir / "user-authority.json"
            base = {"schemaVersion": 1, "kind": "user-merge-authority", "repository": "ShaftHQ/SHAFT_ENGINE", "observedAt": "2026-08-12T10:00:00+00:00"}
            path.write_text(json.dumps({**base, "decision": "allow"}), encoding="utf-8")
            self.assertEqual("allow", validate_authority(evidence, "ShaftHQ/SHAFT_ENGINE", 7, "abc", root=root)["decision"])
            path.write_text(json.dumps({**base, "decision": "deny"}), encoding="utf-8")
            evidence = manifest()
            self.assertEqual("block", validate_authority(evidence, "ShaftHQ/SHAFT_ENGINE", 7, "abc", root=root)["decision"])

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
                    return subprocess.CompletedProcess(command, 0, f"worktree {root}\nworktree {other}\n", "")
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

            plan["cleanup"]["repositories"][0]["unrelatedDirtyWorktrees"] = []
            self.assertFalse(inspect_cleanup(plan, runner=runner, executable="git")["unrelatedDirtyPreserved"])


if __name__ == "__main__":
    unittest.main()
