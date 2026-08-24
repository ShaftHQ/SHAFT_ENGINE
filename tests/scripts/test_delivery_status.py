"""Owned pull-request delivery completion tests (#4766)."""

import copy
import json
import subprocess  # nosec B404 - fixed local test doubles and commands.
import tempfile
import unittest

from pathlib import Path

from scripts.agents.delivery_status import _normalized_path, evaluate_delivery, inspect_cleanup, validate_authority


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


class _CleanupScenario:
    def __init__(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.task = self.root / "task"
        self.unrelated = self.root / "unrelated"
        self.task.mkdir()
        self.unrelated.mkdir()
        self.plan = manifest()
        self.plan["cleanup"]["repositories"][0].update(
            root=str(self.root),
            taskWorktrees=[str(self.task)],
            taskBranches=["ChaosEngine/task"],
            unrelatedDirtyWorktrees=[],
            degradedResidues=[{
                "repository": "ShaftHQ/SHAFT_ENGINE",
                "pullRequest": 7,
                "worktree": str(self.task),
                "branch": "ChaosEngine/task",
            }],
        )
        self.state = {
            "dirty": False, "unique": False, "locked": False, "pruned": False,
            "prune_on_remove": False, "head": "abc", "repository": "ShaftHQ/SHAFT_ENGINE",
            "default_remote_head": "same", "branch_present": True,
            "unexpected_dirty_worktree": False, "change_remote_on_remove": False,
            "desync_primary_on_remove": False, "drop_branch_on_remove": False,
            "add_dirty_worktree_on_remove": False, "denial": "host policy denied",
        }
        self.removal_calls = []

    def close(self):
        self.temporary.cleanup()

    def inspect(self, statuses=None):
        return inspect_cleanup(
            self.plan, [merged()] if statuses is None else statuses,
            runner=self, executable="git",
        )

    def __call__(self, command, **kwargs):
        joined = " ".join(command)
        if "remote get-url origin" in joined:
            return subprocess.CompletedProcess(command, 0, f"https://github.com/{self.state['repository']}.git\n", "")
        if "rev-parse" in joined:
            return self._rev_parse(command)
        if "worktree list" in joined:
            return self._worktree_list(command)
        if "branch --format" in joined:
            task_branch = "ChaosEngine/task\n" if self.state["branch_present"] else ""
            return subprocess.CompletedProcess(command, 0, f"main\n{task_branch}", "")
        if "status --porcelain" in joined:
            is_unrelated = Path(kwargs["cwd"]) == self.unrelated
            dirty = self.state["dirty"] or (self.state["unexpected_dirty_worktree"] and is_unrelated)
            return subprocess.CompletedProcess(command, 0, " M file\n" if dirty else "", "")
        if "cherry origin/main ChaosEngine/task" in joined:
            return subprocess.CompletedProcess(command, 0, "+ abc\n" if self.state["unique"] else "- abc\n", "")
        if "worktree remove --" in joined:
            return self._remove(command)
        raise AssertionError(command)

    def _rev_parse(self, command):
        if command[-1] in {"ChaosEngine/task", "HEAD"}:
            return subprocess.CompletedProcess(command, 0, f"{self.state['head']}\n", "")
        if command[-1] == "origin/main":
            return subprocess.CompletedProcess(command, 0, f"{self.state['default_remote_head']}\n", "")
        return subprocess.CompletedProcess(command, 0, "same\n", "")

    def _worktree_list(self, command):
        if self.state["pruned"]:
            return subprocess.CompletedProcess(command, 0, f"worktree {self.root}\nbranch refs/heads/main\n", "")
        locked = "\nlocked active-owner" if self.state["locked"] else ""
        unrelated = (
            f"\n\nworktree {self.unrelated}\nbranch refs/heads/unrelated"
            if self.state["unexpected_dirty_worktree"] else ""
        )
        output = (
            f"worktree {self.root}\nbranch refs/heads/main\n\nworktree {self.task}"
            f"\nbranch refs/heads/ChaosEngine/task{locked}{unrelated}\n"
        )
        return subprocess.CompletedProcess(command, 0, output, "")

    def _remove(self, command):
        self.removal_calls.append(command)
        for trigger, mutation, value in (
            ("prune_on_remove", "pruned", True),
            ("change_remote_on_remove", "repository", "evil/other"),
            ("desync_primary_on_remove", "default_remote_head", "changed"),
            ("drop_branch_on_remove", "branch_present", False),
            ("add_dirty_worktree_on_remove", "unexpected_dirty_worktree", True),
        ):
            if self.state[trigger]:
                self.state[mutation] = value
        return subprocess.CompletedProcess(command, 1, "", self.state["denial"])


class DeliveryStatusTest(unittest.TestCase):
    cleanup = {"primarySynced": True, "taskWorktreesAbsent": True, "taskBranchesAbsent": True, "unrelatedDirtyPreserved": True, "repositories": []}

    def test_authorized_merged_pr_and_scoped_cleanup_allow_completion(self):
        receipt = evaluate_delivery(manifest(), [merged()], self.cleanup, execution_repository="ShaftHQ/SHAFT_ENGINE", execution_head="abc")
        self.assertEqual("allow", receipt["decision"])
        self.assertEqual(1, receipt["mergedCount"])
        self.assertEqual("complete", receipt["cleanupDecision"])

    def test_legacy_commit_proofs_are_manifest_bound_to_owned_heads(self):
        plan = manifest()
        head = "a" * 40
        plan["ownedPullRequests"][0]["headOid"] = head
        plan["legacyCommitProofs"] = [
            {"repository": "ShaftHQ/SHAFT_ENGINE", "headOid": head}
        ]
        status = {**merged(), "headOid": head}

        receipt = evaluate_delivery(plan, [status], self.cleanup)

        self.assertEqual("allow", receipt["decision"])
        self.assertEqual(
            [{"repository": "ShaftHQ/SHAFT_ENGINE", "head": head}],
            receipt["legacyCommitProofs"],
        )

    def test_legacy_commit_proof_outside_owned_heads_fails_closed(self):
        plan = manifest()
        plan["legacyCommitProofs"] = [
            {"repository": "other/project", "headOid": "b" * 40}
        ]

        receipt = evaluate_delivery(plan, [merged()], self.cleanup)

        self.assertNotEqual("allow", receipt["decision"])
        self.assertTrue(any("legacy commit proof" in reason for reason in receipt["reasons"]))

    def test_merged_delivery_reports_safe_denied_cleanup_as_degraded(self):
        cleanup = {
            **self.cleanup,
            "taskWorktreesAbsent": False,
            "taskBranchesAbsent": False,
            "outcome": "degraded",
            "residueSafe": True,
            "residues": [
                {
                    "repository": "ShaftHQ/SHAFT_ENGINE",
                    "pullRequest": 7,
                    "worktree": "C:/task",
                    "branch": "ChaosEngine/task",
                    "reasonCode": "removal-denied",
                }
            ],
            "warnings": ["cleanup-residue-remains"],
        }

        receipt = evaluate_delivery(manifest(), [merged()], cleanup)

        self.assertEqual("allow", receipt["decision"])
        self.assertEqual("allow", receipt["deliveryDecision"])
        self.assertEqual("degraded", receipt["cleanupDecision"])
        self.assertEqual(cleanup["residues"], receipt["cleanup"]["residues"])

    def test_cleanup_unavailability_does_not_hide_successful_delivery(self):
        receipt = evaluate_delivery(manifest(), [merged()], None)

        self.assertEqual("unavailable", receipt["decision"])
        self.assertEqual("allow", receipt["deliveryDecision"])
        self.assertEqual("unavailable", receipt["cleanupDecision"])

    def test_degraded_cleanup_rejects_unmerged_or_unsafe_residue(self):
        base = {
            **self.cleanup,
            "taskWorktreesAbsent": False,
            "taskBranchesAbsent": False,
            "outcome": "degraded",
            "residueSafe": True,
            "residues": [{"repository": "ShaftHQ/SHAFT_ENGINE", "pullRequest": 7, "worktree": "C:/task", "branch": "ChaosEngine/task", "reasonCode": "removal-denied"}],
            "warnings": ["cleanup-residue-remains"],
        }
        open_status = {**merged(), "state": "OPEN", "mergedAt": None}
        self.assertEqual("block", evaluate_delivery(manifest(), [open_status], base)["decision"])
        for field, value in (
            ("residueSafe", False),
            ("residues", []),
            ("warnings", []),
            ("residues", [{"repository": "ShaftHQ/SHAFT_ENGINE", "pullRequest": 7, "worktree": "C:/task", "branch": "ChaosEngine/task", "reason": "token=secret"}]),
            ("warnings", ["token=secret"]),
        ):
            with self.subTest(field=field):
                cleanup = {**base, field: value}
                self.assertEqual("block", evaluate_delivery(manifest(), [merged()], cleanup)["decision"])

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
            receipt_dir = root / ".git" / "chaos-engine"
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
                if "remote get-url origin" in joined:
                    return subprocess.CompletedProcess(command, 0, "https://github.com/ShaftHQ/SHAFT_ENGINE.git\n", "")
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

    def test_degraded_residue_requires_clean_nonunique_unlocked_single_owner_and_denial(self):
        scenario = _CleanupScenario()
        self.addCleanup(scenario.close)
        plan = scenario.plan
        target = plan["cleanup"]["repositories"][0]
        state = scenario.state
        removal_calls = scenario.removal_calls
        runner = scenario

        with self.subTest(contract="degraded-cleanup-state-matrix"):
            observed = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertEqual("degraded", observed.get("outcome"))
            self.assertTrue(observed.get("residueSafe"))
            self.assertTrue(observed.get("warnings"))
            self.assertEqual(1, len(removal_calls))
            self.assertEqual("removal-denied", observed["residues"][0]["reasonCode"])

            target["degradedResidues"] *= 2
            before_duplicate = len(removal_calls)
            duplicate = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertFalse(duplicate["residueSafe"])
            self.assertEqual(before_duplicate, len(removal_calls))
            target["degradedResidues"] = target["degradedResidues"][:1]

            state["denial"] = "Access is denied"
            access_denied = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertFalse(access_denied["residueSafe"])
            self.assertEqual([], access_denied["residues"])
            state["denial"] = "not blocked by host policy; Access is denied by concurrent owner"
            negated_policy = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertFalse(negated_policy["residueSafe"])
            self.assertEqual([], negated_policy["residues"])
            for mixed_denial in (
                "host policy denied\nAccess is denied by concurrent owner",
                "not blocked by host policy\nhost policy denied",
            ):
                state["denial"] = mixed_denial
                mixed = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
                self.assertFalse(mixed["residueSafe"])
                self.assertEqual([], mixed["residues"])
            state["denial"] = "host policy denied"

            for field in ("dirty", "unique", "locked"):
                with self.subTest(field=field):
                    state[field] = True
                    self.assertFalse(
                        inspect_cleanup(plan, [merged()], runner=runner, executable="git").get("residueSafe")
                    )
                    state[field] = False

            state["head"] = "wrong"
            before_wrong_head = len(removal_calls)
            self.assertFalse(
                inspect_cleanup(plan, [merged()], runner=runner, executable="git")["residueSafe"]
            )
            self.assertEqual(before_wrong_head, len(removal_calls))
            state["head"] = "abc"

            state["repository"] = "evil/other"
            before_wrong_repository = len(removal_calls)
            self.assertFalse(
                inspect_cleanup(plan, [merged()], runner=runner, executable="git")["residueSafe"]
            )
            self.assertEqual(before_wrong_repository, len(removal_calls))
            state["repository"] = "ShaftHQ/SHAFT_ENGINE"

            state["change_remote_on_remove"] = True
            changed_remote = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertFalse(changed_remote["residueSafe"])
            self.assertEqual([], changed_remote["residues"])
            state["change_remote_on_remove"] = False
            state["repository"] = "ShaftHQ/SHAFT_ENGINE"

            for trigger, mutated, restored in (
                ("desync_primary_on_remove", "default_remote_head", "same"),
                ("drop_branch_on_remove", "branch_present", True),
                ("add_dirty_worktree_on_remove", "unexpected_dirty_worktree", False),
            ):
                with self.subTest(post_denial_mutation=trigger):
                    state[trigger] = True
                    changed = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
                    self.assertFalse(changed["residueSafe"])
                    self.assertEqual([], changed["residues"])
                    state[trigger] = False
                    state[mutated] = restored

            before_unmerged = len(removal_calls)
            unsafe = inspect_cleanup(plan, [{**merged(), "mergedAt": None}], runner=runner, executable="git")
            self.assertFalse(unsafe["residueSafe"])
            self.assertEqual(before_unmerged, len(removal_calls))

            for status in (
                {**merged(), "headOid": "wrong"},
                {**merged(), "auditDecision": "block"},
            ):
                with self.subTest(status=status):
                    before_unauthorized = len(removal_calls)
                    self.assertFalse(
                        inspect_cleanup(plan, [status], runner=runner, executable="git")["residueSafe"]
                    )
                    self.assertEqual(before_unauthorized, len(removal_calls))

            state["prune_on_remove"] = True
            partial = inspect_cleanup(plan, [merged()], runner=runner, executable="git")
            self.assertFalse(partial["residueSafe"])
            self.assertEqual([], partial["residues"])

    def test_cleanup_path_normalization_preserves_posix_case(self):
        self.assertNotEqual(
            _normalized_path("/repo/Task", "posix"),
            _normalized_path("/repo/task", "posix"),
        )


if __name__ == "__main__":
    unittest.main()
