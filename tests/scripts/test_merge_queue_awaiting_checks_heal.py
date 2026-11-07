"""Fail-closed merge-queue diagnostic. No dequeue and no re-arm (#7704)."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import yaml

from scripts.ci.merge_queue_awaiting_checks_heal import (
    REQUIRED_MERGE_GROUP_WORKFLOWS,
    REQUIRED_QUEUE_CHECKS,
    RETRY_BUDGET,
    classify,
    conclusion_is_green,
    dependabot_enqueue_decision,
    installer_result_is_green,
    may_enqueue,
    queue_shas,
    required_workflows_declare_merge_group,
)

ROOT = Path(__file__).resolve().parents[2]
HEAL = ROOT / "scripts" / "ci" / "merge_queue_awaiting_checks_heal.py"
HEAD = "a" * 40
QUEUE = "b" * 40
NOW = 1_700_000_000


def _checks(sha: str, conclusions: dict[str, str] | None = None) -> list[dict[str, str]]:
    names = conclusions or {
        "PR Gate Summary": "success",
        "Release-note governance": "success",
    }
    return [{"name": name, "sha": sha, "conclusion": conclusion} for name, conclusion in names.items()]


def _entry(**overrides: object) -> dict[str, object]:
    entry: dict[str, object] = {
        "state": "AWAITING_CHECKS",
        "head_sha": HEAD,
        "queue_sha": QUEUE,
        "queued": True,
        "auto_merge_request": None,
        "checks": _checks(QUEUE),
        "now_epoch": NOW,
        "enqueued_at_epoch": NOW - 3600,
        "timeouts": 0,
        "event": "schedule",
        "hold_heads": [],
        "opt_in_head": None,
        "opt_in_verified": False,
    }
    entry.update(overrides)
    return entry


class QueueDiagnosticTest(unittest.TestCase):
    def test_explicit_hold_blocks_even_with_verified_opt_in(self) -> None:
        entry = _entry(
            hold_heads=[HEAD],
            opt_in_head=HEAD,
            opt_in_verified=True,
            queued=False,
            checks=_checks(HEAD),
        )
        verdict = classify(entry)
        self.assertEqual("explicit_hold", verdict.code)
        self.assertFalse(verdict.mutate)
        self.assertEqual(0, verdict.exit_code)
        self.assertFalse(may_enqueue(entry))

    def test_already_queued_with_null_auto_merge_is_not_a_rearm(self) -> None:
        verdict = classify(_entry(queued=True, auto_merge_request=None))
        self.assertEqual("already_queued", verdict.code)
        self.assertFalse(verdict.mutate)
        self.assertEqual(0, verdict.exit_code)
        self.assertFalse(may_enqueue(_entry(
            queued=True,
            auto_merge_request=None,
            opt_in_head=HEAD,
            opt_in_verified=True,
        )))

    def test_synchronize_after_hold_does_not_clear_the_head(self) -> None:
        held = _entry(
            event="synchronize",
            hold_heads=[HEAD],
            opt_in_head=HEAD,
            opt_in_verified=True,
            queued=False,
            checks=_checks(HEAD),
        )
        self.assertFalse(may_enqueue(held))
        moved = _entry(
            event="synchronize",
            head_sha="c" * 40,
            hold_heads=[HEAD],
            opt_in_head=HEAD,
            opt_in_verified=True,
            queued=False,
            checks=_checks("c" * 40),
        )
        self.assertFalse(may_enqueue(moved))

    def test_repeated_timeout_stops_instead_of_rearming(self) -> None:
        entry = _entry(timeouts=RETRY_BUDGET, checks=_checks(QUEUE, {"PR Gate Summary": "success"}))
        verdict = classify(entry)
        self.assertEqual("retry_exhausted", verdict.code)
        self.assertEqual(1, verdict.exit_code)
        self.assertFalse(verdict.mutate)
        self.assertFalse(may_enqueue(_entry(
            timeouts=RETRY_BUDGET,
            queued=False,
            opt_in_head=HEAD,
            opt_in_verified=True,
            checks=_checks(HEAD),
        )))

    def test_pull_request_head_sha_is_not_the_queue_sha(self) -> None:
        entry = _entry(checks=_checks(HEAD))
        verdict = classify(entry)
        self.assertEqual("wrong_sha", verdict.code)
        self.assertEqual(1, verdict.exit_code)
        self.assertFalse(verdict.mutate)
        self.assertFalse(queue_checks_green_safe(entry))

    def test_missing_either_required_check_is_not_healthy(self) -> None:
        only_gate = _entry(checks=_checks(QUEUE, {"PR Gate Summary": "success"}))
        only_notes = _entry(checks=_checks(QUEUE, {"Release-note governance": "success"}))
        workflow_name = _entry(checks=_checks(QUEUE, {"PR Gate": "success", "Release-note governance": "success"}))
        self.assertEqual("missing_required_check", classify(only_gate).code)
        self.assertEqual("missing_required_check", classify(only_notes).code)
        self.assertEqual("missing_required_check", classify(workflow_name).code)
        self.assertFalse(classify(only_gate).mutate)

    def test_young_and_healthy_entries_are_not_mutations(self) -> None:
        young = classify(_entry(
            checks=[],
            enqueued_at_epoch=NOW - 60,
            now_epoch=NOW,
        ))
        self.assertEqual("young_queue_entry", young.code)
        self.assertEqual(0, young.exit_code)
        self.assertFalse(young.mutate)
        healthy = classify(_entry())
        self.assertEqual("already_queued", healthy.code)
        self.assertFalse(healthy.mutate)

    def test_api_error_is_not_an_empty_run_list(self) -> None:
        errored = classify(_entry(checks_api_error=True, checks=[]))
        empty = classify(_entry(checks=[], enqueued_at_epoch=NOW - 3600))
        self.assertEqual("api_error", errored.code)
        self.assertEqual(1, errored.exit_code)
        self.assertNotEqual(errored.code, empty.code)
        self.assertFalse(errored.mutate)
        self.assertFalse(may_enqueue(_entry(
            checks_api_error=True,
            queued=False,
            opt_in_head=HEAD,
            opt_in_verified=True,
            checks=_checks(HEAD),
        )))

    def test_opt_in_is_head_bound_and_off_by_default(self) -> None:
        ready = _entry(
            queued=False,
            opt_in_head=HEAD,
            opt_in_verified=True,
            checks=_checks(HEAD),
            installer_result="success",
        )
        self.assertTrue(may_enqueue(ready))
        self.assertFalse(may_enqueue(_entry(queued=False, checks=_checks(HEAD))))
        self.assertFalse(may_enqueue({**ready, "opt_in_verified": False}))
        self.assertFalse(may_enqueue({**ready, "opt_in_head": "d" * 40}))

    def test_abandoned_and_cancelled_installer_results_are_not_green(self) -> None:
        self.assertTrue(installer_result_is_green("success"))
        self.assertTrue(installer_result_is_green("skipped"))
        self.assertTrue(conclusion_is_green("success"))
        for value in ("abandoned", "cancelled", "canceled", "failure", "", None):
            self.assertFalse(installer_result_is_green(value), value)
        verdict = classify(_entry(installer_result="abandoned"))
        self.assertEqual("installer_not_green", verdict.code)
        self.assertEqual(1, verdict.exit_code)
        self.assertFalse(verdict.mutate)
        cancelled = _entry(checks=_checks(QUEUE) + [
            {"name": "Installer verification (macos-15)", "sha": QUEUE, "conclusion": "cancelled"},
        ])
        self.assertEqual("installer_not_green", classify(cancelled).code)
        self.assertFalse(may_enqueue(_entry(
            queued=False,
            opt_in_head=HEAD,
            opt_in_verified=True,
            checks=_checks(HEAD),
            installer_result="cancelled",
        )))


def queue_checks_green_safe(entry: dict[str, object]) -> bool:
    from scripts.ci.merge_queue_awaiting_checks_heal import queue_checks_green
    return queue_checks_green(entry)


class RequiredWorkflowsMergeGroupTest(unittest.TestCase):
    """Required status checks must declare merge_group or the queue stalls."""

    REQUIRED_FILES = (
        "pr-gate.yml",
        "release-note-governance.yml",
    )

    def test_required_workflows_declare_merge_group(self) -> None:
        documents = {}
        for name in self.REQUIRED_FILES:
            path = ROOT / ".github" / "workflows" / name
            documents[name] = yaml.safe_load(path.read_text(encoding="utf-8"))
        missing = required_workflows_declare_merge_group(documents)
        self.assertEqual([], missing)
        self.assertEqual(
            REQUIRED_MERGE_GROUP_WORKFLOWS,
            {"PR Gate", "Release-note governance"},
        )
        self.assertEqual(
            REQUIRED_QUEUE_CHECKS,
            {"PR Gate Summary", "Release-note governance"},
        )


class DependabotEnqueueDecisionTest(unittest.TestCase):
    def test_open_dependabot_pull_request_enqueues_once(self) -> None:
        self.assertEqual("enqueue", dependabot_enqueue_decision({
            "author": {"login": "app/dependabot"},
            "isDraft": False,
            "mergeable": "MERGEABLE",
            "mergeStateStatus": "CLEAN",
            "labels": [{"name": "dependencies"}],
            "mergeQueueEntry": None,
            "autoMergeRequest": None,
        }))

    def test_hold_queue_and_arm_skip(self) -> None:
        base = {
            "author": {"login": "dependabot[bot]"},
            "isDraft": False,
            "mergeable": "MERGEABLE",
            "labels": [],
            "mergeQueueEntry": None,
            "autoMergeRequest": None,
        }
        self.assertEqual("skip:hold", dependabot_enqueue_decision({
            **base, "labels": [{"name": "dependabot-hold"}],
        }))
        self.assertEqual("skip:already-queued", dependabot_enqueue_decision({
            **base, "autoMergeRequest": None, "mergeQueueEntry": {"state": "AWAITING_CHECKS"},
        }))
        self.assertEqual("skip:already-armed", dependabot_enqueue_decision({
            **base, "autoMergeRequest": {"enabledAt": "2026-11-07T00:00:00Z"},
        }))
        self.assertEqual("skip:draft", dependabot_enqueue_decision({**base, "isDraft": True}))
        self.assertEqual("skip:conflict", dependabot_enqueue_decision({
            **base, "mergeable": "CONFLICTING",
        }))
        self.assertEqual("skip:not-dependabot", dependabot_enqueue_decision({
            **base, "author": {"login": "someone"},
        }))


class DiagnoseCliTest(unittest.TestCase):
    def test_graphql_error_exits_nonzero_and_shas_do_not_fall_back_to_head(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            queue = Path(tmp) / "queue.json"
            queue.write_text(json.dumps({"errors": [{"message": "boom"}]}), encoding="utf-8")
            ran = subprocess.run(  # nosec B603 - repository script, fixed argv
                [sys.executable, str(HEAL), "diagnose", "--queue-json", str(queue)],
                capture_output=True, text=True, check=False,
            )
            shas = subprocess.run(  # nosec B603 - repository script, fixed argv
                [sys.executable, str(HEAL), "shas", "--queue-json", str(queue)],
                capture_output=True, text=True, check=False,
            )
        self.assertEqual(1, ran.returncode)
        self.assertIn("api_error", ran.stdout)
        self.assertEqual(1, shas.returncode)
        self.assertNotIn(HEAD, shas.stdout)

    def test_shas_prints_the_synthetic_commit_only(self) -> None:
        document = {
            "data": {
                "repository": {
                    "mergeQueue": {
                        "entries": {
                            "nodes": [
                                {
                                    "state": "AWAITING_CHECKS",
                                    "enqueuedAt": "2026-11-06T18:00:00Z",
                                    "headCommit": {"oid": QUEUE},
                                    "pullRequest": {"number": 7, "headRefOid": HEAD},
                                }
                            ]
                        }
                    }
                }
            }
        }
        self.assertEqual([QUEUE], queue_shas(document))
        with tempfile.TemporaryDirectory() as tmp:
            queue = Path(tmp) / "queue.json"
            checks = Path(tmp) / "checks"
            checks.mkdir()
            queue.write_text(json.dumps(document), encoding="utf-8")
            (checks / f"{QUEUE}.json").write_text(json.dumps({
                "sha": QUEUE,
                "runs": _checks(QUEUE, {"Docs": "success"}),
            }), encoding="utf-8")
            ran = subprocess.run(  # nosec B603 - repository script, fixed argv
                [sys.executable, str(HEAL), "diagnose", "--queue-json", str(queue),
                 "--checks-dir", str(checks)],
                capture_output=True, text=True, check=False,
            )
        self.assertEqual(1, ran.returncode, ran.stdout + ran.stderr)
        self.assertIn("missing_required_check", ran.stdout)
        self.assertNotIn("mutate=true", ran.stdout)


if __name__ == "__main__":
    unittest.main()
