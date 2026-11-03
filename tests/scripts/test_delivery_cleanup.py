#!/usr/bin/env python3
"""Tests for delivery cleanup close-gate helpers (#7755)."""
from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts.ci import delivery_cleanup, worker_ownership
from scripts.ci.box_disk_guard import item_close_blockers
from scripts.ci.worktree_hygiene import task_cleanup_blockers


class WorkerOwnershipLedgerTests(unittest.TestCase):
    def test_record_mark_and_remaining(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            artifact = root / "wt-demo"
            artifact.mkdir()
            manifest = worker_ownership.record(
                "t7755",
                artifact,
                kind="worktree",
                checkpoints_dir=root,
            )
            self.assertTrue(manifest.is_file())
            leftover = worker_ownership.remaining_artifacts(manifest)
            self.assertEqual(1, len(leftover))
            self.assertTrue(worker_ownership.remaining_artifact_blockers(manifest))
            worker_ownership.mark_accounted(manifest, artifact, disposition="removed")
            # still on disk but accounted → not a blocker
            self.assertEqual([], worker_ownership.remaining_artifacts(manifest))
            self.assertEqual([], worker_ownership.remaining_artifact_blockers(manifest))

    def test_rejects_unknown_kind(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            with self.assertRaises(ValueError):
                worker_ownership.record(
                    "t7755",
                    root / "x",
                    kind="not-a-kind",
                    checkpoints_dir=root,
                )


class ReportServerMatchTests(unittest.TestCase):
    def test_matches_headless_and_allure_only(self) -> None:
        cmdlines = {
            11: "/usr/bin/google-chrome --headless --disable-gpu about:blank",
            12: "/usr/bin/allure serve /tmp/results -p 8080",
            13: "/usr/bin/google-chrome --user-data-dir=/home/box/chrome-profile",
            14: "python3 -m http.server 8000",
        }
        hits = delivery_cleanup.list_leftover_report_servers(
            proc_pids=list(cmdlines),
            read_cmdline=lambda pid: cmdlines[pid],
        )
        kinds = sorted(h["kind"] for h in hits)
        self.assertEqual(["allure-serve", "headless-chrome"], kinds)
        self.assertEqual(
            ["report-servers-remain:2"],
            delivery_cleanup.leftover_report_server_blockers(hits),
        )

    def test_stop_signals_matched_pids(self) -> None:
        sent: list[tuple[int, int]] = []
        rows = [
            {"pid": 42, "kind": "allure-serve", "cmdline": "allure serve /workspace/wt-mine/results"},
            {"pid": 43, "kind": "allure-serve", "cmdline": "allure serve /workspace/wt-other/results"},
        ]
        out = delivery_cleanup.stop_leftover_report_servers(
            rows,
            owned_path_substrings=["/workspace/wt-mine"],
            kill=lambda pid, sig: sent.append((pid, sig)),
        )
        self.assertEqual([(42, delivery_cleanup.signal.SIGTERM)], sent)
        self.assertEqual(1, len(out))
        # Without owned substrings, report-only (no signals).
        sent.clear()
        out2 = delivery_cleanup.stop_leftover_report_servers(
            rows, kill=lambda pid, sig: sent.append((pid, sig))
        )
        self.assertEqual([], sent)
        self.assertEqual([], out2)


class DeliveryCloseGateTests(unittest.TestCase):
    def test_fails_on_merged_worktree_and_artifacts(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            wt = root / "wt-left"
            wt.mkdir()
            manifest = worker_ownership.record(
                "t7755", wt, kind="worktree", checkpoints_dir=root
            )
            blockers = delivery_cleanup.delivery_close_blockers(
                git_status_clean=True,
                ownership_manifest=manifest,
                leftover_merged_worktrees=[str(wt)],
                leftover_report_servers=[{"pid": 9, "kind": "headless-chrome"}],
            )
            self.assertTrue(any(b.startswith("merged-worktrees-remain") for b in blockers))
            self.assertTrue(any(b.startswith("registered-artifacts-remain") for b in blockers))
            self.assertTrue(any(b.startswith("report-servers-remain") for b in blockers))

    def test_item_close_extras_empty_when_clean(self) -> None:
        self.assertEqual(
            [],
            item_close_blockers(
                git_status_clean=True,
                created_files=[],
                created_files_accounted=True,
                leftover_merged_worktrees=[],
                leftover_registered_artifacts=[],
                leftover_report_servers=[],
            ),
        )


class MergedAncestorSkipTests(unittest.TestCase):
    def test_merged_skips_ancestor_requirement(self) -> None:
        entry = {
            "path": "/tmp/owned",
            "branch": "ChaosEngine/owned-task",
            "is_current": False,
            "is_main": False,
            "locked": False,
            "prunable": False,
            "uncommitted_files": 0,
        }
        evidence = {
            "owned_path": Path("/tmp/owned"),
            "owned_branch": "ChaosEngine/owned-task",
            "expected_head": "a" * 40,
            "actual_head": "a" * 40,
            "actual_branch_head": "a" * 40,
            "expected_diff_sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "actual_diff_sha256": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "pull_request_state": "MERGED",
            "is_ancestor": False,
            "active_operations": [],
            "ownership_verified": True,
        }
        self.assertEqual([], task_cleanup_blockers(entry, **evidence))
        evidence_open = {**evidence, "pull_request_state": "OPEN"}
        self.assertIn("branch-not-upstream", task_cleanup_blockers(entry, **evidence_open))
        self.assertIn("pull-request-not-merged", task_cleanup_blockers(entry, **evidence_open))


if __name__ == "__main__":
    unittest.main()
