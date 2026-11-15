"""CE 2026-11-06 lessons: delegation, Python floor, watcher, suite lock, gates."""

from __future__ import annotations

import datetime as dt
import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from unittest import TestCase, main

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DETAIL = CE / "references/details/ce-lessons-2026-11-06"
sys.path.insert(0, str(ROOT))

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod

class VersionFutureLintTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_version_1106", ROOT / "scripts/ci/ce_version.py")

    def test_future_stamp_rejected_and_today_allowed(self) -> None:
        today = dt.date(2026, 11, 6)
        self.assertIsNone(self.mod.future_version_error("0.4.20261106.1", today))
        message = self.mod.future_version_error("0.4.20261107.1", today)
        self.assertIsNotNone(message)
        self.assertIn("future-dated", message or "")
        # Ordering of a future stamp still works; the lint is separate.
        self.assertLess(
            self.mod.version_key("0.4.20261231.1"),
            self.mod.version_key("0.1.20270101.1"),
        )

class WatcherSuiteAndMergeTest(TestCase):
    def test_stuck_head_dequeue_and_unbounded_preflight(self) -> None:
        watch = _load("ce_watch_1106", ROOT / "scripts/agents/watch_pr_checks.py")
        self.assertEqual("queue position 2 of 5", watch.queue_position_line(2, 5))
        self.assertEqual(
            "stuck-queue-head",
            watch.stuck_queue_head(
                {"enqueued": True, "isMergeQueueHead": True, "minutes_since_merge_group": 25}
            ),
        )
        self.assertIsNone(watch.stuck_queue_head({"enqueued": True, "queuePosition": 3, "minutes_since_merge_group": 25}))
        self.assertEqual("dequeued", watch.dequeue_wake({"dequeued": True, "state": "OPEN"}))
        bucket, failing = watch.classify_unattended(
            [{"name": "unit", "state": "SUCCESS", "link": ""}],
            {"state": "OPEN", "dequeued": True},
        )
        self.assertEqual("RED", bucket)
        self.assertEqual("dequeued", failing[0]["name"])
        self.assertEqual(["unbounded-loop"], watch.preflight_unbounded_loops("while True:\n    poll()\n"))
        self.assertEqual([], watch.preflight_unbounded_loops("for attempt in range(max_polls):\n    pass\n"))

    def test_merge_ready_groups_authors_and_match_head(self) -> None:
        ready = _load("ce_merge_ready_1106", ROOT / "scripts/ci/merge_ready.py")
        grouped = ready.unresolved_threads_by_author(
            [
                {"author": "bot", "resolved": False, "path": "a.py"},
                {"author": "bot", "isResolved": True, "path": "gone"},
                {"authorLogin": "reviewer", "title": "nit"},
                {
                    "isResolved": False,
                    "path": "g.py",
                    "comments": {"nodes": [{"author": {"login": "graphql-author"}}]},
                },
            ]
        )
        self.assertEqual(["a.py"], grouped["bot"])
        self.assertIn("reviewer", grouped)
        self.assertEqual(["g.py"], grouped["graphql-author"])
        command = ready.merge_command(12, "abc123")
        self.assertIn("--match-head-commit abc123", command)
        with self.assertRaises(ValueError):
            ready.merge_command(12, "")

    def test_suite_wrapper_refuses_a_second_run(self) -> None:
        text = (ROOT / "scripts/ci/run_full_scripts_unittest.py").read_text(encoding="utf-8")
        self.assertIn("blocking=False", text)
        self.assertIn("refusing a second full-suite run", text)
        self.assertIn("foreground", text)
        self.assertIn("os.execvpe", text)
        overlay = _load("ce_overlay_lock_1106", ROOT / "scripts/ci/overlay_pre_push.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with overlay.full_scripts_unittest_lock(root, blocking=False):
                with self.assertRaises(BlockingIOError):
                    with overlay.full_scripts_unittest_lock(root, blocking=False):
                        pass

class CloseBlockersTest(TestCase):
    def test_live_publish_and_renders_fail_closed(self) -> None:
        cleanup = _load("ce_delivery_cleanup_1106", ROOT / "scripts/ci/delivery_cleanup.py")
        blocked = cleanup.delivery_close_blockers(
            git_status_clean=True,
            live_publish=True,
            publish_package={},
            media_delivery={},
            dirty_unpushed_worktrees=["wt"],
            leftover_browsers=["chrome"],
            allure_serve=True,
            leftover_report_servers=[],
        )
        self.assertIn("live-publish-without-coach-accept", blocked)
        self.assertIn("missing-390px-render", blocked)
        self.assertIn("missing-desktop-render", blocked)
        self.assertIn("missing-contrast-note", blocked)
        self.assertIn("media-missing-human-listen", blocked)
        self.assertTrue(any(item.startswith("dirty-unpushed-worktrees") for item in blocked))
        self.assertIn("allure-serve-running", blocked)
        clear = cleanup.delivery_close_blockers(
            git_status_clean=True,
            live_publish=True,
            coach_accept={"accepted": True, "facts_checked": ["title", "year"]},
            publish_package={"render_390": True, "render_desktop": True, "contrast_note": "pass"},
            media_delivery={"human_listen_start": True, "human_listen_end": True},
            leftover_report_servers=[],
        )
        self.assertEqual([], clear)

    def test_omitted_none_and_wrong_type_evidence_fail_closed(self) -> None:
        cleanup = _load("ce_delivery_cleanup_absent", ROOT / "scripts/ci/delivery_cleanup.py")
        omitted = cleanup.delivery_close_blockers(
            git_status_clean=True,
            live_publish=True,
            delivery_kind="video",
            leftover_report_servers=[],
        )
        self.assertIn("missing-publish-package", omitted)
        self.assertIn("missing-390px-render", omitted)
        self.assertIn("missing-media-evidence", omitted)
        self.assertIn("media-missing-human-listen", omitted)
        none_package = cleanup.delivery_close_blockers(
            git_status_clean=True,
            live_publish=True,
            publish_package=None,
            delivery_kind="visual",
            leftover_report_servers=[],
        )
        self.assertIn("missing-publish-package", none_package)
        self.assertIn("missing-contrast-note", none_package)
        wrong = cleanup.delivery_close_blockers(
            git_status_clean=True,
            live_publish=True,
            publish_package="not-a-package",
            media_delivery="not-a-record",
            delivery_kind="audio",
            leftover_report_servers=[],
        )
        self.assertIn("missing-publish-package", wrong)
        self.assertIn("missing-media-evidence", wrong)
        media_only = cleanup.delivery_close_blockers(
            git_status_clean=True,
            delivery_kind="video",
            leftover_report_servers=[],
        )
        self.assertIn("missing-media-evidence", media_only)
        self.assertNotIn("missing-publish-package", media_only)

class QueueSnapshotTest(TestCase):
    def test_graphql_fetch_populates_classifier_fields(self) -> None:
        watch = _load("ce_watch_queue_1106", ROOT / "scripts/agents/watch_pr_checks.py")

        def check(event: str, status: str, started: str, completed: str | None = None, conclusion: str | None = None) -> dict:
            item = {
                "name": event,
                "startedAt": started,
                "status": status,
                "checkSuite": {"workflowRun": {"event": event}},
            }
            if completed:
                item["completedAt"] = completed
            if conclusion:
                item["conclusion"] = conclusion
            return item

        def node_for(enqueued_at: str, group_checks: list[dict] | None) -> dict:
            head = None if group_checks is None else {
                "oid": "groupsha",
                "statusCheckRollup": {"contexts": {"nodes": group_checks}},
            }
            return {
                "state": "OPEN",
                "mergedAt": None,
                "mergeStateStatus": "CLEAN",
                "isInMergeQueue": True,
                "autoMergeRequest": None,
                "mergeQueueEntry": {"position": 1, "enqueuedAt": enqueued_at, "headCommit": head},
                "commits": {
                    "nodes": [
                        {
                            "commit": {
                                "oid": "tipsha",
                                "statusCheckRollup": {
                                    "contexts": {
                                        "nodes": [check("pull_request", "IN_PROGRESS", "2026-11-06T00:00:00Z")]
                                    }
                                },
                            }
                        }
                    ]
                },
            }

        now = dt.datetime(2026, 11, 6, 2, 0, tzinfo=dt.timezone.utc)
        green = [{"name": "unit", "state": "SUCCESS", "link": ""}]
        healthy = node_for(
            "2026-11-06T00:00:00Z",
            [check("merge_group", "IN_PROGRESS", "2026-11-06T01:55:00Z")],
        )
        view = watch.apply_queue_snapshot(healthy, now=now)
        self.assertEqual(1, view["queuePosition"])
        self.assertNotIn("queuePosition", healthy)
        self.assertEqual("tipsha", view["prTipOid"])
        self.assertEqual("groupsha", view["mergeGroupSha"])
        self.assertNotEqual(view["prTipOid"], view["mergeGroupSha"])
        self.assertIsNone(watch.stuck_queue_head(view))
        self.assertNotEqual("stuck-queue-head", watch.classify_unattended(green, view)[1][0]["name"] if watch.classify_unattended(green, view)[1] else "")
        missing = node_for("2026-11-06T01:30:00Z", None)
        missing_view = watch.apply_queue_snapshot(missing, now=now)
        self.assertEqual("absent", missing_view["mergeGroupState"])
        self.assertEqual("stuck-queue-head", watch.stuck_queue_head(missing_view))
        stalled = node_for(
            "2026-11-06T00:00:00Z",
            [check("merge_group", "IN_PROGRESS", "2026-11-06T01:20:00Z")],
        )
        self.assertEqual("stuck-queue-head", watch.stuck_queue_head(watch.apply_queue_snapshot(stalled, now=now)))
        passed = node_for(
            "2026-11-06T00:00:00Z",
            [check("merge_group", "COMPLETED", "2026-11-06T01:50:00Z", "2026-11-06T01:58:00Z", "SUCCESS")],
        )
        self.assertIsNone(watch.stuck_queue_head(watch.apply_queue_snapshot(passed, now=now)))
        stale_pass = node_for(
            "2026-11-06T00:00:00Z",
            [check("merge_group", "COMPLETED", "2026-11-06T01:00:00Z", "2026-11-06T01:20:00Z", "SUCCESS")],
        )
        self.assertEqual("stuck-queue-head", watch.stuck_queue_head(watch.apply_queue_snapshot(stale_pass, now=now)))
        node = healthy
        left = dict(node)
        left["isInMergeQueue"] = False
        left["mergeQueueEntry"] = None
        departed = watch.apply_queue_snapshot(left, previous=view, now=now)
        self.assertTrue(departed["dequeued"])
        self.assertEqual("dequeued", watch.classify_unattended(green, departed)[1][0]["name"])
        payload = json.dumps({"data": {"repository": {"pullRequest": node}}})

        class _Proc:
            returncode = 0
            stdout = payload
            stderr = ""

        import unittest.mock

        with unittest.mock.patch.object(watch, "run_gh", return_value=_Proc()) as run:
            fetched = watch.fetch_pull("gh", Path("."), "owner/name", 9, now=now)
        query = " ".join(run.call_args[0][1])
        self.assertIn("graphql", query)
        self.assertIn("headCommit", query)
        self.assertNotIn("commits(last:1){nodes{commit{statusCheckRollup", query)
        self.assertEqual(1, fetched["queuePosition"])
        self.assertEqual("groupsha", fetched["mergeGroupSha"])
        self.assertNotEqual(fetched["prTipOid"], fetched["mergeGroupSha"])
        self.assertIsNone(watch.stuck_queue_head(fetched))
        self.assertLess(fetched["minutes_since_merge_group"], 20)
        blocked = {
            "state": "OPEN",
            "mergedAt": None,
            "mergeStateStatus": "BLOCKED",
            "isInMergeQueue": False,
            "mergeQueueEntry": None,
            "commits": {"nodes": []},
        }
        blocked_proc = _Proc()
        blocked_proc.stdout = json.dumps({"data": {"repository": {"pullRequest": blocked}}})
        with (
            unittest.mock.patch.object(watch, "run_gh", return_value=blocked_proc),
            unittest.mock.patch.object(watch, "count_unresolved_threads", return_value=0),
        ):
            fetched_blocked = watch.fetch_pull("gh", Path("."), "owner/name", 9, now=now)
        self.assertEqual(0, fetched_blocked["unresolvedReviewThreads"])
        kind, rows = watch.classify_unattended(green, fetched_blocked)
        self.assertEqual("RED", kind)
        self.assertEqual("blocked-merge-state", rows[0]["name"])

    def test_failed_group_check_is_not_masked(self) -> None:
        watch = _load("ce_watch_queue_mixed_1106", ROOT / "scripts/agents/watch_pr_checks.py")

        def check(
            name: str,
            status: str,
            started: str,
            completed: str | None = None,
            conclusion: str | None = None,
            event: str = "merge_group",
        ) -> dict:
            item = {
                "name": name,
                "startedAt": started,
                "status": status,
                "checkSuite": {"workflowRun": {"event": event}},
            }
            if completed:
                item["completedAt"] = completed
            if conclusion:
                item["conclusion"] = conclusion
            return item

        def node(checks: list[dict]) -> dict:
            return {
                "state": "OPEN",
                "mergedAt": None,
                "mergeStateStatus": "CLEAN",
                "isInMergeQueue": True,
                "autoMergeRequest": None,
                "mergeQueueEntry": {
                    "position": 1,
                    "enqueuedAt": "2026-11-06T00:00:00Z",
                    "headCommit": {
                        "oid": "groupsha",
                        "statusCheckRollup": {"contexts": {"nodes": checks}},
                    },
                },
                "commits": {
                    "nodes": [
                        {
                            "commit": {
                                "oid": "tipsha",
                                "statusCheckRollup": {
                                    "contexts": {
                                        "nodes": [
                                            check(
                                                "pull tip",
                                                "IN_PROGRESS",
                                                "2026-11-06T00:00:00Z",
                                                event="pull_request",
                                            )
                                        ]
                                    }
                                },
                            }
                        }
                    ]
                },
            }

        now = dt.datetime(2026, 11, 6, 2, 0, tzinfo=dt.timezone.utc)
        green = [{"name": "unit", "state": "SUCCESS", "link": ""}]
        failed = check("gate", "COMPLETED", "2026-11-06T01:00:00Z", "2026-11-06T01:10:00Z", "FAILURE")
        later_ok = check("lint", "COMPLETED", "2026-11-06T01:50:00Z", "2026-11-06T01:58:00Z", "SUCCESS")
        later_run = check("docs", "IN_PROGRESS", "2026-11-06T01:55:00Z")

        def assert_wakes(checks: list[dict]) -> None:
            payload_node = node(checks)
            view = watch.apply_queue_snapshot(payload_node, now=now)
            self.assertEqual("failed", view["mergeGroupState"])
            self.assertEqual("groupsha", view["mergeGroupSha"])
            self.assertNotEqual(view["prTipOid"], view["mergeGroupSha"])
            self.assertEqual("stuck-queue-head", watch.stuck_queue_head(view))
            kind, rows = watch.classify_unattended(green, view)
            self.assertEqual("RED", kind)
            self.assertEqual("stuck-queue-head", rows[0]["name"])

            class _Proc:
                returncode = 0
                stdout = json.dumps({"data": {"repository": {"pullRequest": payload_node}}})
                stderr = ""

            import unittest.mock

            with unittest.mock.patch.object(watch, "run_gh", return_value=_Proc()):
                fetched = watch.fetch_pull("gh", Path("."), "owner/name", 9, now=now)
            self.assertEqual("failed", fetched["mergeGroupState"])
            fetched_kind, fetched_rows = watch.classify_unattended(green, fetched)
            self.assertEqual("RED", fetched_kind)
            self.assertEqual("stuck-queue-head", fetched_rows[0]["name"])

        assert_wakes([failed, later_ok])
        assert_wakes([later_ok, failed])
        assert_wakes([failed, later_run])
        assert_wakes([later_run, failed])
        for conclusion in ("CANCELLED", "TIMED_OUT", "ACTION_REQUIRED"):
            terminal = check("gate", "COMPLETED", "2026-11-06T01:00:00Z", "2026-11-06T01:10:00Z", conclusion)
            assert_wakes([later_ok, terminal])
        same = "2026-11-06T01:40:00Z"
        tie_fail = check("gate", "COMPLETED", same, same, "FAILURE")
        tie_ok = check("lint", "COMPLETED", same, same, "SUCCESS")
        assert_wakes([tie_fail, tie_ok])
        assert_wakes([tie_ok, tie_fail])
        tie_same_fail = check("gate", "COMPLETED", same, same, "FAILURE")
        tie_same_ok = check("gate", "COMPLETED", same, same, "SUCCESS")
        assert_wakes([tie_same_fail, tie_same_ok])
        assert_wakes([tie_same_ok, tie_same_fail])
        retry_fail = check("gate", "COMPLETED", "2026-11-06T01:00:00Z", "2026-11-06T01:10:00Z", "FAILURE")
        retry_run = check("gate", "IN_PROGRESS", "2026-11-06T01:55:00Z")
        assert_wakes([retry_fail, retry_run])
        assert_wakes([retry_run, retry_fail])
        retry_ok = check("gate", "COMPLETED", "2026-11-06T01:50:00Z", "2026-11-06T01:58:00Z", "SUCCESS")
        for order in ([retry_fail, retry_ok], [retry_ok, retry_fail]):
            superseded = watch.apply_queue_snapshot(node(order), now=now)
            self.assertEqual("passed", superseded["mergeGroupState"])
            self.assertIsNone(watch.stuck_queue_head(superseded))
            self.assertNotEqual("stuck-queue-head", watch.classify_unattended(green, superseded)[1][0]["name"] if watch.classify_unattended(green, superseded)[1] else "")
        healthy = watch.apply_queue_snapshot(
            node([check("gate", "IN_PROGRESS", "2026-11-06T01:55:00Z")]),
            now=now,
        )
        self.assertEqual("running", healthy["mergeGroupState"])
        self.assertIsNone(watch.stuck_queue_head(healthy))
        self.assertNotEqual("RED", watch.classify_unattended(green, healthy)[0])

    def test_main_runs_unbounded_preflight_before_polling(self) -> None:
        watch = _load("ce_watch_preflight_1106", ROOT / "scripts/agents/watch_pr_checks.py")
        import unittest.mock

        with unittest.mock.patch.object(watch, "preflight_unbounded_loops", return_value=["unbounded-loop"]):
            self.assertEqual(2, watch.main(["--poll-once", "--pr", "1"]))

if __name__ == "__main__":
    raise SystemExit(main())
