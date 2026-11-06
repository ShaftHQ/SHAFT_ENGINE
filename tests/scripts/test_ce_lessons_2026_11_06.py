"""CE 2026-11-06 lessons: delegation, Python floor, watcher, suite lock, gates."""

from __future__ import annotations

import datetime as dt
import importlib.util
import re
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


class LessonsLedgerTest(TestCase):
    REQUIRED = (
        "chaos.md",
        "coach.md",
        "carl.md",
        "cleo.md",
        "cortex.md",
        "hrsd.md",
        "chaos-analysis.md",
        "ledger.md",
    )

    def test_full_markdown_present_and_abstract(self) -> None:
        email = re.compile(r"(?<![<\w])[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b")
        phone = re.compile(r"(?:\+|00)\d[\d().\s-]{7,}\d")
        secret = re.compile(r"\b(?:ghp_|github_pat_|sk-|xox[baprs]-|AKIA)[A-Za-z0-9_\-]{8,}")
        for name in self.REQUIRED:
            path = DETAIL / name
            self.assertTrue(path.is_file(), name)
            text = path.read_text(encoding="utf-8")
            self.assertGreater(len(text), 80, name)
            self.assertIsNone(email.search(text), name)
            self.assertIsNone(phone.search(text), name)
            self.assertIsNone(secret.search(text), name)
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        self.assertEqual("0.4.20261106.1", version)


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


class DelegationAndBoardTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_2026_11_06", CE / "rules/lessons_2026_11_06.py")
        self.prior = _load("ce_lessons_2026_11_02_ext", CE / "rules/lessons_2026_11_02.py")

    def test_delegation_cycle_red_then_green(self) -> None:
        bad = self.mod.evaluate_delegation_cycle(
            {
                "multi_agent_order": True,
                "peer_message": "do all of this in chat",
                "essay_in_chat": True,
                "owner_status_requested": True,
                "fresh_poll_complete": False,
                "radio_silence": True,
                "messaged_human": True,
                "writers_per_file": {"a.md": ["one", "two"]},
            }
        )
        self.assertFalse(bad["ok"], bad)
        self.assertIn("missing-shared-instruction-file", bad["hits"])
        good = self.mod.evaluate_delegation_cycle(
            {
                "multi_agent_order": True,
                "shared_instruction_file": "specs/order.md",
                "peer_message": "read and execute specs/order.md",
                "owner_status_requested": True,
                "fresh_poll_complete": True,
                "radio_silence": True,
                "human_asked": True,
                "messaged_human": True,
                "lessons_cycle": True,
                "one_ce_pr": True,
                "pr_merged": True,
                "installed_before_cleanup": True,
                "cleanup_started": True,
                "router_cleanup_signal": True,
                "sent_done": True,
                "every_cleanup_reported": True,
            }
        )
        self.assertTrue(good["ok"], good)

    def test_proceed_with_n_and_done_needs_a_link(self) -> None:
        bad = self.prior.evaluate_status_board(
            {
                "numbered_ids": True,
                "proceed_ids": [1],
                "items": [
                    {"id": "a", "numeric_id": 1, "owner": "o", "link": "https://example.com/a", "resumed": True},
                    {
                        "id": "b",
                        "numeric_id": 2,
                        "owner": "o",
                        "status": "done",
                        "resumed": True,
                    },
                ],
            }
        )
        self.assertFalse(bad["ok"], bad)
        self.assertTrue(any(hit.startswith("proceed-woke-other") for hit in bad["hits"]))
        self.assertTrue(any(hit.startswith("done-without-link-or-file") for hit in bad["hits"]))
        good = self.prior.evaluate_status_board(
            {
                "numbered_ids": True,
                "proceed_ids": [1],
                "items": [
                    {
                        "id": "a",
                        "numeric_id": 1,
                        "owner": "o",
                        "link": "https://example.com/a",
                        "resumed": True,
                        "status": "in progress",
                    }
                ],
            }
        )
        self.assertTrue(good["ok"], good)

    def test_token_restore_is_not_resume(self) -> None:
        bad = self.prior.evaluate_run_state(
            {"token_restored": True, "resumed_work": True, "about_to_dispatch": True}
        )
        self.assertIn("token-restore-is-not-resume", bad["hits"])
        self.assertIn("dispatch-without-run-state-check", bad["hits"])
        good = self.prior.evaluate_run_state(
            {
                "token_restored": True,
                "resumed_work": True,
                "proceed_id": 1,
                "about_to_dispatch": True,
                "run_state_checked": True,
                "run_state": "active",
            }
        )
        self.assertTrue(good["ok"], good)


class ExtendedGatesTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_2026_11_06_gates", CE / "rules/lessons_2026_11_06.py")
        self.prior = _load("ce_lessons_2026_11_02_gates", CE / "rules/lessons_2026_11_02.py")
        self.pdca = _load("ce_pdca_1106", CE / "rules/lessons_repeat_failure_pdca.py")

    def test_docs_hygiene_and_contribution_and_job_apply(self) -> None:
        self.assertFalse(
            self.mod.evaluate_docs_hygiene(
                {
                    "require_sources": True,
                    "source_of_truth_files": ["a"],
                    "status_rows": [{"status": "WIP"}],
                    "hours_tracked": True,
                    "done_hours": 1,
                    "planned_hours": 2,
                    "partial_journey": True,
                    "merged_as_full": True,
                    "delete_guard_change": True,
                    "review_claimed": True,
                    "review_sha": "abc",
                    "head_sha": "def",
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_docs_hygiene(
                {
                    "require_sources": True,
                    "source_of_truth_files": ["a", "b", "c"],
                    "status_rows": [{"status": "Done"}],
                    "hours_tracked": True,
                    "done_hours": 3,
                    "planned_hours": 3,
                    "live_formulas": True,
                    "journey_maps": True,
                    "journey_maps_agree": True,
                    "review_claimed": True,
                    "review_sha": "abc",
                    "head_sha": "abc",
                }
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_external_contribution({"closing": True, "fork_pr": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_external_contribution(
                {
                    "closing": True,
                    "upstream_merged": True,
                    "upstream_released": True,
                    "product_adopted": True,
                    "fork_pr": True,
                    "fork_label_asked": True,
                    "upstream_text": "English only text.",
                }
            )["ok"]
        )
        bad_apply = self.mod.evaluate_job_apply(
            {
                "remote_role": True,
                "subject": "Application",
                "body": "hello",
                "sending": True,
                "reviewers": ["a"],
                "ats_identical_errors": 2,
                "tracker_write": True,
                "treated_ledger_as_source": True,
                "contacts": ["c1", "c2"],
                "single_application": True,
            }
        )
        self.assertFalse(bad_apply["ok"], bad_apply)
        good_apply = self.mod.evaluate_job_apply(
            {
                "remote_role": True,
                "subject": "Remote application",
                "body": "I am seeking a remote role.",
                "sending": True,
                "reviewers": ["a", "b"],
                "sent_readback": True,
                "signature_count": 1,
                "ats_identical_errors": 2,
                "switched_channel": True,
                "tracker_write": True,
                "tracker_readback": True,
            }
        )
        self.assertTrue(good_apply["ok"], good_apply)

    def test_cover_letter_secrets_force_and_cleanup(self) -> None:
        letter = self.mod.evaluate_cover_letter(
            "I am the CEO and I built the platform.",
            claimed_tools=["Widget"],
            sourced_tools=["Other"],
            client_names=["Acme"],
        )
        self.assertIn("founder-or-ceo-title", letter["hits"])
        self.assertIn("unsourced-tool:Widget", letter["hits"])
        clean = self.mod.evaluate_cover_letter(
            "I am seeking a remote role.",
            claimed_tools=["Widget"],
            sourced_tools=["Widget"],
        )
        self.assertTrue(clean["ok"], clean)
        self.assertFalse(self.mod.evaluate_secrets_text("token ghp_abcdefghij123456", kind="lessons")["ok"])
        self.assertFalse(self.mod.evaluate_force_push("git push --force origin HEAD")["ok"])
        self.assertTrue(self.mod.evaluate_force_push("git push --force origin HEAD", approved=True)["ok"])
        self.assertFalse(
            self.mod.evaluate_private_remote({"private_working_copy": True, "adding_remote": True})["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_relayed_delete(
                {"delete_requested": True, "code_relayed_by_agent": True}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_cleanup_safety(
                {"created_paths": ["a"], "registered_paths": [], "delete_worktree": True, "unpushed": True}
            )["ok"]
        )

    def test_media_cancel_live_scheduler_and_review(self) -> None:
        self.assertIn(
            "missing-human-listen-start",
            self.prior.evaluate_clean_start({"require_human_listen": True})["hits"],
        )
        self.assertTrue(
            self.prior.evaluate_clean_start(
                {"require_human_listen": True, "human_listen_start": True, "human_listen_end": True}
            )["ok"]
        )
        self.assertIn(
            "missing-post-cancel-live-recheck",
            self.prior.evaluate_stop_scope_guard({"cancelled_after_live_write": True})["hits"],
        )
        self.assertIn(
            "write-probe-on-live-sot",
            self.prior.evaluate_live_change_diff(
                {"live_source_of_truth": True, "write_probe": True}
            )["hits"],
        )
        self.assertIn(
            "missing-redownload-before-edit",
            self.prior.evaluate_live_change_diff({"about_to_edit_live": True})["hits"],
        )
        self.assertIn(
            "missing-studio-duplicate-search",
            self.prior.evaluate_platform_scheduler({"uploading": True})["hits"],
        )
        self.assertIn(
            "hashtag-helper-without-retype",
            self.prior.evaluate_platform_scheduler({"hashtag_helper_used": True})["hits"],
        )
        review = self.prior.evaluate_adversarial_pass_sha(
            {
                "external_cli_failed": True,
                "head_sha": "abc",
                "designer_generated": True,
                "designer": "d",
                "reviewer": "d",
            }
        )
        self.assertIn("missing-fallback-reason", review["hits"])
        self.assertIn("designer-reviewed-own-work", review["hits"])
        passed = self.prior.evaluate_adversarial_pass_sha(
            {
                "external_cli_failed": True,
                "fallback_reason": "cli missing",
                "head_sha": "abc",
                "verdicts": [
                    {"method": "cli", "worker": "a", "sha": "abc"},
                    {"method": "reader", "worker": "b", "sha": "abc"},
                ],
                "designer_generated": True,
                "designer": "d",
                "reviewer": "r",
                "independent_review": True,
            }
        )
        self.assertTrue(passed["ok"], passed)

    def test_pr_body_watchdog_plugin_issue_and_pdca_push(self) -> None:
        self.assertFalse(self.mod.evaluate_pr_body("## Plan\nno receipt")["ok"])
        self.assertTrue(self.mod.evaluate_pr_body("retrieve: skipped(read-only check)\n")["ok"])
        self.assertFalse(
            self.mod.evaluate_long_task_watchdog({"expected_minutes": 45, "closed": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_long_task_watchdog(
                {
                    "expected_minutes": 45,
                    "hourly_watchdog": True,
                    "progress_file": "progress.md",
                    "closed": True,
                    "watchdog_removed": True,
                }
            )["ok"]
        )
        self.assertFalse(self.mod.evaluate_plugin_delivery({"plugin_or_ide": True})["ok"])
        self.assertTrue(
            self.mod.evaluate_plugin_delivery(
                {
                    "plugin_or_ide": True,
                    "real_ide": True,
                    "headless_display": True,
                    "evidence_folder": "evidence/",
                }
            )["ok"]
        )
        self.assertFalse(self.mod.evaluate_public_issue_text("hello", reviewed=False)["ok"])
        self.assertTrue(self.mod.evaluate_public_issue_text("hello", reviewed=True)["ok"])
        self.assertFalse(self.mod.evaluate_email_resend({"resend": True})["ok"])
        self.assertFalse(self.mod.evaluate_gate_seeded_defect({"new_gate": True})["ok"])
        self.assertFalse(self.mod.evaluate_bot_test_run({"headless": False, "report_serve": True})["ok"])
        push = self.pdca.evaluate_repeat_failure_pdca(
            {"stage": "unit", "consecutive_failures": [], "push_attempted": True, "consecutive_failure_count": 2}
        )
        self.assertIn("push-without-rca", push["hits"])


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
            ]
        )
        self.assertEqual(["a.py"], grouped["bot"])
        self.assertIn("reviewer", grouped)
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


class PythonProvisioningTest(TestCase):
    def test_windows_install_provisions_before_the_prompt(self) -> None:
        ps1 = (CE / "install.ps1").read_text(encoding="utf-8")
        sh = (CE / "install.sh").read_text(encoding="utf-8")
        for text, prompt in (
            (ps1, "Confirm Download bootstrap"),
            (sh, "Confirm Download bootstrap"),
        ):
            self.assertLess(text.index("python install 3.14"), text.index(prompt))
            self.assertIn("hook-python", text)
        self.assertIn("Install-ChaosEngineManagedPython", ps1)
        self.assertLess(ps1.index("Install-ChaosEngineManagedPython $managedUv"), ps1.index("Confirm Download bootstrap"))
        hosts = _load("ce_hosts_1106", CE / "hosts.py")
        reason = hosts.HOOK_LAUNCH_PYTHON_BLOCK
        self.assertIn("no Python 3.11+", reason)
        self.assertIn("hooks need it", reason)
        self.assertIn("rerun installer", reason)
        command = hosts.chaos_guard_locator_command(
            windows=True, host="copilot", event="userPromptSubmitted"
        )
        self.assertLessEqual(len(command), hosts.HOOK_COMMAND_MAX_CHARS)
        self.assertIn(reason, command)

    def test_locate_supported_python_ignores_path(self) -> None:
        installer = _load("ce_install_floor_1106", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            missing = installer.locate_supported_python([root / "absent"])
            self.assertIsNone(missing)
            bin_dir = root / "cpython" / "bin"
            bin_dir.mkdir(parents=True)
            link = bin_dir / "python3"
            link.symlink_to(sys.executable)
            found = installer.locate_supported_python([root])
            self.assertIsNotNone(found)
            self.assertTrue(installer.interpreter_meets_floor(found))


class DocsHygieneSkillTest(TestCase):
    def test_skill_is_on_demand_and_linked(self) -> None:
        skill = (CE / "skills/docs-hygiene/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("name: docs-hygiene", skill)
        self.assertIn("On demand only", skill)
        self.assertIn("Done", skill)
        self.assertIn("In progress", skill)
        self.assertIn("Not started", skill)
        router = (CE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertNotIn("docs-hygiene", router)
        catalog = (CE / "references/level-1-catalog.md").read_text(encoding="utf-8")
        self.assertIn("skills/docs-hygiene/SKILL.md", catalog)
        delegation = (CE / "references/delegation-cycle.md").read_text(encoding="utf-8")
        self.assertIn("shared instruction file", delegation.casefold())
        self.assertIn("delegation-cycle.md", (CE / "references/bot-entry.md").read_text(encoding="utf-8"))


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


if __name__ == "__main__":
    raise SystemExit(main())
