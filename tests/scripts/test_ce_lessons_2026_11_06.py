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
        # Structural detectors. They do not embed a standing-order paragraph or a biography.
        scope_or_identity = (
            re.compile(r"\b\d{2,}\+\s*trainees\b", re.I),
            re.compile(r"(?i)found(?:ing|ed)\b[^\n]{0,60}\b(?:19|20)\d{2}\b"),
            re.compile(r"(?i)submit\s+automatically"),
            re.compile(r"(?i)hand submissions back"),
            re.compile(r"(?i)home country is not listed"),
            re.compile(r"(?i)\bfounder and ceo\b"),
            re.compile(r'correct\s+"'),
            re.compile(r"[\u0600-\u06FF]"),
        )
        for name in self.REQUIRED:
            path = DETAIL / name
            self.assertTrue(path.is_file(), name)
            text = path.read_text(encoding="utf-8")
            self.assertGreater(len(text), 80, name)
            self.assertIsNone(email.search(text), name)
            self.assertIsNone(phone.search(text), name)
            self.assertIsNone(secret.search(text), name)
            for pattern in scope_or_identity:
                self.assertIsNone(pattern.search(text), f"{name} matched {pattern.pattern}")
            if name in {"coach.md", "carl.md", "cleo.md", "cortex.md"}:
                self.assertIn("<profile.", text, name)
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        versions = _load("ce_version_ledger_1106", ROOT / "scripts/ci/ce_version.py")
        self.assertRegex(version, r"^0\.\d+\.\d{8}\.\d+$")
        self.assertIsNone(versions.future_version_error(version, dt.date.today()))


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
            {"token_restored": True, "resumed_work": True, "about_to_dispatch": True}  # nosec B105 - gate flag, not a secret
        )
        self.assertIn("token-restore-is-not-resume", bad["hits"])
        self.assertIn("dispatch-without-run-state-check", bad["hits"])
        good = self.prior.evaluate_run_state(
            {
                "token_restored": True,  # nosec B105 - gate flag, not a secret
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
                "writer": "author",
                "authorization_record": "human-auth-1",
                "reviews": [
                    {"reviewer": "a", "verdict": "PASS"},
                    {"reviewer": "b", "verdict": "PASS"},
                ],
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
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            approval = root / ".chaos-engine-state"
            approval.mkdir()
            (approval / "force-push-approval.txt").write_text(
                "git push --force origin HEAD\n", encoding="utf-8"
            )
            self.assertTrue(
                self.mod.evaluate_force_push("git push --force origin HEAD", root=root)["ok"]
            )
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
                    {"method": "cli", "worker": "a", "reviewer": "a", "verdict": "PASS", "sha": "abc"},
                    {"method": "reader", "worker": "b", "reviewer": "b", "verdict": "PASS", "sha": "abc"},
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


class WindowsPyShimTest(TestCase):
    def test_fresh_install_runs_hook_through_the_cmd_shim(self) -> None:
        installer = CE / "install.ps1"
        installer_text = installer.read_text(encoding="utf-8")
        self.assertIn('"$quoted" "$shimQuoted" "$quoted" %*', installer_text)
        self.assertNotIn('if /I "%~1"=="-3"', installer_text)
        self.assertNotRegex(installer_text, r"Copy-Item[^\n]*windows_py_shim")
        self.assertNotIn("chaos-engine\\windows_py_shim.py", installer_text)
        shim_mod = _load("ce_windows_py_shim", CE / "windows_py_shim.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "adopter project"
            project.mkdir()
            self.assertEqual([], list(project.iterdir()))
            interpreter = project / "py dir" / "python3"
            interpreter.parent.mkdir()
            interpreter.symlink_to(sys.executable)
            hook = project / "hook dir" / "launch.py"
            hook.parent.mkdir()
            record = project / "record.txt"
            hook.write_text(
                "import os, sys\nfrom pathlib import Path\n"
                "Path(os.environ['SHIM_RECORD']).write_text('\\n'.join(sys.argv))\n",
                encoding="utf-8",
            )
            shim_dir = project / "local bin"
            env = os.environ.copy()
            env["CHAOS_ENGINE_SHIM_INTERPRETER"] = str(interpreter)
            env["CHAOS_ENGINE_SHIM_DIR"] = str(shim_dir)
            env["SHIM_RECORD"] = str(record)
            env["PATH"] = str(shim_dir)
            powershell = shutil.which("powershell") or shutil.which("pwsh")
            if sys.platform == "win32" and powershell:
                completed = subprocess.run(  # nosec B603 - the real installer switch, list argv.
                    [powershell, "-NoProfile", "-File", str(installer), "-ProvisionPythonShim"],
                    cwd=project,
                    env=env,
                    check=False,
                    capture_output=True,
                    text=True,
                )
                self.assertEqual(0, completed.returncode, completed.stderr)
            else:
                completed = subprocess.run(  # nosec B603 - apply the embedded installer payload.
                    [
                        sys.executable,
                        str(CE / "windows_py_shim.py"),
                        "--apply-installer",
                        str(installer),
                        "--interpreter",
                        str(interpreter),
                        "--shim-dir",
                        str(shim_dir),
                    ],
                    cwd=project,
                    env=env,
                    check=False,
                    capture_output=True,
                    text=True,
                )
                self.assertEqual(0, completed.returncode, completed.stderr)
            self.assertFalse((project / "chaos-engine").exists())
            written = (shim_dir / "windows_py_shim.py").read_text(encoding="utf-8")
            self.assertEqual(shim_mod.LAUNCHER_SOURCE, written)
            self.assertNotIn("render_py_cmd", written)
            ran = shim_mod.execute_installed_shim(
                str(shim_dir / "py.cmd"),
                ["-3", str(hook), "grok"],
                env,
            )
            self.assertEqual(0, ran.returncode, ran.stderr)
            recorded = record.read_text(encoding="utf-8").splitlines()
            self.assertNotIn("-3", recorded)
            self.assertEqual(str(hook), recorded[0])
            self.assertIn("grok", recorded)
            self.assertIn("hook dir", recorded[0])


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


class OperationBoundaryTest(TestCase):
    def test_push_force_remote_delete_and_secrets_use_disk_evidence(self) -> None:
        boundary = _load("ce_operation_boundary_1106", CE / "rules/operation_boundary.py")
        overlay = _load("ce_overlay_boundary_1106", ROOT / "scripts/ci/overlay_pre_push.py")
        guard = _load("ce_guard_boundary_1106", CE / "hooks/guard.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            failure = {
                "hook_event_name": "PostToolUse",
                "cwd": str(root),
                "tool_response": {"exit_code": 1, "stdout": "FAIL\n", "stderr": ""},
            }
            command = ("python3 -m unittest tests.sample",)
            for _ in range(2):
                recorded = guard._command_guard_state(
                    failure, "PostToolUse", command, "Shell", {"command": command[0]}, "", "", "session"
                )
                self.assertEqual("", recorded[2])
            pushed = overlay._operation_disk_failures(root, [])
            self.assertTrue(any("push-without-rca:test" in item for item in pushed))
            note = {
                "stage": "test",
                "failures": ["a", "b"],
                "hypothesis": "same stage failed twice",
                "evidence": "log",
                "plan": "one change",
            }
            state = root / ".chaos-engine-state" / "repeat-failure-pdca"
            (state / "test.json").write_text(json.dumps(note), encoding="utf-8")
            cleared = overlay._operation_disk_failures(root, [])
            self.assertFalse(any("push-without-rca" in item for item in cleared))
            reset = {
                "hook_event_name": "PostToolUse",
                "cwd": str(root),
                "tool_response": {"exit_code": 0, "stdout": "ok\n", "stderr": ""},
            }
            guard._command_guard_state(
                reset, "PostToolUse", command, "Shell", {"command": command[0]}, "", "", "session"
            )
            self.assertFalse(any("push-without-rca" in item for item in overlay._operation_disk_failures(root, [])))
            (state / "events.jsonl").write_text("{", encoding="utf-8")
            unavailable = overlay._operation_disk_failures(root, [])
            self.assertTrue(any("pdca-state-unavailable" in item for item in unavailable))
            push = "git push --force origin HEAD"
            self.assertIn("force-push-without-approval", guard._operation_boundary_block((push,), root))
            approval = root / ".chaos-engine-state"
            (approval / "force-push-approval.txt").write_text(push + "\n", encoding="utf-8")
            self.assertEqual("", guard._operation_boundary_block((push,), root))
            (approval / "private-working-copy").write_text("private\n", encoding="utf-8")
            remote = boundary.command_blocks(root, "git remote add origin https://example.invalid/private.git")
            self.assertIn("remote-on-private-copy", remote)
            deleted = boundary.command_blocks(root, "cleanup --delete-code 123456")
            self.assertIn("relayed-delete-code", deleted)
            secret = root / "status.md"
            secret.write_text("token ghp_abcdefghij123456\n", encoding="utf-8")
            subprocess.run(["git", "init"], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607
            subprocess.run(["git", "config", "user.email", "dev@example.com"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "config", "user.name", "Dev"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "add", "status.md"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "commit", "-m", "status"], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607
            secret.write_text("clean\n", encoding="utf-8")
            self.assertNotIn("ghp_", secret.read_text(encoding="utf-8"))
            file_hits = boundary.disk_blocks(root, ["status.md"])
            self.assertTrue(any(item.startswith("secret-in-commit") for item in file_hits))
            staged = root / "notes.md"
            staged.write_text("github_pat_abcdefghij\n", encoding="utf-8")
            subprocess.run(["git", "add", "notes.md"], cwd=root, check=True)  # nosec B603 B607
            staged.write_text("clean\n", encoding="utf-8")
            staged_hits = boundary.committed_secret_failures(root)
            self.assertTrue(any(item.startswith("secret-in-commit") for item in staged_hits))
            leaked = {
                "hook_event_name": "PostToolUse",
                "cwd": str(root),
                "tool_response": {"exit_code": 0, "stdout": "ghp_abcdefghij123456\n", "stderr": ""},
            }
            output_block = guard._command_guard_state(
                leaked, "PostToolUse", ("python3 -c \"print(1)\"",), "Shell", {"command": "python3 -c \"print(1)\""}, "", "", "session"
            )[2]
            self.assertIn("secret-in-output", output_block)
            blocked_state = root / "blocked-state"
            blocked_state.mkdir()
            (blocked_state / ".chaos-engine-state" / "repeat-failure-pdca").mkdir(parents=True)
            (blocked_state / ".chaos-engine-state" / "repeat-failure-pdca" / "events.jsonl").mkdir()
            self.assertIn("pdca-state-unavailable", guard._command_guard_state(
                {**failure, "cwd": str(blocked_state)},
                "PostToolUse",
                command,
                "Shell",
                {"command": command[0]},
                "",
                "",
                "session-2",
            )[2])
            command_hits = boundary.command_blocks(root, "echo ghp_abcdefghij123456")
            self.assertTrue(any(item.startswith("secret-in-command") for item in command_hits))
            watcher = root / "scripts" / "agents" / "watch_pr_checks.py"
            watcher.parent.mkdir(parents=True)
            watcher.write_text("while True:\n    poll()\n", encoding="utf-8")
            loops = boundary.disk_blocks(root, ["scripts/agents/watch_pr_checks.py"])
            self.assertIn("watcher:unbounded-loop", loops)

    def test_failure_hook_events_reach_the_pre_push_consumer(self) -> None:
        overlay = _load("ce_overlay_failure_hook_1106", ROOT / "scripts/ci/overlay_pre_push.py")
        guard = _load("ce_guard_failure_hook_1106", CE / "hooks/guard.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            command = ("python3 -m unittest tests.sample",)
            missing_code = {
                "hook_event_name": "PostToolUseFailure",
                "cwd": str(root),
                "tool_response": {"stdout": "FAIL\n", "stderr": ""},
            }
            alternate_result = {
                "hook_event_name": "PostToolUseFailure",
                "cwd": str(root),
                "tool_result": {"output": "still failing\n"},
            }
            for event in (missing_code, alternate_result):
                recorded = guard._command_guard_state(
                    event,
                    "PostToolUseFailure",
                    command,
                    "Shell",
                    {"command": command[0]},
                    "",
                    "",
                    "session",
                )
                self.assertEqual("", recorded[2])
            pushed = overlay._operation_disk_failures(root, [])
            self.assertTrue(any(item == "push-without-rca:test" or item.endswith("push-without-rca:test") for item in pushed))
            secret_root = Path(temporary) / "secret output"
            secret_root.mkdir()
            leaked = {
                "hook_event_name": "PostToolUseFailure",
                "cwd": str(secret_root),
                "tool_response": {"stdout": "ghp_abcdefghij123456\n"},
            }
            reason = guard._command_guard_state(
                leaked,
                "PostToolUseFailure",
                ("python3 -c \"print(1)\"",),
                "Shell",
                {"command": "python3 -c \"print(1)\""},
                "",
                "",
                "session",
            )[2]
            self.assertIn("secret-in-output", reason)
            self.assertNotIn("ghp_", reason)

    def test_removed_outgoing_secret_still_fails_closed(self) -> None:
        boundary = _load("ce_operation_history_1106", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            secret = root / "status.md"
            subprocess.run(["git", "init"], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607
            subprocess.run(["git", "config", "user.email", "dev@example.com"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "config", "user.name", "Dev"], cwd=root, check=True)  # nosec B603 B607
            secret.write_text("token ghp_abcdefghij123456\n", encoding="utf-8")
            subprocess.run(["git", "add", "status.md"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "commit", "-m", "add status"], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607
            secret.write_text("clean\n", encoding="utf-8")
            subprocess.run(["git", "add", "status.md"], cwd=root, check=True)  # nosec B603 B607
            subprocess.run(["git", "commit", "-m", "remove status"], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607
            net = subprocess.run(  # nosec B603 B607 - fixed git diff of the temp repo.
                ["git", "diff", "--no-color", "4b825dc642cb6eb9a060e54bf8d69288fbee4904", "HEAD"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            )
            self.assertNotIn("ghp_", net.stdout)
            self.assertNotIn("ghp_", secret.read_text(encoding="utf-8"))
            hits = boundary.disk_blocks(root, ["status.md"])
            self.assertTrue(any(item.startswith("secret-in-commit") for item in hits))
            self.assertFalse(any("ghp_" in item for item in hits))

    def test_merge_resolution_secret_still_fails_closed(self) -> None:
        boundary = _load("ce_operation_merge_history_1106", CE / "rules/operation_boundary.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)

            def git(*args: str) -> None:
                subprocess.run(["git", *args], cwd=root, check=True, capture_output=True, text=True)  # nosec B603 B607

            git("init")
            git("config", "user.email", "dev@example.com")
            git("config", "user.name", "Dev")
            (root / "status.md").write_text("clean\n", encoding="utf-8")
            git("add", "status.md")
            git("commit", "-m", "root")
            git("checkout", "-b", "left")
            (root / "left.md").write_text("left\n", encoding="utf-8")
            git("add", "left.md")
            git("commit", "-m", "left")
            git("checkout", "-b", "right", "HEAD~1")
            (root / "right.md").write_text("right\n", encoding="utf-8")
            git("add", "right.md")
            git("commit", "-m", "right")
            git("checkout", "left")
            git("merge", "right", "--no-commit")
            (root / "status.md").write_text("token ghp_abcdefghij123456\n", encoding="utf-8")
            git("add", "status.md")
            git("commit", "-m", "merge in")
            git("checkout", "-b", "later", "right")
            (root / "later.md").write_text("later\n", encoding="utf-8")
            git("add", "later.md")
            git("commit", "-m", "later")
            git("checkout", "left")
            git("merge", "later", "--no-commit")
            (root / "status.md").write_text("clean\n", encoding="utf-8")
            git("add", "status.md")
            git("commit", "-m", "merge out")
            empty = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
            plain = subprocess.run(  # nosec B603 B607 - fixed git log of the temp repo.
                ["git", "log", "-p", "--no-color", f"{empty}..HEAD"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            )
            net = subprocess.run(  # nosec B603 B607 - fixed git diff of the temp repo.
                ["git", "diff", "--no-color", empty, "HEAD"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            )
            index = subprocess.run(  # nosec B603 B607 - fixed git diff of the temp index.
                ["git", "diff", "--cached", "--no-color"],
                cwd=root,
                check=True,
                capture_output=True,
                text=True,
            )
            self.assertNotIn("ghp_", plain.stdout)
            self.assertNotIn("ghp_", net.stdout)
            self.assertNotIn("ghp_", index.stdout)
            self.assertNotIn("ghp_", (root / "status.md").read_text(encoding="utf-8"))
            hits = boundary.disk_blocks(root, ["status.md"])
            self.assertTrue(any(item.startswith("secret-in-commit") for item in hits))
            self.assertFalse(any("ghp_" in item for item in hits))


class AdversarialEvidenceTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_0206_adv", CE / "rules/lessons_2026_11_02.py")

    def _hits(self, record: dict) -> list[str]:
        return self.mod.evaluate_adversarial_pass_sha(record)["hits"]

    def test_missing_sha_fails(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "verdicts": [
                    {"worker": "a", "reviewer": "a", "verdict": "PASS", "method": "cli"},
                    {"worker": "b", "reviewer": "b", "verdict": "PASS", "method": "reader", "sha": "abc"},
                ],
            }
        )
        self.assertIn("verdict-missing-sha", hits)

    def test_missing_verdict_value_fails(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "verdicts": [
                    {"worker": "a", "reviewer": "a", "method": "cli", "sha": "abc"},
                    {"worker": "b", "reviewer": "b", "method": "reader", "sha": "abc"},
                ],
            }
        )
        self.assertIn("verdict-not-pass", hits)

    def test_same_worker_different_method_fails(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "verdicts": [
                    {"worker": "same", "reviewer": "same", "verdict": "PASS", "method": "cli", "sha": "abc"},
                    {"worker": "same", "reviewer": "same", "verdict": "PASS", "method": "reader", "sha": "abc"},
                ],
            }
        )
        self.assertIn("verdicts-not-independent", hits)

    def test_self_review_fails(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "designer": "d",
                "designer_generated": True,
                "reviewer": "r",
                "verdicts": [
                    {"worker": "d", "reviewer": "d", "verdict": "PASS", "sha": "abc"},
                    {"worker": "b", "reviewer": "b", "verdict": "PASS", "sha": "abc"},
                ],
            }
        )
        self.assertIn("self-review", hits)

    def test_missing_identities_do_not_pass_on_a_flag(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "designer_generated": True,
                "independent_review": True,
            }
        )
        self.assertIn("missing-producer-or-reviewer", hits)
        self.assertIn("missing-second-verdict", hits)

    def test_two_verdicts_required_without_a_cli_failure(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "verdicts": [
                    {"method": "cli"},
                    {"method": "reader"},
                ],
            }
        )
        self.assertIn("verdict-not-pass", hits)
        self.assertIn("verdict-missing-sha", hits)
        self.assertNotIn("missing-fallback-reason", hits)

    def test_merge_claim_without_verdicts_fails(self) -> None:
        result = self.mod.evaluate_adversarial_pass_sha(
            {"merge_or_enqueue": True, "head_sha": "H", "adversarial_pass_sha": "H"}  # nosec B105 - fixture sha, not a secret
        )
        self.assertFalse(result["ok"])
        self.assertIn("missing-second-verdict", result["hits"])

    def test_missing_head_rejects_unbound_verdicts(self) -> None:
        hits = self._hits(
            {
                "merge_or_enqueue": True,
                "verdicts": [
                    {"worker": "a", "reviewer": "a", "verdict": "PASS", "sha": "abc"},
                    {"worker": "b", "reviewer": "b", "verdict": "PASS", "sha": "def"},
                ],
            }
        )
        self.assertIn("missing-head-sha", hits)
        self.assertIn("verdict-sha-mismatch", hits)

    def test_repeated_reviewer_is_not_independent(self) -> None:
        hits = self._hits(
            {
                "head_sha": "abc",
                "adversarial_pass_sha": "abc",  # nosec B105 - fixture sha, not a secret
                "verdicts": [
                    {"worker": "a", "reviewer": "same", "verdict": "PASS", "sha": "abc"},
                    {"worker": "b", "reviewer": "same", "verdict": "PASS", "sha": "abc"},
                ],
            }
        )
        self.assertIn("reviewers-not-independent", hits)
        self.assertNotIn("verdicts-not-independent", hits)


class JobApplyEvidenceTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_job_apply_1106", CE / "rules/lessons_2026_11_06.py")

    def test_authorization_bool_and_missing_signature_fail(self) -> None:
        denied = self.mod.evaluate_job_apply(
            {"sending": True, "authorization_record": True, "signature_count": None}
        )
        self.assertIn("missing-authorization", denied["hits"])
        self.assertIn("signature-not-single", denied["hits"])
        allowed = self.mod.evaluate_job_apply(
            {
                "draft_ready": True,
                "authorization_record": "human-auth-1",
                "writer": "author",
                "reviews": [
                    {"reviewer": "a", "verdict": "PASS"},
                    {"reviewer": "b", "verdict": "PASS"},
                ],
                "signature_count": 1,
            }
        )
        self.assertTrue(allowed["ok"], allowed)

    def test_standing_order_does_not_authorize_send(self) -> None:
        hits = self.mod.evaluate_job_apply(
            {"standing_order_recorded": True, "agent_sends": True, "within_named_scope": True}
        )["hits"]
        self.assertIn("standing-order-does-not-authorize-send", hits)
        self.assertIn("agent-must-not-send", hits)

    def test_reviewer_must_pass_and_must_not_be_the_writer(self) -> None:
        named_only = self.mod.evaluate_job_apply(
            {
                "draft_ready": True,
                "authorization_record": "human-auth-1",
                "writer": "author",
                "reviews": ["a", "b"],
                "signature_count": 1,
            }
        )
        self.assertIn("review-missing-pass", named_only["hits"])
        writer = self.mod.evaluate_job_apply(
            {
                "draft_ready": True,
                "authorization_record": "human-auth-1",
                "writer": "a",
                "reviews": [
                    {"reviewer": "a", "verdict": "PASS"},
                    {"reviewer": "b", "verdict": "PASS"},
                ],
                "signature_count": 1,
            }
        )
        self.assertIn("writer-reviewed-own-draft", writer["hits"])

    def test_missing_writer_fails_otherwise_valid_evidence(self) -> None:
        missing = self.mod.evaluate_job_apply(
            {
                "draft_ready": True,
                "authorization_record": "human-auth-1",
                "reviews": [
                    {"reviewer": "a", "verdict": "PASS"},
                    {"reviewer": "b", "verdict": "PASS"},
                ],
                "signature_count": 1,
            }
        )
        self.assertFalse(missing["ok"])
        self.assertIn("missing-writer", missing["hits"])
        self.assertNotIn("review-missing-pass", missing["hits"])

    def test_public_share_ats_stop_and_tracker_readback(self) -> None:
        public = self.mod.evaluate_job_apply({"public_by_link": True})
        self.assertIn("public-by-link-without-share-dialog", public["hits"])
        self.assertIn("share-not-confirmed-this-turn", public["hits"])
        shared = self.mod.evaluate_job_apply(
            {"public_by_link": True, "share_dialog": True, "confirmed_this_turn": True}
        )
        self.assertTrue(shared["ok"], shared)
        ats = self.mod.evaluate_job_apply({"ats_identical_errors": 2})
        self.assertIn("ats-errors-without-next-channel", ats["hits"])
        stopped = self.mod.evaluate_job_apply({"ats_identical_errors": 2, "switched_channel": True})
        self.assertTrue(stopped["ok"], stopped)
        tracker = self.mod.evaluate_job_apply({"tracker_write": True})
        self.assertIn("tracker-write-without-readback", tracker["hits"])
        read_back = self.mod.evaluate_job_apply({"tracker_write": True, "tracker_readback": True})
        self.assertTrue(read_back["ok"], read_back)


if __name__ == "__main__":
    raise SystemExit(main())
