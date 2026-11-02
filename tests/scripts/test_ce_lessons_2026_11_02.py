"""CE 2026-11-02 lessons: red+green fixtures for #7692–#7721 gates (L74)."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from unittest import TestCase, main

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DETAIL = CE / "references/details/ce-lessons-2026-11-02"
sys.path.insert(0, str(ROOT))


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ce_key(value: str):
    spec = importlib.util.spec_from_file_location(
        "ce_version", ROOT / "scripts/ci/ce_version.py"
    )
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.version_key(value)


class VersionFloorTest(TestCase):
    def test_version_at_least_0_4_20261102_1(self) -> None:
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        self.assertGreaterEqual(_ce_key(version), _ce_key("0.4.20261102.1"))


class CardsPresentTest(TestCase):
    REQUIRED = (
        "status-board.md",
        "run-state.md",
        "owner-media-gate.md",
        "platform-scheduler.md",
        "live-change-diff.md",
        "full-halt.md",
        "board-item-owner.md",
        "egyptian-colloquial-vo.md",
        "fold-alias-proper-nouns.md",
        "avatar-publish.md",
        "voice-clone.md",
        "adversarial-pass-sha.md",
        "clean-start.md",
        "target-dialect.md",
        "founded-year.md",
        "alias-audit-fail-closed.md",
        "script-delta-sha.md",
        "media-durable-job.md",
        "freeze-hygiene.md",
        "stop-scope-guard.md",
        "publish-metadata.md",
        "blocked-infra-apt.md",
        "seo-social-delivery.md",
        "dedupe-shipped.md",
    )

    def test_cards_and_spec(self) -> None:
        for name in self.REQUIRED:
            path = DETAIL / name
            self.assertTrue(path.is_file(), name)
            body = path.read_text(encoding="utf-8")
            self.assertGreater(len(body), 60, name)
            if name != "dedupe-shipped.md":
                self.assertIn("lessons_2026_11_02.py", body)
        self.assertTrue((CE / "guides/ce-lessons-2026-11-02-SPEC.md").is_file())
        self.assertTrue((CE / "references/ce-lessons-2026-11-02-SPEC.md").is_file())


class Lessons20261102GatesTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_2026_11_02", CE / "rules/lessons_2026_11_02.py")

    def test_7692_status_board(self) -> None:
        bad = self.mod.evaluate_status_board(
            {
                "owner_status_requested": True,
                "fresh_poll_complete": False,
                "registered_agents": ["a1"],
                "agent_receipts": [],
                "items": [{"id": "x", "external_claim": "merged"}],
            }
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_status_board(
            {
                "owner_status_requested": True,
                "fresh_poll_complete": True,
                "registered_agents": ["a1"],
                "agent_receipts": [
                    {"agent_id": "a1", "same_turn": True, "status": "ok"}
                ],
                "items": [
                    {
                        "id": "x",
                        "owner": "a1",
                        "link": "https://example.com/1",
                        "section": "delivered",
                        "external_claim": "merged",
                        "claim_status": "confirmed",
                        "confirming_receipt": "pr-1",
                    }
                ],
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7693_run_state(self) -> None:
        self.assertFalse(
            self.mod.evaluate_run_state(
                {"run_state": "frozen", "priority_dispatch": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_run_state(
                {
                    "run_state": "active",
                    "priority_dispatch": True,
                    "ack_idempotent": True,
                    "owner_ack_and_result_only": True,
                }
            )["ok"]
        )

    def test_7694_owner_media(self) -> None:
        self.assertFalse(
            self.mod.evaluate_owner_media_gate(
                {
                    "sent_to_owner": True,
                    "leading_silence_or_garbage": True,
                    "human_likeness_pass": False,
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_owner_media_gate(
                {
                    "sent_to_owner": True,
                    "human_likeness_pass": True,
                    "dialect_mismatch": False,
                }
            )["ok"]
        )

    def test_7695_platform_scheduler(self) -> None:
        bad = self.mod.evaluate_platform_scheduler(
            {
                "scheduled": True,
                "used_studio_chrome_offset": True,
                "posted_or_scheduled": True,
                "acting_identity": "personal",
                "brief_identity": "page",
            }
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_platform_scheduler(
            {
                "scheduled": True,
                "picker_iana_zone": "Africa/Cairo",
                "picker_zone": "Africa/Cairo",
                "intended_local": "2026-11-03 18:00",
                "utc_instant": "2026-11-03T16:00:00Z",
                "converted_via_utc": True,
                "posted_or_scheduled": True,
                "acting_identity": "page",
                "brief_identity": "page",
                "description_sha256_required": True,
                "description_sha256": "abc",
                "brief_description_sha256": "abc",
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7696_live_change_diff(self) -> None:
        self.assertFalse(
            self.mod.evaluate_live_change_diff({"changed_items": ["title"]})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_live_change_diff(
                {
                    "changed_items": ["title"],
                    "before_values": {"title": "old"},
                    "after_values": {"title": "new"},
                    "diff": "title: old -> new",
                }
            )["ok"]
        )

    def test_7697_full_halt(self) -> None:
        self.assertFalse(
            self.mod.evaluate_full_halt(
                {
                    "full_halt_armed": True,
                    "started_new_pr": True,
                    "under_stop_order": True,
                    "started_new_work": True,
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_full_halt(
                {"full_halt_armed": True, "started_new_work": False}
            )["ok"]
        )

    def test_7698_board_owner_and_drop(self) -> None:
        self.assertFalse(
            self.mod.evaluate_board_item_owner(
                {"running_verify": True, "actor_id": "b", "item_owner": "a"}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_board_item_owner(
                {"running_verify": True, "actor_id": "a", "item_owner": "a"}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_drop_verify_before_redo({"redo_dropped": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_drop_verify_before_redo(
                {
                    "redo_dropped": True,
                    "still_dropped": True,
                    "drop_evidence": {"link": "https://example.com/i/1"},
                }
            )["ok"]
        )

    def test_7701_egyptian_vo(self) -> None:
        self.assertFalse(
            self.mod.evaluate_egyptian_colloquial_vo(
                "ثلاث تحديات", dialect="egyptian_colloquial"
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_egyptian_colloquial_vo(
                "تلات تحديات", dialect="egyptian_colloquial"
            )["ok"]
        )

    def test_7703_fold_alias(self) -> None:
        self.assertFalse(
            self.mod.evaluate_fold_alias_proper_nouns(
                {
                    "proper_nouns": ["NameA"],
                    "fold_aliases": {"namea": "nameb"},
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_fold_alias_proper_nouns(
                {
                    "proper_nouns": ["NameA"],
                    "fold_aliases": {"teh": "the"},
                }
            )["ok"]
        )

    def test_7707_avatar_publish(self) -> None:
        self.assertFalse(
            self.mod.evaluate_avatar_publish(
                {"coach_approved": True, "published": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_avatar_publish(
                {
                    "coach_approved": True,
                    "published": True,
                    "owner_signoff": True,
                    "owner_avatar_signoff": True,
                    "artifact_sha": "deadbeef",
                    "ai_labeled": True,
                    "l56_blind_pass": True,
                }
            )["ok"]
        )

    def test_7708_voice_clone(self) -> None:
        bad = self.mod.evaluate_voice_clone(
            {"speaker_pass": True, "wer": 0.4, "language": "en", "unk_frac": 0.2}
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_voice_clone(
            {
                "speaker_pass": True,
                "wer": 0.05,
                "language": "en",
                "unk_frac": 0.01,
                "joint_speaker_asr_ckpt": True,
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7709_adversarial_pass_sha(self) -> None:
        self.assertFalse(
            self.mod.evaluate_adversarial_pass_sha(
                {
                    "merge_or_enqueue": True,
                    "adversarial_pass_sha": "aaa",
                    "head_sha": "bbb",
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_adversarial_pass_sha(
                {
                    "merge_or_enqueue": True,
                    "adversarial_pass_sha": "aaa",
                    "head_sha": "aaa",
                }
            )["ok"]
        )

    def test_7710_clean_start(self) -> None:
        self.assertFalse(
            self.mod.evaluate_clean_start({"leading_garbage": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_clean_start(
                {
                    "onset_skew_ms": 40,
                    "script_first_words": "hello world today",
                    "first_words_asr": "hello world today",
                }
            )["ok"]
        )

    def test_7711_target_dialect(self) -> None:
        self.assertFalse(
            self.mod.evaluate_target_dialect({"register_mismatch": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_target_dialect(
                {
                    "target_dialect": "egyptian_colloquial",
                    "method_names_target_dialect": True,
                }
            )["ok"]
        )

    def test_7712_founded_year(self) -> None:
        pf = _load("ce_public_facts", CE / "rules/public_facts.py")
        bad = self.mod.evaluate_founded_year(
            {
                "founded_year": "2014",
                "assert_founded_year": True,
                "text": "Founded in 2014",
            }
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_founded_year(
            {
                "founded_year": pf.FOUNDED_YEAR_PUBLIC,
                "assert_founded_year": True,
                "trainee_count": "600+ trainees",
                "assert_public_trainee_figure": True,
                "text": f"Founded in {pf.FOUNDED_YEAR_PUBLIC}. 600+ trainees.",
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7713_alias_audit_fail_closed(self) -> None:
        bad = self.mod.evaluate_alias_audit_fail_closed(
            [{"from": "foo", "to": "bar", "kind": "content-rewrite"}],
            aliases_md="ALIASES",
            audited=False,
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_alias_audit_fail_closed(
            [{"from": "API", "to": "اي بي اي", "kind": "transliteration"}],
            aliases_md="Only EN↔AR transliteration allowed",
            audited=True,
        )
        self.assertTrue(good["ok"], good)

    def test_7714_script_delta_sha(self) -> None:
        self.assertFalse(
            self.mod.evaluate_script_delta_sha(
                {"signed_script_sha": "a", "script_sha": "b"}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_script_delta_sha(
                {
                    "signed_script_sha": "a",
                    "script_sha": "b",
                    "coach_reack": True,
                    "script_delta_note": "fixed typo",
                }
            )["ok"]
        )

    def test_7715_media_durable_job(self) -> None:
        self.assertFalse(
            self.mod.evaluate_media_durable_job(
                {"media_step": True, "step_minutes": 45, "bare_nohup": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_media_durable_job(
                {
                    "media_step": True,
                    "step_minutes": 20,
                    "commit_each_step": True,
                    "resume_from_git": True,
                    "job_name": "tts-slice-1",
                    "resume_via_job_status": True,
                }
            )["ok"]
        )

    def test_7716_freeze_hygiene(self) -> None:
        self.assertFalse(
            self.mod.evaluate_freeze_hygiene(
                {"dispatched_freezable_worker": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_freeze_hygiene(
                {
                    "dispatched_freezable_worker": True,
                    "committed_before_dispatch": True,
                    "freeze_issued": True,
                    "recorded_frozen_head": "abc",
                    "recorded_next_step": "resume slice 2",
                }
            )["ok"]
        )

    def test_7718_stop_scope_and_claimed_done(self) -> None:
        self.assertFalse(
            self.mod.evaluate_stop_scope_guard({"all": True, "broadcast": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_stop_scope_guard(
                {
                    "all": True,
                    "broadcast": True,
                    "explicit_reason": "owner ordered",
                    "harness_warning_shown": True,
                    "peer_safe_ack": True,
                }
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_claimed_done_verify({"claiming_done": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_claimed_done_verify(
                {
                    "claiming_done": True,
                    "live_surface_checked": True,
                    "verify_verdict": "SKIP",
                }
            )["ok"]
        )

    def test_7719_publish_metadata(self) -> None:
        bad = self.mod.evaluate_publish_metadata(
            {
                "title": "x" * 71,
                "deep_link_unconfirmed": True,
                "link": "https://example.com/a/b/c",
                "used_site_root": False,
            }
        )
        self.assertFalse(bad["ok"], bad)
        good = self.mod.evaluate_publish_metadata(
            {
                "title": "Short",
                "tags": ["a"],
                "chapters": [
                    {
                        "start": "0:00",
                        "start_seconds": 0,
                        "duration_seconds": 20,
                        "title": "intro",
                    }
                ],
                "deep_link_unconfirmed": True,
                "used_site_root": True,
                "todo_upgrade_link": True,
                "link": "https://example.com/",
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7720_blocked_infra_apt(self) -> None:
        bad = self.mod.evaluate_blocked_infra_apt(
            {
                "apt_mirror_5xx": True,
                "used_force_apt_flag": True,
                "swap_kib": 1,
                "free": 50 * 1024**3,
                "used_percent": 10.0,
            }
        )
        self.assertFalse(bad["ok"], bad)
        self.assertIn("apt-mirror-5xx", bad["hits"])
        good = self.mod.evaluate_blocked_infra_apt(
            {
                "apt_mirror_ok": True,
                "apt_mirror_5xx": False,
                "apt_lists_unsigned": False,
                "swap_kib": 1,
                "free": 50 * 1024**3,
                "used_percent": 10.0,
            }
        )
        self.assertTrue(good["ok"], good)

    def test_7721_seo_admin_scheduler(self) -> None:
        self.assertFalse(
            self.mod.evaluate_seo_founding_date(
                {
                    "founding_year_marketing": 2014,
                    "founding_year_registry": 2018,
                    "foundingDate_published": True,
                }
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_seo_founding_date(
                {
                    "founding_year_marketing": 2014,
                    "founding_year_registry": 2018,
                    "foundingDate_published": False,
                }
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_admin_url_shape({"slug_admin_failed": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_admin_url_shape(
                {"slug_admin_failed": True, "used_numeric_org_id": True}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_scheduler_tz_proof({"scheduled": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_scheduler_tz_proof(
                {
                    "scheduled": True,
                    "owner_tz": "Africa/Cairo",
                    "picker_tz": "America/Los_Angeles",
                    "utc_instant": "2026-11-03T16:00:00Z",
                    "proof_path": "/tmp/proof.png",
                    "converted_via_utc": True,
                }
            )["ok"]
        )

    def test_issue_evaluator_map_covers_scope(self) -> None:
        expected = {
            7692,
            7693,
            7694,
            7695,
            7696,
            7697,
            7698,
            7701,
            7703,
            7707,
            7708,
            7709,
            7710,
            7711,
            7712,
            7713,
            7714,
            7715,
            7716,
            7718,
            7719,
            7720,
            7721,
        }
        self.assertEqual(expected, set(self.mod.ISSUE_EVALUATORS))


class DedupeShippedPointersTest(TestCase):
    def test_dedupe_card_names_prior_gates(self) -> None:
        text = (DETAIL / "dedupe-shipped.md").read_text(encoding="utf-8")
        for needle in (
            "Carl L5",
            "evaluate_stop_scope",
            "public_facts",
            "box_disk_guard",
            "verify_dropped",
            "lessons_2026_11_01",
        ):
            self.assertIn(needle, text)


class PublicFactsExtensionTest(TestCase):
    def test_existing_trainee_gate_still_green(self) -> None:
        pf = _load("ce_public_facts2", CE / "rules/public_facts.py")
        good = pf.evaluate(
            {
                "trainee_count": "600+ trainees",
                "assert_public_trainee_figure": True,
                "text": "We trained 600+ trainees.",
            }
        )
        self.assertTrue(good["ok"], good)
        bad = pf.evaluate({"trainee_count": "484", "assert_public_trainee_figure": True})
        self.assertFalse(bad["ok"])


class BoxDiskGuardAptExtensionTest(TestCase):
    def test_apt_5xx_and_unsigned(self) -> None:
        from scripts.ci import box_disk_guard as bdg

        reasons = bdg.blocked_infra_reasons(
            ".",
            free=50 * 1024**3,
            used_percent=10.0,
            swap_kib=1024,
            apt_mirror_5xx=True,
            apt_lists_unsigned=True,
        )
        self.assertIn("apt-mirror-5xx", reasons)
        self.assertIn("apt-lists-unsigned", reasons)
        clean = bdg.blocked_infra_reasons(
            ".",
            free=50 * 1024**3,
            used_percent=10.0,
            swap_kib=1024,
            apt_mirror_ok=True,
        )
        self.assertEqual([], [r for r in clean if r.startswith("apt-")])


if __name__ == "__main__":
    main()
