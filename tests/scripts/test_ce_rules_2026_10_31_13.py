"""CE 2026.10.31.13: L98–L101 + H/C/E/Carl retro batch (deduped)."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from unittest import TestCase, main

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DETAIL = CE / "references/details/ce-rules-2026-10-31-13"
sys.path.insert(0, str(ROOT))


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class VersionFloorTest(TestCase):
    def test_version_at_least_2026_10_31_13(self) -> None:
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        floor = tuple(int(p) for p in "2026.10.31.13".split("."))
        self.assertGreaterEqual(tuple(int(p) for p in version.split(".")), floor)


class CardsPresentTest(TestCase):
    REQUIRED = (
        "worker-budget.md",
        "review-before-enforced.md",
        "path-filter.md",
        "interrupt-resume.md",
        "alias-audit.md",
        "script-delta-reack.md",
        "resumable-steps.md",
        "stop-scope.md",
        "post-stop-live-state.md",
        "yt-metadata.md",
        "status-template.md",
        "hrs-journal.md",
        "hrs-swiftui-drag.md",
        "hrs-picker-label.md",
        "hrs-map-settle.md",
        "hrs-no-midrun-bump.md",
        "hrs-analyze-first.md",
        "coach-yt-gmt.md",
        "coach-before-values.md",
        "coach-stop-orders.md",
        "coach-ar-msa.md",
        "coach-pass-not-publish.md",
        "dedupe-shipped.md",
    )

    def test_cards_and_spec(self) -> None:
        for name in self.REQUIRED:
            path = DETAIL / name
            self.assertTrue(path.is_file(), name)
            body = path.read_text(encoding="utf-8")
            self.assertGreater(len(body), 80, name)
            if name != "dedupe-shipped.md":
                self.assertIn("lessons_2026_11_01.py", body)
        self.assertTrue((CE / "guides/ce-rules-2026-10-31-13-SPEC.md").is_file())
        self.assertTrue((CE / "references/ce-rules-2026-10-31-13-SPEC.md").is_file())


class Lessons20261101GatesTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_2026_11_01", CE / "rules/lessons_2026_11_01.py")

    def test_l98_worker_budget(self) -> None:
        self.assertFalse(self.mod.evaluate_worker_budget({})["ok"])
        self.assertTrue(
            self.mod.evaluate_worker_budget(
                {
                    "resumable_batches": True,
                    "per_session_hit_target": "+0.2pp",
                    "handoff_file": "handoff.md",
                }
            )["ok"]
        )

    def test_l99_enforcement_claim(self) -> None:
        self.assertFalse(
            self.mod.evaluate_enforcement_claim("We enforce this in prose only.")["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_enforcement_claim(
                "Enforced by rules/lessons_2026_11_01.py evaluate_worker_budget"
            )["ok"]
        )

    def test_l100_path_filter(self) -> None:
        wf = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertTrue(self.mod.evaluate_installer_path_filter(wf)["ok"])
        self.assertFalse(self.mod.evaluate_installer_path_filter("chaos_installer:\n")["ok"])

    def test_l101_interrupt_resume(self) -> None:
        self.assertFalse(self.mod.evaluate_interrupt_resume("continuing blindly")["ok"])
        self.assertTrue(
            self.mod.evaluate_interrupt_resume(
                "Checked live PR #7682 head SHA and CI status first"
            )["ok"]
        )

    def test_carl_l5_alias_audit(self) -> None:
        bad = self.mod.evaluate_alias_audit(
            [{"from": "foo", "to": "bar", "kind": "synonym", "wer_bypass": True}],
            aliases_md="ALIASES",
        )
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_alias_audit(
            [{"from": "API", "to": "اي بي اي", "kind": "transliteration"}],
            aliases_md="Only EN↔AR transliteration allowed",
        )
        self.assertTrue(good["ok"], good)

    def test_carl_l6_script_delta(self) -> None:
        self.assertFalse(
            self.mod.evaluate_script_delta_reack(
                {"script_changed_after_accept": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_script_delta_reack(
                {"script_changed_after_accept": True, "coach_reack": True}
            )["ok"]
        )

    def test_carl_l7_resumable_steps(self) -> None:
        self.assertFalse(self.mod.evaluate_resumable_steps({"step_minutes": 45})["ok"])
        self.assertTrue(
            self.mod.evaluate_resumable_steps(
                {
                    "step_minutes": 25,
                    "commit_each_step": True,
                    "resume_from_git": True,
                }
            )["ok"]
        )

    def test_e1_e2_stop_scope(self) -> None:
        self.assertFalse(self.mod.evaluate_stop_scope({"all": True})["ok"])
        self.assertTrue(
            self.mod.evaluate_stop_scope({"all": True, "peer_safe_ack": True})["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_post_stop_live_state({"stop_received": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_post_stop_live_state(
                {
                    "stop_received": True,
                    "live_state_checked": True,
                    "keep_edit": True,
                    "matches_owner_words": True,
                }
            )["ok"]
        )

    def test_e3_yt_metadata(self) -> None:
        bad = self.mod.evaluate_yt_metadata(
            {
                "title": "x" * 71,
                "tags": "a" * 500,
                "chapters": [{"start": "0:05", "duration_seconds": 5, "title": "a"}],
                "facts": ["600+ trainees"],
                "description": "hello",
            }
        )
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_yt_metadata(
            {
                "title": "Short title",
                "tags": ["automation", "training"],
                "chapters": [
                    {"start": "0:00", "start_seconds": 0, "duration_seconds": 15, "title": "intro"},
                    {"start_seconds": 15, "duration_seconds": 20, "title": "body"},
                ],
                "facts": ["600+ trainees"],
                "description": "We trained 600+ trainees.",
            }
        )
        self.assertTrue(good["ok"], good)

    def test_e4_status_template(self) -> None:
        self.assertFalse(self.mod.evaluate_status_template("all good", delta_mode=True)["ok"])
        self.assertTrue(
            self.mod.evaluate_status_template(
                "DONE a\nINPROG b\nNEXT c\nBLOCKED none\ndropped: none\ndelta: none",
                delta_mode=True,
            )["ok"]
        )

    def test_h_gates(self) -> None:
        self.assertFalse(self.mod.evaluate_test_journal({})["ok"])
        self.assertTrue(
            self.mod.evaluate_test_journal(
                {
                    "progress_file": "progress.md",
                    "detached_queue": True,
                    "journaled": True,
                }
            )["ok"]
        )
        self.assertFalse(self.mod.evaluate_picker_select({"select_by_index": True})["ok"])
        self.assertTrue(
            self.mod.evaluate_picker_select({"select_by_label": True, "label": "Cairo"})["ok"]
        )
        self.assertFalse(self.mod.evaluate_map_confirm({"settle_wait_s": 60})["ok"])
        self.assertTrue(
            self.mod.evaluate_map_confirm({"settle_wait_s": 10, "gps_location_capped": True})["ok"]
        )
        self.assertFalse(self.mod.evaluate_no_midrun_bump({"bumped_mid_run": True})["ok"])
        self.assertTrue(self.mod.evaluate_no_midrun_bump({"bumped_mid_run": False})["ok"])
        self.assertFalse(
            self.mod.evaluate_analyze_before_locator_fix({"locator_changed": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_analyze_before_locator_fix(
                {"locator_changed": True, "ruled_out_bad_locator": True}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_swiftui_drag({"drag": True, "row_by_index": True})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_swiftui_drag(
                {"drag": True, "tallest_scrollview_fallback": True}
            )["ok"]
        )

    def test_c_gates(self) -> None:
        self.assertFalse(self.mod.evaluate_yt_schedule_tz({"marked_done": True})["ok"])
        self.assertTrue(
            self.mod.evaluate_yt_schedule_tz(
                {"marked_done": True, "gmt_label_checked": True}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_gate_fix_before_values({"changed_items": ["title"]})["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_gate_fix_before_values(
                {"changed_items": ["title"], "before_values": {"title": "old"}}
            )["ok"]
        )
        self.assertFalse(
            self.mod.evaluate_stop_orders(
                {"under_stop_order": True, "started_new_work": True}
            )["ok"]
        )
        self.assertFalse(self.mod.evaluate_ar_vo_script("ثلاث تحديات")["ok"])
        self.assertTrue(self.mod.evaluate_ar_vo_script("تلات تحديات")["ok"])
        self.assertFalse(
            self.mod.evaluate_coach_pass_not_publish(
                {"coach_pass": True, "published": True}
            )["ok"]
        )
        self.assertTrue(
            self.mod.evaluate_coach_pass_not_publish(
                {"coach_pass": True, "published": True, "owner_signoff": True}
            )["ok"]
        )


class DedupeShippedPointersTest(TestCase):
    def test_dedupe_card_names_prior_gates(self) -> None:
        text = (DETAIL / "dedupe-shipped.md").read_text(encoding="utf-8")
        for needle in ("L93", "L96", "L90", "L86", "design_fix_loop", "L100"):
            self.assertIn(needle, text)


if __name__ == "__main__":
    main()
