"""HRSD J11 harness helpers (#7493–#7499)."""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "chaos-engine"))
sys.path.insert(0, str(ROOT))

import hrsd_j11  # noqa: E402
from scripts.ci import overlay_pre_push  # noqa: E402


class FreezeAllureSnapshotTest(unittest.TestCase):
    def test_green_then_failing_keeps_green_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            green = root / "allure-results"
            red = root / "allure-results-red"
            snap = root / "allure-results-green"
            green.mkdir()
            (green / "green-result.json").write_text('{"status":"passed"}', encoding="utf-8")
            red.mkdir()
            (red / "red-result.json").write_text('{"status":"failed"}', encoding="utf-8")

            self.assertEqual(hrsd_j11.freeze_results_snapshot(green, snap, green=True), snap)
            self.assertTrue((snap / "green-result.json").is_file())

            kept = hrsd_j11.freeze_results_snapshot(red, snap, green=False)
            self.assertEqual(kept, snap)
            self.assertTrue((snap / "green-result.json").is_file())
            self.assertFalse((snap / "red-result.json").exists())


class BackendBlockedClassifierTest(unittest.TestCase):
    def test_server_side_gate_is_blocked(self) -> None:
        self.assertEqual(
            hrsd_j11.classify_blocker("API ignores app versionCode — server-side gate"),
            "Blocked",
        )

    def test_ordinary_failure_stays_failed(self) -> None:
        self.assertEqual(hrsd_j11.classify_blocker("NullPointerException in onCreate"), "Failed")


class PrimaryPathTimeCapTest(unittest.TestCase):
    def test_cap_triggers_fallback(self) -> None:
        calls: list[str] = []

        def primary() -> str:
            calls.append("primary")
            # Simulate slow primary by returning after injected clock jumps.
            return "primary-ok"

        def fallback() -> str:
            calls.append("fallback")
            return "fallback-ok"

        clock_values = iter([0.0, 10.0])  # start, then elapsed check

        result = hrsd_j11.run_with_time_cap(
            primary,
            fallback,
            cap_s=1.0,
            clock=lambda: next(clock_values),
            sleeper=lambda _s: None,
        )
        self.assertEqual(result, "fallback-ok")
        self.assertEqual(calls, ["primary", "fallback"])

    def test_primary_exception_uses_fallback(self) -> None:
        def primary() -> str:
            raise RuntimeError("ClassNotFoundException")

        self.assertEqual(
            hrsd_j11.run_with_time_cap(primary, lambda: "fb", cap_s=5.0),
            "fb",
        )


class NoPushTrackedGuardTest(unittest.TestCase):
    def test_untracked_guard_fails(self) -> None:
        failures = hrsd_j11.no_push_guard_tracked(
            ROOT, tracked_paths=["README.md", "scripts/ci/overlay_pre_push.py"]
        )
        self.assertTrue(failures)
        self.assertIn("NO_PUSH guard missing", failures[0])

    def test_tracked_guard_passes_in_repo(self) -> None:
        failures = hrsd_j11.no_push_guard_tracked(ROOT)
        self.assertEqual(failures, [])

    def test_overlay_pre_push_includes_tracked_guard(self) -> None:
        # Real pre-push path (paths is None) must see the tracked guard.
        failures = overlay_pre_push.overlay_pre_push_failures(ROOT, paths=None)
        self.assertFalse(
            any("NO_PUSH" in f for f in failures),
            failures,
        )


class EvidencePinTest(unittest.TestCase):
    def test_mismatched_build_rejected(self) -> None:
        pin = hrsd_j11.EvidencePin(build_id="build-1", branch="feature/x")
        self.assertFalse(hrsd_j11.evidence_matches_pin(pin, build_id="build-2", branch="feature/x"))
        self.assertTrue(hrsd_j11.evidence_matches_pin(pin, build_id="build-1", branch="feature/x"))


class NamedBlockByGapIdTest(unittest.TestCase):
    def test_two_scenarios_one_gap_one_block(self) -> None:
        blocks = hrsd_j11.named_blocks_by_gap(
            [
                ("scen-a", "gap-login", "missing grant"),
                ("scen-b", "gap-login", "missing grant"),
                ("scen-c", "gap-pay", "timeout"),
            ]
        )
        by_gap = {b.gap_id: b for b in blocks}
        self.assertEqual(len(blocks), 2)
        self.assertEqual(by_gap["gap-login"].scenarios, ("scen-a", "scen-b"))
        self.assertEqual(by_gap["gap-pay"].scenarios, ("scen-c",))


class LessonCardTest(unittest.TestCase):
    def test_lesson_and_map_exist(self) -> None:
        lesson = (ROOT / "chaos-engine/references/hrsd-j11-harness.md").read_text(encoding="utf-8")
        self.assertIn("Freeze green results", lesson)
        self.assertIn("NO_PUSH", lesson)
        self.assertIn("NamedBlock", lesson)
        harness_map = (ROOT / "chaos-engine/references/harness-map.md").read_text(encoding="utf-8")
        self.assertIn("hrsd-j11-harness.md", harness_map)


if __name__ == "__main__":
    unittest.main()
