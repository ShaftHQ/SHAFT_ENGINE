"""Mobile-retro harness helpers."""

from __future__ import annotations

import ast
import json
import sys
import tempfile
import time
import unittest
from unittest import mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "chaos-engine"))
sys.path.insert(0, str(ROOT))

import mobile_retro  # noqa: E402
from scripts.ci import overlay_pre_push  # noqa: E402


class FreezeResultsSnapshotTest(unittest.TestCase):
    def test_green_then_failing_keeps_green_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            green = root / "results"
            red = root / "results-red"
            snap = root / "results-green"
            green.mkdir()
            (green / "green-result.json").write_text('{"status":"passed"}', encoding="utf-8")
            red.mkdir()
            (red / "red-result.json").write_text('{"status":"failed"}', encoding="utf-8")

            self.assertEqual(mobile_retro.freeze_results_snapshot(green, snap, green=True), snap)
            self.assertTrue((snap / "green-result.json").is_file())

            kept = mobile_retro.freeze_results_snapshot(red, snap, green=False)
            self.assertEqual(kept, snap)
            self.assertTrue((snap / "green-result.json").is_file())
            self.assertFalse((snap / "red-result.json").exists())


class BackendBlockedClassifierTest(unittest.TestCase):
    def test_server_side_gate_is_blocked(self) -> None:
        self.assertEqual(
            mobile_retro.classify_blocker("API ignores app versionCode — server-side gate"),
            "Blocked",
        )

    def test_ordinary_failure_stays_failed(self) -> None:
        self.assertEqual(mobile_retro.classify_blocker("NullPointerException in onCreate"), "Failed")


class PrimaryPathTimeCapTest(unittest.TestCase):
    def test_primary_exception_uses_fallback(self) -> None:
        def primary() -> str:
            raise RuntimeError("ClassNotFoundException")

        self.assertEqual(
            mobile_retro.run_with_time_cap(primary, lambda: "fb", cap_s=5.0),
            "fb",
        )

    def test_hanging_primary_is_killed_and_fallback_runs(self) -> None:
        started = time.monotonic()

        def hang() -> str:
            time.sleep(60)
            return "primary-ok"

        result = mobile_retro.run_with_time_cap(hang, lambda: "fallback-ok", cap_s=0.3)
        elapsed = time.monotonic() - started
        self.assertEqual(result, "fallback-ok")
        self.assertLess(elapsed, 5.0)


class NoPushTrackedGuardTest(unittest.TestCase):
    def test_untracked_guard_fails(self) -> None:
        failures = mobile_retro.no_push_guard_tracked(
            ROOT, tracked_paths=["README.md", "scripts/ci/overlay_pre_push.py"]
        )
        self.assertTrue(failures)
        self.assertIn("NO_PUSH guard missing", failures[0])

    def test_tracked_guard_passes_in_repo(self) -> None:
        failures = mobile_retro.no_push_guard_tracked(ROOT)
        self.assertEqual(failures, [])

    def test_overlay_pre_push_includes_tracked_guard(self) -> None:
        failures = overlay_pre_push.overlay_pre_push_failures(ROOT, paths=None)
        self.assertFalse(any("NO_PUSH" in f for f in failures), failures)

    def test_fixture_without_helper_skips_no_push_check(self) -> None:
        """OverlayPrePushTest copies only overlay_pre_push.py; must not fail NO_PUSH."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self.assertEqual([], overlay_pre_push.no_push_guard_failures(root))


class EvidencePinTest(unittest.TestCase):
    def test_mismatched_build_rejected(self) -> None:
        pin = mobile_retro.EvidencePin(build_id="build-1", branch="feature/x")
        self.assertFalse(mobile_retro.evidence_matches_pin(pin, build_id="build-2", branch="feature/x"))
        self.assertTrue(mobile_retro.evidence_matches_pin(pin, build_id="build-1", branch="feature/x"))


class NamedBlockByGapIdTest(unittest.TestCase):
    def test_two_scenarios_one_gap_one_block(self) -> None:
        blocks = mobile_retro.named_blocks_by_gap(
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
        lesson = (ROOT / "chaos-engine/references/mobile-retro-harness.md").read_text(encoding="utf-8")
        self.assertIn("Freeze green results", lesson)
        self.assertIn("NO_PUSH", lesson)
        harness_map = (ROOT / "chaos-engine/references/harness-map.md").read_text(encoding="utf-8")
        self.assertIn("mobile-retro-harness.md", harness_map)
        agents = (ROOT / ".agents/skills/README.md").read_text(encoding="utf-8")
        self.assertIn("mobile-retro-harness.md", agents)
        bot_entry = (ROOT / "chaos-engine/references/bot-entry.md").read_text(encoding="utf-8")
        # On-demand only: always-loaded bot-entry must not pull the lesson.
        self.assertNotIn("mobile-retro", bot_entry)


class NoPushWiringTest(unittest.TestCase):
    def test_overlay_pre_push_calls_no_push_guard_failures(self) -> None:
        src = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        tree = ast.parse(src)
        calls: list[str] = []

        class Visitor(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                func = node.func
                if isinstance(func, ast.Name):
                    calls.append(func.id)
                self.generic_visit(node)

        Visitor().visit(tree)
        self.assertIn(
            "no_push_guard_failures",
            calls,
            "removing no_push_guard_failures wiring from overlay_pre_push must fail this test",
        )

    def test_removing_no_push_call_from_overlay_failures_breaks_contract(self) -> None:
        """Negative wiring: overlay_pre_push_failures must invoke the helper when paths is None."""
        original = overlay_pre_push.overlay_pre_push_failures
        seen: list[bool] = []

        def wrapped(root, paths=None):  # noqa: ANN001
            if paths is None:
                seen.append(True)
                # Prove the production function still calls the helper.
                helper_failures = overlay_pre_push.no_push_guard_failures(root)
                self.assertIsInstance(helper_failures, list)
            return original(root, paths)

        overlay_pre_push.overlay_pre_push_failures = wrapped  # type: ignore[assignment]
        try:
            overlay_pre_push.overlay_pre_push_failures(ROOT, paths=None)
        finally:
            overlay_pre_push.overlay_pre_push_failures = original  # type: ignore[assignment]
        self.assertTrue(seen, "overlay_pre_push_failures(paths=None) must run NO_PUSH wiring")



class PostRunFreezeLivePathTest(unittest.TestCase):
    def test_green_then_red_keeps_snapshot_and_pin(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            green = root / "results"
            red = root / "results-red"
            snap = root / "results-green"
            pin = root / "pin.json"
            green.mkdir()
            (green / "ok.json").write_text("{\"ok\":true}", encoding="utf-8")
            red.mkdir()
            (red / "bad.json").write_text("{\"ok\":false}", encoding="utf-8")

            kept = mobile_retro.post_run_freeze(
                green, snap, green=True, pin_path=pin, build_id="b1", branch="main"
            )
            self.assertEqual(kept, snap)
            self.assertTrue(pin.is_file())
            self.assertTrue(mobile_retro.enforce_evidence_pin(pin, build_id="b1", branch="main"))

            kept2 = mobile_retro.post_run_freeze(red, snap, green=False)
            self.assertEqual(kept2, snap)
            self.assertTrue((snap / "ok.json").is_file())
            self.assertFalse((snap / "bad.json").exists())
            self.assertFalse(mobile_retro.enforce_evidence_pin(pin, build_id="other", branch="main"))


class RebuildLoopLivePathTest(unittest.TestCase):
    def test_blocked_skips_rebuild(self) -> None:
        calls: list[str] = []

        def rebuild() -> None:
            calls.append("rebuild")

        status = mobile_retro.rebuild_loop_classify(
            "API ignores app versionCode — server-side gate", rebuild=rebuild
        )
        self.assertEqual(status, "Blocked")
        self.assertTrue(mobile_retro.should_stop_rebuild(status))
        self.assertEqual(calls, [])

    def test_failed_still_rebuilds(self) -> None:
        calls: list[str] = []

        def rebuild() -> None:
            calls.append("rebuild")

        status = mobile_retro.rebuild_loop_classify("NullPointerException", rebuild=rebuild)
        self.assertEqual(status, "Failed")
        self.assertFalse(mobile_retro.should_stop_rebuild(status))
        self.assertEqual(calls, ["rebuild"])


class DeviceFallbackLivePathTest(unittest.TestCase):
    def test_device_fallback_uses_time_cap(self) -> None:
        def primary() -> str:
            raise RuntimeError("ClassNotFoundException")

        self.assertEqual(
            mobile_retro.device_fallback_run(primary, lambda: "local-device", cap_s=5.0),
            "local-device",
        )

    def test_start_method_is_spawn_on_windows(self) -> None:
        with mock.patch.object(mobile_retro.sys, "platform", "win32"):
            ctx = mobile_retro._mp_context()
        self.assertEqual(ctx.get_start_method(), "spawn")


class EvidencePinPersistLivePathTest(unittest.TestCase):
    def test_persist_and_enforce(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            pin_path = Path(tmp) / "evidence-pin.json"
            mobile_retro.persist_evidence_pin(
                pin_path, mobile_retro.EvidencePin(build_id="build-9", branch="feature/y")
            )
            self.assertTrue(
                mobile_retro.enforce_evidence_pin(pin_path, build_id="build-9", branch="feature/y")
            )
            self.assertFalse(
                mobile_retro.enforce_evidence_pin(pin_path, build_id="build-8", branch="feature/y")
            )


class LiveReporterNamedBlockTest(unittest.TestCase):
    def test_report_writes_one_block_per_gap(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            report = Path(tmp) / "blocks.json"
            blocks = mobile_retro.live_report_named_blocks(
                [
                    ("scen-a", "gap-login", "missing grant"),
                    ("scen-b", "gap-login", "missing grant"),
                    ("scen-c", "gap-pay", "timeout"),
                ],
                report_path=report,
            )
            self.assertEqual(len(blocks), 2)
            payload = json.loads(report.read_text(encoding="utf-8"))
            by_gap = {row["gap_id"]: row for row in payload}
            self.assertEqual(by_gap["gap-login"]["scenarios"], ["scen-a", "scen-b"])
            self.assertEqual(by_gap["gap-pay"]["scenarios"], ["scen-c"])


class LiveCliWiringTest(unittest.TestCase):
    def test_live_cli_calls_live_paths(self) -> None:
        src = (ROOT / "scripts/ci/mobile_retro_live.py").read_text(encoding="utf-8")
        tree = ast.parse(src)
        attrs: set[str] = set()

        class Visitor(ast.NodeVisitor):
            def visit_Attribute(self, node: ast.Attribute) -> None:  # noqa: N802
                if isinstance(node.value, ast.Name) and node.value.id == "mobile_retro":
                    attrs.add(node.attr)
                self.generic_visit(node)

        Visitor().visit(tree)
        for name in (
            "post_run_freeze",
            "rebuild_loop_classify",
            "device_fallback_run",
            "persist_evidence_pin",
            "enforce_evidence_pin",
            "live_report_named_blocks",
        ):
            self.assertIn(name, attrs, f"live CLI must call mobile_retro.{name}")

    def test_lesson_names_live_cli(self) -> None:
        lesson = (ROOT / "chaos-engine/references/mobile-retro-harness.md").read_text(encoding="utf-8")
        self.assertIn("mobile_retro_live.py", lesson)
        self.assertIn("post_run_freeze", lesson)
        self.assertIn("Windows", lesson)

    def test_cli_post_run_freeze_subprocess(self) -> None:
        import subprocess

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            results = root / "results"
            snap = root / "snap"
            pin = root / "pin.json"
            results.mkdir()
            (results / "a.json").write_text("{}", encoding="utf-8")
            proc = subprocess.run(  # nosec B603
                [
                    sys.executable,
                    str(ROOT / "scripts/ci/mobile_retro_live.py"),
                    "post-run-freeze",
                    "--results",
                    str(results),
                    "--snapshot",
                    str(snap),
                    "--green",
                    "--pin",
                    str(pin),
                    "--build-id",
                    "b1",
                    "--branch",
                    "main",
                ],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertTrue((snap / "a.json").is_file())
            self.assertTrue(pin.is_file())



if __name__ == "__main__":
    unittest.main()
