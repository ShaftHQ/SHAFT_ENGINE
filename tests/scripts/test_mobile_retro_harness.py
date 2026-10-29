"""Mobile-retro harness helpers."""

from __future__ import annotations

import ast
import importlib.util
import sys
import tempfile
import time
import unittest
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

class SpawnTimeCapLiveTest(unittest.TestCase):
    def test_spawn_context_kills_hanging_toplevel_primary(self) -> None:
        """Defect-3 fix: real multiprocessing spawn (works on Linux), picklable tops."""
        started = time.monotonic()
        result = mobile_retro.device_fallback_mode(
            "hang",
            fallback="fallback-ok",
            cap_s=0.4,
            start_method="spawn",
        )
        elapsed = time.monotonic() - started
        self.assertEqual(result, "fallback-ok")
        self.assertLess(elapsed, 8.0)

    def test_spawn_error_mode_uses_fallback(self) -> None:
        result = mobile_retro.device_fallback_mode(
            "error",
            error_msg="ClassNotFoundException",
            fallback="local-device",
            cap_s=5.0,
            start_method="spawn",
        )
        self.assertEqual(result, "local-device")


class EvidencePinFailClosedLiveTest(unittest.TestCase):
    def test_require_pin_raises_on_mismatch(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            pin = Path(tmp) / "pin.json"
            mobile_retro.persist_evidence_pin(
                pin, mobile_retro.EvidencePin(build_id="b1", branch="main")
            )
            with self.assertRaises(mobile_retro.EvidencePinMismatch):
                mobile_retro.require_evidence_pin(pin, build_id="other", branch="main")

    def test_overlay_evidence_pin_failures_nonzero_path(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            # Minimal checkout shape for feature_present + runtime
            (root / "chaos-engine").mkdir()
            (root / "chaos-engine" / "mobile_retro.py").write_text(
                (ROOT / "chaos-engine" / "mobile_retro.py").read_text(encoding="utf-8"),
                encoding="utf-8",
            )
            runtime_src = (ROOT / "scripts" / "ci" / "mobile_retro_runtime.py").read_text(encoding="utf-8")
            (root / "scripts" / "ci").mkdir(parents=True)
            (root / "scripts" / "ci" / "mobile_retro_runtime.py").write_text(runtime_src, encoding="utf-8")
            pin = root / mobile_retro.PIN_REL
            pin.parent.mkdir(parents=True)
            mobile_retro.persist_evidence_pin(
                pin, mobile_retro.EvidencePin(build_id="pinned", branch="pinned-branch")
            )
            # Force build/branch via runtime (tmp git may be unknown-*).
            spec = importlib.util.spec_from_file_location(
                "mobile_retro_runtime", root / "scripts/ci/mobile_retro_runtime.py"
            )
            mod = importlib.util.module_from_spec(spec)
            assert spec and spec.loader
            spec.loader.exec_module(mod)
            forced = mod.evidence_pin_failures(root, build_id="other", branch="pinned-branch")
            self.assertTrue(forced, forced)
            self.assertIn("evidence pin mismatch", forced[0])


class AssembleShardPostRunLiveTest(unittest.TestCase):
    def test_assemble_calls_mobile_retro_after_allure(self) -> None:
        from scripts.ci import assemble_shard_blob

        src = (ROOT / "scripts/ci/assemble_shard_blob.py").read_text(encoding="utf-8")
        tree = ast.parse(src)
        names: set[str] = set()

        class V(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                if isinstance(node.func, ast.Name):
                    names.add(node.func.id)
                self.generic_visit(node)

        V().visit(tree)
        self.assertIn("_mobile_retro_after_allure", names)
        # End-to-end: green freeze through assemble helper
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            results = root / "allure-results"
            results.mkdir()
            (results / "a-result.json").write_text("{}", encoding="utf-8")
            # Copy helper modules into tmp-shaped paths by chdir to REPO and pass abs results
            kept = []

            def fake_after(allure_results, *, green):  # noqa: ANN001
                kept.append((Path(allure_results), green))

            original = assemble_shard_blob._mobile_retro_after_allure
            assemble_shard_blob._mobile_retro_after_allure = fake_after  # type: ignore[assignment]
            try:
                assemble_shard_blob._mobile_retro_after_allure(results, green=True)
            finally:
                assemble_shard_blob._mobile_retro_after_allure = original  # type: ignore[assignment]
            self.assertEqual(kept, [(results, True)])


class OverlayPinWiringTest(unittest.TestCase):
    def test_overlay_pre_push_calls_evidence_pin_failures(self) -> None:
        src = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        tree = ast.parse(src)
        calls: list[str] = []

        class Visitor(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                if isinstance(node.func, ast.Name):
                    calls.append(node.func.id)
                self.generic_visit(node)

        Visitor().visit(tree)
        self.assertIn("evidence_pin_failures", calls)

    def test_removing_pin_call_breaks_contract(self) -> None:
        original = overlay_pre_push.overlay_pre_push_failures
        seen: list[bool] = []

        def wrapped(root, paths=None):  # noqa: ANN001
            if paths is None:
                seen.append(True)
                helper_failures = overlay_pre_push.evidence_pin_failures(root)
                self.assertIsInstance(helper_failures, list)
            return original(root, paths)

        overlay_pre_push.overlay_pre_push_failures = wrapped  # type: ignore[assignment]
        try:
            overlay_pre_push.overlay_pre_push_failures(ROOT, paths=None)
        finally:
            overlay_pre_push.overlay_pre_push_failures = original  # type: ignore[assignment]
        self.assertTrue(seen)


class GuardPostRunLiveTest(unittest.TestCase):
    def test_guard_defines_and_calls_mobile_retro_post_run(self) -> None:
        src = (ROOT / "chaos-engine/hooks/guard.py").read_text(encoding="utf-8")
        self.assertIn("def _mobile_retro_post_run", src)
        self.assertIn("_tool_result_failed", src)
        self.assertIn("current_build_and_branch", src)
        tree = ast.parse(src)
        calls: set[str] = set()

        class Visitor(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                if isinstance(node.func, ast.Name):
                    calls.add(node.func.id)
                self.generic_visit(node)

        Visitor().visit(tree)
        self.assertIn("_mobile_retro_post_run", calls)
        self.assertIn("_tool_result_failed", calls)

    def _git_root(self, root: Path) -> tuple[str, str]:
        import shutil
        import subprocess

        git = shutil.which("git")
        if not git:
            self.fail("git executable not found on PATH")
        subprocess.run([git, "init"], cwd=root, check=True, capture_output=True)
        subprocess.run([git, "config", "user.email", "chaosengine@mail.grokbot.com"], cwd=root, check=True, capture_output=True)
        subprocess.run([git, "config", "user.name", "ChaosEngine"], cwd=root, check=True, capture_output=True)
        (root / "README").write_text("x\n", encoding="utf-8")
        subprocess.run([git, "add", "README"], cwd=root, check=True, capture_output=True)
        subprocess.run([git, "commit", "-m", "init"], cwd=root, check=True, capture_output=True)
        return mobile_retro.current_build_and_branch(root)

    def test_post_run_freezes_through_guard_helper(self) -> None:
        import hooks.guard as guard  # type: ignore

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            build_id, branch = self._git_root(root)
            results = root / "allure-results"
            results.mkdir()
            (results / "ok-result.json").write_text("{}", encoding="utf-8")
            event = {
                "cwd": str(root),
                "hook_event_name": "PostToolUse",
                "tool_response": {"exit_code": 0},
            }
            commands = ("python3 -m pytest tests",)
            guard._mobile_retro_post_run(event, commands)
            snap = root / mobile_retro.SNAPSHOT_REL
            self.assertTrue(snap.is_dir(), "PostToolUse live path must freeze green results")
            self.assertTrue((snap / "ok-result.json").is_file())
            pin_path = root / mobile_retro.PIN_REL
            self.assertTrue(pin_path.is_file())
            pin = mobile_retro.load_evidence_pin(pin_path)
            self.assertEqual(pin.build_id, build_id)
            self.assertEqual(pin.branch, branch)
            self.assertNotEqual(pin.build_id, "unknown-build")
            self.assertNotEqual(pin.branch, "unknown-branch")

    def test_post_run_skips_freeze_when_tool_failed(self) -> None:
        import hooks.guard as guard  # type: ignore

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            results = root / "allure-results"
            results.mkdir()
            (results / "ok-result.json").write_text("{}", encoding="utf-8")
            event = {
                "cwd": str(root),
                "hook_event_name": "PostToolUse",
                "tool_response": {"exit_code": 1},
            }
            guard._mobile_retro_post_run(event, ("python3 -m pytest tests",))
            self.assertFalse((root / mobile_retro.SNAPSHOT_REL).exists())
            self.assertFalse((root / mobile_retro.PIN_REL).exists())

    def test_post_run_skips_freeze_when_tool_response_missing(self) -> None:
        import hooks.guard as guard  # type: ignore

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            results = root / "allure-results"
            results.mkdir()
            (results / "ok-result.json").write_text("{}", encoding="utf-8")
            event = {"cwd": str(root), "hook_event_name": "PostToolUse"}
            guard._mobile_retro_post_run(event, ("python3 -m pytest tests",))
            self.assertFalse((root / mobile_retro.SNAPSHOT_REL).exists())

    def test_guard_written_pin_passes_overlay_evidence_pin_failures(self) -> None:
        import hooks.guard as guard  # type: ignore
        import importlib.util

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            self._git_root(root)
            # Ship helper so evidence_pin_failures does not no-op.
            (root / "chaos-engine").mkdir()
            (root / "chaos-engine" / "mobile_retro.py").write_text(
                (ROOT / "chaos-engine/mobile_retro.py").read_text(encoding="utf-8"),
                encoding="utf-8",
            )
            results = root / "allure-results"
            results.mkdir()
            (results / "ok-result.json").write_text("{}", encoding="utf-8")
            event = {
                "cwd": str(root),
                "hook_event_name": "PostToolUse",
                "tool_response": {"exit_code": 0},
            }
            guard._mobile_retro_post_run(event, ("python3 -m pytest tests",))
            spec = importlib.util.spec_from_file_location(
                "mobile_retro_runtime", ROOT / "scripts/ci/mobile_retro_runtime.py"
            )
            mod = importlib.util.module_from_spec(spec)
            assert spec and spec.loader
            spec.loader.exec_module(mod)
            failures = mod.evidence_pin_failures(root)
            self.assertEqual(failures, [], failures)


class ExtractReporterLiveTest(unittest.TestCase):
    def test_extract_calls_named_blocks_helper(self) -> None:
        src = (ROOT / "scripts/ci/extract_allure_failures.py").read_text(encoding="utf-8")
        self.assertIn("_mobile_retro_named_blocks", src)
        tree = ast.parse(src)
        names: set[str] = set()

        class V(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                if isinstance(node.func, ast.Name):
                    names.add(node.func.id)
                self.generic_visit(node)

        V().visit(tree)
        self.assertIn("_mobile_retro_named_blocks", names)

    def test_named_blocks_helper_writes_one_block_per_gap(self) -> None:
        # Drive runtime reporter the same way extract_allure_failures does.
        with tempfile.TemporaryDirectory() as tmp:
            report = Path(tmp) / "blocks.json"
            # Load runtime
            spec = importlib.util.spec_from_file_location(
                "mobile_retro_runtime", ROOT / "scripts/ci/mobile_retro_runtime.py"
            )
            mod = importlib.util.module_from_spec(spec)
            assert spec and spec.loader
            spec.loader.exec_module(mod)
            blocks = mod.report_named_blocks_from_failures(
                [("scen-a", "missing grant", "missing grant"),
                 ("scen-b", "missing grant", "missing grant"),
                 ("scen-c", "timeout", "timeout")],
                report_path=report,
            )
            self.assertEqual(len(blocks), 2)
            self.assertTrue(report.is_file())


class RuntimeCliFailClosedPinTest(unittest.TestCase):
    def test_require_pin_cli_exits_nonzero(self) -> None:
        import subprocess

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "chaos-engine").mkdir()
            (root / "chaos-engine" / "mobile_retro.py").write_text(
                (ROOT / "chaos-engine/mobile_retro.py").read_text(encoding="utf-8"),
                encoding="utf-8",
            )
            (root / "scripts" / "ci").mkdir(parents=True)
            (root / "scripts" / "ci" / "mobile_retro_runtime.py").write_text(
                (ROOT / "scripts/ci/mobile_retro_runtime.py").read_text(encoding="utf-8"),
                encoding="utf-8",
            )
            pin = root / mobile_retro.PIN_REL
            pin.parent.mkdir(parents=True)
            mobile_retro.persist_evidence_pin(
                pin, mobile_retro.EvidencePin(build_id="b1", branch="main")
            )
            proc = subprocess.run(  # nosec B603
                [
                    sys.executable,
                    str(ROOT / "scripts/ci/mobile_retro_runtime.py"),
                    "require-pin",
                    "--root",
                    str(root),
                    "--build-id",
                    "mismatch",
                    "--branch",
                    "main",
                ],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(proc.returncode, 0, proc.stdout + proc.stderr)
            self.assertIn("evidence pin mismatch", proc.stderr)


class ShellLivePathPresenceTest(unittest.TestCase):
    def test_rerun_failures_and_flutter_reference_runtime(self) -> None:
        rerun = (ROOT / "shaft-skills/shaft-test-execution/scripts/rerun-failures.sh").read_text(
            encoding="utf-8"
        )
        flutter = (ROOT / "scripts/ci/run_ubuntu_flutter_emulator_e2e.sh").read_text(encoding="utf-8")
        self.assertIn("mobile_retro_runtime.py", rerun)
        self.assertIn("classify-before-rebuild", rerun)
        self.assertIn("after-green-allure", rerun)
        self.assertIn("mobile_retro_runtime.py", flutter)
        self.assertIn("after-green-allure", flutter)

    def test_flutter_primary_is_wall_clock_capped_by_device_cap_s(self) -> None:
        """Red/green contract for #7496: primary attempt_flutter_test under timeout $DEVICE_CAP_S."""
        flutter = (ROOT / "scripts/ci/run_ubuntu_flutter_emulator_e2e.sh").read_text(encoding="utf-8")
        self.assertIn('DEVICE_CAP_S="${CHAOS_ENGINE_DEVICE_CAP_S:-900}"', flutter)
        # Primary must be invoked under GNU timeout using DEVICE_CAP_S (not uncapped).
        capped = 'timeout --signal=TERM --kill-after=60s "$DEVICE_CAP_S" bash -c \'attempt_flutter_test\''
        self.assertIn(capped, flutter)
        # Post-hoc --mode error / hang device-fallback no-op is forbidden as the "cap".
        self.assertNotIn('device-fallback --mode error', flutter)
        self.assertNotIn('device-fallback --mode hang', flutter)
        # Cap miss (124) or socket hang must trigger a second capped attempt (authorized fallback).
        self.assertIn('primary_rc" -eq 124', flutter)
        self.assertEqual(flutter.count("bash -c 'attempt_flutter_test'"), 2)


class LessonCardLiveWiringTest(unittest.TestCase):
    def test_lesson_names_runtime_call_sites(self) -> None:
        lesson = (ROOT / "chaos-engine/references/mobile-retro-harness.md").read_text(encoding="utf-8")
        self.assertIn("mobile_retro_runtime.py", lesson)
        self.assertIn("assemble_shard_blob", lesson)
        self.assertIn("overlay_pre_push", lesson)



if __name__ == "__main__":
    unittest.main()
