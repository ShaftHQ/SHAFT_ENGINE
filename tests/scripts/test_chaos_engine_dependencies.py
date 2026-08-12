"""Standalone ChaosEngine dependency provisioning tests (#4794)."""

from __future__ import annotations

import importlib.util
import json
import shutil
import tempfile
import unittest
from unittest import mock
from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"
SPECIFICATION = ROOT / "chaos-engine/dependencies.json"


def load_controller():
    spec = importlib.util.spec_from_file_location("chaos_engine_dependencies", CONTROLLER)
    if spec is None or spec.loader is None:
        raise RuntimeError("dependency controller test module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ChaosEngineDependenciesTest(unittest.TestCase):
    def test_runtime_lock_closes_descriptor_when_stream_creation_fails(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            with mock.patch.object(module.os, "fdopen", side_effect=OSError("stream failed")):
                with mock.patch.object(module.os, "close", wraps=module.os.close) as close:
                    with self.assertRaisesRegex(OSError, "stream failed"):
                        with module.runtime_lock(runtime):
                            self.fail("lock unexpectedly acquired")
            close.assert_called_once()

    @staticmethod
    def fake_runner(root: Path):
        def runner(command, environment):
            del environment
            executable = Path(command[0])
            if not executable.exists() and executable.is_relative_to(root.parent):
                executable.parent.mkdir(parents=True, exist_ok=True)
                executable.write_text("tool\n", encoding="utf-8")
            return SimpleNamespace(stdout="tool 1.0\n", stderr="")

        return runner

    def test_plan_uses_harness_local_commands_for_every_required_tool(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            plan = module.install_plan(runtime, specification)

        self.assertEqual(1, specification["schemaVersion"])
        self.assertEqual({"uv", "mempalace", "graphify", "memory"}, set(plan))
        self.assertEqual("uv", plan["uv"][1][-1])
        self.assertIn("mempalace@latest", plan["mempalace"][0])
        self.assertIn("graphifyy@latest", plan["graphify"][0])
        self.assertIn("tree-sitter-sql", plan["graphify"][0])
        self.assertIn("@aictx/memory@0.1.55", plan["memory"][0])
        environment = module.tool_environment(runtime)
        self.assertEqual(str(runtime / "uv-tools"), environment["UV_TOOL_DIR"])
        self.assertEqual(str(runtime / "bin"), environment["UV_TOOL_BIN_DIR"])
        self.assertEqual(str(runtime / "npm"), environment["NPM_CONFIG_PREFIX"])
        for commands in plan.values():
            for command in commands:
                self.assertNotIn("--global", command)
                self.assertNotIn("-g", command)

    def test_freshness_is_read_only_and_stale_after_24_hours(self):
        module = load_controller()
        now = datetime(2026, 8, 12, tzinfo=timezone.utc)
        fresh = {"checkedAt": (now - timedelta(hours=23)).isoformat()}
        stale = {"checkedAt": (now - timedelta(hours=25)).isoformat()}
        self.assertEqual("fresh", module.freshness(fresh, now))
        self.assertEqual("stale", module.freshness(stale, now))

    def test_repair_runs_install_and_entrypoint_probes_then_writes_a_typed_receipt(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        calls = []

        def runner(command, environment):
            calls.append((command, environment.copy()))
            executable = Path(command[0])
            if not executable.exists() and executable.is_relative_to(Path(temporary)):
                executable.parent.mkdir(parents=True, exist_ok=True)
                executable.write_text("tool\n", encoding="utf-8")
            return SimpleNamespace(stdout="tool 1.0\n", stderr="")

        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            now = datetime(2026, 8, 12, tzinfo=timezone.utc)
            receipt = module.repair(runtime, specification, runner=runner, now=now)
            persisted = json.loads((runtime / "receipt.json").read_text(encoding="utf-8"))

        self.assertEqual(1, receipt["schemaVersion"])
        self.assertEqual(receipt, persisted)
        invoked = {Path(command[0]).stem for command, _ in calls}
        self.assertLessEqual({"mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}, invoked)
        self.assertTrue(all("UV_TOOL_DIR" in environment for _, environment in calls))

    def test_failed_repair_preserves_last_known_good_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))

        def failing_runner(command, environment):
            del command, environment
            raise OSError("offline")

        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            # An unknown addition is drift, so use an owned entrypoint as the marker.
            marker = Path(module.probe_plan(runtime)["graphify"][0][0])
            before = marker.read_text(encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "uv install"):
                module.repair(runtime, specification, runner=failing_runner, force=True)
            self.assertEqual(before, marker.read_text(encoding="utf-8"))

    def test_repair_rejects_an_unknown_runtime_without_claiming_it(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runtime.mkdir()
            marker = runtime / "user-owned.txt"
            marker.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "receipt"):
                module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual("mine\n", marker.read_text(encoding="utf-8"))
            self.assertFalse(runtime.with_name(f"{runtime.name}.backup").exists())

    def test_same_specification_repair_is_an_idempotent_health_check(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            calls = []

            def runner(command, environment):
                calls.append(command)
                return SimpleNamespace(stdout="ok\n", stderr="")

            module.repair(runtime, specification, runner=runner)

        self.assertEqual(6, len(calls))
        self.assertTrue(all("install" not in command for command in calls))

    def test_repair_restores_owned_runtime_when_replacement_creation_fails(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            before = (runtime / "receipt.json").read_bytes()
            original_mkdir = Path.mkdir

            def fail_replacement(path, *args, **kwargs):
                if path == runtime and not path.exists():
                    raise OSError("disk full")
                return original_mkdir(path, *args, **kwargs)

            with mock.patch.object(module.Path, "mkdir", fail_replacement):
                with self.assertRaisesRegex(OSError, "disk full"):
                    module.repair(
                        runtime,
                        specification,
                        runner=self.fake_runner(runtime),
                        force=True,
                    )

            self.assertEqual(before, (runtime / "receipt.json").read_bytes())
            self.assertFalse(runtime.with_name(f"{runtime.name}.backup").exists())

    def test_doctor_rejects_a_fabricated_or_drifted_receipt(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            runtime.mkdir()
            (runtime / "receipt.json").write_text(
                json.dumps({"schemaVersion": 1, "checkedAt": "2026-08-12T00:00:00+00:00"}),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(ValueError, "schema"):
                module.doctor(runtime, runner=self.fake_runner(runtime), specification=specification)

    def test_remove_requires_exact_owned_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            module.remove(runtime, specification)
            self.assertFalse(runtime.exists())

    def test_upgrade_accepts_a_new_specification_and_receipts_uv(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            changed = json.loads(json.dumps(specification))
            changed["tools"]["memory"]["package"] = "@aictx/memory@next"
            receipt = module.repair(
                runtime, changed, runner=self.fake_runner(runtime), force=True
            )

            self.assertEqual(
                module.specification_digest(changed), receipt["specificationSha256"]
            )
            self.assertEqual(
                {"uv", "mempalace", "graphify", "memory"}, set(receipt["tools"])
            )

    def test_receipt_survives_relocation_and_doctor_binds_selected_specification(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            first = Path(temporary) / "first/.chaos-engine-runtime"
            second = Path(temporary) / "second/.chaos-engine-runtime"
            first.parent.mkdir()
            module.repair(first, specification, runner=self.fake_runner(first))
            second.parent.mkdir()
            first.replace(second)

            result = module.doctor(
                second, runner=self.fake_runner(second), specification=specification
            )

        self.assertEqual("healthy", result["status"])

    def test_receipt_metadata_tampering_and_future_timestamps_are_rejected(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            receipt_path = runtime / "receipt.json"
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["checkedAt"] = "2099-01-01T00:00:00+00:00"
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "integrity drift"):
                module.status(runtime, specification)

    def test_post_publish_backup_is_recovered_on_retry(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            backup = runtime.with_name(f"{runtime.name}.backup")
            shutil.copytree(runtime, backup)

            receipt = module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertFalse(backup.exists())
            self.assertEqual(module.specification_digest(specification), receipt["specificationSha256"])

    def test_unknown_empty_directory_is_ownership_drift(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            (runtime / "user-empty").mkdir()

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                module.status(runtime, specification)

    def test_mid_build_crash_restores_the_verified_backup(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            before = (runtime / "receipt.json").read_bytes()
            backup = runtime.with_name(f"{runtime.name}.backup")
            building = runtime.with_name(f"{runtime.name}.building")
            runtime.replace(backup)
            runtime.mkdir()
            (runtime / "partial.txt").write_text("partial", encoding="utf-8")
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual(before, (runtime / "receipt.json").read_bytes())
            self.assertFalse(backup.exists())
            self.assertFalse(building.exists())

    def test_initial_build_crash_removes_only_marker_authorized_partial_runtime(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            building = runtime.with_name(f"{runtime.name}.building")
            runtime.mkdir()
            (runtime / "partial.txt").write_text("partial", encoding="utf-8")
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertTrue((runtime / "receipt.json").exists())
            self.assertFalse(building.exists())

    def test_cli_doctor_rejects_specification_drift(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runtime = root / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            changed = json.loads(json.dumps(specification))
            changed["tools"]["memory"]["package"] = "@aictx/memory@next"
            selected = root / "dependencies.json"
            selected.write_text(json.dumps(changed), encoding="utf-8")
            with mock.patch.object(module, "run_command", self.fake_runner(runtime)):
                with mock.patch("sys.argv", ["dependencies.py", "doctor", "--runtime", str(runtime), "--specification", str(selected)]):
                    self.assertEqual(1, module.main())

    def test_cli_doctor_respects_the_runtime_lock(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            with module.runtime_lock(runtime):
                with mock.patch(
                    "sys.argv",
                    [
                        "dependencies.py",
                        "doctor",
                        "--runtime",
                        str(runtime),
                        "--specification",
                        str(SPECIFICATION),
                    ],
                ):
                    self.assertEqual(1, module.main())

    def test_owned_marker_scratch_is_recovered_on_retry(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            building = runtime.with_name(f"{runtime.name}.building")
            owned = building.with_name(
                f"{building.name}{module.BUILD_MARKER_OWNED_SUFFIX}"
            )
            building.write_text(module.BUILD_MARKER_MAGIC, encoding="utf-8")
            building.replace(owned)

            module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertTrue(runtime.exists())
            self.assertFalse(building.exists())
            self.assertFalse(owned.exists())

    def test_unowned_marker_scratch_collision_is_untouched(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            building = runtime.with_name(f"{runtime.name}.building")
            owned = building.with_name(
                f"{building.name}{module.BUILD_MARKER_OWNED_SUFFIX}"
            )
            owned.write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "scratch collision"):
                module.repair(runtime, specification, runner=self.fake_runner(runtime))

            self.assertEqual("mine\n", owned.read_text(encoding="utf-8"))
            self.assertFalse(building.exists())

    def test_interrupted_remove_is_retryable_from_the_owned_removal_tree(self):
        module = load_controller()
        specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"
            module.repair(runtime, specification, runner=self.fake_runner(runtime))
            original_unlink = Path.unlink
            failed = False

            def fail_once(path, *args, **kwargs):
                nonlocal failed
                if path.name != "receipt.json" and not failed:
                    failed = True
                    raise OSError("busy")
                return original_unlink(path, *args, **kwargs)

            with mock.patch.object(module.Path, "unlink", fail_once):
                with self.assertRaisesRegex(OSError, "busy"):
                    module.remove(runtime, specification)
            module.remove(runtime, specification)

            self.assertFalse(runtime.exists())
            self.assertFalse(runtime.with_name(f"{runtime.name}.removing").exists())

    def test_dependency_tests_are_reached_by_pull_request_gate(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        self.assertIn(
            "tests/scripts/test_chaos_engine_dependencies.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_dependencies", workflow)


if __name__ == "__main__":
    unittest.main()
