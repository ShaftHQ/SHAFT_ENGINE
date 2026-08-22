"""Immutable ChaosEngine dependency-generation tests (#5299)."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import tempfile
import unittest
import unittest.mock as mock
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"


def load_controller():
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_generation_dependencies", CONTROLLER
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("dependency controller could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class GenerationRuntimeTests(unittest.TestCase):
    @staticmethod
    def publish(module, project: Path, active: dict[str, str]):
        return module.publish_pointer(
            project,
            active,
            transaction_id="d" * 32,
            expected_specification_sha256=active["specificationSha256"],
            expected_core_sha256=active["coreSha256"],
        )

    @staticmethod
    def select(module, project: Path, active: dict[str, str]):
        return module.active_generation(
            project,
            expected_specification_sha256=active["specificationSha256"],
            expected_core_sha256=active["coreSha256"],
        )

    @staticmethod
    def generation_fixture(module, project: Path) -> tuple[Path, dict[str, str]]:
        core = project / ".chaos-engine/manifest.json"
        core.parent.mkdir()
        core.write_text('{"owned":true}\n', encoding="utf-8")
        generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
        generation.mkdir(parents=True)
        scripts = "Scripts" if os.name == "nt" else "bin"
        python_name = "python.exe" if os.name == "nt" else "python"
        uv_name = "uv.exe" if os.name == "nt" else "uv"
        dispatches = {}
        uv = generation / f"bootstrap/{scripts}/{uv_name}"
        uv.parent.mkdir(parents=True)
        uv.write_bytes(b"uv")
        dispatches["uv"] = {
            "dispatch": {
                "kind": "executable",
                "path": uv.relative_to(generation).as_posix(),
                "sha256": module.sha256(uv),
                "size": uv.stat().st_size,
            }
        }
        for name, environment, distribution in (
            ("mempalace", "mempalace", "mempalace"),
            ("mempalace-mcp", "mempalace", "mempalace"),
            ("graphify", "graphifyy", "graphifyy"),
        ):
            interpreter = generation / f"uv-tools/{environment}/{scripts}/{python_name}"
            interpreter.parent.mkdir(parents=True, exist_ok=True)
            interpreter.write_bytes(f"python-{environment}".encode())
            dispatches[name] = {
                "dispatch": {
                    "kind": "python",
                    "interpreter": interpreter.relative_to(generation).as_posix(),
                    "interpreterSha256": module.sha256(interpreter),
                    "interpreterSize": interpreter.stat().st_size,
                    "distribution": distribution,
                    "entrypoint": name,
                }
            }
        for name in ("memory", "memory-mcp"):
            suffix = "dist/cli/main.js" if name == "memory" else "dist/mcp/server.js"
            script = generation / f"npm/node_modules/@aictx/memory/{suffix}"
            script.parent.mkdir(parents=True, exist_ok=True)
            script.write_text("process.exit(0);\n", encoding="utf-8")
            dispatches[name] = {
                "dispatch": {
                    "kind": "npm",
                    "script": script.relative_to(generation).as_posix(),
                    "scriptSha256": module.sha256(script),
                    "scriptSize": script.stat().st_size,
                    "entrypoint": name,
                }
            }
        receipt_value = {
            "schemaVersion": 2,
            "runtimeContractVersion": 3,
            "checkedAt": datetime.now(timezone.utc).isoformat(),
            "specificationSha256": "b" * 64,
            "coreSha256": module.sha256(core),
            "environment": {},
            "installed": {},
            "tools": dispatches,
            "ownership": {
                "directories": [],
                "files": {},
                "links": [],
                "sha256": module.ownership_digest({}),
            },
        }
        receipt_value["receiptIntegritySha256"] = module.json_integrity(
            receipt_value
        )
        receipt = generation / "receipt.json"
        receipt.write_text(json.dumps(receipt_value) + "\n", encoding="utf-8")
        return generation, {
            "generationId": "a" * 32,
            "specificationSha256": "b" * 64,
            "coreSha256": module.sha256(core),
            "receiptSha256": module.sha256(receipt),
        }

    def test_pointer_selects_only_a_strict_generation_identifier(self):
        module = load_controller()
        self.assertTrue(
            hasattr(module, "publish_pointer"),
            "generation pointer publication is not implemented",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            self.publish(module, project, active)

            selected, pointer = self.select(module, project, active)

            self.assertEqual(generation, selected)
            self.assertEqual(active, pointer["active"])
            self.assertFalse(
                (project / ".chaos-engine-runtime-current.json.tmp").exists()
            )

            pointer_path = project / ".chaos-engine-runtime-current.json"
            value = json.loads(pointer_path.read_text(encoding="utf-8"))
            value["active"]["generationId"] = "../outside"
            value["integritySha256"] = module.json_integrity(value)
            pointer_path.write_text(json.dumps(value), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "generation identifier"):
                self.select(module, project, active)

    def test_generation_ancestor_and_interpreter_links_fail_closed(self):
        module = load_controller()
        if not hasattr(os, "symlink"):
            self.skipTest("symlinks unavailable")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            project.mkdir()
            outside = root / "outside"
            outside.mkdir()
            try:
                (project / ".chaos-engine-runtime-generations").symlink_to(
                    outside, target_is_directory=True
                )
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")
            generation, active = self.generation_fixture(module, project)
            with self.assertRaisesRegex(ValueError, "ancestor|link|reparse"):
                self.publish(module, project, active)

        with tempfile.TemporaryDirectory() as temporary:
            generation = Path(temporary) / "generation"
            generation.mkdir()
            outside = Path(temporary) / "python"
            outside.write_bytes(b"python")
            scripts = "Scripts" if os.name == "nt" else "bin"
            python_name = "python.exe" if os.name == "nt" else "python"
            relative_interpreter = f"uv-tools/mempalace/{scripts}/{python_name}"
            interpreter = generation / relative_interpreter
            interpreter.parent.mkdir(parents=True)
            interpreter.symlink_to(outside)
            receipt = {
                "tools": {
                    "mempalace": {
                        "dispatch": {
                            "kind": "python",
                            "interpreter": relative_interpreter,
                            "interpreterSha256": module.sha256(outside),
                            "interpreterSize": outside.stat().st_size,
                            "distribution": "mempalace",
                            "entrypoint": "mempalace",
                        }
                    }
                }
            }
            with self.assertRaisesRegex(ValueError, "interpreter.*link|reparse|unsafe"):
                module.dispatch_command(generation, receipt, "mempalace", [])

    def test_controls_are_bounded_and_stale_temporary_never_blocks_publish(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            _, active = self.generation_fixture(module, project)
            stale = project / ".chaos-engine-runtime-current.json.tmp"
            stale.write_text("interrupted\n", encoding="utf-8")

            self.publish(module, project, active)

            pointer = project / ".chaos-engine-runtime-current.json"
            pointer.write_bytes(b"{" + b" " * module.MAX_CONTROL_BYTES + b"}")
            with mock.patch.object(
                module.Path,
                "read_text",
                side_effect=AssertionError("bounded reader must not call read_text"),
            ):
                with self.assertRaisesRegex(ValueError, "too large"):
                    self.select(module, project, active)

    def test_pointer_digests_bind_receipt_and_installed_core(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            self.publish(module, project, active)

            receipt_path = generation / "receipt.json"
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["specificationSha256"] = "e" * 64
            receipt["receiptIntegritySha256"] = module.json_integrity(receipt)
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            pointer = json.loads(
                (project / ".chaos-engine-runtime-current.json").read_text(
                    encoding="utf-8"
                )
            )
            pointer["active"]["receiptSha256"] = module.sha256(receipt_path)
            pointer["integritySha256"] = module.json_integrity(pointer)
            (project / ".chaos-engine-runtime-current.json").write_text(
                json.dumps(pointer), encoding="utf-8"
            )

            with self.assertRaisesRegex(ValueError, "specification digest"):
                self.select(module, project, active)

    def test_publish_derives_previous_and_rejects_untracked_candidate(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            first, active = self.generation_fixture(module, project)
            self.publish(module, project, active)
            second = first.with_name("e" * 32)
            shutil.copytree(first, second)
            replacement = {
                **active,
                "generationId": "e" * 32,
                "receiptSha256": module.sha256(second / "receipt.json"),
            }

            result = self.publish(module, project, replacement)
            selected, pointer = self.select(module, project, replacement)

            self.assertEqual(second, selected)
            self.assertEqual(active, pointer["previous"])
            self.assertEqual("durable", result["publicationStatus"])
            with self.assertRaisesRegex(ValueError, "requested specification"):
                module.publish_pointer(
                    project,
                    replacement,
                    expected_specification_sha256="f" * 64,
                    expected_core_sha256=replacement["coreSha256"],
                )

    def test_structurally_incomplete_receipt_is_never_active(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            receipt_path = generation / "receipt.json"
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["tools"] = {}
            receipt["receiptIntegritySha256"] = module.json_integrity(receipt)
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            active["receiptSha256"] = module.sha256(receipt_path)
            with self.assertRaisesRegex(ValueError, "receipt schema|tool metadata"):
                self.publish(module, project, active)

    def test_linked_pointer_is_rejected_before_read(self):
        module = load_controller()
        if not hasattr(os, "symlink"):
            self.skipTest("symlinks unavailable")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            outside = project / "outside.json"
            outside.write_text("{}", encoding="utf-8")
            pointer = project / ".chaos-engine-runtime-current.json"
            try:
                pointer.symlink_to(outside)
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")

            with self.assertRaisesRegex(ValueError, "pointer.*link|reparse"):
                module.active_generation(
                    project,
                    expected_specification_sha256="b" * 64,
                    expected_core_sha256="c" * 64,
                )

    def test_linked_project_or_generation_root_is_rejected(self):
        module = load_controller()
        if not hasattr(os, "symlink"):
            self.skipTest("symlinks unavailable")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            real = root / "real"
            real.mkdir()
            generation, active = self.generation_fixture(module, real)
            self.publish(module, real, active)
            linked = root / "linked"
            try:
                linked.symlink_to(real, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")
            with self.assertRaisesRegex(ValueError, "root.*unsafe|ancestor|link"):
                self.select(module, linked, active)

            generation_link = root / "generation-link"
            generation_link.symlink_to(generation, target_is_directory=True)
            receipt = json.loads((generation / "receipt.json").read_text(encoding="utf-8"))
            with self.assertRaisesRegex(ValueError, "root.*unsafe|ancestor|link"):
                module.dispatch_command(generation_link, receipt, "mempalace", [])

    @unittest.skipIf(os.name == "nt", "directory fsync is POSIX-only")
    def test_post_replace_directory_fsync_failure_reports_committed_pointer(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            real_fsync = module.os.fsync
            calls = 0

            def fail_directory_fsync(descriptor):
                nonlocal calls
                calls += 1
                if calls == 2:
                    raise OSError("directory fsync unavailable")
                return real_fsync(descriptor)

            with mock.patch.object(module.os, "fsync", side_effect=fail_directory_fsync):
                pointer = self.publish(module, project, active)

            self.assertEqual("d" * 32, pointer["transactionId"])
            self.assertEqual("committed-not-durable", pointer["publicationStatus"])
            self.assertEqual(generation, self.select(module, project, active)[0])

    def test_dispatch_uses_recorded_environment_python_not_a_uv_shim(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            generation = Path(temporary)
            scripts = "Scripts" if os.name == "nt" else "bin"
            python_name = "python.exe" if os.name == "nt" else "python"
            relative_interpreter = f"uv-tools/mempalace/{scripts}/{python_name}"
            interpreter = generation / relative_interpreter
            interpreter.parent.mkdir(parents=True)
            interpreter.write_bytes(b"python")
            receipt = {
                "tools": {
                    "mempalace": {
                        "dispatch": {
                            "kind": "python",
                            "interpreter": relative_interpreter,
                            "interpreterSha256": module.sha256(interpreter),
                            "interpreterSize": interpreter.stat().st_size,
                            "distribution": "mempalace",
                            "entrypoint": "mempalace",
                        }
                    }
                }
            }

            command = module.dispatch_command(generation, receipt, "mempalace", ["--version"])

            self.assertEqual(str(interpreter), command[0])
            self.assertEqual("-c", command[1])
            self.assertIn("importlib.metadata", command[2])
            self.assertEqual(["mempalace", "mempalace", "--version"], command[3:])
            self.assertNotIn(str(generation / "bin/mempalace"), command)

            receipt["tools"]["mempalace"]["dispatch"]["distribution"] = "foreign"
            with self.assertRaisesRegex(ValueError, "metadata.*invalid"):
                module.dispatch_command(generation, receipt, "mempalace", [])

            receipt["tools"]["mempalace"]["dispatch"]["distribution"] = "mempalace"
            receipt["tools"]["mempalace"]["dispatch"]["interpreterSize"] = (
                module.MAX_EXECUTABLE_BYTES + 1
            )
            with self.assertRaisesRegex(ValueError, "metadata.*invalid"):
                module.dispatch_command(generation, receipt, "mempalace", [])

    @unittest.skipUnless(os.name == "nt", "Windows rooted-path rule")
    def test_windows_rooted_interpreter_path_cannot_escape_generation(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            generation = root / "generation"
            generation.mkdir()
            outside = root / "outside-python.exe"
            outside.write_bytes(b"python")
            rooted = os.path.splitdrive(str(outside))[1]
            receipt = {
                "tools": {
                    "graphify": {
                        "dispatch": {
                            "kind": "python",
                            "interpreter": rooted,
                            "interpreterSha256": module.sha256(outside),
                            "interpreterSize": outside.stat().st_size,
                            "distribution": "graphifyy",
                            "entrypoint": "graphify",
                        }
                    }
                }
            }

            with self.assertRaisesRegex(
                ValueError, "path.*unsafe|interpreter.*unsafe|metadata.*invalid"
            ):
                module.dispatch_command(generation, receipt, "graphify", [])


if __name__ == "__main__":
    unittest.main()
