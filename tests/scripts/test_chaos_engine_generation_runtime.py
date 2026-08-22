"""Immutable ChaosEngine dependency-generation tests (#5299)."""

from __future__ import annotations

import importlib.util
import json
import os
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
    def generation_fixture(module, project: Path) -> tuple[Path, dict[str, str]]:
        core = project / ".chaos-engine/manifest.json"
        core.parent.mkdir()
        core.write_text('{"owned":true}\n', encoding="utf-8")
        generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
        generation.mkdir(parents=True)
        interpreter = generation / "uv-tools/shared/bin/python"
        interpreter.parent.mkdir(parents=True)
        interpreter.write_bytes(b"python")
        dispatches = {}
        for name in (
            "uv",
            "mempalace",
            "mempalace-mcp",
            "graphify",
            "memory",
            "memory-mcp",
        ):
            dispatches[name] = {
                "dispatch": {
                    "kind": "python",
                    "interpreter": "uv-tools/shared/bin/python",
                    "interpreterSha256": module.sha256(interpreter),
                    "distribution": "mempalace",
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
            module.publish_pointer(project, active, None, transaction_id="d" * 32)

            selected, pointer = module.active_generation(project)

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
                module.active_generation(project)

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
            module.publish_pointer(project, active, None, transaction_id="d" * 32)

            with self.assertRaisesRegex(ValueError, "ancestor|link|reparse"):
                module.active_generation(project)

        with tempfile.TemporaryDirectory() as temporary:
            generation = Path(temporary) / "generation"
            generation.mkdir()
            outside = Path(temporary) / "python"
            outside.write_bytes(b"python")
            interpreter = generation / "uv-tools/mempalace/bin/python"
            interpreter.parent.mkdir(parents=True)
            interpreter.symlink_to(outside)
            receipt = {
                "tools": {
                    "mempalace": {
                        "dispatch": {
                            "kind": "python",
                            "interpreter": "uv-tools/mempalace/bin/python",
                            "interpreterSha256": module.sha256(outside),
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

            module.publish_pointer(project, active, None, transaction_id="d" * 32)

            pointer = project / ".chaos-engine-runtime-current.json"
            pointer.write_bytes(b"{" + b" " * module.MAX_CONTROL_BYTES + b"}")
            with mock.patch.object(
                module.Path,
                "read_text",
                side_effect=AssertionError("bounded reader must not call read_text"),
            ):
                with self.assertRaisesRegex(ValueError, "too large"):
                    module.active_generation(project)

    def test_pointer_digests_bind_receipt_and_installed_core(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            module.publish_pointer(project, active, None, transaction_id="d" * 32)

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
                module.active_generation(project)

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
            module.publish_pointer(project, active, None, transaction_id="d" * 32)

            with self.assertRaisesRegex(ValueError, "receipt schema|tool metadata"):
                module.active_generation(project)

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
                module.active_generation(project)

    def test_linked_project_or_generation_root_is_rejected(self):
        module = load_controller()
        if not hasattr(os, "symlink"):
            self.skipTest("symlinks unavailable")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            real = root / "real"
            real.mkdir()
            generation, active = self.generation_fixture(module, real)
            module.publish_pointer(real, active, None, transaction_id="d" * 32)
            linked = root / "linked"
            try:
                linked.symlink_to(real, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")
            with self.assertRaisesRegex(ValueError, "root.*unsafe|ancestor|link"):
                module.active_generation(linked)

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
                pointer = module.publish_pointer(
                    project, active, None, transaction_id="d" * 32
                )

            self.assertEqual("d" * 32, pointer["transactionId"])
            self.assertEqual(generation, module.active_generation(project)[0])

    def test_dispatch_uses_recorded_environment_python_not_a_uv_shim(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            generation = Path(temporary)
            interpreter = generation / "uv-tools/mempalace/bin/python"
            interpreter.parent.mkdir(parents=True)
            interpreter.write_bytes(b"python")
            receipt = {
                "tools": {
                    "mempalace": {
                        "dispatch": {
                            "kind": "python",
                            "interpreter": "uv-tools/mempalace/bin/python",
                            "interpreterSha256": module.sha256(interpreter),
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
                            "distribution": "graphifyy",
                            "entrypoint": "graphify",
                        }
                    }
                }
            }

            with self.assertRaisesRegex(ValueError, "path.*unsafe|interpreter.*unsafe"):
                module.dispatch_command(generation, receipt, "graphify", [])


if __name__ == "__main__":
    unittest.main()
