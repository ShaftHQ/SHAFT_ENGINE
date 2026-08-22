"""Immutable ChaosEngine dependency-generation tests (#5299)."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import tempfile
import threading
import unittest
import unittest.mock as mock
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"
TOOL = ROOT / "chaos-engine/tool.py"
INSTALLER = ROOT / "chaos-engine/install.py"
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "1" * 40


def load_controller():
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_generation_dependencies", CONTROLLER
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("dependency controller could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def load_tool():
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_generation_tool", TOOL
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("tool launcher could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def load_installer():
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_generation_installer", INSTALLER
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("installer could not be loaded")
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
    def generation_fixture(
        module,
        project: Path,
        *,
        generation_id: str = "a" * 32,
        specification_sha256: str = "b" * 64,
    ) -> tuple[Path, dict[str, str]]:
        core = project / ".chaos-engine/manifest.json"
        core.parent.mkdir(exist_ok=True)
        if not core.exists():
            core.write_text('{"owned":true}\n', encoding="utf-8")
        generation = project / ".chaos-engine-runtime-generations" / generation_id
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
            "specificationSha256": specification_sha256,
            "coreSha256": module.sha256(core),
            "environment": {},
            "installed": {},
            "tools": dispatches,
            "ownership": module.sealed_ownership_record(generation),
        }
        receipt_value["receiptIntegritySha256"] = module.json_integrity(
            receipt_value
        )
        receipt = generation / "receipt.json"
        receipt.write_text(json.dumps(receipt_value) + "\n", encoding="utf-8")
        return generation, {
            "generationId": generation_id,
            "specificationSha256": specification_sha256,
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

    def test_stable_launcher_dispatches_all_tools_from_active_generation(self):
        controller = load_controller()
        launcher = load_tool()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(controller, project)
            shutil.copy2(CONTROLLER, project / ".chaos-engine/dependencies.py")
            self.publish(controller, project, active)
            receipt = json.loads(
                (generation / controller.RECEIPT_NAME).read_text(encoding="utf-8")
            )

            for name in controller.REQUIRED_DISPATCHES:
                with self.subTest(tool=name):
                    expected = controller.dispatch_command(
                        generation, receipt, name, ["--version"]
                    )
                    self.assertEqual(
                        expected,
                        launcher.resolve_command(
                            project / ".chaos-engine", name, ["--version"]
                        ),
                    )

    def test_fresh_install_builds_candidate_then_publishes_pointer_last(self):
        installer = load_installer()
        controller = load_controller()
        events: list[str] = []
        legacy_repairs: list[Path] = []
        original_load_installed = installer.load_installed_controller

        def prepare(project, specification, core_sha256):
            events.append("candidate")
            _, record = self.generation_fixture(
                controller,
                project,
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            return record

        def publish(project, active, **kwargs):
            events.append("publish")
            return controller.publish_pointer(project, active, **kwargs)

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=publish,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            repair=lambda runtime, _specification: legacy_repairs.append(runtime),
        )

        def load_installed(installed_root, name):
            loaded = original_load_installed(installed_root, name)
            if name != "hosts":
                return loaded
            real_install = loaded.install
            real_initialize = loaded.initialize_mempalace_runtime

            def install_hosts(*args, **kwargs):
                events.append("hosts")
                return real_install(*args, **kwargs)

            def initialize(project):
                events.append("mempalace")
                return real_initialize(project)

            loaded.install = install_hosts
            loaded.initialize_mempalace_runtime = initialize
            return loaded

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ), mock.patch.object(
                installer, "load_installed_controller", side_effect=load_installed
            ):
                installer.install_with_dependencies(project, source, TEST_COMMIT)
                installed_status = installer.status_with_dependencies(project)

            pointer = json.loads(
                (project / controller.POINTER_NAME).read_text(encoding="utf-8")
            )
            self.assertEqual(["candidate", "hosts", "mempalace", "publish"], events)
            self.assertEqual("a" * 32, pointer["active"]["generationId"])
            self.assertEqual("healthy", installed_status["dependencies"]["status"])
            self.assertEqual([], legacy_repairs)
            self.assertFalse((project / ".chaos-engine-runtime").exists())

    def test_healthy_same_spec_install_makes_no_candidate_network_calls(self):
        installer = load_installer()
        controller = load_controller()
        builds: list[str] = []

        def prepare(project, specification, core_sha256):
            generation_id = ("a" if not builds else "b") * 32
            builds.append(generation_id)
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=generation_id,
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            return record

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, TEST_COMMIT)
                pointer_before = (project / controller.POINTER_NAME).read_bytes()
                installer.install_with_dependencies(project, source, TEST_COMMIT)

            self.assertEqual(["a" * 32], builds)
            self.assertEqual(
                pointer_before, (project / controller.POINTER_NAME).read_bytes()
            )

    def test_remove_generation_validates_full_content_before_deleting(self):
        controller = load_controller()
        self.assertTrue(
            hasattr(controller, "remove_generation"),
            "safe generation removal is not implemented",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, record = self.generation_fixture(controller, project)
            uv = next(path for path in generation.rglob("uv*") if path.is_file())
            original = uv.stat()
            uv.write_bytes(b"xx")
            os.utime(uv, ns=(original.st_atime_ns, original.st_mtime_ns))

            with self.assertRaisesRegex(ValueError, "content drift"):
                controller.remove_generation(project, record)

            self.assertTrue(generation.is_dir())

    def test_failure_before_pointer_restores_core_hosts_and_removes_candidate(self):
        installer = load_installer()
        controller = load_controller()
        builds: list[dict[str, str]] = []
        original_load_installed = installer.load_installed_controller

        def prepare(project, specification, core_sha256):
            generation_id = ("a" if not builds else "b") * 32
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=generation_id,
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            builds.append(record)
            return record

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
        )

        def load_installed(installed_root, name):
            loaded = original_load_installed(installed_root, name)
            if name == "hosts":
                manifest = installer.verify_install(installed_root)
                if manifest["source"]["commit"] == "2" * 40:
                    loaded.initialize_mempalace_runtime = mock.Mock(
                        side_effect=RuntimeError("initialization failed")
                    )
            return loaded

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ), mock.patch.object(
                installer, "load_installed_controller", side_effect=load_installed
            ):
                installer.install_with_dependencies(project, source, TEST_COMMIT)
                pointer_before = (project / controller.POINTER_NAME).read_bytes()
                core_before = (project / ".chaos-engine/manifest.json").read_bytes()
                hosts_before = (project / ".chaos-engine-hosts.json").read_bytes()
                with self.assertRaisesRegex(RuntimeError, "initialization failed"):
                    installer.install_with_dependencies(
                        project, source, "2" * 40
                    )

            self.assertEqual(
                pointer_before, (project / controller.POINTER_NAME).read_bytes()
            )
            self.assertEqual(
                core_before, (project / ".chaos-engine/manifest.json").read_bytes()
            )
            self.assertEqual(
                hosts_before, (project / ".chaos-engine-hosts.json").read_bytes()
            )
            self.assertFalse(
                (project / controller.GENERATIONS_NAME / ("b" * 32)).exists()
            )

    def test_failure_after_pointer_restores_only_validated_previous_generation(self):
        installer = load_installer()
        controller = load_controller()
        builds: list[dict[str, str]] = []
        publications = 0

        def prepare(project, specification, core_sha256):
            generation_id = ("a" if not builds else "b") * 32
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=generation_id,
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            builds.append(record)
            return record

        def publish(project, active, **kwargs):
            nonlocal publications
            publications += 1
            result = controller.publish_pointer(project, active, **kwargs)
            if publications == 2:
                raise RuntimeError("failure after pointer")
            return result

        def validated_previous(project, expected_specification_sha256, expected_core_sha256):
            pointer = controller._read_pointer(project)
            previous = controller._validate_generation_record(pointer["previous"])
            controller._validate_selected_generation(
                project,
                previous,
                expected_specification_sha256,
                expected_core_sha256,
                verify_installed_core=False,
            )
            return previous

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=publish,
            remove_generation=controller.remove_generation,
            pointer_records=controller._read_pointer,
            validated_previous=validated_previous,
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, TEST_COMMIT)
                old_core = (project / ".chaos-engine/manifest.json").read_bytes()
                with self.assertRaisesRegex(RuntimeError, "failure after pointer"):
                    installer.install_with_dependencies(
                        project, source, "2" * 40
                    )

            pointer = controller._read_pointer(project)
            self.assertEqual("a" * 32, pointer["active"]["generationId"])
            self.assertEqual("b" * 32, pointer["previous"]["generationId"])
            self.assertEqual(
                old_core, (project / ".chaos-engine/manifest.json").read_bytes()
            )

    def test_previous_generation_is_probed_with_exact_launcher_dispatch(self):
        controller = load_controller()
        self.assertTrue(
            hasattr(controller, "validated_previous"),
            "validated previous-generation probe is not implemented",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, previous = self.generation_fixture(controller, project)
            self.publish(controller, project, previous)
            core = project / ".chaos-engine/manifest.json"
            core.write_text('{"owned":"new"}\n', encoding="utf-8")
            _, active = self.generation_fixture(
                controller,
                project,
                generation_id="b" * 32,
                specification_sha256=previous["specificationSha256"],
            )
            self.publish(controller, project, active)
            receipt = json.loads(
                (generation / controller.RECEIPT_NAME).read_text(encoding="utf-8")
            )
            commands: list[list[str]] = []

            def runner(command, _environment):
                commands.append(command)
                return SimpleNamespace(stdout="ok\n", stderr="")

            selected = controller.validated_previous(
                project,
                previous["specificationSha256"],
                previous["coreSha256"],
                runner=runner,
            )

            probes = {
                "uv": ["--version"],
                "mempalace": ["--version"],
                "mempalace-mcp": ["--help"],
                "graphify": ["--version"],
                "memory": ["--help"],
                "memory-mcp": ["--help"],
            }
            expected = {
                tuple(controller.dispatch_command(generation, receipt, name, arguments))
                for name, arguments in probes.items()
            }
            self.assertEqual(previous, selected)
            self.assertEqual(expected, {tuple(command) for command in commands})

    def test_active_doctor_probes_exact_launcher_dispatch(self):
        controller = load_controller()
        self.assertTrue(
            hasattr(controller, "probe_active"),
            "active generation probe is not implemented",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(controller, project)
            self.publish(controller, project, active)
            receipt = json.loads(
                (generation / controller.RECEIPT_NAME).read_text(encoding="utf-8")
            )
            commands: list[list[str]] = []

            def runner(command, _environment):
                commands.append(command)
                return SimpleNamespace(stdout="ok\n", stderr="")

            controller.probe_active(
                project,
                active["specificationSha256"],
                active["coreSha256"],
                runner=runner,
            )
            probes = {
                "uv": ["--version"],
                "mempalace": ["--version"],
                "mempalace-mcp": ["--help"],
                "graphify": ["--version"],
                "memory": ["--help"],
                "memory-mcp": ["--help"],
            }
            self.assertEqual(
                {
                    tuple(
                        controller.dispatch_command(
                            generation, receipt, name, arguments
                        )
                    )
                    for name, arguments in probes.items()
                },
                {tuple(command) for command in commands},
            )

    def test_successful_activation_retains_exactly_one_previous_generation(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32, "c" * 32))

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            return record

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            validated_previous=lambda *_args, **_kwargs: self.fail(
                "rollback was not expected"
            ),
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                for commit in ("1" * 40, "2" * 40, "3" * 40):
                    installer.install_with_dependencies(project, source, commit)

            pointer = controller.pointer_records(project)
            self.assertEqual("c" * 32, pointer["active"]["generationId"])
            self.assertEqual("b" * 32, pointer["previous"]["generationId"])
            self.assertEqual(
                {"b" * 32, "c" * 32},
                {
                    path.name
                    for path in (project / controller.GENERATIONS_NAME).iterdir()
                },
            )

    def test_repair_after_upgrade_retains_last_valid_previous_generation(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32, "c" * 32))
        records: list[dict[str, str]] = []

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            records.append(record)
            return record

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            validated_previous=controller.validated_previous,
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, "1" * 40)
                installer.install_with_dependencies(project, source, "2" * 40)
                damaged = project / controller.GENERATIONS_NAME / ("b" * 32)
                receipt = json.loads(
                    (damaged / controller.RECEIPT_NAME).read_text(encoding="utf-8")
                )
                for tool in ("graphify", "mempalace"):
                    (damaged / receipt["tools"][tool]["dispatch"]["interpreter"]).unlink()

                installer.install_with_dependencies(project, source, "2" * 40)

            pointer = controller.pointer_records(project)
            self.assertEqual("c" * 32, pointer["active"]["generationId"])
            self.assertEqual("a" * 32, pointer["previous"]["generationId"])
            self.assertNotIn(records[1], (pointer["active"], pointer["previous"]))
            self.assertTrue(damaged.is_dir(), "unsafe damaged generation was deleted")
            controller.validated_previous(
                project,
                records[0]["specificationSha256"],
                records[0]["coreSha256"],
                runner=lambda *_args: SimpleNamespace(stdout="ok\n", stderr=""),
            )

    def test_missing_managed_tool_builds_complete_candidate_without_invalid_previous(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32))

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            return record

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, TEST_COMMIT)
                first = project / controller.GENERATIONS_NAME / ("a" * 32)
                receipt = json.loads(
                    (first / controller.RECEIPT_NAME).read_text(encoding="utf-8")
                )
                graphify = first / receipt["tools"]["graphify"]["dispatch"][
                    "interpreter"
                ]
                graphify.unlink()
                installer.install_with_dependencies(project, source, TEST_COMMIT)

            pointer = controller.pointer_records(project)
            self.assertEqual("b" * 32, pointer["active"]["generationId"])
            self.assertIsNone(pointer["previous"])

    def test_offline_rollback_validates_previous_before_core_swap(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32))
        validations: list[str] = []

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            self.assertEqual(core_sha256, record["coreSha256"])
            return record

        def validated_previous(
            project, expected_specification_sha256, expected_core_sha256
        ):
            manifest = installer.verify_install(project / installer.INSTALL_DIRECTORY)
            expected_commit = ("2" if not validations else "1") * 40
            self.assertEqual(expected_commit, manifest["source"]["commit"])
            validations.append("before-core-swap")
            return controller.validated_previous(
                project,
                expected_specification_sha256,
                expected_core_sha256,
                runner=lambda *_args: SimpleNamespace(stdout="ok\n", stderr=""),
            )

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            validated_previous=validated_previous,
            repair=lambda *_args, **_kwargs: self.fail(
                "legacy flat-runtime repair was called"
            ),
        )

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, "1" * 40)
                installer.install_with_dependencies(project, source, "2" * 40)
                installer.rollback(project)

            pointer = controller.pointer_records(project)
            self.assertEqual(["before-core-swap"], validations)
            self.assertEqual("1" * 40, installer.status(project)["commit"])
            self.assertEqual("a" * 32, pointer["active"]["generationId"])
            self.assertEqual("b" * 32, pointer["previous"]["generationId"])
            previous = project / controller.GENERATIONS_NAME / ("b" * 32)
            receipt = json.loads(
                (previous / controller.RECEIPT_NAME).read_text(encoding="utf-8")
            )
            graphify = previous / receipt["tools"]["graphify"]["dispatch"][
                "interpreter"
            ]
            graphify.unlink()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                with self.assertRaisesRegex(ValueError, "unexpected or missing"):
                    installer.rollback(project)
            self.assertEqual("1" * 40, installer.status(project)["commit"])
            self.assertIsNotNone(installer.read_cross_rollback_journal(project))

    def test_generation_rollback_resumes_after_core_swap_before_pointer(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32))

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            return record

        def validated_previous(
            project, expected_specification_sha256, expected_core_sha256
        ):
            return controller.validated_previous(
                project,
                expected_specification_sha256,
                expected_core_sha256,
                runner=lambda *_args: SimpleNamespace(stdout="ok\n", stderr=""),
            )

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=controller.publish_pointer,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            validated_previous=validated_previous,
        )
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, "1" * 40)
                installer.install_with_dependencies(project, source, "2" * 40)
                installer.write_cross_rollback_journal(project, "1" * 40, "2" * 40)
                installer.rollback(project, _locked=True)
                self.assertEqual("1" * 40, installer.status(project)["commit"])
                self.assertEqual(
                    "b" * 32,
                    controller.pointer_records(project)["active"]["generationId"],
                )
                installer.rollback(project)

            pointer = controller.pointer_records(project)
            self.assertEqual("1" * 40, installer.status(project)["commit"])
            self.assertEqual("a" * 32, pointer["active"]["generationId"])

    def test_generation_rollback_resumes_after_pointer_publish_before_cleanup(self):
        installer = load_installer()
        controller = load_controller()
        identifiers = iter(("a" * 32, "b" * 32))
        publications = 0

        def prepare(project, specification, core_sha256):
            _, record = self.generation_fixture(
                controller,
                project,
                generation_id=next(identifiers),
                specification_sha256=controller.specification_digest(specification),
            )
            return record

        def publish(project, active, **kwargs):
            nonlocal publications
            publications += 1
            result = controller.publish_pointer(project, active, **kwargs)
            if publications == 3:
                raise RuntimeError("crash after pointer publication")
            return result

        def validated_previous(
            project, expected_specification_sha256, expected_core_sha256
        ):
            return controller.validated_previous(
                project,
                expected_specification_sha256,
                expected_core_sha256,
                runner=lambda *_args: SimpleNamespace(stdout="ok\n", stderr=""),
            )

        fake_dependencies = SimpleNamespace(
            load_specification=controller.load_specification,
            specification_digest=controller.specification_digest,
            active_generation=controller.active_generation,
            prepare_candidate=prepare,
            publish_pointer=publish,
            pointer_records=controller.pointer_records,
            remove_generation=controller.remove_generation,
            validated_previous=validated_previous,
        )
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            shutil.copytree(SOURCE, source)
            source.joinpath("hooks/kernel.py").unlink()
            project = root / "consumer"
            project.mkdir()
            with mock.patch.object(
                installer, "load_dependency_controller", return_value=fake_dependencies
            ):
                installer.install_with_dependencies(project, source, "1" * 40)
                desired_core = (project / ".chaos-engine/manifest.json").read_bytes()
                host_controller = installer.load_installed_controller(
                    project / ".chaos-engine", "hosts"
                )
                desired_host_receipt = (project / ".chaos-engine-hosts.json").read_bytes()
                desired_host_files = host_controller.current_images(project)
                installer.install_with_dependencies(project, source, "2" * 40)
                with self.assertRaisesRegex(
                    RuntimeError, "crash after pointer publication"
                ):
                    installer.rollback(project)

                pointer_path = project / controller.POINTER_NAME
                crash_pointer = pointer_path.read_bytes()
                crash_records = controller.pointer_records(project)
                self.assertEqual("a" * 32, crash_records["active"]["generationId"])
                self.assertEqual("b" * 32, crash_records["previous"]["generationId"])
                self.assertEqual(
                    desired_core,
                    (project / ".chaos-engine/manifest.json").read_bytes(),
                )
                journal_path = (
                    project / installer.CROSS_ROLLBACK_JOURNAL_NAME / "journal.json"
                )
                journal_body = {
                    "schemaVersion": 1,
                    "desiredCommit": "1" * 40,
                    "priorCommit": "2" * 40,
                }
                journal_body["integritySha256"] = installer.hashlib.sha256(
                    json.dumps(
                        journal_body, sort_keys=True, separators=(",", ":")
                    ).encode()
                ).hexdigest()
                self.assertEqual(
                    (json.dumps(journal_body, sort_keys=True) + "\n").encode(),
                    journal_path.read_bytes(),
                )

                installer.rollback(project)

            self.assertEqual(3, publications)
            self.assertEqual(
                desired_core,
                (project / ".chaos-engine/manifest.json").read_bytes(),
            )
            self.assertEqual(
                desired_host_receipt,
                (project / ".chaos-engine-hosts.json").read_bytes(),
            )
            self.assertEqual(desired_host_files, host_controller.current_images(project))
            self.assertEqual(crash_pointer, pointer_path.read_bytes())
            self.assertIsNone(installer.read_cross_rollback_journal(project))
            self.assertFalse(
                (project / installer.CROSS_ROLLBACK_JOURNAL_NAME).exists()
            )

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
            with self.assertRaisesRegex(ValueError, "ancestor|link|reparse"):
                self.generation_fixture(module, project)

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
            second_receipt_path = second / "receipt.json"
            second_receipt = json.loads(second_receipt_path.read_text(encoding="utf-8"))
            second_receipt["ownership"] = module.sealed_ownership_record(second)
            second_receipt["receiptIntegritySha256"] = module.json_integrity(
                second_receipt
            )
            second_receipt_path.write_text(json.dumps(second_receipt), encoding="utf-8")
            replacement = {
                **active,
                "generationId": "e" * 32,
                "receiptSha256": module.sha256(second_receipt_path),
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

    def test_active_generation_denies_added_or_changed_sealed_content(self):
        module = load_controller()
        self.assertTrue(
            hasattr(module, "sealed_ownership_record"),
            "sealed generation ownership is not implemented",
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            generation, active = self.generation_fixture(module, project)
            receipt_path = generation / "receipt.json"
            receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
            receipt["ownership"] = module.sealed_ownership_record(generation)
            receipt["receiptIntegritySha256"] = module.json_integrity(receipt)
            receipt_path.write_text(json.dumps(receipt), encoding="utf-8")
            active["receiptSha256"] = module.sha256(receipt_path)
            self.publish(module, project, active)

            added = generation / "unexpected.py"
            added.write_text("print('owned?')\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "sealed generation|ownership"):
                self.select(module, project, active)
            added.unlink()

            scripts = "Scripts" if os.name == "nt" else "bin"
            python_name = "python.exe" if os.name == "nt" else "python"
            interpreter = generation / f"uv-tools/mempalace/{scripts}/{python_name}"
            original = interpreter.stat()
            interpreter.write_bytes(b"X" * original.st_size)
            os.utime(
                interpreter,
                ns=(original.st_atime_ns, original.st_mtime_ns),
            )
            with self.assertRaisesRegex(ValueError, "sealed generation|ownership"):
                self.select(module, project, active)

    def test_sealed_capture_detects_mutation_after_hash_before_identity_check(self):
        module = load_controller()
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary)
            owned = runtime / "owned.bin"
            owned.write_bytes(b"original")
            original = owned.stat()
            real_read = module.os.read
            mutated = False

            def mutate_after_hash(descriptor, size):
                nonlocal mutated
                chunk = real_read(descriptor, size)
                if not chunk and not mutated:
                    mutated = True
                    owned.write_bytes(b"changed!")
                    os.utime(owned, ns=(original.st_atime_ns, original.st_mtime_ns))
                return chunk

            with mock.patch.object(module.os, "read", side_effect=mutate_after_hash):
                with self.assertRaisesRegex(ValueError, "changed while hashing"):
                    module.sealed_ownership_record(runtime)

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

    def test_candidate_builds_once_at_final_path_with_transaction_local_caches(self):
        module = load_controller()
        self.assertTrue(
            hasattr(module, "prepare_candidate"),
            "immutable generation preparation is not implemented",
        )
        specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine/manifest.json"
            core.parent.mkdir()
            core.write_text('{"owned":true}\n', encoding="utf-8")
            commands = []
            environments = []

            def runner(command, environment):
                commands.append(command)
                environments.append(dict(environment))
                generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
                scripts = "Scripts" if os.name == "nt" else "bin"
                python_name = "python.exe" if os.name == "nt" else "python"
                uv_name = "uv.exe" if os.name == "nt" else "uv"
                if command[1:3] == ["-m", "venv"]:
                    python = generation / f"bootstrap/{scripts}/{python_name}"
                    python.parent.mkdir(parents=True, exist_ok=True)
                    python.write_bytes(b"python")
                if "pip" in command and "install" in command:
                    uv = generation / f"bootstrap/{scripts}/{uv_name}"
                    uv.parent.mkdir(parents=True, exist_ok=True)
                    uv.write_bytes(b"uv")
                if command[1:3] == ["tool", "install"]:
                    environment_name = "graphifyy" if "graphifyy" in command[-1] else "mempalace"
                    python = generation / f"uv-tools/{environment_name}/{scripts}/{python_name}"
                    python.parent.mkdir(parents=True, exist_ok=True)
                    python.write_bytes(f"python-{environment_name}".encode())
                if "npm" in Path(command[0]).name and "install" in command:
                    for name, suffix in (
                        ("memory", "dist/cli/main.js"),
                        ("memory-mcp", "dist/mcp/server.js"),
                    ):
                        script = generation / f"npm/node_modules/@aictx/memory/{suffix}"
                        script.parent.mkdir(parents=True, exist_ok=True)
                        script.write_text(f"// {name}\n", encoding="utf-8")
                return SimpleNamespace(stdout="ok\n", stderr="")

            record = module.prepare_candidate(
                project,
                specification,
                module.sha256(core),
                runner=runner,
                generation_id="a" * 32,
                transaction_id="d" * 32,
            )

            generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
            self.assertEqual("a" * 32, record["generationId"])
            self.assertTrue((generation / "receipt.json").is_file())
            self.assertFalse((project / ".chaos-engine-runtime-current.json").exists())
            self.assertFalse((generation / "uv-cache").exists())
            self.assertFalse((generation / "bin").exists())
            transaction_root = project / ".chaos-engine-runtime-transactions"
            self.assertEqual([], list(transaction_root.iterdir()))
            self.assertTrue(any("--no-cache" in command for command in commands))
            self.assertTrue(
                any("--python" in command and "3.10" in command for command in commands)
            )
            self.assertTrue(
                all(
                    not value.startswith(str(generation))
                    for environment in environments
                    for key, value in environment.items()
                    if key in {"UV_CACHE_DIR", "UV_TOOL_BIN_DIR", "NPM_CONFIG_CACHE"}
                )
            )

    def test_candidate_cleanup_covers_transaction_creation_failure(self):
        module = load_controller()
        specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine/manifest.json"
            core.parent.mkdir()
            core.write_text('{"owned":true}\n', encoding="utf-8")
            transaction_id = "d" * 32
            if os.name == "nt":
                original = module.Path.mkdir

                def fail_transaction(path, *args, **kwargs):
                    if path.name == transaction_id:
                        raise OSError("transaction mkdir failed")
                    return original(path, *args, **kwargs)

                patcher = mock.patch.object(module.Path, "mkdir", fail_transaction)
            else:
                original = module.os.mkdir

                def fail_transaction(path, *args, **kwargs):
                    if str(path) == transaction_id:
                        raise OSError("transaction mkdir failed")
                    return original(path, *args, **kwargs)

                patcher = mock.patch.object(module.os, "mkdir", fail_transaction)
            with patcher, self.assertRaisesRegex(OSError, "transaction mkdir failed"):
                module.prepare_candidate(
                    project,
                    specification,
                    module.sha256(core),
                    runner=lambda *_args: self.fail("runner must not execute"),
                    generation_id="a" * 32,
                    transaction_id=transaction_id,
                )

            generations = project / ".chaos-engine-runtime-generations"
            transactions = project / ".chaos-engine-runtime-transactions"
            self.assertFalse((generations / ("a" * 32)).exists())
            self.assertFalse((transactions / transaction_id).exists())

    def test_final_probe_mutation_cannot_be_sealed(self):
        module = load_controller()
        specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine/manifest.json"
            core.parent.mkdir()
            core.write_text('{"owned":true}\n', encoding="utf-8")
            base_test = GenerationRuntimeTests.test_candidate_builds_once_at_final_path_with_transaction_local_caches
            del base_test  # Runner below mirrors only external boundary materialization.
            probe_count = 0

            def runner(command, environment):
                nonlocal probe_count
                del environment
                generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
                scripts = "Scripts" if os.name == "nt" else "bin"
                python_name = "python.exe" if os.name == "nt" else "python"
                uv_name = "uv.exe" if os.name == "nt" else "uv"
                if command[1:3] == ["-m", "venv"]:
                    path = generation / f"bootstrap/{scripts}/{python_name}"
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(b"python")
                elif "pip" in command and "install" in command:
                    path = generation / f"bootstrap/{scripts}/{uv_name}"
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(b"uv")
                elif command[1:3] == ["tool", "install"]:
                    name = "graphifyy" if "graphifyy" in command[-1] else "mempalace"
                    path = generation / f"uv-tools/{name}/{scripts}/{python_name}"
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(f"python-{name}".encode())
                elif "npm" in Path(command[0]).name and "install" in command:
                    for suffix in ("dist/cli/main.js", "dist/mcp/server.js"):
                        path = generation / f"npm/node_modules/@aictx/memory/{suffix}"
                        path.parent.mkdir(parents=True, exist_ok=True)
                        path.write_text("// memory\n", encoding="utf-8")
                else:
                    probe_count += 1
                    if probe_count == 6:
                        target = generation / f"uv-tools/mempalace/{scripts}/{python_name}"
                        target.write_bytes(b"X" * target.stat().st_size)
                return SimpleNamespace(stdout="ok\n", stderr="")

            with self.assertRaisesRegex(ValueError, "drift|identity|digest"):
                module.prepare_candidate(
                    project,
                    specification,
                    module.sha256(core),
                    runner=runner,
                    generation_id="a" * 32,
                    transaction_id="d" * 32,
                )

            self.assertFalse(
                (project / ".chaos-engine-runtime-generations" / ("a" * 32)).exists()
            )

    def test_failed_candidate_cleanup_never_uses_pathname_rmtree(self):
        module = load_controller()
        specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            core = project / ".chaos-engine/manifest.json"
            core.parent.mkdir()
            core.write_text('{"owned":true}\n', encoding="utf-8")

            def fail(_command, _environment):
                generation = project / ".chaos-engine-runtime-generations" / ("a" * 32)
                marker = generation / "partial/file.txt"
                marker.parent.mkdir(parents=True, exist_ok=True)
                marker.write_text("partial\n", encoding="utf-8")
                raise OSError("offline")

            with mock.patch.object(
                module.shutil,
                "rmtree",
                side_effect=AssertionError("pathname rmtree is forbidden"),
            ), self.assertRaisesRegex(RuntimeError, "install command failed"):
                module.prepare_candidate(
                    project,
                    specification,
                    module.sha256(core),
                    runner=fail,
                    generation_id="a" * 32,
                    transaction_id="d" * 32,
                )

            self.assertFalse(
                (project / ".chaos-engine-runtime-generations" / ("a" * 32)).exists()
            )

    def test_candidate_trust_boundary_is_explicit(self):
        module = load_controller()
        self.assertTrue(
            hasattr(module, "CANDIDATE_TRUST_BOUNDARY"),
            "candidate trust boundary is not encoded",
        )
        boundary = module.CANDIDATE_TRUST_BOUNDARY.lower()
        self.assertIn("same-user", boundary)
        self.assertIn("trusted subprocess", boundary)
        self.assertIn("cannot sandbox", boundary)

    @unittest.skipIf(os.name == "nt", "POSIX ancestor substitution regression")
    def test_candidate_ancestor_swap_fails_before_outside_canary_is_touched(self):
        module = load_controller()
        specification = json.loads(
            (ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8")
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            core = project / ".chaos-engine/manifest.json"
            core.parent.mkdir()
            core.write_text('{"owned":true}\n', encoding="utf-8")
            outside = Path(temporary) / "outside"
            outside.mkdir()
            canary = outside / "canary.txt"
            canary.write_text("keep\n", encoding="utf-8")
            foreign = outside / "foreign/tree/data.bin"
            foreign.parent.mkdir(parents=True)
            foreign.write_bytes(b"foreign-owned")
            swapped = threading.Event()

            def actor():
                generations = project / ".chaos-engine-runtime-generations"
                generations.rename(project / ".generations-displaced")
                generations.symlink_to(outside, target_is_directory=True)
                swapped.set()

            started = False

            def runner(_command, _environment):
                nonlocal started
                if not started:
                    started = True
                    thread = threading.Thread(target=actor)
                    thread.start()
                    thread.join(timeout=5)
                    self.assertTrue(swapped.is_set())
                return SimpleNamespace(stdout="ok\n", stderr="")

            with self.assertRaisesRegex(ValueError, "unsafe|identity changed"):
                module.prepare_candidate(
                    project,
                    specification,
                    module.sha256(core),
                    runner=runner,
                    generation_id="a" * 32,
                    transaction_id="d" * 32,
                )

            self.assertEqual("keep\n", canary.read_text(encoding="utf-8"))
            self.assertEqual(b"foreign-owned", foreign.read_bytes())
            self.assertEqual({"canary.txt", "foreign"}, {path.name for path in outside.iterdir()})
            self.assertFalse(
                (project / ".generations-displaced" / ("a" * 32)).exists()
            )


if __name__ == "__main__":
    unittest.main()
