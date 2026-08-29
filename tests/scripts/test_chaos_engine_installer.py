"""Transactional standalone ChaosEngine installer tests (#4793)."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import os
import shutil
import sqlite3
import subprocess  # nosec B404 - tests run the fixed local installer only.
import sys
import tempfile
import unittest
import unittest.mock as mock
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/install.py"
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "1" * 40

SPEC = importlib.util.spec_from_file_location("chaos_engine_installer", INSTALLER)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("installer test module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def load_module(path: Path):
    spec = importlib.util.spec_from_file_location("chaos_engine_test_dependency", path)
    if spec is None or spec.loader is None:
        raise RuntimeError("installed controller test module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class LegacyDependencyController:
    """Expose an installed controller through the pre-generation test seam."""

    GENERATION_CAPABILITIES = frozenset(
        {
            "active_generation",
            "generation_environment",
            "generation_install_plan",
            "pointer_records",
            "prepare_candidate",
            "publish_pointer",
            "remove_generation",
            "validated_previous",
        }
    )

    def __init__(self, controller):
        """Wrap a controller while hiding generation-era capabilities."""
        self._controller = controller

    def __getattr__(self, name):
        """Delegate capabilities that existed before immutable generations."""
        if name in self.GENERATION_CAPABILITIES:
            raise AttributeError(name)
        return getattr(self._controller, name)


class AccountDependencyController:
    """Retain generation helpers while replacing only account provisioning."""

    def __init__(self, controller):
        self._controller = controller

    def __getattr__(self, name):
        return getattr(self._controller, name)

    def install_account_dependencies(self, project, _specification):
        receipt = {
            "schemaVersion": 2,
            "scope": "user",
            "components": {
                name: {"status": "healthy", "action": "reused"}
                for name in ("uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7")
            },
            "commands": {
                name: str(Path(sys.executable).resolve())
                for name in ("python3", "node", "memory-mcp", "mempalace-mcp")
            },
        }
        project.joinpath(".chaos-engine-dependencies.json").write_text(
            json.dumps(receipt), encoding="utf-8"
        )
        return receipt


def legacy_dependency_controller_fixture():
    load_controller = MODULE.load_dependency_controller

    def load_without_generation_capabilities(installed_root: Path):
        return LegacyDependencyController(load_controller(installed_root))

    return mock.patch.object(
        MODULE,
        "load_dependency_controller",
        side_effect=load_without_generation_capabilities,
    )


class ChaosEngineDependenciesRunner:
    def __init__(self, runtime: Path):
        self.runtime = runtime

    def __call__(self, command, environment):
        del environment
        executable = Path(command[0])
        if not executable.exists() and executable.is_relative_to(self.runtime.parent):
            executable.parent.mkdir(parents=True, exist_ok=True)
            executable.write_text("tool\n", encoding="utf-8")
        return SimpleNamespace(stdout="tool 1.0\n", stderr="")


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def copy_source(destination: Path) -> Path:
    ignored = shutil.ignore_patterns("__pycache__", "*.pyc")
    return Path(shutil.copytree(SOURCE, destination, ignore=ignored))


def tree_digest(root: Path) -> dict[str, str]:
    return {
        path.relative_to(root).as_posix(): sha256(path)
        for path in root.rglob("*")
        if path.is_file()
    }


def create_chroma_state(path: Path) -> None:
    database = sqlite3.connect(path)
    try:
        database.executescript(
            """
            CREATE TABLE collections (
                id TEXT PRIMARY KEY, name TEXT NOT NULL, dimension INTEGER,
                database_id TEXT NOT NULL, config_json_str TEXT, schema_str TEXT
            );
            CREATE TABLE segments (
                id TEXT PRIMARY KEY, type TEXT NOT NULL, scope TEXT NOT NULL,
                collection TEXT NOT NULL
            );
            CREATE TABLE embeddings_queue (
                seq_id INTEGER PRIMARY KEY, created_at TIMESTAMP NOT NULL,
                operation INTEGER NOT NULL, topic TEXT NOT NULL, id TEXT NOT NULL,
                vector BLOB, encoding TEXT, metadata TEXT
            );
            """
        )
        database.commit()
    finally:
        database.close()


class ChaosEngineInstallerTest(unittest.TestCase):
    def symlink_or_skip(self, target: Path | str, link: Path) -> None:
        try:
            link.symlink_to(target)
        except OSError as error:
            if os.name == "nt" and getattr(error, "winerror", None) == 1314:
                self.skipTest("Windows symlink privilege is unavailable")
            raise

    def test_cache_commands_are_public_and_component_scoped(self):
        status = MODULE.parser().parse_args(
            ["cache", "status", "--component", "maven-tools-mcp"]
        )
        purge = MODULE.parser().parse_args(
            [
                "cache", "purge", "--component", "maven-tools-mcp",
                "--version", "3.2.0",
            ]
        )

        self.assertEqual(("cache", "status", "maven-tools-mcp"), (status.command, status.cache_command, status.component))
        self.assertEqual("3.2.0", purge.version)

    def test_default_install_uses_user_account_dependencies_not_private_generation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            specification = json.loads((SOURCE / "dependencies.json").read_text(encoding="utf-8"))
            controller = SimpleNamespace(
                load_specification=lambda _path: specification,
                install_account_dependencies=mock.Mock(
                    return_value={
                        "schemaVersion": 2,
                        "components": {},
                        "commands": {
                            "memory-mcp": "/user/bin/memory-mcp",
                            "mempalace-mcp": "/user/bin/mempalace-mcp",
                            "node": "/user/bin/node",
                            "python3": "/user/bin/python3.14",
                        },
                    }
                ),
            )
            with mock.patch.object(MODULE, "load_dependency_controller", return_value=controller):
                installed = MODULE.install_with_dependencies(
                    project, SOURCE, TEST_COMMIT, with_maven_tools=False
                )

            self.assertEqual(project / ".chaos-engine", installed)
            controller.install_account_dependencies.assert_called_once_with(project, specification)
            self.assertFalse(project.joinpath(".chaos-engine-runtime-current.json").exists())
            mcp = json.loads(project.joinpath(".mcp.json").read_text(encoding="utf-8"))
            self.assertEqual(
                "https://mcp.context7.com/mcp",
                mcp["mcpServers"]["context7"]["url"],
            )

    def test_account_update_rollback_does_not_require_a_generation_pointer(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            load_controller = MODULE.load_dependency_controller

            def load_account_controller(installed_root):
                return AccountDependencyController(load_controller(installed_root))

            with mock.patch.object(
                MODULE, "load_dependency_controller", side_effect=load_account_controller
            ):
                MODULE.install_with_dependencies(project, SOURCE, "1" * 40)
                MODULE.install_with_dependencies(project, SOURCE, "2" * 40)
                MODULE.rollback(project)

            self.assertEqual("1" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine-runtime-current.json").exists())

    def test_status_reads_account_dependency_receipt_without_network_or_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
                with_maven_tools=False,
            )
            receipt = {
                "schemaVersion": 2,
                "checkedAt": "2026-08-25T00:00:00+00:00",
                "scope": "user",
                "components": {
                    name: {
                        "status": "healthy",
                        "action": "reused",
                        "provider": "path",
                        "latestVersionVerified": True,
                        "probe": "passed",
                    }
                    for name in ("uv", "node", "java", "mempalace", "graphify", "memory", "context7")
                },
                "commands": {},
            }
            path = project / ".chaos-engine-dependencies.json"
            path.write_text(json.dumps(receipt), encoding="utf-8")
            before = path.read_bytes()

            host_controller = MODULE.load_installed_controller(
                project / ".chaos-engine", "hosts"
            )
            load_controller = MODULE.load_installed_controller
            with mock.patch.object(
                host_controller,
                "mempalace_runtime_status",
                return_value={"status": "recovery-required"},
            ), mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=lambda target, name: (
                    host_controller if name == "hosts" else load_controller(target, name)
                ),
            ):
                result = MODULE.status_with_dependencies(project)

            self.assertEqual("healthy", result["status"], result)
            self.assertEqual("healthy", result["dependencies"]["status"])
            self.assertEqual("healthy", result["components"]["mempalace"]["status"])
            self.assertEqual(2, result["dependencies"]["schemaVersion"])
            self.assertEqual("reused", result["dependencies"]["components"]["node"]["action"])
            self.assertEqual(before, path.read_bytes())

    def test_maven_tools_uses_stable_tag_system_java_and_upstream_ci_build(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            target = root / ".chaos-engine"
            target.mkdir()
            java = root / "bin/java"
            git = root / "bin/git"
            java.parent.mkdir()
            java.write_text("java\n", encoding="utf-8")
            git.write_text("git\n", encoding="utf-8")
            cache = root / "cache"
            calls = []
            published = []

            def runner(command, **kwargs):
                calls.append(command)
                if "clone" in command:
                    source = Path(command[-1])
                    source.mkdir(parents=True)
                    wrapper = source / "mvnw"
                    wrapper.write_text("wrapper\n", encoding="utf-8")
                if "package" in command:
                    source = Path(kwargs["cwd"])
                    built = source / "target/maven-tools-mcp-3.2.1.jar"
                    built.parent.mkdir()
                    built.write_bytes(b"jar")
                return SimpleNamespace(
                    returncode=0,
                    stdout=("a" * 40 + "\n") if "rev-parse" in command else "",
                    stderr="",
                )

            hosts = SimpleNamespace(
                maven_tools_cache_status=lambda version: {"status": "absent", "version": version},
                java_major=lambda path: 25 if path == java.resolve() else None,
                maven_tools_cache_root=lambda: cache,
                MAVEN_TOOLS_MCP_RECEIPT="install-receipt.json",
                publish_maven_tools_cache=lambda staging: published.append(staging),
                discover_maven_tools_runtime=lambda: (java.resolve(), root / "maven-tools-mcp-3.2.1.jar"),
                probe_maven_tools_runtime=lambda *_args: True,
            )
            dependencies = SimpleNamespace(
                resolve_stable_version=lambda *_args, **_kwargs: "3.2.1"
            )
            specification = {
                "dependencies": {"maven-tools-mcp": {"stableChannel": "https://example.invalid"}}
            }
            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                return_value=hosts,
            ), mock.patch.object(
                MODULE, "load_dependency_controller", return_value=dependencies
            ), mock.patch.object(
                MODULE.shutil,
                "which",
                side_effect=lambda name: str(git if name == "git" else java if name == "java" else ""),
            ):
                MODULE.ensure_maven_tools(target, specification, runner=runner)

            self.assertIn(
                [str(git), "clone", "--branch", "v3.2.1", "--depth", "1",
                 "https://github.com/arvindand/maven-tools-mcp.git", calls[0][-1]],
                calls,
            )
            self.assertTrue(any(command[-3:] == ["clean", "package", "-Pci"] for command in calls))
            self.assertEqual(1, len(published))

    def test_explicit_maven_tools_docker_mode_requires_healthy_existing_docker(self):
        specification = {
            "dependencies": {"maven-tools-mcp": {"stableChannel": "https://example.invalid"}}
        }
        dependencies = SimpleNamespace(
            resolve_stable_version=lambda *_args, **_kwargs: "3.2.1"
        )
        healthy = mock.Mock(returncode=0, stdout="27.0.0\n", stderr="")
        with mock.patch.object(
            MODULE, "load_installed_controller", return_value=SimpleNamespace()
        ), mock.patch.object(
            MODULE, "load_dependency_controller", return_value=dependencies
        ), mock.patch.object(
            MODULE.shutil, "which", return_value="/usr/bin/docker"
        ):
            result = MODULE.ensure_maven_tools(
                Path("."), specification, mode="docker", runner=mock.Mock(return_value=healthy)
            )
        self.assertEqual("arvindand/maven-tools-mcp:3.2.1", result["image"])

    def test_status_and_explain_json_v2_are_deterministic_and_secret_free(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )

            with mock.patch.object(
                MODULE,
                "status_with_dependencies",
                return_value={
                    "status": "healthy",
                    "commit": TEST_COMMIT,
                    "distribution": "portable",
                    "policySha256": "a" * 64,
                    "path": str(project / "private"),
                    "apiToken": "do-not-render",
                    "kernel": {"url": "https://user:password@example.invalid/path"},
                    "hosts": {"status": "healthy"},
                    "dependencies": {"status": "healthy"},
                    "components": {},
                },
            ):
                first = MODULE.status_json(project)
                second = MODULE.status_json(project)
            explained = MODULE.explain_json(
                project,
                "Stop",
                host="codex",
                session_id="diagnostic",
            )

            self.assertEqual(first, second)
            self.assertEqual(
                (2, "chaos-engine", "status"),
                (first["schemaVersion"], first["identity"], first["kind"]),
            )
            rendered = json.dumps(first, sort_keys=True)
            self.assertNotIn(str(project), rendered)
            self.assertNotIn("do-not-render", rendered)
            self.assertNotIn("user:password", rendered)
            self.assertEqual(
                (2, "chaos-engine", "explain", "complete"),
                (
                    explained["schemaVersion"],
                    explained["identity"],
                    explained["kind"],
                    explained["terminalReason"],
                ),
            )
            removed = dict(first)
            removed["legacyStatus"] = "healthy"
            with self.assertRaisesRegex(ValueError, "removed fields"):
                MODULE.validate_diagnostic_json(removed)
            for document, field in ((first, "status"), (explained, "decision")):
                with self.subTest(kind=document["kind"], field=field):
                    missing = dict(document)
                    missing.pop(field)
                    with self.assertRaisesRegex(ValueError, "required fields"):
                        MODULE.validate_diagnostic_json(missing)

            parsed = MODULE.parser().parse_args(
                [
                    "explain",
                    "Stop",
                    "--project",
                    str(project),
                    "--host",
                    "copilot",
                    "--json",
                ]
            )
            self.assertTrue(parsed.json)

    def test_status_exposes_validated_capability_metadata_for_every_component(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )

            result = MODULE.status_with_dependencies(project)
            manifest = json.loads((project / ".chaos-engine/manifest.json").read_text(encoding="utf-8"))
            host_receipt = json.loads((project / ".chaos-engine-hosts.json").read_text(encoding="utf-8"))

            expected = {
                "core", "skills", "playbooks", "hooks", "plugins", "roles", "mcps",
                "retrieval-config", "projection-policy", "tools", "memory", "mempalace",
                "graphify", "maven-tools-mcp",
            }
            self.assertEqual(expected, set(result["components"]))
            for component in result["components"].values():
                self.assertIn(component["owner"], {"installer", "project", "user"})
                self.assertIn(component["scope"], {"project", "repository", "user"})
                self.assertIn(component["lifecycle"], {"receipt-owned", "persistent-data", "derived-single-writer", "user-managed-cache"})
                self.assertIn(component["taskImpact"], {"required", "advisory", "optional"})
            for name in ("memory", "mempalace", "graphify"):
                self.assertEqual("advisory", result["components"][name]["taskImpact"])
            self.assertEqual("optional", result["components"]["maven-tools-mcp"]["taskImpact"])
            self.assertEqual("receipt-owned", result["components"]["maven-tools-mcp"]["lifecycle"])
            self.assertEqual("recovery-required", result["status"])
            self.assertEqual(manifest["capabilityPolicySha256"], host_receipt["capabilityPolicySha256"])
            self.assertEqual(manifest["capabilities"], MODULE.legacy_capability_policy())

    def test_new_manifest_binds_capabilities_and_legacy_v1_upgrades_in_place(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            target = MODULE.install(project, SOURCE, TEST_COMMIT)
            manifest_path = target / MODULE.MANIFEST_NAME
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertRegex(manifest["capabilityPolicySha256"], r"^[0-9a-f]{64}$")
            self.assertTrue(manifest["capabilities"])

            manifest.pop("capabilities")
            manifest.pop("capabilityPolicySha256")
            manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
            MODULE.install(project, SOURCE, TEST_COMMIT)

            upgraded = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertTrue(upgraded["capabilities"])
            self.assertRegex(upgraded["capabilityPolicySha256"], r"^[0-9a-f]{64}$")

    def test_legacy_manifest_upgrade_failure_does_not_roll_back_the_core_generation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            prior_commit = "0" * 40
            MODULE.install_with_dependencies(
                project, SOURCE, prior_commit, provisioner=lambda *_args: None
            )
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            manifest_path = project / ".chaos-engine/manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest.pop("capabilities")
            manifest.pop("capabilityPolicySha256")
            manifest_path.write_text(
                json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )

            with self.assertRaisesRegex(RuntimeError, "provision failed"):
                MODULE.install_with_dependencies(
                    project,
                    SOURCE,
                    TEST_COMMIT,
                    provisioner=lambda *_args: (_ for _ in ()).throw(RuntimeError("provision failed")),
                )

            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_public_status_reads_legacy_manifest_before_in_place_upgrade(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            manifest_path = project / ".chaos-engine/manifest.json"
            legacy = json.loads(manifest_path.read_text(encoding="utf-8"))
            legacy.pop("capabilities")
            legacy.pop("capabilityPolicySha256")
            manifest_path.write_text(
                json.dumps(legacy, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )

            observed = MODULE.status_with_dependencies(project)

            self.assertEqual("advisory", observed["components"]["memory"]["taskImpact"])
            self.assertEqual("optional", observed["components"]["maven-tools-mcp"]["taskImpact"])
            doctor = MODULE.doctor_with_dependencies(project, verify_clients=False)
            self.assertEqual("advisory", doctor["components"]["graphify"]["taskImpact"])
            self.assertNotIn("capabilities", json.loads(manifest_path.read_text(encoding="utf-8")))
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            self.assertIn(
                "capabilities", json.loads(manifest_path.read_text(encoding="utf-8"))
            )

    def test_invalid_capability_descriptor_is_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            source = copy_source(Path(temporary) / "source")
            catalog_path = source / "distributions.json"
            catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
            catalog["distributions"]["portable"]["components"] = {
                "core": {"owner": "nobody", "scope": "project", "lifecycle": "receipt-owned", "taskImpact": "required"}
            }
            catalog_path.write_text(json.dumps(catalog), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "capability"):
                MODULE.load_capability_policy(source, "portable")

    def test_project_uninstall_preserves_user_managed_maven_cache(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            data = root / "data"
            cache = data / "ChaosEngine/tools/maven-tools-mcp/3.2.0"
            cache.mkdir(parents=True)
            jar = cache / "maven-tools-mcp-3.2.0.jar"
            jar.write_bytes(b"jar")
            receipt = cache / "install-receipt.json"
            receipt.write_text(json.dumps({
                "version": "3.2.0",
                "commit": "4475ff6c61f23ea9a93cb6d5665a63235ef2ef36",
                "jar": jar.name,
                "sha256": sha256(jar),
            }), encoding="utf-8")
            variable = "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME"

            with mock.patch.dict(os.environ, {variable: str(data)}, clear=False):
                MODULE.install_with_dependencies(
                    project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
                )
                MODULE.uninstall_with_dependencies(project)

            self.assertTrue(jar.is_file())
            self.assertTrue(receipt.is_file())

    def test_doctor_keeps_advisory_store_health_strict(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            dependency_module = load_module(SOURCE / "dependencies.py")

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            controller = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            controller.mempalace_runtime_status = mock.Mock(
                return_value={"status": "recovery-required", "detail": "fixture"}
            )
            controller.retrieval_runtime_healthy = mock.Mock(return_value=True)
            controller.retrieval_runtime_status = mock.Mock(return_value={"status": "healthy"})
            controller.mcp_runtime_healthy = mock.Mock(return_value=True)
            original_load = MODULE.load_installed_controller

            def load_for_doctor(root, name):
                if name == "hosts":
                    return controller
                loaded = original_load(root, name)
                if name == "dependencies":
                    loaded.doctor = loaded.status
                return loaded

            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=load_for_doctor,
            ):
                result = MODULE.doctor_with_dependencies(project, verify_clients=False)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual("advisory", result["components"]["mempalace"]["taskImpact"])
            self.assertEqual("recovery-required", result["components"]["mempalace"]["status"])

    def test_optional_maven_cache_health_does_not_fail_status(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            dependency_module = load_module(SOURCE / "dependencies.py")

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            data = root / "data"
            invalid = data / "ChaosEngine/tools/maven-tools-mcp/3.2.0"
            invalid.mkdir(parents=True)
            (invalid / "unknown").write_text("user data", encoding="utf-8")
            variable = "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME"
            with mock.patch.dict(os.environ, {variable: str(data)}, clear=False):
                MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
                result = MODULE.status_with_dependencies(project)

            self.assertEqual("healthy", result["status"])
            self.assertEqual("invalid", result["components"]["maven-tools-mcp"]["status"])
            self.assertEqual("optional", result["components"]["maven-tools-mcp"]["taskImpact"])

    def test_doctor_command_uses_the_full_status_contract(self):
        arguments = MODULE.parser().parse_args(["doctor", "--project", "."])

        self.assertEqual("doctor", arguments.command)

    def test_doctor_uses_active_dependency_probes(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            dependency_module = load_module(SOURCE / "dependencies.py")

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            controller = MODULE.load_dependency_controller(project / ".chaos-engine")
            with mock.patch.object(
                controller,
                "doctor",
                side_effect=RuntimeError("active probe failed"),
            ), mock.patch.object(MODULE, "load_dependency_controller", return_value=controller):
                with self.assertRaisesRegex(RuntimeError, "active probe failed"):
                    MODULE.doctor_with_dependencies(project)

    def test_install_initializes_fresh_mempalace_without_receipt_ownership(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary).resolve() / "consumer"
            project.mkdir()
            original_load = MODULE.load_installed_controller
            controllers = []

            def load_with_initializer(installed_root, name):
                controller = original_load(installed_root, name)
                if name == "hosts":
                    controller.initialize_mempalace_runtime = mock.Mock()
                    controllers.append(controller)
                return controller

            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=load_with_initializer,
            ):
                MODULE.install_with_dependencies(
                    project,
                    SOURCE,
                    TEST_COMMIT,
                    provisioner=lambda *_args, **_kwargs: None,
                )

            controller = controllers[-1]
            controller.initialize_mempalace_runtime.assert_called_once_with(project)
            receipt = json.loads(
                (project / controller.RECEIPT_NAME).read_text(encoding="utf-8")
            )
            owned = json.dumps({"before": receipt["before"], "after": receipt["after"]})
            self.assertNotIn(".chaos-engine-state/mempalace", owned)

    def test_status_rejects_semantically_invalid_retrieval_configuration(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            controller = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            original_load = MODULE.load_installed_controller
            with mock.patch.object(
                controller,
                "retrieval_configs_healthy",
                return_value=False,
            ), mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=lambda root, name: (
                    controller if name == "hosts" else original_load(root, name)
                ),
            ):
                result = MODULE.status_with_dependencies(project)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual("absent", result["components"]["retrieval-config"]["status"])

    def test_doctor_rejects_a_memory_runtime_that_cannot_use_its_store(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            controller = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            original_load = MODULE.load_installed_controller
            with mock.patch.object(
                controller,
                "retrieval_runtime_status",
                return_value={"status": "recovery-required", "reason": "memory check reported invalid store"},
            ), mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=lambda root, name: (
                    controller if name == "hosts" else original_load(root, name)
                ),
            ):
                result = MODULE.doctor_with_dependencies(project, verify_clients=False)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual(
                "recovery-required",
                result["components"]["memory"]["status"],
            )
            self.assertEqual(
                "memory check reported invalid store",
                result["components"]["memory"]["reason"],
            )
            self.assertEqual(
                "healthy",
                result["components"]["retrieval-config"]["status"],
            )

    def test_doctor_rejects_an_mcp_runtime_that_cannot_initialize(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            controller = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            original_load = MODULE.load_installed_controller
            with mock.patch.object(
                controller,
                "retrieval_runtime_status",
                return_value={"status": "healthy"},
            ), mock.patch.object(
                controller,
                "mcp_runtime_healthy",
                return_value=False,
            ), mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=lambda root, name: (
                    controller if name == "hosts" else original_load(root, name)
                ),
            ):
                result = MODULE.doctor_with_dependencies(project, verify_clients=False)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual("recovery-required", result["components"]["mcps"]["status"])

    def test_status_maps_legacy_mempalace_classifier_without_launching(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary).resolve() / "consumer"
            project.mkdir()
            original_load = MODULE.load_installed_controller
            controllers = []

            def load_with_initializer(installed_root, name):
                controller = original_load(installed_root, name)
                if name == "hosts":
                    controller.initialize_mempalace_runtime = mock.Mock()
                    controllers.append(controller)
                return controller

            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=load_with_initializer,
            ):
                MODULE.install_with_dependencies(
                    project,
                    SOURCE,
                    TEST_COMMIT,
                    provisioner=lambda *_args, **_kwargs: None,
                )
            palace = project / ".chaos-engine-state/mempalace"
            palace.mkdir(parents=True, exist_ok=True)
            create_chroma_state(palace / "chroma.sqlite3")
            palace.joinpath("00000000-0000-0000-0000-000000000001").mkdir()
            controller = controllers[-1]
            controller.mempalace_runtime_status = mock.Mock(
                return_value={
                    "status": "migration-required",
                    "detail": "Legacy Chroma state requires migration",
                }
            )
            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=lambda root, name: (
                    controller if name == "hosts" else original_load(root, name)
                ),
            ):
                passive = MODULE.status_with_dependencies(project)

            self.assertEqual("recovery-required", passive["status"])
            self.assertEqual(
                "migration-required",
                passive["components"]["mempalace"]["status"],
            )
            controller.mempalace_runtime_status.assert_called_once_with(project)

    def test_detect_distribution_stays_portable_without_matching_pom(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            self.assertEqual("portable", MODULE.detect_distribution(project, SOURCE))
            project.joinpath("pom.xml").write_text(
                """<project>
                  <modelVersion>4.0.0</modelVersion>
                  <groupId>example</groupId>
                  <artifactId>demo</artifactId>
                  <version>1.0.0</version>
                  <dependencies>
                    <dependency>
                      <groupId>org.junit.jupiter</groupId>
                      <artifactId>junit-jupiter</artifactId>
                      <version>5.11.0</version>
                    </dependency>
                  </dependencies>
                </project>
                """,
                encoding="utf-8",
            )
            self.assertEqual("portable", MODULE.detect_distribution(project, SOURCE))

    def test_detect_distribution_selects_repository_from_matching_pom(self):
        wanted = json.loads(
            (SOURCE / "profiles/shaft/profile.json").read_text(encoding="utf-8")
        )["installWhen"]["mavenArtifactIds"][0]
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            project.joinpath("pom.xml").write_text(
                f"""<project>
                  <modelVersion>4.0.0</modelVersion>
                  <groupId>example</groupId>
                  <artifactId>consumer</artifactId>
                  <version>1.0.0</version>
                  <dependencies>
                    <dependency>
                      <groupId>io.github.example</groupId>
                      <artifactId>{wanted}</artifactId>
                      <version>1.0.0</version>
                    </dependency>
                  </dependencies>
                </project>
                """,
                encoding="utf-8",
            )
            self.assertEqual("repository", MODULE.detect_distribution(project, SOURCE))

    def test_detect_distribution_selects_repository_from_reactor_module(self):
        self.assertEqual("repository", MODULE.detect_distribution(ROOT, SOURCE))

    def test_maven_coordinate_ids_ignore_plugins_comments_and_broken_xml(self):
        wanted = json.loads(
            (SOURCE / "profiles/shaft/profile.json").read_text(encoding="utf-8")
        )["installWhen"]["mavenArtifactIds"][0]
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            project.joinpath("pom.xml").write_text(
                f"""<project>
                  <parent>
                    <artifactId>{wanted}</artifactId>
                  </parent>
                  <artifactId>demo</artifactId>
                  <build>
                    <plugins>
                      <plugin>
                        <artifactId>{wanted}</artifactId>
                      </plugin>
                    </plugins>
                  </build>
                  <dependencies>
                    <dependency>
                      <artifactId>junit-jupiter</artifactId>
                      <exclusions>
                        <exclusion>
                          <artifactId>{wanted}</artifactId>
                        </exclusion>
                      </exclusions>
                    </dependency>
                  </dependencies>
                  <!-- <dependency><artifactId>{wanted}</artifactId></dependency> -->
                </project>
                """,
                encoding="utf-8",
            )
            self.assertEqual("portable", MODULE.detect_distribution(project, SOURCE))
            self.assertEqual(
                {"demo", "junit-jupiter"},
                MODULE.maven_coordinate_ids(project / "pom.xml"),
            )
            project.joinpath("pom.xml").write_text("<project><unclosed>", encoding="utf-8")
            self.assertEqual("portable", MODULE.detect_distribution(project, SOURCE))

    def test_portable_installer_avoids_flagged_host_and_xml_parsers(self):
        installer = INSTALLER.read_text(encoding="utf-8")
        self.assertNotIn("xml.etree", installer)
        self.assertNotIn("ElementTree", installer)
        self.assertNotIn("Write-Host", (SOURCE / "install.ps1").read_text(encoding="utf-8"))

    def test_portable_installer_source_does_not_name_the_repository_profile(self):
        text = INSTALLER.read_text(encoding="utf-8").casefold()
        self.assertNotIn("shaft", text)

    def test_default_distribution_installs_only_neutral_portable_payload(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            MODULE.install(project, SOURCE, TEST_COMMIT)

            install_root = project / ".chaos-engine"
            manifest = json.loads((install_root / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertRegex(manifest["distribution"]["policySha256"], r"^[0-9a-f]{64}$")
            runtime_files = set(
                json.loads((SOURCE / "distributions.json").read_text(encoding="utf-8"))[
                    "distributions"
                ]["portable"]["runtimeFiles"]
            )
            self.assertEqual(
                {
                    "hooks/kernel.py",
                    "hooks/launch.js",
                    "hooks/lifecycle.py",
                    "hooks/matchers.json",
                },
                runtime_files,
            )
            owned_text = "\n".join(
                path.read_text(encoding="utf-8", errors="ignore")
                for path in install_root.rglob("*")
                if path.is_file()
            ).casefold()
            owned_paths = "\n".join(manifest["files"]).casefold()
            self.assertNotIn("shaft", owned_paths)
            self.assertNotIn("shaft", owned_text)
            self.assertNotIn("act-as-mohab", owned_paths)
            self.assertNotIn("act-as-mohab", owned_text)
            self.assertNotIn("mohab", owned_paths)
            self.assertNotIn("mohab", owned_text)
            hook = install_root / "hooks/guard.py"
            for relative in (
                "hooks/kernel.py",
                "hooks/lifecycle.py",
                "hooks/reflection.py",
            ):
                installed = install_root / relative
                self.assertTrue(installed.is_file(), relative)
                self.assertEqual(sha256(installed), manifest["files"][relative])
            (project / "lifecycle.py").write_text(
                "raise RuntimeError('consumer lifecycle shadow imported')\n",
                encoding="utf-8",
            )
            (project / "reflection.py").write_text(
                "raise RuntimeError('consumer reflection shadow imported')\n",
                encoding="utf-8",
            )
            environment = {**os.environ, "TMPDIR": temporary, "TEMP": temporary}
            start = subprocess.run(  # nosec B603 - fixed interpreter and installed local hook.
                [sys.executable, str(hook)],
                input=json.dumps({"hook_event_name": "SessionStart", "session_id": "real-install"}),
                capture_output=True, text=True, env=environment, check=False,
            )
            locator_start = subprocess.run(  # nosec B603 - mirrors generated runpy locator.
                [
                    sys.executable,
                    "-c",
                    "import runpy,sys;runpy.run_path(sys.argv[1],run_name='__main__')",
                    str(hook),
                ],
                cwd=project,
                input=json.dumps({"hook_event_name": "SessionStart", "session_id": "locator-install"}),
                capture_output=True,
                text=True,
                env=environment,
                check=False,
            )
            failure = {
                "hook_event_name": "PostToolUseFailure",
                "tool_name": "PowerShell",
                "tool_input": {"command": "py -3 -m unittest installed.case"},
                "session_id": "real-install",
            }
            first = subprocess.run(  # nosec B603 - fixed interpreter and installed local hook.
                [sys.executable, str(hook)], input=json.dumps(failure),
                capture_output=True, text=True, env=environment, check=False,
            )
            second = subprocess.run(  # nosec B603 - fixed interpreter and installed local hook.
                [sys.executable, str(hook)], input=json.dumps(failure),
                capture_output=True, text=True, env=environment, check=False,
            )
            self.assertEqual(0, start.returncode, start.stderr)
            self.assertEqual(0, locator_start.returncode, locator_start.stderr)
            self.assertIn("additionalContext", json.loads(locator_start.stdout))
            installed_context = json.loads(start.stdout).get("additionalContext")
            for relative in (
                "vendor/caveman/skills/caveman/SKILL.md",
                "vendor/ponytail/skills/ponytail/SKILL.md",
            ):
                vendor = install_root / relative
                self.assertTrue(vendor.is_file(), relative)
                self.assertIn(relative, installed_context)
                self.assertNotIn(vendor.read_text(encoding="utf-8"), installed_context)
            self.assertIn("caveman=ultra; ponytail=ultra", installed_context)
            self.assertEqual(0, first.returncode, first.stderr)
            self.assertEqual(0, second.returncode, second.stderr)
            self.assertIn("Reflection required", second.stdout)

    def test_distribution_cannot_change_during_an_update(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(project / ".chaos-engine")

            with self.assertRaisesRegex(ValueError, "uninstall before changing"):
                MODULE.install(project, SOURCE, "2" * 40, distribution="repository")

            self.assertEqual(before, tree_digest(project / ".chaos-engine"))
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())

    def test_distributionless_legacy_manifest_upgrades_in_place(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            manifest_path = project / ".chaos-engine/manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest.pop("distribution")
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            self.assertEqual("legacy", MODULE.status(project)["distribution"])
            MODULE.install(project, SOURCE, "2" * 40)
            upgraded = MODULE.status(project)
            self.assertEqual("portable", upgraded["distribution"])
            self.assertEqual("2" * 40, upgraded["commit"])

    def test_distribution_rejects_a_missing_profile_before_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = copy_source(root / "source")
            project = root / "consumer"
            project.mkdir()
            catalog_path = source / "distributions.json"
            catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
            catalog["distributions"]["portable"]["profile"] = "missing"
            catalog_path.write_text(json.dumps(catalog), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "profile is incomplete"):
                MODULE.install(project, source, TEST_COMMIT)
            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_distribution_rejects_profile_traversal_before_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = copy_source(root / "source")
            outside = root / "outside"
            outside.mkdir()
            for name in ("entrypoint.md", "profile.json"):
                outside.joinpath(name).write_text("{}", encoding="utf-8")
            project = root / "consumer"
            project.mkdir()
            catalog_path = source / "distributions.json"
            catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
            catalog["distributions"]["portable"]["profile"] = "../../outside"
            catalog_path.write_text(json.dumps(catalog), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "policy is invalid"):
                MODULE.install(project, source, TEST_COMMIT)
            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_new_commit_can_harden_policy_without_changing_distribution(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = copy_source(root / "source")
            project = root / "consumer"
            project.mkdir()
            MODULE.install(project, source, TEST_COMMIT)
            catalog_path = source / "distributions.json"
            catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
            catalog["distributions"]["portable"]["forbiddenTokens"].append(
                "never-present-marker"
            )
            catalog_path.write_text(json.dumps(catalog), encoding="utf-8")

            MODULE.install(project, source, "2" * 40)

            manifest = MODULE.verify_install(project / ".chaos-engine")
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertEqual("2" * 40, manifest["source"]["commit"])

    def test_full_status_reports_distribution_identity(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )

            self.assertEqual(
                "portable", MODULE.status_with_dependencies(project)["distribution"]
            )

    def test_full_status_is_not_healthy_when_runtime_is_absent(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )

            result = MODULE.status_with_dependencies(project)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual("absent", result["dependencies"]["status"])
            self.assertIn("components", result)
            self.assertEqual("absent", result["components"]["tools"]["status"])

    def test_project_lock_closes_descriptor_when_stream_creation_fails(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with mock.patch.object(MODULE.os, "fdopen", side_effect=OSError("stream failed")):
                with mock.patch.object(MODULE.os, "close", wraps=MODULE.os.close) as close:
                    with self.assertRaisesRegex(OSError, "stream failed"):
                        with MODULE.project_lock(project):
                            self.fail("lock unexpectedly acquired")
            close.assert_called_once()

    def test_clean_install_is_complete_and_manifest_bound(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer project"
            project.mkdir()

            completed = subprocess.run(  # nosec B603 - fixed interpreter and repository installer.
                [
                    sys.executable,
                    str(INSTALLER),
                    "install",
                    "--project",
                    str(project),
                    "--source",
                    str(SOURCE),
                    "--commit",
                    TEST_COMMIT,
                    "--skip-tools",
                ],
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(0, completed.returncode, completed.stderr)
            install_root = project / ".chaos-engine"
            manifest = json.loads((install_root / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(1, manifest["schemaVersion"])
            self.assertEqual(TEST_COMMIT, manifest["source"]["commit"])

            expected = {
                path.relative_to(SOURCE).as_posix(): sha256(path)
                for path in MODULE.source_files(SOURCE, "portable")
            }
            self.assertEqual(expected, manifest["files"])
            for relative, digest in manifest["files"].items():
                self.assertEqual(digest, sha256(install_root / relative), relative)

            self.assertFalse(
                any("__pycache__" in Path(relative).parts for relative in manifest["files"])
            )
            self.assertFalse(any(relative.endswith(".pyc") for relative in manifest["files"]))
            self.assertFalse(install_root.joinpath("__pycache__").exists())

    def test_portable_source_files_omit_origin_only_docs(self):
        relatives = [
            path.relative_to(SOURCE).as_posix()
            for path in MODULE.source_files(SOURCE, "portable")
        ]

        self.assertFalse(
            any(
                relative == "assets/brand" or relative.startswith("assets/brand/")
                for relative in relatives
            )
        )
        self.assertNotIn("RESEARCH.md", relatives)
        self.assertNotIn("STANDALONE.md", relatives)
        self.assertNotIn("README.md", relatives)
        self.assertTrue((SOURCE / "README.md").is_file())
        self.assertTrue((SOURCE / "STANDALONE.md").is_file())
        self.assertTrue(any(relative.startswith("assets/memory-v5/") for relative in relatives))
        self.assertNotIn("INSTALL.md", relatives)
        self.assertTrue((SOURCE / "INSTALL.md").is_file())
        self.assertIn("LICENSE", relatives)

    def test_stage_loss_fails_before_publish(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            real_copy = MODULE.shutil.copy2

            def copy_then_remove(source, destination):
                result = real_copy(source, destination)
                if Path(source).name == "README.md":
                    Path(destination).unlink()
                return result

            with mock.patch.object(MODULE.shutil, "copy2", side_effect=copy_then_remove):
                with self.assertRaisesRegex(ValueError, "staged payload"):
                    MODULE.install(project, SOURCE, TEST_COMMIT)

            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_installed_tree_is_rejected_as_a_source(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            first = root / "first"
            second = root / "second"
            first.mkdir()
            second.mkdir()
            installed = MODULE.install(first, SOURCE, TEST_COMMIT)

            with self.assertRaisesRegex(ValueError, "reserved manifest"):
                MODULE.install(second, installed, TEST_COMMIT)

            self.assertFalse(second.joinpath(".chaos-engine").exists())

    def test_default_install_provisions_every_dependency_in_a_project_local_runtime(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary).resolve() / "consumer"
            project.mkdir()
            calls = []

            def provisioner(runtime, specification):
                calls.append((runtime, specification))
                return {"schemaVersion": 1}

            installed = MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=provisioner
            )

            self.assertEqual(project / ".chaos-engine", installed)
            self.assertEqual(project / ".chaos-engine-runtime", calls[0][0])
            self.assertEqual(
                {"uv", "mempalace", "graphify", "memory"},
                set(calls[0][1]["tools"]),
            )
            self.assertFalse(installed.joinpath("__pycache__").exists())
            self.assertEqual("healthy", MODULE.status(project)["status"])
            self.assertTrue(project.joinpath(".agents/skills/chaos-engine/SKILL.md").is_file())
            self.assertTrue(project.joinpath(".claude/skills/chaos-engine/SKILL.md").is_file())
            self.assertTrue(project.joinpath(".gemini/skills/chaos-engine/SKILL.md").is_file())
            self.assertTrue(project.joinpath(".github/skills/chaos-engine/SKILL.md").is_file())
            self.assertIn("chaosengine-memory", project.joinpath(".mcp.json").read_text())

    def test_public_install_repairs_a_missing_managed_tool_on_normal_upgrade(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=provision,
                distribution="repository",
            )
            graphify = Path(
                dependency_module.executable(
                    project / ".chaos-engine-runtime/bin", "graphify"
                )
            )
            graphify.unlink()

            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=provision,
                distribution="repository",
            )

            self.assertTrue(graphify.is_file())
            self.assertEqual(
                "healthy", MODULE.status_with_dependencies(project)["dependencies"]["status"]
            )

    def test_absent_core_recovery_removes_owned_links_without_following_targets(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        specification = dependency_module.load_specification(SOURCE / "dependencies.json")
        with tempfile.TemporaryDirectory() as temporary:
            runtime = Path(temporary) / ".chaos-engine-runtime"

            def runner(command, environment):
                del environment
                executable = Path(command[0])
                if command[1:3] == ["tool", "install"] and command[-1].startswith(
                    "graphifyy=="
                ):
                    target = runtime / "uv-tools/graphifyy/bin/graphify"
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_text("tool\n", encoding="utf-8")
                    link = runtime / "bin/graphify"
                    link.parent.mkdir(parents=True, exist_ok=True)
                    self.symlink_or_skip(target, link)
                if not executable.exists() and executable.is_relative_to(runtime.parent):
                    executable.parent.mkdir(parents=True, exist_ok=True)
                    executable.write_text("tool\n", encoding="utf-8")
                return SimpleNamespace(stdout="tool 1.0\n", stderr="")

            dependency_module.repair(runtime, specification, runner=runner)
            removing = runtime.with_name(f"{runtime.name}.removing")
            runtime.replace(removing)

            MODULE.finalize_dependency_tombstone(removing)

            self.assertFalse(removing.exists())

    def test_real_controller_rollback_accepts_new_runtime_receipt(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            legacy_source = copy_source(root / "legacy-source")
            legacy_dependencies = legacy_source / "dependencies.py"
            text = legacy_dependencies.read_text(encoding="utf-8")
            legacy_text = text.replace(
                "RUNTIME_CONTRACT_VERSION = 2\n", ""
            ).replace(
                '        "runtimeContractVersion": RUNTIME_CONTRACT_VERSION,\n', ""
            ).replace(
                '    encoded = json.dumps(\n'
                '        {"runtimeContractVersion": RUNTIME_CONTRACT_VERSION, "specification": tool_specification},\n'
                '        sort_keys=True,\n'
                '        separators=(",", ":"),\n'
                '    ).encode()\n',
                '    encoded = json.dumps(tool_specification, sort_keys=True, separators=(",", ":")).encode()\n',
            ).replace(
                '        contract_changed = receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION\n'
                '        return (\n'
                '            "specification-stale"\n'
                '            if specification_changed or capability_changed or contract_changed\n'
                '            else "healthy"\n'
                '        )\n',
                '        return "specification-stale" if specification_changed or capability_changed else "healthy"\n',
            ).replace(
                '    if (\n'
                '        specification is not None\n'
                '        and receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION\n'
                '    ):\n'
                '        raise ValueError("dependency runtime contract drift detected")\n',
                "",
            ).replace(
                '            state = runtime_ownership_state(runtime, current, specification)\n'
                '            if state == "healthy" and not force:\n',
                '            verify_receipt(runtime, current)\n'
                '            if not force:\n',
            )
            self.assertNotEqual(text, legacy_text)
            legacy_dependencies.write_text(legacy_text, encoding="utf-8")
            legacy_module = load_module(legacy_dependencies)
            current_module = load_module(SOURCE / "dependencies.py")

            def legacy_provision(runtime, specification):
                return legacy_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            def current_provision(runtime, specification):
                base_runner = ChaosEngineDependenciesRunner(runtime)

                def linked_runner(command, environment):
                    if command[1:3] == ["tool", "install"] and command[-1].startswith(
                        "graphifyy=="
                    ):
                        target = runtime / "uv-tools/graphifyy/bin/graphify"
                        target.parent.mkdir(parents=True, exist_ok=True)
                        target.write_text("tool\n", encoding="utf-8")
                        link = runtime / "bin/graphify"
                        link.parent.mkdir(parents=True, exist_ok=True)
                        if not link.exists() and not link.is_symlink():
                            self.symlink_or_skip(target, link)
                    return base_runner(command, environment)

                return current_module.repair(
                    runtime,
                    specification,
                    runner=linked_runner,
                )

            rollback_calls = []

            MODULE.install_with_dependencies(
                project,
                legacy_source,
                "1" * 40,
                provisioner=legacy_provision,
                distribution="repository",
            )
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                "2" * 40,
                provisioner=current_provision,
                distribution="repository",
            )

            def rollback_provision(runtime, specification):
                rollback_calls.append(runtime)
                previous = MODULE.load_dependency_controller(project / ".chaos-engine")
                self.assertFalse(hasattr(previous, "RUNTIME_CONTRACT_VERSION"))
                self.assertNotIn(
                    "links",
                    json.loads((runtime / "receipt.json").read_text(encoding="utf-8"))[
                        "ownership"
                    ],
                )

                def legacy_ownership_record(root):
                    files = {}
                    directories = []
                    for path in sorted(root.rglob("*")):
                        relative = path.relative_to(root).as_posix()
                        if previous.is_link_or_reparse(path):
                            raise ValueError(
                                f"dependency runtime contains a link: {relative}"
                            )
                        if path.is_dir():
                            if not previous.is_generated_python_cache(
                                relative, directory=True
                            ):
                                directories.append(relative)
                        elif path.is_file() and relative != previous.RECEIPT_NAME:
                            if not previous.is_generated_python_cache(relative):
                                files[relative] = previous.sha256(path)
                    return {
                        "directories": directories,
                        "files": files,
                        "sha256": previous.ownership_digest(files),
                    }

                previous.ownership_record = legacy_ownership_record
                self.assertNotIn("links", previous.ownership_record(runtime))
                previous.verify_receipt(
                    runtime, previous.read_receipt(runtime), specification
                )
                return previous.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            with legacy_dependency_controller_fixture():
                MODULE.rollback(project, provisioner=rollback_provision)

            self.assertEqual([], rollback_calls)
            status = MODULE.status_with_dependencies(project)
            self.assertEqual("1" * 40, status["commit"])
            self.assertEqual("absent", status["dependencies"]["status"])
            self.assertEqual("healthy", status["hosts"]["status"])
            self.assertFalse((project / MODULE.CROSS_ROLLBACK_JOURNAL_NAME).exists())

    def test_status_reports_dependency_freshness_without_mutating_it(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            old = datetime(2026, 8, 10, tzinfo=timezone.utc)

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                    now=old,
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            before = tree_digest(project / ".chaos-engine-runtime")
            result = MODULE.status_with_dependencies(project)

            self.assertEqual("stale", result["dependencies"]["freshness"])
            self.assertEqual(before, tree_digest(project / ".chaos-engine-runtime"))

    def test_status_rejects_missing_host_adapter(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            project.joinpath(".agents/skills/chaos-engine/SKILL.md").unlink()

            with self.assertRaisesRegex(ValueError, "host adapter drift"):
                MODULE.status_with_dependencies(project)

    def test_dependency_failure_compensates_only_a_core_published_by_this_call(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def fail(*args):
                del args
                raise RuntimeError("offline")

            with self.assertRaisesRegex(RuntimeError, "offline"):
                MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=fail)
            self.assertFalse(project.joinpath(".chaos-engine").exists())

            MODULE.install(project, SOURCE, "1" * 40)
            MODULE.install(project, SOURCE, TEST_COMMIT)
            with self.assertRaisesRegex(RuntimeError, "offline"):
                MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=fail)
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_nullable_legacy_receipt_preflight_blocks_before_core_or_account_setup(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            hosts = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            receipt, _ = hosts.read_receipt(project)
            legacy_path = ".grok/hooks/lifecycle.json"
            receipt["before"][legacy_path] = None
            receipt["after"][legacy_path] = None
            project.joinpath(legacy_path).unlink()
            project.joinpath(legacy_path).write_text("unexpected\n", encoding="utf-8")
            project.joinpath(".chaos-engine-hosts.json").write_bytes(
                hosts.receipt_bytes(receipt, project)
            )

            account_setup = mock.Mock()
            controller = SimpleNamespace(
                install_account_dependencies=account_setup,
            )
            with mock.patch.object(
                MODULE, "load_dependency_controller", return_value=controller
            ):
                with mock.patch.object(MODULE, "install") as core_install:
                    with self.assertRaisesRegex(ValueError, "host adapter drift"):
                        MODULE.install_with_dependencies(project, SOURCE, "2" * 40)

            core_install.assert_not_called()
            account_setup.assert_not_called()
            self.assertFalse(project.joinpath("graphify-out").exists())
            self.assertFalse(project.joinpath(".agents/skills/graphify").exists())

    def test_receipt_preflight_allows_a_legacy_core_binding_mismatch(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            hosts = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            receipt, _ = hosts.read_receipt(project)
            receipt["coreCommit"] = "7" * 40
            project.joinpath(".chaos-engine-hosts.json").write_bytes(
                hosts.receipt_bytes(receipt, project)
            )
            specification = json.loads((SOURCE / "dependencies.json").read_text(encoding="utf-8"))
            account_setup = mock.Mock(return_value={
                "commands": {
                    name: str(Path(sys.executable).resolve())
                    for name in ("python3", "node", "memory-mcp", "mempalace-mcp")
                }
            })
            controller = SimpleNamespace(
                load_specification=lambda _path: specification,
                install_account_dependencies=account_setup,
            )
            with mock.patch.object(
                MODULE, "load_dependency_controller", return_value=controller
            ):
                with mock.patch.object(
                    MODULE, "install", return_value=project / ".chaos-engine"
                ) as core_install:
                    MODULE.install_with_dependencies(project, SOURCE, "2" * 40)

            core_install.assert_called_once()
            account_setup.assert_called_once_with(project, specification)
            repaired, _ = hosts.read_receipt(project)
            self.assertEqual("2" * 40, repaired["coreCommit"])

    def test_receipt_preflight_rejects_tampering_before_core_or_account_setup(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args: None
            )
            receipt_path = project / ".chaos-engine-hosts.json"
            receipt_path.write_bytes(receipt_path.read_bytes().replace(b"installed", b"tampered", 1))

            account_setup = mock.Mock()
            controller = SimpleNamespace(
                install_account_dependencies=account_setup,
            )
            with mock.patch.object(
                MODULE, "load_dependency_controller", return_value=controller
            ):
                with mock.patch.object(MODULE, "install") as core_install:
                    with self.assertRaisesRegex(ValueError, "integrity drift"):
                        MODULE.install_with_dependencies(project, SOURCE, "2" * 40)

            core_install.assert_not_called()
            account_setup.assert_not_called()

    def test_post_setup_host_failure_restores_exact_project_setup_outputs(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            specification = json.loads((SOURCE / "dependencies.json").read_text(encoding="utf-8"))
            project.joinpath(".agents/skills/graphify/user.txt").parent.mkdir(parents=True)
            project.joinpath(".agents/skills/graphify/user.txt").write_text(
                "keep graphify\n", encoding="utf-8"
            )
            project.joinpath("graphify-out/user.txt").parent.mkdir(parents=True)
            project.joinpath("graphify-out/user.txt").write_text("keep graph\n", encoding="utf-8")
            project.joinpath(".memory/config.json").parent.mkdir(parents=True)
            project.joinpath(".memory/config.json").write_text("keep memory\n", encoding="utf-8")
            project.joinpath(".chaos-engine-state/mempalace/user.txt").parent.mkdir(parents=True)
            project.joinpath(".chaos-engine-state/mempalace/user.txt").write_text(
                "keep palace\n", encoding="utf-8"
            )
            project.joinpath("mempalace.yaml").write_text("wing: keep\n", encoding="utf-8")

            def install_account_dependencies(target, _specification):
                target.joinpath(".chaos-engine-dependencies.json").write_text(
                    "new receipt\n", encoding="utf-8"
                )
                target.joinpath(".agents/skills/graphify/SKILL.md").parent.mkdir(
                    parents=True, exist_ok=True
                )
                target.joinpath(".agents/skills/graphify/SKILL.md").write_text(
                    "generated\n", encoding="utf-8"
                )
                target.joinpath("graphify-out/graph.json").parent.mkdir(parents=True, exist_ok=True)
                target.joinpath("graphify-out/graph.json").write_text("{}\n", encoding="utf-8")
                target.joinpath(".memory/config.json").parent.mkdir(parents=True, exist_ok=True)
                target.joinpath(".memory/config.json").write_text("{}\n", encoding="utf-8")
                target.joinpath(".chaos-engine-state/mempalace").mkdir(parents=True, exist_ok=True)
                target.joinpath(".chaos-engine-state/mempalace/sqlite_exact.sqlite3").write_bytes(
                    b"SQLite format 3\\x00"
                )
                target.joinpath("mempalace.yaml").write_text("wing: generated\n", encoding="utf-8")
                return {"commands": {}}

            controller = SimpleNamespace(
                load_specification=lambda _path: specification,
                install_account_dependencies=install_account_dependencies,
            )
            original_load = MODULE.load_installed_controller
            fail_once = True

            def load_with_post_setup_failure(installed_root, name):
                nonlocal fail_once
                loaded = original_load(installed_root, name)
                if name == "hosts" and fail_once:
                    fail_once = False
                    loaded.install = mock.Mock(side_effect=RuntimeError("host install failed"))
                return loaded

            with mock.patch.object(
                MODULE, "load_dependency_controller", return_value=controller
            ):
                with mock.patch.object(
                    MODULE, "load_installed_controller", side_effect=load_with_post_setup_failure
                ):
                    with self.assertRaisesRegex(RuntimeError, "host install failed"):
                        MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT)

            for relative in (
                ".chaos-engine-dependencies.json",
                ".agents/skills/graphify/SKILL.md",
                "graphify-out/graph.json",
                ".chaos-engine-state/mempalace/sqlite_exact.sqlite3",
            ):
                self.assertFalse(project.joinpath(relative).exists(), relative)
            self.assertEqual(
                "keep graphify\n",
                project.joinpath(".agents/skills/graphify/user.txt").read_text(encoding="utf-8"),
            )
            self.assertEqual(
                "keep graph\n", project.joinpath("graphify-out/user.txt").read_text(encoding="utf-8")
            )
            self.assertEqual(
                "keep memory\n", project.joinpath(".memory/config.json").read_text(encoding="utf-8")
            )
            self.assertEqual(
                "keep palace\n",
                project.joinpath(".chaos-engine-state/mempalace/user.txt").read_text(encoding="utf-8"),
            )
            self.assertEqual(
                "wing: keep\n", project.joinpath("mempalace.yaml").read_text(encoding="utf-8")
            )
            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_keyboard_interrupt_skips_install_compensation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def interrupt(*_args, **_kwargs):
                raise KeyboardInterrupt()

            with mock.patch.object(MODULE, "uninstall", wraps=MODULE.uninstall) as uninstall:
                with mock.patch.object(MODULE, "rollback", wraps=MODULE.rollback) as rollback:
                    with self.assertRaises(KeyboardInterrupt):
                        MODULE.install_with_dependencies(
                            project,
                            SOURCE,
                            TEST_COMMIT,
                            provisioner=interrupt,
                        )
                    uninstall.assert_not_called()
                    rollback.assert_not_called()
            self.assertTrue((project / ".chaos-engine").is_dir())
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_keyboard_interrupt_skips_uninstall_compensation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            original_hosts = MODULE.load_installed_controller
            cancelled = []

            def load_hosts(installed_root, name):
                controller = original_hosts(installed_root, name)
                if name == "hosts":
                    real_cancel = controller.cancel_uninstall

                    def cancel(target):
                        cancelled.append(True)
                        return real_cancel(target)

                    controller.cancel_uninstall = cancel
                return controller

            with mock.patch.object(
                MODULE, "load_installed_controller", side_effect=load_hosts
            ):
                with mock.patch.object(MODULE, "uninstall", side_effect=KeyboardInterrupt()):
                    with self.assertRaises(KeyboardInterrupt):
                        MODULE.uninstall_with_dependencies(project)

            self.assertEqual([], cancelled)
            self.assertTrue((project / ".chaos-engine").is_dir())
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_initializer_failure_removes_only_runtime_created_by_install(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            original_load = MODULE.load_installed_controller

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            def load_with_failure(installed_root, name):
                controller = original_load(installed_root, name)
                if name == "hosts":
                    controller.initialize_mempalace_runtime = mock.Mock(
                        side_effect=RuntimeError("initialization failed")
                    )
                return controller

            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=load_with_failure,
            ):
                with self.assertRaisesRegex(RuntimeError, "initialization failed"):
                    MODULE.install_with_dependencies(
                        project,
                        SOURCE,
                        TEST_COMMIT,
                        provisioner=provision,
                    )

            self.assertFalse((project / ".chaos-engine-runtime").exists())

    def test_initializer_failure_preserves_preexisting_verified_runtime(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            runtime = project / ".chaos-engine-runtime"
            specification = dependency_module.load_specification(
                SOURCE / "dependencies.json"
            )
            dependency_module.repair(
                runtime,
                specification,
                runner=ChaosEngineDependenciesRunner(runtime),
            )
            before = tree_digest(runtime)
            original_load = MODULE.load_installed_controller

            def load_with_failure(installed_root, name):
                controller = original_load(installed_root, name)
                if name == "hosts":
                    controller.initialize_mempalace_runtime = mock.Mock(
                        side_effect=RuntimeError("initialization failed")
                    )
                return controller

            with mock.patch.object(
                MODULE,
                "load_installed_controller",
                side_effect=load_with_failure,
            ):
                with self.assertRaisesRegex(RuntimeError, "initialization failed"):
                    MODULE.install_with_dependencies(
                        project,
                        SOURCE,
                        TEST_COMMIT,
                        provisioner=lambda *_args: None,
                    )

            self.assertEqual(before, tree_digest(runtime))

    def test_failed_update_restores_the_previous_host_generation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            before_receipt = project.joinpath(".chaos-engine-hosts.json").read_bytes()
            before_adapter = project.joinpath(
                ".agents/skills/chaos-engine/SKILL.md"
            ).read_bytes()
            changed_source = copy_source(root / "changed")
            hosts = changed_source / "hosts.py"
            hosts.write_text(
                hosts.read_text(encoding="utf-8").replace(
                    "Load the canonical installed ChaosEngine before every task.",
                    "UPDATED host generation.",
                ),
                encoding="utf-8",
            )

            with self.assertRaisesRegex(RuntimeError, "offline"):
                MODULE.install_with_dependencies(
                    project,
                    changed_source,
                    "2" * 40,
                    provisioner=lambda *_args, **_kwargs: (_ for _ in ()).throw(
                        RuntimeError("offline")
                    ),
                )

            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])
            self.assertEqual(before_receipt, project.joinpath(".chaos-engine-hosts.json").read_bytes())
            self.assertEqual(
                before_adapter,
                project.joinpath(".agents/skills/chaos-engine/SKILL.md").read_bytes(),
            )

    def test_failed_host_compensation_does_not_skip_core_rollback(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            changed_source = copy_source(root / "changed")
            hosts = changed_source / "hosts.py"
            hosts.write_text(
                hosts.read_text(encoding="utf-8").replace(
                    "Load the canonical installed ChaosEngine before every task.",
                    "UPDATED host generation.",
                ),
                encoding="utf-8",
            )

            def fail_after_host_update(*_args, **_kwargs):
                project.joinpath("AGENTS.md").write_text("concurrent edit\n", encoding="utf-8")
                raise RuntimeError("offline")

            with self.assertRaisesRegex(ValueError, "host adapter drift"):
                MODULE.install_with_dependencies(
                    project,
                    changed_source,
                    "2" * 40,
                    provisioner=fail_after_host_update,
                )

            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_public_rollback_restores_the_matching_host_generation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            old_adapter = project.joinpath(".agents/skills/chaos-engine/SKILL.md").read_bytes()
            changed_source = copy_source(root / "changed")
            hosts = changed_source / "hosts.py"
            hosts.write_text(
                hosts.read_text(encoding="utf-8").replace(
                    "Load the canonical installed ChaosEngine before every task.",
                    "UPDATED host generation.",
                ),
                encoding="utf-8",
            )
            MODULE.install_with_dependencies(
                project, changed_source, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )

            with legacy_dependency_controller_fixture():
                MODULE.rollback(project, provisioner=lambda *_args, **_kwargs: None)

            self.assertEqual(TEST_COMMIT, MODULE.status_with_dependencies(project)["commit"])
            self.assertEqual(
                old_adapter,
                project.joinpath(".agents/skills/chaos-engine/SKILL.md").read_bytes(),
            )

    def test_public_rollback_resumes_the_recorded_target_generation_after_core_swap(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            changed_source = copy_source(root / "changed")
            hosts = changed_source / "hosts.py"
            hosts.write_text(
                hosts.read_text(encoding="utf-8").replace(
                    "Load the canonical installed ChaosEngine before every task.",
                    "UPDATED host generation.",
                ),
                encoding="utf-8",
            )
            MODULE.install_with_dependencies(
                project, changed_source, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )
            MODULE.write_cross_rollback_journal(project, TEST_COMMIT, "2" * 40)
            MODULE.rollback(project, _locked=True)

            with legacy_dependency_controller_fixture():
                MODULE.rollback(project, provisioner=lambda *_args, **_kwargs: None)

            self.assertEqual(TEST_COMMIT, MODULE.status_with_dependencies(project)["commit"])
            self.assertFalse(project.joinpath(MODULE.CROSS_ROLLBACK_JOURNAL_NAME).exists())

    def test_status_reports_recovery_during_cross_resource_rollback(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            changed_source = copy_source(root / "changed")
            MODULE.install_with_dependencies(
                project, changed_source, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )
            MODULE.write_cross_rollback_journal(project, TEST_COMMIT, "2" * 40)
            MODULE.rollback(project, _locked=True)

            result = MODULE.status_with_dependencies(project)

            self.assertEqual("recovery-required", result["status"])
            self.assertEqual("recovery-required", result["hosts"]["status"])

    def test_cross_rollback_journal_scratch_recovers_and_swapped_intent_is_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            changed_source = copy_source(root / "changed")
            MODULE.install_with_dependencies(
                project, changed_source, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )
            journal = MODULE.write_cross_rollback_journal(project, TEST_COMMIT, "2" * 40)
            journal.write_bytes(journal.read_bytes()[:17])
            recovered = MODULE.read_cross_rollback_journal(project)
            self.assertEqual(TEST_COMMIT, recovered["desiredCommit"])
            self.assertTrue(journal.exists())

            value = json.loads(journal.read_text(encoding="utf-8"))
            value["desiredCommit"], value["priorCommit"] = value["priorCommit"], value["desiredCommit"]
            body = {key: item for key, item in value.items() if key != "integritySha256"}
            value["integritySha256"] = hashlib.sha256(
                json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
            ).hexdigest()
            journal.write_text(json.dumps(value), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "not authenticated"):
                MODULE.rollback(project, provisioner=lambda *_args, **_kwargs: None)

    def test_cross_rollback_recovery_never_executes_a_drifted_controller(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            MODULE.write_cross_rollback_journal(project, TEST_COMMIT, "2" * 40)
            marker = project / "controller-executed.txt"
            hosts = project / MODULE.INSTALL_DIRECTORY / "hosts.py"
            hosts.write_text(
                f"from pathlib import Path\nPath({str(marker)!r}).write_text('executed')\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                MODULE.read_cross_rollback_journal(project)

            self.assertFalse(marker.exists())

    def test_completed_rollback_with_only_receipt_intent_finishes_cleanup(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            changed = copy_source(root / "changed")
            MODULE.install_with_dependencies(
                project, changed, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )
            MODULE.write_cross_rollback_journal(project, TEST_COMMIT, "2" * 40)
            MODULE.rollback(project, _locked=True)
            hosts = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            hosts.install(project, core_commit=TEST_COMMIT)
            transaction = project / MODULE.CROSS_ROLLBACK_JOURNAL_NAME
            transaction.joinpath("journal.json").unlink()
            transaction.rmdir()

            with legacy_dependency_controller_fixture():
                MODULE.rollback(project, provisioner=lambda *_args, **_kwargs: None)

            self.assertEqual(TEST_COMMIT, MODULE.status_with_dependencies(project)["commit"])

    def test_cross_rollback_journal_does_not_require_hard_links(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install_with_dependencies(
                project, SOURCE, TEST_COMMIT, provisioner=lambda *_args, **_kwargs: None
            )
            hosts = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            hosts.set_rollback_intent(project, "2" * 40, TEST_COMMIT)
            with mock.patch.object(MODULE.os, "link", side_effect=OSError("unsupported")):
                journal = MODULE.write_cross_rollback_journal(project, "2" * 40, TEST_COMMIT)

            self.assertTrue(journal.is_file())

    def test_dependency_failure_does_not_delete_a_concurrent_core_update(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            changed_source = copy_source(root / "changed")
            changed_source.joinpath("references/roles.md").write_text("changed\n", encoding="utf-8")

            def interleave(runtime, specification):
                del runtime, specification
                MODULE.install(project, changed_source, "2" * 40)
                raise RuntimeError("offline")

            with self.assertRaisesRegex(RuntimeError, "already running"):
                MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=interleave)

            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_default_uninstall_removes_an_owned_dependency_runtime(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime,
                    specification,
                    runner=ChaosEngineDependenciesRunner(runtime),
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            MODULE.uninstall_with_dependencies(project)

            self.assertFalse(project.joinpath(".chaos-engine").exists())
            self.assertFalse(project.joinpath(".chaos-engine-runtime").exists())

    def test_default_uninstall_restores_host_files_and_configs(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            project.joinpath("AGENTS.md").write_bytes(b"user instructions\r\n")
            project.joinpath(".mcp.json").write_text(
                json.dumps({"mcpServers": {"other": {"command": "other"}}}) + "\n",
                encoding="utf-8",
            )
            before_agents = project.joinpath("AGENTS.md").read_bytes()
            before_mcp = project.joinpath(".mcp.json").read_bytes()

            MODULE.install_with_dependencies(
                project,
                SOURCE,
                TEST_COMMIT,
                provisioner=lambda *_args, **_kwargs: None,
            )
            MODULE.uninstall_with_dependencies(project)

            self.assertEqual(before_agents, project.joinpath("AGENTS.md").read_bytes())
            self.assertEqual(before_mcp, project.joinpath(".mcp.json").read_bytes())
            self.assertFalse(project.joinpath(".chaos-engine-hosts.json").exists())
            self.assertFalse(project.joinpath(".agents/skills/chaos-engine/SKILL.md").exists())

    def test_core_uninstall_collision_is_preflighted_before_runtime_removal(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            (project / MODULE.UNINSTALL_ARCHIVE_NAME).write_text("mine", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "archive"):
                MODULE.uninstall_with_dependencies(project)

            self.assertTrue(project.joinpath(".chaos-engine").exists())
            self.assertTrue(project.joinpath(".chaos-engine-runtime").exists())

    def test_uninstall_never_executes_a_drifted_dependency_controller(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            sentinel = project / "executed.txt"
            controller = installed / "dependencies.py"
            controller.write_text(
                controller.read_text(encoding="utf-8")
                + f"\nfrom pathlib import Path\nPath({str(sentinel)!r}).write_text('executed')\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                MODULE.uninstall_with_dependencies(project)

            self.assertFalse(sentinel.exists())

    def test_late_core_uninstall_failure_restores_prepared_runtime(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            with mock.patch.object(MODULE, "uninstall", side_effect=RuntimeError("changed")):
                with self.assertRaisesRegex(RuntimeError, "changed"):
                    MODULE.uninstall_with_dependencies(project)

            self.assertTrue(project.joinpath(".chaos-engine").exists())
            self.assertTrue(project.joinpath(".chaos-engine-runtime").exists())
            self.assertFalse(project.joinpath(".chaos-engine-runtime.removing").exists())

    def test_dependency_cancel_failure_does_not_skip_host_cancel(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            real_controller = MODULE.load_dependency_controller(project / ".chaos-engine")
            real_controller.cancel_remove = mock.Mock(side_effect=RuntimeError("cancel failed"))
            with mock.patch.object(MODULE, "load_dependency_controller", return_value=real_controller):
                with mock.patch.object(MODULE, "uninstall", side_effect=RuntimeError("core failed")):
                    with self.assertRaises(Exception):
                        MODULE.uninstall_with_dependencies(project)

            host = MODULE.load_installed_controller(project / ".chaos-engine", "hosts")
            self.assertEqual("healthy", host.verify(project, core_commit=TEST_COMMIT)["status"])

    def test_absent_core_retry_finishes_owned_runtime_and_tombstone(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            specification = dependency_module.load_specification(SOURCE / "dependencies.json")
            runtime = project / ".chaos-engine-runtime"
            dependency_module.repair(
                runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
            )
            removing = project / ".chaos-engine-runtime.removing"
            shutil.copytree(runtime, removing)

            MODULE.uninstall_with_dependencies(project)

            self.assertFalse(runtime.exists())
            self.assertFalse(removing.exists())

    def test_absent_core_retry_respects_the_dependency_runtime_lock(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            specification = dependency_module.load_specification(SOURCE / "dependencies.json")
            runtime = project / ".chaos-engine-runtime"
            dependency_module.repair(
                runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
            )

            with dependency_module.runtime_lock(runtime):
                with self.assertRaisesRegex(RuntimeError, "already running"):
                    MODULE.uninstall_with_dependencies(project)

            self.assertTrue(runtime.exists())

    def test_absent_core_invalid_journal_fails_before_runtime_removal(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            specification = dependency_module.load_specification(SOURCE / "dependencies.json")
            runtime = project / ".chaos-engine-runtime"
            dependency_module.repair(
                runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
            )
            (project / MODULE.JOURNAL_NAME).write_text("{invalid", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "journal"):
                MODULE.uninstall_with_dependencies(project)

            self.assertTrue(runtime.exists())

    def test_absent_core_with_unprepared_hosts_fails_before_runtime_removal(self):
        dependency_module = load_module(SOURCE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            runtime = project / ".chaos-engine-runtime"

            def provision(path, specification):
                return dependency_module.repair(
                    path,
                    specification,
                    runner=ChaosEngineDependenciesRunner(path),
                )

            MODULE.install_with_dependencies(project, SOURCE, TEST_COMMIT, provisioner=provision)
            shutil.rmtree(project / ".chaos-engine")

            with self.assertRaisesRegex(ValueError, "host removal"):
                MODULE.uninstall_with_dependencies(project)

            self.assertTrue(runtime.exists())
            self.assertTrue(project.joinpath(".chaos-engine-hosts.json").exists())

    def test_source_and_project_trees_must_be_disjoint(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = copy_source(root / "source")
            nested_project = source / "consumer"
            nested_project.mkdir()
            with self.assertRaisesRegex(ValueError, "disjoint"):
                MODULE.install(nested_project, source, TEST_COMMIT)
            with self.assertRaisesRegex(ValueError, "disjoint"):
                MODULE.install(source, source, TEST_COMMIT)
            outer_project = root / "outer-project"
            outer_project.mkdir()
            nested_source = copy_source(outer_project / "portable-source")
            with self.assertRaisesRegex(ValueError, "disjoint"):
                MODULE.install(outer_project, nested_source, TEST_COMMIT)

    def test_status_and_same_commit_install_are_read_only(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "مشروع consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(project)

            status = MODULE.status(project)
            same = MODULE.install(project, SOURCE, TEST_COMMIT)

            self.assertEqual("healthy", status["status"])
            self.assertEqual(TEST_COMMIT, status["commit"])
            self.assertEqual(installed, same)
            self.assertEqual(before, tree_digest(project))
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())

    def test_update_and_rollback_swap_only_verified_trees(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            old = MODULE.install(project, source, TEST_COMMIT)
            old_digest = tree_digest(old)
            source.joinpath("profiles/README.md").write_text("updated\n", encoding="utf-8")

            MODULE.install(project, source, "2" * 40)
            new_digest = tree_digest(project / ".chaos-engine")
            self.assertNotEqual(old_digest, new_digest)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])

            MODULE.rollback(project)
            self.assertEqual(old_digest, tree_digest(project / ".chaos-engine"))
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_content_drift_repairs_on_update_and_still_rejects_uninstall(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            installed = MODULE.install(project, source, TEST_COMMIT)
            installed.joinpath("profiles/README.md").write_text("user edit\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "drift"):
                MODULE.uninstall(project)

            MODULE.install(project, source, "2" * 40)

            repaired = project / ".chaos-engine"
            self.assertNotEqual("user edit\n", (repaired / "profiles/README.md").read_text(encoding="utf-8"))
            MODULE.verify_install(repaired)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())
            self.assertTrue(installed.exists())

    def test_crlf_drifted_install_upgrades_to_clean_payload(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            installed = MODULE.install(project, source, TEST_COMMIT)
            host_token = json.loads((installed / "manifest.json").read_text(encoding="utf-8"))["hostToken"]
            for path in installed.rglob("*"):
                if path.is_file():
                    data = path.read_bytes()
                    path.write_bytes(data.replace(b"\n", b"\r\n"))

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                MODULE.verify_install(installed)

            MODULE.install(project, source, "2" * 40)

            repaired = project / ".chaos-engine"
            MODULE.verify_install(repaired)
            manifest = json.loads((repaired / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(host_token, manifest["hostToken"])
            self.assertEqual("2" * 40, manifest["source"]["commit"])
            self.assertNotIn(b"\r\n", (repaired / "profiles/README.md").read_bytes())
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())

    def test_generated_python_cache_is_not_ownership_drift(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            cache = installed / "__pycache__"
            cache.mkdir()
            (cache / "hosts.cpython-314.pyc").write_bytes(b"generated")
            before = tree_digest(project)

            MODULE.verify_install(installed)
            same = MODULE.install(project, SOURCE, TEST_COMMIT)

            self.assertEqual(installed, same)
            self.assertEqual(before, tree_digest(project))
            self.assertTrue((cache / "hosts.cpython-314.pyc").is_file())

    def test_repair_install_never_executes_a_drifted_controller(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            sentinel = project / "executed.txt"
            for name in ("hosts.py", "dependencies.py"):
                controller = installed / name
                controller.write_text(
                    controller.read_text(encoding="utf-8")
                    + f"\nfrom pathlib import Path\nPath({str(sentinel)!r}).write_text('executed')\n",
                    encoding="utf-8",
                )

            MODULE.install_with_dependencies(
                project, SOURCE, "2" * 40, provisioner=lambda *_args, **_kwargs: None
            )

            self.assertFalse(sentinel.exists())
            MODULE.verify_install(project / ".chaos-engine")
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())

    def test_invalid_manifest_tree_is_replaced_on_install(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            leftover = project / ".chaos-engine"
            leftover.mkdir()
            leftover.joinpath("leftover.txt").write_text("junk\n", encoding="utf-8")

            MODULE.install(project, SOURCE, TEST_COMMIT)

            repaired = project / ".chaos-engine"
            self.assertFalse((repaired / "leftover.txt").exists())
            MODULE.verify_install(repaired)
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())

    def test_content_drift_repair_ignores_link_in_the_project_path(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "linkage-app"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            installed.joinpath("profiles/README.md").write_text("user edit\n", encoding="utf-8")

            MODULE.install(project, SOURCE, "2" * 40)

            MODULE.verify_install(project / ".chaos-engine")
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())

    def test_rollback_never_executes_a_drifted_controller(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            sentinel = project / "executed.txt"
            controller = installed / "hosts.py"
            controller.write_text(
                controller.read_text(encoding="utf-8")
                + f"\nfrom pathlib import Path\nPath({str(sentinel)!r}).write_text('executed')\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "ownership drift"):
                MODULE.rollback(project)

            self.assertFalse(sentinel.exists())
            self.assertTrue(installed.exists())

    def test_failed_repair_publish_is_retryable(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            installed = MODULE.install(project, source, TEST_COMMIT)
            installed.joinpath("profiles/README.md").write_text("user edit\n", encoding="utf-8")
            source.joinpath("profiles/README.md").write_text("updated\n", encoding="utf-8")
            real_replace = Path.replace

            def fail_stage_publish(path, destination):
                if path.name.startswith(".chaos-engine-stage-") and destination == installed:
                    raise OSError("injected")
                return real_replace(path, destination)

            with mock.patch.object(Path, "replace", autospec=True, side_effect=fail_stage_publish):
                with self.assertRaisesRegex(OSError, "injected"):
                    MODULE.install(project, source, "2" * 40)

            MODULE.install(project, source, "2" * 40)

            repaired = project / ".chaos-engine"
            MODULE.verify_install(repaired)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine.transaction.json").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup.next").exists())

    def test_update_recovery_discards_a_drifted_displaced_tree(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            other = root / "other"
            project.mkdir()
            other.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            MODULE.install(other, SOURCE, TEST_COMMIT)
            drifted = other / ".chaos-engine"
            drifted.joinpath("profiles/README.md").write_text("user edit\n", encoding="utf-8")
            MODULE.write_journal(project, "update", "2" * 40)
            drifted.replace(project / MODULE.NEXT_BACKUP_NAME)

            MODULE.recover_transaction(project)

            MODULE.verify_install(project / ".chaos-engine")
            self.assertFalse((project / MODULE.NEXT_BACKUP_NAME).exists())
            self.assertFalse((project / MODULE.BACKUP_NAME).exists())
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_update_recovery_restores_verified_displaced_over_drifted_target(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            other = root / "other"
            project.mkdir()
            other.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            MODULE.install(other, SOURCE, TEST_COMMIT)
            (project / ".chaos-engine" / "profiles/README.md").write_text(
                "user edit\n", encoding="utf-8"
            )
            MODULE.write_journal(project, "update", "2" * 40)
            (other / ".chaos-engine").replace(project / MODULE.NEXT_BACKUP_NAME)

            MODULE.recover_transaction(project)

            MODULE.verify_install(project / ".chaos-engine")
            self.assertFalse((project / MODULE.NEXT_BACKUP_NAME).exists())
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_failed_publish_restores_last_known_good(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            installed = MODULE.install(project, source, TEST_COMMIT)
            before = tree_digest(installed)
            source.joinpath("profiles/README.md").write_text("updated\n", encoding="utf-8")

            real_replace = Path.replace

            def fail_stage_publish(path, destination):
                if path.name.startswith(".chaos-engine-stage-") and destination == installed:
                    raise OSError("injected")
                return real_replace(path, destination)

            with mock.patch.object(Path, "replace", autospec=True, side_effect=fail_stage_publish):
                with self.assertRaisesRegex(OSError, "injected"):
                    MODULE.install(project, source, "2" * 40)

            self.assertEqual(before, tree_digest(installed))
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_exclusive_lock_rejects_a_concurrent_operation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            with MODULE.project_lock(project):
                with self.assertRaisesRegex(RuntimeError, "already running"):
                    with MODULE.project_lock(project):
                        self.fail("contended lock was acquired")

    def test_uninstall_removes_only_a_verified_install(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            keep = project / "keep.txt"
            keep.write_text("user\n", encoding="utf-8")
            MODULE.install(project, SOURCE, TEST_COMMIT)

            MODULE.uninstall(project)

            self.assertFalse(project.joinpath(".chaos-engine").exists())
            self.assertEqual("user\n", keep.read_text(encoding="utf-8"))

    def test_retry_recovers_a_crash_moved_tree_before_staging(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(installed)
            installed.replace(project / ".chaos-engine.backup")
            MODULE.write_journal(project, "update", "2" * 40)

            with mock.patch.object(MODULE.shutil, "copy2", side_effect=OSError("injected")):
                with self.assertRaisesRegex(OSError, "injected"):
                    MODULE.install(project, SOURCE, "2" * 40)

            self.assertEqual(before, tree_digest(project / ".chaos-engine"))

    def test_rollback_second_swap_failure_preserves_both_verified_trees(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve()
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("updated\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            real_replace = Path.replace
            def fail_second_swap(path, destination):
                if path == project / ".chaos-engine.backup" and destination == project / ".chaos-engine":
                    raise OSError("injected second swap")
                return real_replace(path, destination)

            with mock.patch.object(Path, "replace", autospec=True, side_effect=fail_second_swap):
                with self.assertRaisesRegex(OSError, "second swap"):
                    MODULE.rollback(project)

            MODULE.recover_transaction(project)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertTrue((project / ".chaos-engine.backup").exists())

    def test_uninstall_is_absent_noop_and_removes_recovery_after_success(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.uninstall(project)
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(installed)

            MODULE.uninstall(project)

            self.assertFalse(installed.exists())
            self.assertFalse(project.joinpath(".chaos-engine.backup").exists())
            self.assertTrue(before)

    def test_failed_third_update_preserves_the_existing_rollback_point(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("v2\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            rollback_before = tree_digest(project / ".chaos-engine.backup")
            source.joinpath("profiles/README.md").write_text("v3\n", encoding="utf-8")

            with mock.patch.object(MODULE, "publish_staged_tree", side_effect=OSError("injected")):
                with self.assertRaisesRegex(OSError, "injected"):
                    MODULE.install(project, source, "3" * 40)

            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertEqual(rollback_before, tree_digest(project / ".chaos-engine.backup"))

    def test_partial_obsolete_backup_cleanup_does_not_brick_recovery(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("v2\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            source.joinpath("profiles/README.md").write_text("v3\n", encoding="utf-8")
            real_rmtree = MODULE.shutil.rmtree

            def fail_obsolete(path, *args, **kwargs):
                if Path(path).name == ".chaos-engine.backup.old":
                    raise OSError("obsolete cleanup")
                return real_rmtree(path, *args, **kwargs)

            with mock.patch.object(MODULE.shutil, "rmtree", side_effect=fail_obsolete):
                with self.assertRaisesRegex(OSError, "obsolete cleanup"):
                    MODULE.install(project, source, "3" * 40)

            MODULE.recover_transaction(project)
            self.assertEqual("3" * 40, MODULE.status(project)["commit"])
            MODULE.rollback(project)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])

    def test_update_rejects_obsolete_backup_collision_before_publish(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("v2\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            collision = project / ".chaos-engine.backup.old"
            collision.mkdir()
            collision.joinpath("mine.txt").write_text("mine\n", encoding="utf-8")
            before = tree_digest(project / ".chaos-engine")

            with self.assertRaisesRegex(ValueError, "collision"):
                MODULE.install(project, source, "3" * 40)

            self.assertEqual(before, tree_digest(project / ".chaos-engine"))
            self.assertFalse(project.joinpath(".chaos-engine.transaction.json").exists())

    def test_scratch_collision_fails_before_journal_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            source = copy_source(Path(temporary) / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("updated\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            collision = project / ".chaos-engine-rollback"
            collision.mkdir()
            collision.joinpath("user.txt").write_text("mine\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "collision"):
                MODULE.rollback(project)

            self.assertFalse(project.joinpath(".chaos-engine.transaction.json").exists())
            self.assertEqual("mine\n", collision.joinpath("user.txt").read_text(encoding="utf-8"))

    def test_control_and_payload_symlinks_are_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            external = root / "external.txt"
            external.write_text("outside\n", encoding="utf-8")
            lock = project / ".chaos-engine.lock"
            try:
                lock.symlink_to(external)
            except OSError as error:
                self.skipTest(f"symlinks unavailable: {error}")
            with self.assertRaisesRegex(ValueError, "link or reparse"):
                MODULE.install(project, SOURCE, TEST_COMMIT)
            self.assertEqual("outside\n", external.read_text(encoding="utf-8"))
            lock.unlink()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            victim = installed / "profiles/README.md"
            victim.unlink()
            victim.symlink_to(external)
            with self.assertRaisesRegex(ValueError, "link or reparse"):
                MODULE.status(project)

    def test_unowned_regular_lock_file_is_rejected_without_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            lock = project / ".chaos-engine.lock"
            lock.write_bytes(b"")
            with self.assertRaisesRegex(ValueError, "lock collision"):
                MODULE.install(project, SOURCE, TEST_COMMIT)
            self.assertEqual(b"", lock.read_bytes())

    def test_status_reports_an_outstanding_transaction(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            MODULE.write_journal(project, "update", "2" * 40)
            self.assertEqual("recovery-required", MODULE.status(project)["status"])

    def test_failed_uninstall_restore_keeps_archive_for_later_recovery(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            real_rmtree = MODULE.shutil.rmtree

            def partial_delete(path, *args, **kwargs):
                if Path(path).name == ".chaos-engine-uninstall-current":
                    victim = Path(path) / "profiles/README.md"
                    victim.unlink(missing_ok=True)
                    raise OSError("partial delete")
                return real_rmtree(path, *args, **kwargs)

            with mock.patch.object(MODULE.shutil, "rmtree", side_effect=partial_delete), mock.patch.object(
                MODULE, "restore_archive", side_effect=OSError("restore failed")
            ):
                with self.assertRaisesRegex(OSError, "restore failed"):
                    MODULE.uninstall(project)

            self.assertTrue(project.joinpath(".chaos-engine-uninstall-recovery.zip").exists())
            MODULE.recover_transaction(project)
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])

    def test_partial_archive_creation_recovers_from_verified_removed_tree(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            with mock.patch.object(MODULE.zipfile.ZipFile, "write", side_effect=OSError("zip failed")):
                with self.assertRaisesRegex(OSError, "zip failed"):
                    MODULE.uninstall(project)
            MODULE.recover_transaction(project)
            self.assertEqual(TEST_COMMIT, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine-uninstall-recovery.zip").exists())

    def test_uninstall_archive_survives_obsolete_backup_cleanup_failure(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            MODULE.install(project, source, TEST_COMMIT)
            source.joinpath("profiles/README.md").write_text("v2\n", encoding="utf-8")
            MODULE.install(project, source, "2" * 40)
            real_rmtree = MODULE.shutil.rmtree

            def fail_old(path, *args, **kwargs):
                if Path(path).name == ".chaos-engine-uninstall-old-backup":
                    raise OSError("old cleanup")
                return real_rmtree(path, *args, **kwargs)

            with mock.patch.object(MODULE.shutil, "rmtree", side_effect=fail_old):
                with self.assertRaisesRegex(OSError, "old cleanup"):
                    MODULE.uninstall(project)
            self.assertTrue(project.joinpath(".chaos-engine-uninstall-recovery.zip").exists())
            MODULE.recover_transaction(project)
            self.assertEqual("2" * 40, MODULE.status(project)["commit"])
            self.assertFalse(project.joinpath(".chaos-engine-uninstall-old-backup").exists())
            MODULE.uninstall(project)
            self.assertFalse(project.joinpath(".chaos-engine").exists())

    def test_uninstall_archive_collision_fails_before_tree_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(installed)
            collision = project / ".chaos-engine-uninstall-recovery.zip"
            collision.write_text("mine\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "collision"):
                MODULE.uninstall(project)
            self.assertEqual(before, tree_digest(installed))
            self.assertEqual("mine\n", collision.read_text(encoding="utf-8"))

    def test_uninstall_archive_scratch_collision_fails_before_tree_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = tree_digest(installed)
            collision = project / ".chaos-engine-uninstall-recovery.zip.tmp"
            collision.write_text("mine\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "collision"):
                MODULE.uninstall(project)
            self.assertEqual(before, tree_digest(installed))
            self.assertFalse(project.joinpath(".chaos-engine.transaction.json").exists())
            self.assertFalse(project.joinpath(".chaos-engine-uninstall-current").exists())
            self.assertEqual("mine\n", collision.read_text(encoding="utf-8"))

    def test_installer_tests_are_reached_by_pull_request_gate(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        self.assertIn(
            "tests/scripts/test_chaos_engine_installer.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("python scripts/ci/harness_pr_gate.py", workflow)
        gate = (ROOT / "scripts/ci/harness_pr_gate.py").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_installer", gate)


if __name__ == "__main__":
    unittest.main()
