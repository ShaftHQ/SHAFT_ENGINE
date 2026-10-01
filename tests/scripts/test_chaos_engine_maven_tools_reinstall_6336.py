"""#6336/#6337/#6338/#6339: Maven Tools doctor, repair, and rollback truth after a reinstall.

After a reinstall on a SHAFT_ENGINE checkout the installer reused the newest
healthy cached Maven Tools JAR (3.2.2) while doctor only looked at a hardcoded
3.2.0 tree. Verification then failed, the bootstrap rolled the core back to the
previous commit without saying so, and a corrupt cached JAR pointed operators
at the merge handoff because `repair` could not target Maven Tools.
"""

from __future__ import annotations

import contextlib
import hashlib
import importlib.util
import io
import json
import os
import re
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path

from tests.scripts.test_chaos_engine_installer import (
    MODULE,
    REAL_REPAIR_COMPONENT,
    SOURCE,
    TEST_COMMIT,
)

ROOT = Path(__file__).resolve().parents[2]
OLD_COMMIT = "1" * 40
NEW_COMMIT = "2" * 40
UPSTREAM_COMMIT = "a" * 40
REPAIR = "install.py repair --project . --component maven-tools-mcp"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def publish(root: Path, version: str, *, corrupt: bool = False) -> Path:
    """Write one receipt-owned cache tree; ``corrupt`` flips JAR bytes after the receipt."""
    tree = root / version
    tree.mkdir(parents=True)
    jar = tree / f"maven-tools-mcp-{version}.jar"
    jar.write_bytes(b"jar-" + version.encode())
    receipt = {
        "version": version,
        "commit": UPSTREAM_COMMIT,
        "jar": jar.name,
        "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
    }
    (tree / "install-receipt.json").write_text(json.dumps(receipt), encoding="utf-8")
    if corrupt:
        jar.write_bytes(b"bit-rot: invalid CRC")
    return jar


class SelectedMavenToolsStatus6336Test(unittest.TestCase):
    """Doctor must judge the version the installer selects, not a pinned 3.2.0."""

    def setUp(self):
        self.hosts = _load("ce_hosts_6336", SOURCE / "hosts.py")
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name) / "cache"
        environment = mock.patch.dict(os.environ, {}, clear=False)
        environment.start()
        self.addCleanup(environment.stop)
        os.environ.pop("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR", None)

    def test_newest_healthy_cached_version_is_healthy_without_3_2_0(self):
        publish(self.root, "3.2.1")
        publish(self.root, "3.2.2")
        status = self.hosts.selected_maven_tools_cache_status(root=self.root)
        self.assertEqual("healthy", status["status"], status)
        self.assertEqual("3.2.2", status["version"])

    def test_corrupt_newer_tree_is_skipped_like_runtime_discovery(self):
        publish(self.root, "3.2.1")
        publish(self.root, "3.2.3", corrupt=True)
        status = self.hosts.selected_maven_tools_cache_status(root=self.root)
        self.assertEqual("healthy", status["status"], status)
        self.assertEqual("3.2.1", status["version"])

    def test_selection_order_is_shared_with_runtime_discovery(self):
        publish(self.root, "3.2.10")
        publish(self.root, "3.2.9")
        self.assertEqual(
            ["3.2.10", "3.2.9"], self.hosts.maven_tools_cached_versions(root=self.root)
        )

    def test_only_a_corrupt_tree_reports_invalid_with_the_receipt_reason(self):
        publish(self.root, "3.2.0", corrupt=True)
        status = self.hosts.selected_maven_tools_cache_status(root=self.root)
        self.assertEqual("invalid", status["status"], status)
        self.assertEqual("3.2.0", status["version"])
        self.assertIn("receipt validation failed", status["reason"])

    def test_empty_cache_is_absent(self):
        status = self.hosts.selected_maven_tools_cache_status(root=self.root)
        self.assertEqual("absent", status["status"], status)

    def test_configured_verified_jar_wins_like_runtime_discovery(self):
        elsewhere = Path(self.root.parent) / "elsewhere"
        jar = publish(elsewhere, "3.3.0")
        with mock.patch.dict(os.environ, {"CHAOSENGINE_MAVEN_TOOLS_MCP_JAR": str(jar)}):
            status = self.hosts.selected_maven_tools_cache_status(root=self.root)
        self.assertEqual("healthy", status["status"], status)
        self.assertEqual("3.3.0", status["version"])

    def test_doctor_row_has_no_hardcoded_version(self):
        source = (SOURCE / "install.py").read_text(encoding="utf-8")
        self.assertIn("selected_maven_tools_cache_status", source)
        self.assertNotIn(
            "cache_state = host_controller.maven_tools_cache_status()", source
        )


class DoctorRowUsesSelectedVersion6336Test(unittest.TestCase):
    def test_install_doctor_row_is_healthy_for_a_reused_newer_cache(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            data = root / "data"
            publish(data / "ChaosEngine/tools/maven-tools-mcp", "3.2.2")
            variable = "LOCALAPPDATA" if os.name == "nt" else "XDG_DATA_HOME"
            dependency_module = _load("ce_dependencies_6336", SOURCE / "dependencies.py")
            from tests.scripts.test_chaos_engine_installer import (
                ChaosEngineDependenciesRunner,
            )

            def provision(runtime, specification):
                return dependency_module.repair(
                    runtime, specification, runner=ChaosEngineDependenciesRunner(runtime)
                )

            with mock.patch.dict(os.environ, {variable: str(data)}, clear=False):
                os.environ.pop("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR", None)
                MODULE.install_with_dependencies(
                    project, SOURCE, TEST_COMMIT, provisioner=provision
                )
                result = MODULE.status_with_dependencies(project)
            row = result["components"]["maven-tools-mcp"]
            self.assertEqual("healthy", row["status"], row)
            self.assertEqual("3.2.2", row["version"])


class MavenToolsRepair6337Test(unittest.TestCase):
    def setUp(self):
        self.hosts = _load("ce_hosts_6337", SOURCE / "hosts.py")
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name) / "cache"
        self.target = Path(temporary.name) / "project/.chaos-engine"
        self.target.mkdir(parents=True)
        patcher = mock.patch.object(self.hosts, "maven_tools_cache_root", return_value=self.root)
        patcher.start()
        self.addCleanup(patcher.stop)
        environment = mock.patch.dict(os.environ, {}, clear=False)
        environment.start()
        self.addCleanup(environment.stop)
        os.environ.pop("CHAOSENGINE_MAVEN_TOOLS_MCP_JAR", None)

    def test_repair_component_is_offered_by_the_cli(self):
        self.assertIn("maven-tools-mcp", MODULE.REPAIRABLE_COMPONENTS)
        arguments = MODULE.parser().parse_args(
            ["repair", "--project", ".", "--component", "maven-tools-mcp"]
        )
        self.assertEqual("maven-tools-mcp", arguments.component)

    def test_repair_discards_the_corrupt_tree_and_reuses_a_healthy_version(self):
        publish(self.root, "3.2.0", corrupt=True)
        publish(self.root, "3.2.2")
        rebind = mock.Mock()
        with mock.patch.object(
            MODULE, "ensure_maven_tools", side_effect=AssertionError("must reuse")
        ):
            result = MODULE.repair_maven_tools(
                self.target.parent, self.target, self.hosts, rebind=rebind
            )
        self.assertEqual("repaired", result["status"], result)
        self.assertEqual("reused", result["action"])
        self.assertEqual(["3.2.0"], result["discarded"])
        self.assertEqual("3.2.2", result["version"])
        self.assertFalse((self.root / "3.2.0").exists())
        rebind.assert_called_once_with()

    def test_repair_reinstalls_when_no_healthy_version_remains(self):
        publish(self.root, "3.2.0", corrupt=True)

        def ensure(target, specification, **_kwargs):
            publish(self.root, "3.2.2")
            return (Path("java"), self.root / "3.2.2/maven-tools-mcp-3.2.2.jar")

        controller = mock.Mock()
        controller.load_specification.return_value = {"dependencies": {}}
        with mock.patch.object(MODULE, "ensure_maven_tools", side_effect=ensure), mock.patch.object(
            MODULE, "load_dependency_controller", return_value=controller
        ):
            result = MODULE.repair_maven_tools(
                self.target.parent, self.target, self.hosts, rebind=mock.Mock()
            )
        self.assertEqual("repaired", result["status"], result)
        self.assertEqual("reinstalled", result["action"])
        self.assertEqual(["3.2.0"], result["discarded"])
        self.assertEqual("3.2.2", result["version"])

    def test_repair_component_dispatches_to_the_maven_tools_repair(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            observed = {"status": "repaired", "component": "maven-tools-mcp", "action": "reused"}
            with mock.patch.object(MODULE, "repair_maven_tools", return_value=observed) as repair:
                result = REAL_REPAIR_COMPONENT(project, "maven-tools-mcp")
        self.assertEqual(observed, result)
        repair.assert_called_once()

    def test_corrupt_cache_row_names_the_repair_even_under_a_heal_handoff(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            state = project / ".chaos-engine-state"
            state.mkdir()
            for name in ("heal-handoff.md", "merge-handoff.md"):
                (state / name).write_text("# handoff\n", encoding="utf-8")
                components = {
                    "maven-tools-mcp": {
                        "status": "invalid",
                        "reason": "JAR receipt validation failed",
                        "taskImpact": "required",
                        "fixNext": MODULE.maven_tools_repair_fix_next(),
                    },
                    "hooks": {"status": "recovery-required", "taskImpact": "required"},
                }
                MODULE.apply_merge_handoff_fix_next(project, components)
                self.assertIn(REPAIR, components["maven-tools-mcp"]["fixNext"])
                self.assertIn(name, components["hooks"]["fixNext"])
                (state / name).unlink()

    def test_doctor_row_for_a_corrupt_cache_carries_the_repair_fix_next(self):
        source = (SOURCE / "install.py").read_text(encoding="utf-8")
        self.assertIn("maven_tools_repair_fix_next()", source)
        self.assertIn(REPAIR, MODULE.maven_tools_repair_fix_next())

    def test_install_failure_checksum_hint_names_the_repair(self):
        bootstrap = _load("ce_bootstrap_6337", SOURCE / "bootstrap.py")
        for cause in (
            "Maven Tools MCP JAR checksum mismatch",
            "Maven Tools MCP probe failed: invalid CRC in maven-tools-mcp-3.2.0.jar",
        ):
            hint = bootstrap.next_fix_hint(
                "CE-INSTALL-FAILED", ValueError(cause), "python3 .chaos-engine/install.py"
            )
            self.assertIn(REPAIR, hint, cause)

    def test_docs_list_every_repairable_component(self):
        expected = set(MODULE.REPAIRABLE_COMPONENTS)
        for relative in ("chaos-engine/INSTALL.md", "chaos-engine/references/heal-route.md"):
            text = (ROOT / relative).read_text(encoding="utf-8")
            match = re.search(r"(?:Supported components|Components):((?:\s*`[a-z0-9-]+`,?)+)", text)
            self.assertIsNotNone(match, relative)
            listed = set(re.findall(r"`([a-z0-9-]+)`", match.group(1)))
            self.assertEqual(expected, listed, relative)


class RollbackTruth6338Test(unittest.TestCase):
    def setUp(self):
        self.bootstrap = _load("ce_bootstrap_6338", SOURCE / "bootstrap.py")

    @staticmethod
    def manifest(tree: Path, commit: str) -> None:
        tree.mkdir(parents=True, exist_ok=True)
        (tree / "manifest.json").write_text(
            json.dumps({"source": {"commit": commit, "kind": "local"}}), encoding="utf-8"
        )

    def test_failed_verify_reports_the_rollback_and_the_restored_commit(self):
        module = self.bootstrap
        with tempfile.TemporaryDirectory() as temporary:
            project = (Path(temporary) / "project").resolve()
            project.mkdir()
            self.manifest(project / ".chaos-engine", NEW_COMMIT)
            self.manifest(project / ".chaos-engine.backup", OLD_COMMIT)
            installer = mock.Mock()
            installer.install_with_dependencies.return_value = project / ".chaos-engine"
            installer.doctor_with_dependencies.return_value = {
                "commit": NEW_COMMIT,
                "components": {
                    "maven-tools-mcp": {"status": "absent", "taskImpact": "required"}
                },
                "kernel": {"status": "healthy"},
                "hosts": {"status": "healthy"},
                "dependencies": {"status": "healthy"},
            }

            def rollback(_project):
                self.manifest(project / ".chaos-engine", OLD_COMMIT)
                self.manifest(project / ".chaos-engine.backup", NEW_COMMIT)

            installer.rollback.side_effect = rollback
            from tests.scripts.test_chaos_engine_bootstrap import ChaosEngineBootstrapTest

            opener, _ = ChaosEngineBootstrapTest.opener(self, [(NEW_COMMIT, "full")])
            with mock.patch.object(module, "load_installer", return_value=installer):
                with self.assertRaises(module.InstallHealthError) as raised:
                    module.install_latest(
                        project, repository="Example/Project", branch="main", opener=opener
                    )
            error = raised.exception
            self.assertEqual(OLD_COMMIT, error.rolled_back_to)
            self.assertEqual(NEW_COMMIT, error.rolled_back_from)
            marker = json.loads(
                (project / ".chaos-engine-state/install-rollback.json").read_text(
                    encoding="utf-8"
                )
            )
            self.assertEqual(
                {
                    "schemaVersion": 1,
                    "status": "rolled-back",
                    "requestedCommit": NEW_COMMIT,
                    "restoredCommit": OLD_COMMIT,
                },
                marker,
            )
            stderr = io.StringIO()
            with contextlib.redirect_stderr(stderr):
                module.emit_install_failure(
                    "CE-INSTALL-FAILED", error, "Example/Project", project=project
                )
            text = stderr.getvalue()
            self.assertIn("Rolled back", text)
            self.assertIn(NEW_COMMIT[:12], text)
            self.assertIn(OLD_COMMIT[:12], text)

    def test_status_and_doctor_name_the_installed_commit_and_the_rollback(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            state = project / ".chaos-engine-state"
            state.mkdir(exist_ok=True)
            marker = state / "install-rollback.json"
            marker.write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "status": "rolled-back",
                        "requestedCommit": NEW_COMMIT,
                        "restoredCommit": TEST_COMMIT,
                    }
                ),
                encoding="utf-8",
            )
            observed = MODULE.read_install_rollback(project, TEST_COMMIT)
            self.assertEqual(
                {"status": "rolled-back", "requestedCommit": NEW_COMMIT, "restoredCommit": TEST_COMMIT},
                observed,
            )
            status = MODULE.status(project)
            self.assertEqual(TEST_COMMIT, status["commit"])
            self.assertEqual(observed, status["lastInstall"])
            # A marker for a different installed core is stale and ignored.
            self.assertIsNone(MODULE.read_install_rollback(project, NEW_COMMIT))
            report = MODULE.format_health_report(
                {"status": "healthy", "commit": TEST_COMMIT, "lastInstall": observed,
                 "components": {}},
                kind="doctor",
            )
            self.assertIn("rolled back", report)
            self.assertIn(NEW_COMMIT[:12], report)
            self.assertIn("lastInstall", MODULE._DIAGNOSTIC_FIELDS["doctor"])
            self.assertIn("lastInstall", MODULE._DIAGNOSTIC_FIELDS["status"])
            self.assertIn("lastInstall", MODULE._DIAGNOSTIC_OPTIONAL_FIELDS["doctor"])

    def test_a_healthy_install_clears_the_rollback_marker(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            state = project / ".chaos-engine-state"
            state.mkdir()
            (state / "install-rollback.json").write_text("{}", encoding="utf-8")
            self.bootstrap.clear_stale_install_failure_artifacts(project)
            self.assertFalse((state / "install-rollback.json").exists())


class UpstreamSourceBuild6339Test(unittest.TestCase):
    def test_source_build_skips_upstream_tests(self):
        source = (SOURCE / "install.py").read_text(encoding="utf-8")
        self.assertIn('"-B", "clean", "package", "-Pci", "-DskipTests"', source)
        guide = (SOURCE / "INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("./mvnw -B clean package -Pci -DskipTests", guide)


if __name__ == "__main__":
    unittest.main()
