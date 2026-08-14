"""Transactional standalone ChaosEngine installer tests (#4793)."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import shutil
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


class ChaosEngineInstallerTest(unittest.TestCase):
    def test_doctor_command_uses_the_full_status_contract(self):
        arguments = MODULE.parser().parse_args(["doctor", "--project", "."])

        self.assertEqual("doctor", arguments.command)

    def test_default_distribution_installs_only_neutral_portable_payload(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()

            MODULE.install(project, SOURCE, TEST_COMMIT)

            install_root = project / ".chaos-engine"
            manifest = json.loads((install_root / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertRegex(manifest["distribution"]["policySha256"], r"^[0-9a-f]{64}$")
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

    def test_legacy_manifest_is_verified_but_requires_explicit_reinstall(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            MODULE.install(project, SOURCE, TEST_COMMIT)
            manifest_path = project / ".chaos-engine/manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest.pop("distribution")
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            self.assertEqual("legacy", MODULE.status(project)["distribution"])
            before = tree_digest(project / ".chaos-engine")
            with self.assertRaisesRegex(ValueError, "uninstall before changing"):
                MODULE.install(project, SOURCE, "2" * 40)
            self.assertEqual(before, tree_digest(project / ".chaos-engine"))

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
            project = Path(temporary) / "consumer"
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

    def test_drift_rejects_update_and_uninstall(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source = copy_source(root / "source")
            installed = MODULE.install(project, source, TEST_COMMIT)
            installed.joinpath("profiles/README.md").write_text("user edit\n", encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "drift"):
                MODULE.install(project, source, "2" * 40)
            with self.assertRaisesRegex(ValueError, "drift"):
                MODULE.uninstall(project)

            self.assertTrue(installed.exists())

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
            root = Path(temporary)
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
        self.assertIn("tests.scripts.test_chaos_engine_installer", workflow)


if __name__ == "__main__":
    unittest.main()
