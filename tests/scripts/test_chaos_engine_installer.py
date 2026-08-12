"""Transactional standalone ChaosEngine installer tests (#4793)."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/install.py"
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "1" * 40

SPEC = importlib.util.spec_from_file_location("chaos_engine_installer", INSTALLER)
assert SPEC and SPEC.loader
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


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
    def test_clean_install_is_complete_and_manifest_bound(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer project"
            project.mkdir()

            completed = subprocess.run(
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
                for path in SOURCE.rglob("*")
                if path.is_file()
                and "__pycache__" not in path.relative_to(SOURCE).parts
                and path.suffix != ".pyc"
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
