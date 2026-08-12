"""Transactional standalone ChaosEngine installer tests (#4793)."""

from __future__ import annotations

import hashlib
import importlib.util
import json
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


if __name__ == "__main__":
    unittest.main()
