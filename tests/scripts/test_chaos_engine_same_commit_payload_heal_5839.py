"""Same-commit payload drift rematerializes instead of fail-closing (#5839)."""

from __future__ import annotations

import importlib.util
import json
import shutil
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/install.py"
SOURCE = ROOT / "chaos-engine"
TEST_COMMIT = "a" * 40

SPEC = importlib.util.spec_from_file_location("chaos_engine_installer_5839", INSTALLER)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("installer test module could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def copy_source(destination: Path) -> Path:
    ignored = shutil.ignore_patterns("__pycache__", "*.pyc")
    return Path(shutil.copytree(SOURCE, destination, ignore=ignored))


class SameCommitPayloadHeal5839Test(unittest.TestCase):
    def test_same_commit_payload_drift_rematerializes_preserving_host_token(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "consumer"
            project.mkdir()
            source_a = copy_source(root / "source-a")
            source_b = copy_source(root / "source-b")
            record = {
                "kind": "git",
                "commit": TEST_COMMIT,
                "repository": "shafthq/shaft_engine",
                "branch": "main",
            }
            installed = MODULE.install(
                project,
                source_a,
                TEST_COMMIT,
                source_record=record,
                distribution="repository",
            )
            manifest = json.loads((installed / "manifest.json").read_text(encoding="utf-8"))
            host_token = manifest["hostToken"]

            # Simulate overlay_match rewriting owned digests while keeping commit.
            identity = installed / "identity.md"
            identity.write_text(identity.read_text(encoding="utf-8") + "\n# overlay-drift\n", encoding="utf-8")
            rewritten = json.loads((installed / "manifest.json").read_text(encoding="utf-8"))
            rewritten["files"] = MODULE.installed_payload(installed)
            (installed / "manifest.json").write_text(
                json.dumps(rewritten, indent=2, sort_keys=True) + "\n",
                encoding="utf-8",
            )
            MODULE.verify_install(installed)

            # Distinct bytes at the same SOURCE commit (network vs overlay).
            drifted = source_b / "identity.md"
            drifted.write_text(drifted.read_text(encoding="utf-8") + "\n# resolved-source\n", encoding="utf-8")

            healed = MODULE.install(
                project,
                source_b,
                TEST_COMMIT,
                source_record=record,
                distribution="repository",
            )
            MODULE.verify_install(healed)
            after = json.loads((healed / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(host_token, after["hostToken"])
            self.assertEqual(TEST_COMMIT, after["source"]["commit"])
            self.assertEqual(
                MODULE.file_sha256(source_b / "identity.md"),
                after["files"]["identity.md"],
            )
            self.assertIn("# resolved-source", (healed / "identity.md").read_text(encoding="utf-8"))

    def test_identical_same_commit_install_remains_noop(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "consumer"
            project.mkdir()
            installed = MODULE.install(project, SOURCE, TEST_COMMIT)
            before = (installed / "manifest.json").read_bytes()
            again = MODULE.install(project, SOURCE, TEST_COMMIT)
            self.assertEqual(installed, again)
            self.assertEqual(before, (again / "manifest.json").read_bytes())


if __name__ == "__main__":
    unittest.main()
