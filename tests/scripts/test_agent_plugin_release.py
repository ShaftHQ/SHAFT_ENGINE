"""Release-contract coverage for portable Agent Plugin packages (#4576)."""

import json
import hashlib
import tempfile
import unittest
import zipfile
from pathlib import Path

try:
    from scripts.ci.agent_plugin_release import build_release_artifacts, load_release_manifest
except ModuleNotFoundError:
    build_release_artifacts = None
    load_release_manifest = None


class AgentPluginReleaseTest(unittest.TestCase):
    def write_manifest(self, document: dict) -> Path:
        temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(temporary_directory.cleanup)
        root = Path(temporary_directory.name)
        target = root / "agent-plugins/release.json"
        target.parent.mkdir(parents=True)
        target.write_text(json.dumps(document), encoding="utf-8")
        return root

    def test_release_manifest_rejects_missing_package(self):
        self.assertTrue(callable(load_release_manifest), "release manifest loader must be available")
        root = self.write_manifest(
            {"packages": [{"name": "shaft-skills", "version": "1.0.0"}]}
        )

        with self.assertRaisesRegex(ValueError, "declare every package"):
            load_release_manifest(root)

    def test_release_manifest_rejects_non_stable_semver(self):
        self.assertTrue(callable(load_release_manifest), "release manifest loader must be available")
        root = self.write_manifest(
            {
                "packages": [
                    {"name": "act-as-mohab", "version": "preview"},
                    {"name": "shaft-skills", "version": "1.0.0"},
                ]
            }
        )

        with self.assertRaisesRegex(ValueError, "stable SemVer"):
            load_release_manifest(root)

    def test_build_release_artifacts_is_deterministic(self):
        self.assertTrue(callable(build_release_artifacts), "release artifact builder must be available")
        repository_root = Path(__file__).resolve().parents[2]
        with tempfile.TemporaryDirectory() as temporary_directory:
            first_output = Path(temporary_directory) / "first"
            second_output = Path(temporary_directory) / "second"

            first = build_release_artifacts(repository_root, first_output)
            second = build_release_artifacts(repository_root, second_output)

            self.assertEqual([path.name for path in first], [path.name for path in second])
            self.assertEqual(
                ["act-as-mohab-1.0.0.zip", "act-as-mohab-1.0.0.zip.sha256", "shaft-skills-1.0.0.zip", "shaft-skills-1.0.0.zip.sha256"],
                [path.name for path in first],
            )
            for first_path, second_path in zip(first, second):
                self.assertEqual(first_path.read_bytes(), second_path.read_bytes(), first_path.name)
            for archive in (first_output / "act-as-mohab-1.0.0.zip", first_output / "shaft-skills-1.0.0.zip"):
                checksum = archive.with_suffix(archive.suffix + ".sha256").read_text(encoding="utf-8")
                self.assertEqual(checksum, f"{hashlib.sha256(archive.read_bytes()).hexdigest()}  {archive.name}\n")
                with zipfile.ZipFile(archive) as package:
                    self.assertIn("LICENSE", package.namelist())
                    self.assertIn("CHANGELOG.md", package.namelist())
                    self.assertIn("COMPATIBILITY.md", package.namelist())


if __name__ == "__main__":
    unittest.main()
