"""Release-contract coverage for portable Agent Plugin packages (#4576)."""

import json
import hashlib
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest import mock

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
                checksum = archive.with_suffix(archive.suffix + ".sha256").read_bytes()
                self.assertEqual(
                    checksum,
                    f"{hashlib.sha256(archive.read_bytes()).hexdigest()}  {archive.name}\n".encode("utf-8"),
                )
                with zipfile.ZipFile(archive) as package:
                    self.assertEqual(package.namelist(), sorted(package.namelist()))
                    self.assertIn("LICENSE", package.namelist())
                    self.assertIn("CHANGELOG.md", package.namelist())
                    self.assertIn("COMPATIBILITY.md", package.namelist())
                    self.assertTrue(all(item.create_system == 3 for item in package.infolist()))
                    self.assertTrue(all(item.compress_type == zipfile.ZIP_STORED for item in package.infolist()))
                    for item in package.infolist():
                        if item.filename.endswith((".md", ".json", ".yaml", ".yml", ".LICENSE")) or item.filename == "LICENSE":
                            self.assertNotIn(b"\r\n", package.read(item))

    def test_failed_package_validation_leaves_no_partial_release_assets(self):
        self.assertTrue(callable(build_release_artifacts), "release artifact builder must be available")
        repository_root = Path(__file__).resolve().parents[2]
        with tempfile.TemporaryDirectory() as temporary_directory:
            output = Path(temporary_directory) / "assets"
            findings = [{"path": "plugin.json", "message": "invalid test package"}]
            with mock.patch("scripts.ci.validate_agent_plugins.validate_package", side_effect=[[], findings]):
                with self.assertRaisesRegex(ValueError, "plugin.json: invalid test package"):
                    build_release_artifacts(repository_root, output)

            self.assertFalse(output.exists())

    def test_failed_final_promotion_leaves_no_partial_release_assets(self):
        self.assertTrue(callable(build_release_artifacts), "release artifact builder must be available")
        repository_root = Path(__file__).resolve().parents[2]
        with tempfile.TemporaryDirectory() as temporary_directory:
            output = Path(temporary_directory) / "assets"
            with mock.patch("scripts.ci.agent_plugin_release.os.replace", side_effect=OSError("test failure")):
                with self.assertRaisesRegex(OSError, "test failure"):
                    build_release_artifacts(repository_root, output)

            self.assertFalse(output.exists())

    def test_normal_release_workflow_builds_and_attaches_plugin_assets(self):
        workflow = (Path(__file__).resolve().parents[2] / ".github/workflows/mavenCentral_cd.yml").read_text(
            encoding="utf-8"
        )

        self.assertIn("- 'agent-plugins/**'", workflow)
        self.assertIn("python3 scripts/ci/agent_plugin_release.py agent-plugin-release-assets", workflow)
        self.assertIn("name: agent-plugin-release-assets", workflow)
        self.assertIn("actions/upload-artifact@v7", workflow)
        self.assertIn("actions/download-artifact@v8", workflow)
        self.assertIn("artifacts: /tmp/agent-plugin-release-assets/*", workflow)
        self.assertLess(
            workflow.index("- name: Build portable Agent Plugin release assets"),
            workflow.index("- name: Deploy to Maven Central"),
        )

    def test_reconciliation_rejects_a_mutable_source_ref_before_checkout(self):
        workflow = (
            Path(__file__).resolve().parents[2] / ".github/workflows/maven-central-reconcile.yml"
        ).read_text(encoding="utf-8")

        self.assertIn('description: "Full 40-character commit SHA containing the release source."', workflow)
        self.assertIn("- name: Validate immutable source commit", workflow)
        self.assertIn("SOURCE_COMMIT: ${{ inputs.source_ref }}", workflow)
        self.assertIn('[[ ! "${SOURCE_COMMIT}" =~ ^[0-9a-f]{40}$ ]]', workflow)
        validation = workflow[
            workflow.index("- name: Validate immutable source commit"):
            workflow.index("- name: Checkout Code")
        ]
        self.assertIn("exit 1", validation)
        self.assertNotIn("continue-on-error", validation)
        self.assertLess(
            workflow.index("- name: Validate immutable source commit"),
            workflow.index("- name: Checkout Code"),
        )
        checkout_verification = workflow[
            workflow.index("- name: Verify checked out source commit"):
            workflow.index("- name: Reclaim disk space")
        ]
        self.assertIn('ACTUAL_COMMIT="$(git rev-parse HEAD)"', checkout_verification)
        self.assertIn('[[ "${ACTUAL_COMMIT}" != "${SOURCE_COMMIT}" ]]', checkout_verification)
        self.assertIn("exit 1", checkout_verification)
        self.assertNotIn("continue-on-error", checkout_verification)


if __name__ == "__main__":
    unittest.main()
