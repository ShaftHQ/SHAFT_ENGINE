"""Release-contract coverage for portable Agent Plugin packages (#4576)."""

import json
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.agent_plugin_release import load_release_manifest
except ModuleNotFoundError:
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

    def test_release_manifest_rejects_missing_package_and_non_stable_semver(self):
        self.assertTrue(callable(load_release_manifest), "release manifest loader must be available")
        root = self.write_manifest(
            {"packages": [{"name": "shaft-skills", "version": "preview"}]}
        )

        with self.assertRaisesRegex(ValueError, "stable SemVer"):
            load_release_manifest(root)


if __name__ == "__main__":
    unittest.main()
