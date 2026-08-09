"""Phase-0 Agent Plugin contract validator tests (#4576)."""

import json
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.validate_agent_plugins import validate_package
except ModuleNotFoundError:
    validate_package = None


SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"


class ValidateAgentPluginsTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name) / "fixture-plugin"
        self.root.mkdir()
        self.manifest = {"$schema": SCHEMA_URL, "name": "fixture-plugin"}
        self.write_manifest()

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def write_manifest(self):
        self.write("plugin.json", json.dumps(self.manifest))

    def codes(self):
        self.assertTrue(callable(validate_package), "validate_package must be available")
        return {issue["code"] for issue in validate_package(self.root)}

    def test_valid_minimal_package_passes(self):
        self.assertTrue(callable(validate_package), "validate_package must be available")
        self.assertEqual(validate_package(self.root), [])

    def test_rejects_invalid_required_manifest_fields(self):
        self.manifest = {"$schema": "https://example.invalid/schema.json", "name": "Not Valid"}
        self.write_manifest()

        self.assertEqual(
            self.codes(),
            {"plugin-schema", "plugin-name"},
        )

    def test_reports_unknown_manifest_fields_without_rejecting_the_package(self):
        self.manifest["future-field"] = {"kept": "for a newer host"}
        self.write_manifest()

        self.assertTrue(callable(validate_package), "validate_package must be available")
        issues = validate_package(self.root)

        self.assertEqual([issue["code"] for issue in issues], ["manifest-unknown-field"])
        self.assertEqual(issues[0]["severity"], "warning")

    def test_reports_invalid_skill_component_without_masking_valid_manifest(self):
        self.write("skills/broken/SKILL.md", "# Missing frontmatter\n")

        self.assertEqual(self.codes(), {"skill-frontmatter"})

    def test_rejects_a_skills_directory_that_resolves_outside_the_package(self):
        outside = self.root.parent / "outside"
        outside.mkdir()
        try:
            (self.root / "skills").symlink_to(outside, target_is_directory=True)
        except OSError as error:  # unprivileged Windows runner
            self.skipTest(f"symlinks unavailable: {error}")

        self.assertEqual(self.codes(), {"component-escapes-root"})


if __name__ == "__main__":
    unittest.main()
