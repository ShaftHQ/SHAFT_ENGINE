"""Phase-0 Agent Plugin contract validator tests (#4576)."""

import json
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.validate_agent_plugins import resolves_inside, validate_package
except ModuleNotFoundError:
    resolves_inside = None
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

    def write_skill(self, name="valid-skill", line_ending="\n"):
        self.write(
            f"skills/{name}/SKILL.md",
            line_ending.join(
                [
                    "---",
                    f"name: {name}",
                    "description: A valid skill used when validating a portable package.",
                    "---",
                    "",
                    "# Instructions",
                ]
            ),
        )

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

    def test_rejects_invalid_manifest_field_types_but_ignores_non_object_extensions(self):
        self.manifest.update(
            {
                "version": 7,
                "description": ["not a string"],
                "author": {"name": 2},
                "keywords": "not an array",
                "extensions": "old client data",
            }
        )
        self.write_manifest()

        issues = validate_package(self.root)

        self.assertEqual(
            {issue["code"] for issue in issues},
            {"manifest-field", "extensions-invalid"},
        )
        self.assertEqual(
            [issue["severity"] for issue in issues if issue["code"] == "extensions-invalid"],
            ["warning"],
        )

    def test_rejects_each_invalid_permitted_manifest_field(self):
        invalid_fields = {
            "version": 7,
            "description": ["not a string"],
            "homepage": 7,
            "repository": 7,
            "license": 7,
            "author": {"unexpected": "field"},
            "keywords": ["valid", 7],
            "extensions": {"org.example": "not an object"},
        }
        for field, value in invalid_fields.items():
            with self.subTest(field=field):
                self.manifest = {"$schema": SCHEMA_URL, "name": "fixture-plugin", field: value}
                self.write_manifest()
                self.assertEqual(self.codes(), {"manifest-field"})

    def test_accepts_a_complete_valid_manifest(self):
        self.manifest.update(
            {
                "version": "1.0.0",
                "description": "A portable package fixture.",
                "author": {"name": "ShaftHQ", "email": "maintainers@example.test", "url": "https://example.test"},
                "homepage": "https://example.test",
                "repository": "https://github.com/ShaftHQ/SHAFT_ENGINE",
                "license": "MIT",
                "keywords": ["agent", "fixture"],
                "extensions": {"org.example": {"host-setting": "enabled"}},
            }
        )
        self.write_manifest()

        self.assertEqual(validate_package(self.root), [])

    def test_accepts_complete_crlf_skill_frontmatter(self):
        self.write_skill(line_ending="\r\n")

        self.assertEqual(validate_package(self.root), [])

    def test_rejects_unclosed_or_incomplete_skill_frontmatter(self):
        self.write("skills/broken/SKILL.md", "---\nname: broken\n")

        self.assertEqual(self.codes(), {"skill-frontmatter"})

    def test_rejects_skill_name_that_does_not_match_its_directory(self):
        self.write_skill(name="actual-name")
        self.write(
            "skills/actual-name/SKILL.md",
            "---\nname: different-name\ndescription: A valid description with the wrong name.\n---\n",
        )

        self.assertEqual(self.codes(), {"skill-name"})

    def test_rejects_each_invalid_optional_skill_field(self):
        invalid_fields = {
            "license": "license: []",
            "compatibility": "compatibility: []",
            "metadata": "metadata: []",
            "allowed-tools": "allowed-tools: []",
        }
        for field, line in invalid_fields.items():
            with self.subTest(field=field):
                self.write(
                    "skills/valid-skill/SKILL.md",
                    "---\n"
                    "name: valid-skill\n"
                    "description: A valid skill with one invalid optional field.\n"
                    f"{line}\n"
                    "---\n",
                )
                self.assertEqual(self.codes(), {"skill-field"})

    def test_accepts_valid_optional_skill_fields(self):
        self.write(
            "skills/valid-skill/SKILL.md",
            "---\n"
            "name: valid-skill\n"
            "description: A valid skill with optional metadata for testing.\n"
            "license: MIT\n"
            "compatibility: Requires Python.\n"
            "metadata:\n"
            "  author: ShaftHQ\n"
            "  version: \"1.0\"\n"
            "allowed-tools: Read Bash(git:*)\n"
            "---\n",
        )

        self.assertEqual(validate_package(self.root), [])

    def test_containment_predicate_rejects_a_lexical_escape_without_symlink_support(self):
        self.assertTrue(callable(resolves_inside), "resolves_inside must be available")

        self.assertFalse(resolves_inside(self.root, self.root / ".." / "outside"))

    def test_absent_skills_component_is_ignored(self):
        self.assertEqual(validate_package(self.root), [])

    def test_rejects_a_dangling_skills_link_outside_the_package(self):
        outside = self.root.parent / "missing-outside"
        try:
            (self.root / "skills").symlink_to(outside, target_is_directory=True)
        except OSError as error:  # unprivileged Windows runner
            self.skipTest(f"symlinks unavailable: {error}")

        self.assertEqual(self.codes(), {"component-escapes-root"})

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
