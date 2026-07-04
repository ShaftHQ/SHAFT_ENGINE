import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_documentation_boundaries import (
    DOCS_BASE,
    validate_repository,
)


class ValidateDocumentationBoundariesTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        routes = (
            "start/overview",
            "start/installation",
            "start/upgrade",
            "testing/web",
            "testing/mobile",
            "testing/api",
            "agentic/mcp",
            "agentic/doctor",
            "agentic/heal",
        )
        self.write("README.md", "\n".join(f"{DOCS_BASE}{route}" for route in routes))

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def test_allows_workflow_readme(self):
        self.write(".github/workflows/README.md", "# GitHub Actions Workflows\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_repository_map_readme(self):
        self.write("tools/repository-map/README.md", "# Repository Map\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_internal_tool_markdown(self):
        self.write("tools/repository-map/details/usage.md", "# Usage\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_codex_internal_markdown(self):
        self.write(".codex/tools/graphify.md", "# Graphify\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_claude_project_skill(self):
        self.write(".claude/skills/graphify/SKILL.md", "# Graphify\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_installable_shaft_skills(self):
        self.write("shaft-skills/evaluation-prompts.md", "# Evaluation Prompts\n")
        self.write("shaft-skills/writing-shaft-tests/SKILL.md", "# Writing SHAFT Tests\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_rejects_unapproved_nested_readme(self):
        self.write(".github/other/README.md", "# Other\n")

        self.assertIn(
            "non-root README is prohibited: .github/other/README.md",
            validate_repository(self.root),
        )


if __name__ == "__main__":
    unittest.main()
