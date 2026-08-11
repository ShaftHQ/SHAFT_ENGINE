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
            "start/quick-start",
            "start/installation",
            "start/upgrade",
            "testing/web",
            "testing/mobile",
            "testing/api",
            "agentic/mcp",
            "agentic/skills",
            "agentic/doctor",
            "agentic/heal",
        )
        links = [
            *(f"[{route}]({DOCS_BASE}{route})" for route in routes),
            "[Sponsor SHAFT](https://github.com/sponsors/MohabMohie)",
        ]
        self.write("README.md", "\n".join(links))
        self.write(
            "modular-era-feature-catalog.md",
            "[Release history](https://github.com/ShaftHQ/SHAFT_ENGINE/releases)",
        )

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

    def test_allows_canonical_project_skill(self):
        self.write(".agents/skills/act-as-mohab/SKILL.md", "# Act as Mohab\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_installable_shaft_skills(self):
        self.write("shaft-skills/evaluation-prompts.md", "# Evaluation Prompts\n")
        self.write("shaft-skills/writing-shaft-tests/SKILL.md", "# Writing SHAFT Tests\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_markdown_inside_claude_worktrees(self):
        self.write(
            ".claude/worktrees/agent-a/docs/anything.md",
            "# Concurrent agent worktree content\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_markdown_inside_memory_recovery(self):
        self.write(
            ".memory/recovery/2026-01-01T00-00-00/memory/gotchas/example.md",
            "# Memory recovery gotcha\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_rejects_unapproved_nested_readme(self):
        self.write(".github/other/README.md", "# Other\n")

        self.assertIn(
            "non-root README is prohibited: .github/other/README.md",
            validate_repository(self.root),
        )

    def test_requires_growth_landing_routes_and_sponsor_cta(self):
        legacy_routes = (
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
        self.write("README.md", "\n".join(f"{DOCS_BASE}{route}" for route in legacy_routes))

        errors = validate_repository(self.root)

        self.assertIn("README.md is missing canonical route: start/quick-start", errors)
        self.assertIn("README.md is missing canonical route: agentic/skills", errors)
        self.assertIn("README.md is missing the GitHub Sponsors call to action", errors)

    def test_rejects_bare_route_and_sponsor_urls_that_are_not_links(self):
        routes = (
            "start/overview",
            "start/quick-start",
            "start/installation",
            "start/upgrade",
            "testing/web",
            "testing/mobile",
            "testing/api",
            "agentic/mcp",
            "agentic/skills",
            "agentic/doctor",
            "agentic/heal",
        )
        self.write(
            "README.md",
            "\n".join(
                [
                    *(f"[{route}]({DOCS_BASE}{route})" for route in routes[:-1]),
                    f"plain text: {DOCS_BASE}{routes[-1]}",
                    "plain text: https://github.com/sponsors/MohabMohie",
                ]
            ),
        )

        errors = validate_repository(self.root)

        self.assertIn("README.md is missing canonical route: agentic/heal", errors)
        self.assertIn("README.md is missing the GitHub Sponsors call to action", errors)

    def test_requires_catalog_release_history_link(self):
        self.write(
            "modular-era-feature-catalog.md",
            "plain text: https://github.com/ShaftHQ/SHAFT_ENGINE/releases",
        )

        self.assertIn(
            "modular-era-feature-catalog.md is missing the canonical release-history link",
            validate_repository(self.root),
        )


if __name__ == "__main__":
    unittest.main()
