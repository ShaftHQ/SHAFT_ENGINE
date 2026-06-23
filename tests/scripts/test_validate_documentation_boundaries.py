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

    def test_rejects_unapproved_nested_readme(self):
        self.write(".github/other/README.md", "# Other\n")

        self.assertIn(
            "non-root README is prohibited: .github/other/README.md",
            validate_repository(self.root),
        )


if __name__ == "__main__":
    unittest.main()
