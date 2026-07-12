import importlib.util
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "ci" / "validate_shaft_mcp_configuration.py"
SPEC = importlib.util.spec_from_file_location("validate_shaft_mcp_configuration", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
SPEC.loader.exec_module(MODULE)


class ValidateShaftMcpConfigurationTest(unittest.TestCase):
    def test_tool_manifest_metadata_errors_are_reported(self):
        errors = MODULE.validate_tool_manifest({
            "schemaVersion": "2.0",
            "tools": [
                {"name": "driver_initialize", "mutation": True, "sensitive": False, "deprecated": False},
                {"name": "driver_initialize", "mutation": True, "sensitive": False, "deprecated": False},
                {"name": "browser_get_page_dom", "mutation": False, "sensitive": "yes"},
                {"mutation": False, "sensitive": False, "deprecated": False},
            ],
        })

        self.assertIn("MCP tool manifest schemaVersion must remain 1.0", errors)
        self.assertIn("MCP tool manifest contains duplicate tool: driver_initialize", errors)
        self.assertIn("MCP tool manifest tool browser_get_page_dom must define boolean sensitive", errors)
        self.assertIn("MCP tool manifest tool browser_get_page_dom must define boolean deprecated", errors)
        self.assertIn("MCP tool manifest contains a tool without a name", errors)

    def test_selector_bound_ignores_yaml_comments(self):
        workflow = (
            "# leading comment\n"
            "jobs:\n"
            "  mcp-tests:\n"
            "    steps:\n"
            "      # another comment with extra # characters ##\n"
            "      - env:\n"
            "          MCP_RELATED_TESTS: ATest#one,ATest#two,BTest#three\n"
        )

        self.assertEqual(MODULE.validate_mcp_test_selector(workflow), [])

    def test_selector_bound_rejects_more_than_ten_methods(self):
        selector = ",".join(f"ATest#method{index}" for index in range(11))
        workflow = f"          MCP_RELATED_TESTS: {selector}\n"

        errors = MODULE.validate_mcp_test_selector(workflow)

        self.assertEqual(
            errors,
            ["shaft-mcp workflow MCP test selector must stay bounded to at most 10 selected test methods"],
        )

    def test_selector_bound_requires_explicit_method_entries(self):
        missing = MODULE.validate_mcp_test_selector("jobs: {}\n")
        class_only = MODULE.validate_mcp_test_selector("MCP_RELATED_TESTS: ATest,BTest\n")

        self.assertEqual(
            missing,
            ["shaft-mcp workflow must define a single-line MCP_RELATED_TESTS selector"],
        )
        self.assertEqual(
            class_only,
            ["shaft-mcp workflow MCP test selector must select explicit Class#method entries"],
        )


if __name__ == "__main__":
    unittest.main()
