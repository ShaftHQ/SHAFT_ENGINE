import contextlib
import importlib.util
import io
import sys
import tempfile
import unittest
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "mcp" / "generate_shaft_skills_tool_catalog.py"
SPEC = importlib.util.spec_from_file_location("generate_shaft_skills_tool_catalog", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
# Registered in sys.modules before exec: the module uses @dataclass, whose field-type
# resolution looks the module up via sys.modules[cls.__module__] and raises AttributeError
# on a module that was never registered there.
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


def write_fixture(directory: Path, name: str, content: str) -> Path:
    path = directory / name
    path.write_text(content, encoding="utf-8")
    return path


class ParseToolAnnotationTest(unittest.TestCase):
    """Unit tests for the character-by-character @Tool annotation parser."""

    def test_single_line_annotation(self):
        text = (
            "package com.shaft.mcp;\n"
            "class EngineService {\n"
            "    @Tool(name = \"driver_quit\", description = \"closes browser\")\n"
            "    public void quit() {}\n"
            "}\n"
        )
        entries, errors = MODULE.parse_file(Path("EngineService.java"), text)

        self.assertEqual([], errors)
        self.assertEqual([MODULE.ToolEntry(name="driver_quit", description="closes browser")], entries)

    def test_multi_line_concatenated_description(self):
        text = (
            "class GuideService {\n"
            "    @Tool(name = \"shaft_guide_search\",\n"
            "            description = \"searches the live official SHAFT user guide before writing SHAFT tests, page objects,\"\n"
            "                    + \" locators, API, GUI, CLI, mobile, troubleshooting, or best-practice code\")\n"
            "    public Object search() { return null; }\n"
            "}\n"
        )
        entries, errors = MODULE.parse_file(Path("GuideService.java"), text)

        self.assertEqual([], errors)
        self.assertEqual(1, len(entries))
        self.assertEqual("shaft_guide_search", entries[0].name)
        self.assertEqual(
            "searches the live official SHAFT user guide before writing SHAFT tests, page objects, "
            "locators, API, GUI, CLI, mobile, troubleshooting, or best-practice code",
            entries[0].description,
        )

    def test_escaped_quote_inside_description(self):
        text = (
            "class BrowserService {\n"
            "    @Tool(name = \"browser_navigate\", description = \"navigates to a \\\"quoted\\\" URL\")\n"
            "    public void navigate() {}\n"
            "}\n"
        )
        entries, errors = MODULE.parse_file(Path("BrowserService.java"), text)

        self.assertEqual([], errors)
        self.assertEqual(1, len(entries))
        self.assertEqual('navigates to a "quoted" URL', entries[0].description)

    def test_two_tools_in_one_file(self):
        text = (
            "class ElementService {\n"
            "    @Tool(name = \"element_click\", description = \"clicks an element\")\n"
            "    public void click() {}\n"
            "\n"
            "    @Tool(name = \"element_hover\", description = \"hovers over an element\")\n"
            "    public void hover() {}\n"
            "}\n"
        )
        entries, errors = MODULE.parse_file(Path("ElementService.java"), text)

        self.assertEqual([], errors)
        self.assertEqual(
            [
                MODULE.ToolEntry(name="element_click", description="clicks an element"),
                MODULE.ToolEntry(name="element_hover", description="hovers over an element"),
            ],
            entries,
        )

    def test_file_with_zero_tools(self):
        text = (
            "class McpCodeBlock {\n"
            "    private final String id;\n"
            "}\n"
        )
        entries, errors = MODULE.parse_file(Path("McpCodeBlock.java"), text)

        self.assertEqual([], entries)
        self.assertEqual([], errors)


class BuildCatalogGuardsTest(unittest.TestCase):
    """Tests for the correctness guards enforced by build_catalog()."""

    def test_unparseable_attribute_via_constant_reference_fails(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            source_dir = Path(temp_dir)
            write_fixture(
                source_dir,
                "BrokenService.java",
                "class BrokenService {\n"
                "    @Tool(name = TOOL_NAME_CONSTANT, description = \"does something\")\n"
                "    public void run() {}\n"
                "}\n",
            )

            with self.assertRaises(MODULE.CatalogError):
                MODULE.build_catalog(source_dir)

    def test_duplicate_tool_name_across_two_files_fails(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            source_dir = Path(temp_dir)
            write_fixture(
                source_dir,
                "FirstService.java",
                "class FirstService {\n"
                "    @Tool(name = \"shared_tool\", description = \"first\")\n"
                "    public void run() {}\n"
                "}\n",
            )
            write_fixture(
                source_dir,
                "SecondService.java",
                "class SecondService {\n"
                "    @Tool(name = \"shared_tool\", description = \"second\")\n"
                "    public void run() {}\n"
                "}\n",
            )

            with self.assertRaises(MODULE.CatalogError):
                MODULE.build_catalog(source_dir)

    def test_zero_tools_found_fails(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            source_dir = Path(temp_dir)
            write_fixture(
                source_dir,
                "PlainClass.java",
                "class PlainClass {\n    private final String id;\n}\n",
            )

            with self.assertRaises(MODULE.CatalogError):
                MODULE.build_catalog(source_dir)

    def test_valid_fixtures_build_a_sorted_catalog(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            source_dir = Path(temp_dir)
            write_fixture(
                source_dir,
                "BrowserService.java",
                "class BrowserService {\n"
                "    @Tool(name = \"browser_navigate\", description = \"opens a URL\")\n"
                "    public void navigate() {}\n"
                "}\n",
            )
            write_fixture(
                source_dir,
                "AutobotService.java",
                "class AutobotService {\n"
                "    @Tool(name = \"autobot_status\", description = \"reports status\")\n"
                "    public void status() {}\n"
                "}\n",
            )
            write_fixture(
                source_dir,
                "McpCodeBlock.java",
                "class McpCodeBlock {\n    private final String id;\n}\n",
            )

            services, total = MODULE.build_catalog(source_dir)

            self.assertEqual(2, total)
            self.assertEqual(["AutobotService", "BrowserService"], [s.class_name for s in services])


class HumanHeadingTest(unittest.TestCase):
    def test_strips_service_suffix_and_splits_camel_case(self):
        self.assertEqual("Browser", MODULE.human_heading("BrowserService"))
        self.assertEqual("Coding Partner", MODULE.human_heading("CodingPartnerService"))
        self.assertEqual("Natural Action", MODULE.human_heading("NaturalActionService"))
        self.assertEqual("Shaft Project", MODULE.human_heading("ShaftProjectService"))
        self.assertEqual("Test Automation", MODULE.human_heading("TestAutomationService"))


class RealRepositoryDriftGateTest(unittest.TestCase):
    """Runs the real generator over the real shaft-mcp sources and compares against the
    checked-in catalog file: this is the drift gate that keeps the committed markdown in sync
    with the Java @Tool annotations."""

    def test_check_mode_passes_against_committed_catalog(self):
        stdout = io.StringIO()
        with contextlib.redirect_stdout(stdout):
            exit_code = MODULE.main(["--check"])

        self.assertEqual(0, exit_code, stdout.getvalue())

    def test_real_sources_parse_to_expected_magnitude(self):
        content, total, service_count = MODULE.build(MODULE.SOURCE_DIR)

        self.assertEqual(165, total)
        self.assertEqual(16, service_count)
        self.assertIn("SHAFT MCP Tool Catalog", content)


class DeterminismTest(unittest.TestCase):
    def test_generating_twice_yields_identical_bytes(self):
        first, _, _ = MODULE.build(MODULE.SOURCE_DIR)
        second, _, _ = MODULE.build(MODULE.SOURCE_DIR)

        self.assertEqual(first.encode("utf-8"), second.encode("utf-8"))


if __name__ == "__main__":
    unittest.main()
