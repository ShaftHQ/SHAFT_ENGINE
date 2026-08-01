"""
Tests for scripts/mcp/validate_tool_index_examples.py -- the offline "example-schema staleness"
gate from the tool architecture sweep design doc, amendment A6: every `## Example calls` request
JSON block committed to shaft-skills/*/SKILL.md is cross-checked against the actual tool's
params in tool-index.json, so a renamed/removed param or a tool that no longer exists fails CI
without needing a live MCP run.
"""

import importlib.util
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = REPO_ROOT / "scripts" / "mcp" / "validate_tool_index_examples.py"
SPEC = importlib.util.spec_from_file_location("validate_tool_index_examples", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


class ExtractExamplesTest(unittest.TestCase):
    """Unit tests for the markdown example-block parser against small synthetic fixtures."""

    def test_extracts_a_fenced_json_request_after_a_wrapped_marker_line(self):
        text = (
            "## Example calls\n\n"
            "`doctor_analyze_failed_allure` — request (empty `allureResultPaths` analyzes\n"
            "the newest evidence found in the workspace):\n\n"
            "```json\n"
            '{"allureResultPaths": [], "minimumAllureResults": 1}\n'
            "```\n\n"
            "response (`McpAnalysisReport`, truncated):\n\n"
            "```json\n"
            '{"schemaVersion": "1.0"}\n'
            "```\n"
        )
        examples = MODULE.extract_examples(text)

        self.assertEqual(1, len(examples))
        self.assertEqual("doctor_analyze_failed_allure", examples[0].tool_name)
        self.assertEqual({"allureResultPaths": [], "minimumAllureResults": 1}, examples[0].request)

    def test_extracts_an_inline_backtick_request_with_no_fenced_block(self):
        text = (
            "## Example calls\n\n"
            "`capture_status` — request: `{}` (no params). Response has the same\n"
            "`McpCaptureUnionStatus` shape as above.\n"
        )
        examples = MODULE.extract_examples(text)

        self.assertEqual(1, len(examples))
        self.assertEqual("capture_status", examples[0].tool_name)
        self.assertEqual({}, examples[0].request)

    def test_extracts_multiple_examples_in_one_document_without_bleeding_into_each_other(self):
        text = (
            "## Example calls\n\n"
            "`shaft_guide_search` — request:\n\n"
            "```json\n"
            '{"query": "x", "maxResults": 2}\n'
            "```\n\n"
            "response (`McpGuideSearchResult`, truncated):\n\n"
            "```json\n"
            '{"schemaVersion": "1.0"}\n'
            "```\n\n"
            "`shaft_coding_partner_plan` — request:\n\n"
            "```json\n"
            '{"intent": "add a test"}\n'
            "```\n"
        )
        examples = MODULE.extract_examples(text)

        self.assertEqual(2, len(examples))
        self.assertEqual("shaft_guide_search", examples[0].tool_name)
        self.assertEqual({"query": "x", "maxResults": 2}, examples[0].request)
        self.assertEqual("shaft_coding_partner_plan", examples[1].tool_name)
        self.assertEqual({"intent": "add a test"}, examples[1].request)


class CheckExampleAgainstToolIndexTest(unittest.TestCase):
    """Unit tests for cross-checking one extracted example against tool-index.json params."""

    def _tool_index(self):
        return {
            "tools": [
                {"name": "element_click", "params": [
                    {"name": "locatorStrategy"}, {"name": "locatorValue"}, {"name": "mode"},
                ]},
            ]
        }

    def test_no_problem_when_every_request_key_is_a_real_param(self):
        example = MODULE.Example(tool_name="element_click", request={"locatorStrategy": "ID", "locatorValue": "x"},
                                  source="SKILL.md:1")
        problems = MODULE.check_example(example, self._tool_index())
        self.assertEqual([], problems)

    def test_flags_an_unknown_tool_name(self):
        example = MODULE.Example(tool_name="element_click_renamed", request={}, source="SKILL.md:1")
        problems = MODULE.check_example(example, self._tool_index())
        self.assertEqual(1, len(problems))
        self.assertIn("element_click_renamed", problems[0])

    def test_flags_a_request_key_that_is_not_a_real_param(self):
        example = MODULE.Example(tool_name="element_click", request={"locator": "ID"}, source="SKILL.md:1")
        problems = MODULE.check_example(example, self._tool_index())
        self.assertEqual(1, len(problems))
        self.assertIn("locator", problems[0])


class MainIntegrationTest(unittest.TestCase):
    """--check over the real checked-in shaft-skills/*/SKILL.md files and the real tool-index.json."""

    def test_real_skill_examples_match_the_real_tool_index(self):
        tool_index = json.loads(MODULE.DEFAULT_TOOL_INDEX_PATH.read_text(encoding="utf-8"))
        self.assertEqual([], MODULE.validate_examples(MODULE.DEFAULT_SKILLS_ROOT, tool_index))

    @unittest.skipUnless(
        (MODULE.DEFAULT_SKILLS_ROOT / MODULE.HUB_SKILL_NAME / "SKILL.md").is_file(),
        "shaft-developer integration lands separately",
    )
    def test_real_delivered_skill_family_has_thirty_valid_routed_skills(self):
        tool_index = json.loads(MODULE.DEFAULT_TOOL_INDEX_PATH.read_text(encoding="utf-8"))
        skill_directories = [
            path
            for path in MODULE.DEFAULT_SKILLS_ROOT.iterdir()
            if path.is_dir() and path.name != "references"
        ]

        self.assertEqual(30, len(skill_directories))
        self.assertEqual([], MODULE.validate_delivery(MODULE.DEFAULT_SKILLS_ROOT, tool_index))

    def test_fails_when_a_skill_file_references_a_stale_param_name(self):
        with tempfile.TemporaryDirectory() as tmp:
            skills_dir = Path(tmp) / "shaft-skills" / "fake-skill"
            skills_dir.mkdir(parents=True)
            (skills_dir / "SKILL.md").write_text(
                "## Example calls\n\n"
                "`element_click` — request:\n\n"
                "```json\n"
                '{"locatorStrategyThatWasRenamed": "ID", "locatorValue": "x"}\n'
                "```\n",
                encoding="utf-8",
            )
            index_path = Path(tmp) / "tool-index.json"
            index_path.write_text(json.dumps(self._tool_index()), encoding="utf-8")

            problems = MODULE.validate_examples(Path(tmp) / "shaft-skills", self._tool_index())
            self.assertTrue(any("locatorStrategyThatWasRenamed" in problem for problem in problems))

    def _tool_index(self):
        return {
            "tools": [
                {"name": "element_click", "params": [
                    {"name": "locatorStrategy"}, {"name": "locatorValue"}, {"name": "mode"},
                ]},
            ]
        }


class DeliveredSkillContractTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.skills_root = Path(self.temporary_directory.name) / "shaft-skills"
        self.tool_index = {
            "schemaVersion": "1.0",
            "tools": [
                {
                    "name": "shaft_guide_search",
                    "params": [{"name": "query"}],
                }
            ],
        }
        self._write(
            "references/shaft-mcp-tools.md",
            "# SHAFT MCP Tool Catalog\n\nTotal tools: 1\n\n"
            "- `shaft_guide_search` — Searches the guide.\n",
        )
        self._write(
            "references/shaft-cli-commands.md",
            "# SHAFT CLI Commands\n\nUse `shaft-cli call <TOOL>`.\n",
        )
        self._write_skill(
            "shaft-developer",
            "Use when starting any SHAFT testing task; route to one specialist skill.",
            "# SHAFT Developer\n\n"
            "## Routing\n\n"
            "- [Write tests](../shaft-write-tests/SKILL.md)\n\n"
            "## Examples\n\n"
            "- Route a request to write a browser test.\n"
            "- Route a request to inspect an existing failure.\n",
        )
        self._write_skill(
            "shaft-write-tests",
            "Use when writing or repairing SHAFT tests from current evidence.",
            "# Write SHAFT Tests\n\n"
            "Load [SHAFT Developer](../shaft-developer/SKILL.md) first. Read the "
            "[MCP catalog](../references/shaft-mcp-tools.md) and "
            "[CLI catalog](../references/shaft-cli-commands.md).\n\n"
            "Call `shaft-mcp:shaft_guide_search` before unfamiliar syntax.\n\n"
            "## Examples\n\n"
            "- Write a focused browser test from acceptance criteria.\n"
            "- Repair a test while preserving its assertion intent.\n",
        )

    def tearDown(self):
        self.temporary_directory.cleanup()

    def _write(self, relative_path: str, content: str) -> None:
        path = self.skills_root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def _write_skill(self, name: str, description: str, body: str) -> None:
        self._write(
            f"{name}/SKILL.md",
            f"---\nname: {name}\ndescription: {description}\n---\n\n{body}",
        )

    def problems(self):
        return MODULE.validate_delivery(self.skills_root, self.tool_index)

    def test_valid_hub_and_specialist_contract_passes(self):
        self.assertEqual([], self.problems())

    def test_rejects_non_shaft_identifier_and_extra_frontmatter(self):
        path = self.skills_root / "shaft-write-tests" / "SKILL.md"
        content = path.read_text(encoding="utf-8").replace(
            "name: shaft-write-tests\n",
            "name: write-tests\ndistribution: full\n",
        )
        path.write_text(content, encoding="utf-8")

        problems = self.problems()

        self.assertTrue(any("frontmatter name" in problem for problem in problems), problems)
        self.assertTrue(any("unsupported frontmatter" in problem for problem in problems), problems)

    def test_rejects_skill_folder_outside_shaft_family(self):
        self._write_skill(
            "write-tests",
            "Use when testing invalid delivered skill identifiers.",
            "# Invalid identifier\n\n"
            "Load [SHAFT Developer](../shaft-developer/SKILL.md).\n\n"
            "## Examples\n\n- First example.\n- Second example.\n",
        )

        self.assertTrue(
            any("skill identifier must use shaft-*" in problem for problem in self.problems())
        )

    def test_rejects_specialist_missing_from_hub_routing(self):
        hub = self.skills_root / "shaft-developer" / "SKILL.md"
        content = hub.read_text(encoding="utf-8").replace(
            "- [Write tests](../shaft-write-tests/SKILL.md)\n",
            "",
        )
        hub.write_text(content, encoding="utf-8")

        self.assertTrue(
            any("missing route for specialist 'shaft-write-tests'" in problem for problem in self.problems())
        )

    def test_rejects_missing_or_duplicate_hub_routes(self):
        hub = self.skills_root / "shaft-developer" / "SKILL.md"
        content = hub.read_text(encoding="utf-8").replace(
            "- [Write tests](../shaft-write-tests/SKILL.md)\n",
            "- [Write tests](../shaft-write-tests/SKILL.md)\n"
            "- [Write tests again](../shaft-write-tests/SKILL.md)\n"
            "- [Ghost](../shaft-ghost/SKILL.md)\n",
        )
        hub.write_text(content, encoding="utf-8")

        problems = self.problems()

        self.assertTrue(any("duplicate route" in problem for problem in problems), problems)
        self.assertTrue(any("orphan route" in problem for problem in problems), problems)

    def test_rejects_dead_local_link_and_fewer_than_two_examples(self):
        path = self.skills_root / "shaft-write-tests" / "SKILL.md"
        content = path.read_text(encoding="utf-8").replace(
            "[CLI catalog](../references/shaft-cli-commands.md)",
            "[CLI catalog](../references/missing.md)",
        ).replace(
            "- Repair a test while preserving its assertion intent.\n",
            "",
        )
        path.write_text(content, encoding="utf-8")

        problems = self.problems()

        self.assertTrue(any("missing local reference" in problem for problem in problems), problems)
        self.assertTrue(any("at least two examples" in problem for problem in problems), problems)

    def test_mutating_current_tool_literal_to_retired_name_fails(self):
        self.assertEqual([], self.problems())
        path = self.skills_root / "shaft-write-tests" / "SKILL.md"
        content = path.read_text(encoding="utf-8").replace(
            "shaft-mcp:shaft_guide_search",
            "shaft-mcp:natural_act",
        )
        path.write_text(content, encoding="utf-8")

        problems = self.problems()

        self.assertTrue(
            any("natural_act" in problem and "canonical tool-index.json" in problem for problem in problems),
            problems,
        )

    def test_example_request_in_linked_playbook_is_schema_checked(self):
        path = self.skills_root / "shaft-write-tests" / "SKILL.md"
        path.write_text(
            path.read_text(encoding="utf-8")
            + "\nRead [worked calls](references/worked-calls.md).\n",
            encoding="utf-8",
        )
        self._write(
            "shaft-write-tests/references/worked-calls.md",
            "# Worked calls\n\n## Example calls\n\n"
            "`shaft_guide_search` — request:\n\n"
            "```json\n{\"retiredParam\": true}\n```\n",
        )

        problems = MODULE.validate_all(self.skills_root, self.tool_index)

        self.assertTrue(any("retiredParam" in problem for problem in problems), problems)

    def test_missing_hub_reports_clear_integration_diagnostic(self):
        shutil.rmtree(self.skills_root / "shaft-developer")

        self.assertTrue(
            any("shaft-developer/SKILL.md is required" in problem for problem in self.problems())
        )


if __name__ == "__main__":
    unittest.main()
