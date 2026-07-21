"""
Tests for scripts/mcp/validate_tool_index_examples.py -- the offline "example-schema staleness"
gate from the tool architecture sweep design doc, amendment A6: every `## Example calls` request
JSON block committed to shaft-skills/*/SKILL.md is cross-checked against the actual tool's
params in tool-index.json, so a renamed/removed param or a tool that no longer exists fails CI
without needing a live MCP run.
"""

import importlib.util
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
        exit_code = MODULE.main([])
        self.assertEqual(0, exit_code)

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
            import json
            index_path.write_text(json.dumps(self._tool_index()), encoding="utf-8")

            exit_code = MODULE.main(["--skills-root", str(Path(tmp) / "shaft-skills"),
                                      "--tool-index-path", str(index_path)])
            self.assertEqual(1, exit_code)

    def _tool_index(self):
        return {
            "tools": [
                {"name": "element_click", "params": [
                    {"name": "locatorStrategy"}, {"name": "locatorValue"}, {"name": "mode"},
                ]},
            ]
        }


if __name__ == "__main__":
    unittest.main()
