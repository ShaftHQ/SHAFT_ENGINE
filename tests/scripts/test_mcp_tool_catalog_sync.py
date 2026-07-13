"""Guards MCP tool-catalog drift from one static-scan source of truth.

Every surface below claims to expose or route shaft-mcp @Tool names by string literal: the
manifest fixture consumed by ShaftMcpApplicationTests, the IntelliJ plugin's ToolTemplates panel,
the shaft-cli command ACTIONS alias maps, and (once present) AssistantCommand.java's
intent-keyword table. This test reuses the same @Tool scanner as
scripts/mcp/generate_shaft_skills_tool_catalog.py (via its importable
``scanned_tool_names()`` helper) so there is exactly one source of truth for "what tools exist",
instead of four hand-maintained lists that can silently drift apart (see issues fixed by
PRs #3460/#3464 after drift incidents in #3454/#3459/#3462).
"""

import importlib.util
import json
import re
import sys
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
GENERATOR_MODULE_PATH = REPO_ROOT / "scripts" / "mcp" / "generate_shaft_skills_tool_catalog.py"
MANIFEST_PATH = (
    REPO_ROOT / "shaft-mcp" / "src" / "test" / "resources" / "fixtures" / "mcp-tool-manifest.json"
)
TOOL_TEMPLATES_PATH = (
    REPO_ROOT / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui" / "ToolTemplates.java"
)
CLI_COMMAND_DIR = REPO_ROOT / "shaft-cli" / "src" / "main" / "java" / "com" / "shaft" / "commandline" / "command"
ASSISTANT_COMMAND_PATH = (
    REPO_ROOT / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui" / "AssistantCommand.java"
)

# The other half of issue #3506: AssistantCommand.java is being refactored (concurrently with this
# gate) to a single static intent-keyword table. That table is expected to carry this exact marker
# comment directly above it. Until it lands, gate 1d's phrase check degrades to a named skip
# instead of failing the whole suite.
INTENT_TABLE_MARKER = "intent-keyword table (gated by tests/scripts/test_mcp_tool_catalog_sync.py)"

TOOL_TEMPLATE_PATTERN = re.compile(r'template\(\s*"[^"]*"\s*,\s*"([A-Za-z_][A-Za-z0-9_]*)"')
ACTIONS_BLOCK_PATTERN = re.compile(
    r"private\s+static\s+final\s+Map<String,\s*String>\s+ACTIONS\s*=\s*Map\.of\((.*?)\);",
    re.DOTALL,
)
QUOTED_STRING_PATTERN = re.compile(r'"((?:[^"\\]|\\.)*)"')


def _load_generator_module():
    spec = importlib.util.spec_from_file_location("generate_shaft_skills_tool_catalog", GENERATOR_MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise ImportError(f"Unable to load {GENERATOR_MODULE_PATH}")
    # Registered in sys.modules before exec: the module uses @dataclass, whose field-type
    # resolution looks the module up via sys.modules[cls.__module__] and raises AttributeError
    # on a module that was never registered there (matches test_generate_shaft_skills_tool_catalog.py).
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


GENERATOR = _load_generator_module()


def _load_manifest() -> dict:
    return json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))


def _tool_template_names(text: str) -> set[str]:
    """Every tool-name literal passed as the second argument to ToolTemplates.java's
    private `template(label, toolName, ...)` factory methods."""
    return set(TOOL_TEMPLATE_PATTERN.findall(text))


def _cli_alias_tool_names(text: str) -> set[str]:
    """Every tool-name VALUE from `Map.of("action", "tool_name", ...)` ACTIONS maps."""
    tool_names: set[str] = set()
    for match in ACTIONS_BLOCK_PATTERN.finditer(text):
        literals = QUOTED_STRING_PATTERN.findall(match.group(1))
        # Map.of(key, value, key, value, ...): tool names are the odd-indexed (value) entries.
        tool_names.update(literals[1::2])
    return tool_names


def _normalize(text: str) -> str:
    """Collapses Java string-literal/comment punctuation and whitespace so a phrase that spans a
    wrapped/concatenated literal (`"...the "` + `"Android emulator"`) or a plain comment still
    matches as one contiguous, case-insensitive run of words."""
    stripped = re.sub(r'["+\\]', " ", text)
    return re.sub(r"\s+", " ", stripped).strip().lower()


class McpToolCatalogSyncTest(unittest.TestCase):
    """Gate 1a: the manifest's tool-name set must equal the live @Tool-annotation scan."""

    def test_manifest_matches_scanned_tool_annotations(self):
        scanned = GENERATOR.scanned_tool_names()
        manifest_names = {tool["name"] for tool in _load_manifest()["tools"]}

        missing_from_manifest = sorted(scanned - manifest_names)
        extra_in_manifest = sorted(manifest_names - scanned)
        self.assertEqual(
            [],
            missing_from_manifest,
            "shaft-mcp @Tool annotation(s) found in the sources but missing from "
            f"mcp-tool-manifest.json: {missing_from_manifest}",
        )
        self.assertEqual(
            [],
            extra_in_manifest,
            "mcp-tool-manifest.json lists tool(s) with no matching @Tool annotation in "
            f"shaft-mcp sources: {extra_in_manifest}",
        )


class ToolTemplatesSubsetTest(unittest.TestCase):
    """Gate 1b: every ToolTemplates.java toolName literal must be a real, scanned tool."""

    def test_tool_templates_are_subset_of_scanned_tools(self):
        scanned = GENERATOR.scanned_tool_names()
        template_names = _tool_template_names(TOOL_TEMPLATES_PATH.read_text(encoding="utf-8"))

        self.assertTrue(template_names, "No template(...) tool names were parsed from ToolTemplates.java")
        unknown = sorted(template_names - scanned)
        self.assertEqual(
            [],
            unknown,
            f"ToolTemplates.java references tool name(s) not found in shaft-mcp @Tool annotations: {unknown}",
        )


class CliAliasSubsetTest(unittest.TestCase):
    """Gate 1c: every shaft-cli *Command.java ACTIONS map value must be a real, scanned tool."""

    def test_cli_alias_actions_are_subset_of_scanned_tools(self):
        scanned = GENERATOR.scanned_tool_names()
        all_alias_tools: set[str] = set()
        for command_file in sorted(CLI_COMMAND_DIR.glob("*Command.java")):
            all_alias_tools.update(_cli_alias_tool_names(command_file.read_text(encoding="utf-8")))

        self.assertTrue(all_alias_tools, "No ACTIONS alias tool names were parsed from shaft-cli command classes")
        unknown = sorted(all_alias_tools - scanned)
        self.assertEqual(
            [],
            unknown,
            "shaft-cli ACTIONS map(s) reference tool name(s) not found in shaft-mcp @Tool "
            f"annotations: {unknown}",
        )


class IntentKeywordsValidityTest(unittest.TestCase):
    """Gate 1d: manifest intentKeywords tools must be real tools, and their trigger phrases must
    be backed by AssistantCommand.java's intent-keyword table."""

    def test_intent_keyword_tools_exist_in_scanned_set(self):
        scanned = GENERATOR.scanned_tool_names()
        unknown = sorted(
            tool["name"]
            for tool in _load_manifest()["tools"]
            if tool.get("intentKeywords") and tool["name"] not in scanned
        )
        self.assertEqual(
            [],
            unknown,
            "mcp-tool-manifest.json intentKeywords reference tool name(s) not found in shaft-mcp "
            f"@Tool annotations: {unknown}",
        )

    def test_intent_keyword_phrases_appear_in_assistant_command_table(self):
        text = ASSISTANT_COMMAND_PATH.read_text(encoding="utf-8")
        marker_index = text.find(INTENT_TABLE_MARKER)
        if marker_index == -1:
            self.skipTest(
                "SKIPPED (not failed): AssistantCommand.java does not yet contain the "
                f"'{INTENT_TABLE_MARKER}' marker comment above its intent-keyword table. "
                "TODO(#3506): remove this skip once the marker/table lands and re-run this check."
            )
            return

        table_text = _normalize(text[marker_index:])
        missing = []
        for tool in _load_manifest()["tools"]:
            for phrase in tool.get("intentKeywords", []):
                if _normalize(phrase) not in table_text:
                    missing.append(f"{tool['name']}: {phrase!r}")

        self.assertEqual(
            [],
            missing,
            "mcp-tool-manifest.json intentKeywords phrase(s) not found in the AssistantCommand.java "
            f"intent-keyword table (after the marker comment): {missing}",
        )


if __name__ == "__main__":
    unittest.main()
