"""
Guards MCP tool-catalog drift from one static-scan source of truth.

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
SHAFT_ASSISTANT_PANEL_PATH = (
    REPO_ROOT / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui" / "ShaftAssistantPanel.java"
)
GUIDED_WORKFLOW_PANEL_PATH = (
    REPO_ROOT / "shaft-intellij" / "src" / "main" / "java" / "com" / "shaft" / "intellij" / "ui" / "GuidedWorkflowPanel.java"
)
# Gate 1e (issue #3866 A4): the three IntelliJ UI classes that actually compose/pass an MCP tool
# name into a tools/call dispatch method. Deliberately narrow to these five dispatch call sites --
# NOT a blanket string scan of the file -- so lookup tables like AssistantCommand's INTENT_KEYWORDS
# (dictionary keys such as "mobile_record_start" used only for natural-language phrase matching,
# never passed to a dispatch call) can never trip this gate.
DISPATCH_UI_CLASS_PATHS = (ASSISTANT_COMMAND_PATH, SHAFT_ASSISTANT_PANEL_PATH, GUIDED_WORKFLOW_PANEL_PATH)

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
    """
    Return every tool-name literal from ToolTemplates.java's factory calls.

    The names are the second argument of the private `template(label, toolName, ...)` methods.
    """
    return set(TOOL_TEMPLATE_PATTERN.findall(text))


def _cli_alias_tool_names(text: str) -> set[str]:
    """Return every tool-name value from `Map.of("action", "tool_name", ...)` ACTIONS maps."""
    tool_names: set[str] = set()
    for match in ACTIONS_BLOCK_PATTERN.finditer(text):
        literals = QUOTED_STRING_PATTERN.findall(match.group(1))
        # Map.of(key, value, key, value, ...): tool names are the odd-indexed (value) entries.
        tool_names.update(literals[1::2])
    return tool_names


def _normalize(text: str) -> str:
    """
    Collapse Java string-literal/comment punctuation and whitespace.

    A phrase that spans a wrapped/concatenated literal (`"...the "` + `"Android emulator"`) or a
    plain comment still matches as one contiguous, case-insensitive run of words.
    """
    stripped = re.sub(r'["+\\]', " ", text)
    return re.sub(r"\s+", " ", stripped).strip().lower()



# Gate 1e: the exact method calls through which these three classes issue (or hand off) an MCP
# tools/call -- Invocation.tool(...)/new ToolCall(...) in AssistantCommand.java build the call
# target directly; ShaftAssistantPanel.java and GuidedWorkflowPanel.java relay it one hop further
# via ShaftMcpInvocationService#startTool, ToolPrefill#prefill, and GuidedWorkflowPanel's own
# invokeStepTool. A call to any *other* method (Map.get, String.contains, ...) is never scanned.
DISPATCH_CALL_PATTERN = re.compile(r"(?:\.startTool|prefill\.prefill|invokeStepTool|Invocation\.tool|new\s+ToolCall)\s*\(")
_STRING_LITERAL_PATTERN = re.compile(r'"(?:[^"\\]|\\.)*"')
_FULL_LITERAL_PATTERN = re.compile(r'^"((?:[^"\\]|\\.)*)"$')


def _first_call_argument(text: str, open_paren_index: int) -> str:
    """
    Return the raw text of the first top-level argument of a call, given the index of its `(`.

    Tracks string-literal state and paren depth so a comma or paren inside a nested call or a
    string literal never ends the argument early (e.g. `foo(bar(a, b), "x, y")`'s first argument
    is `bar(a, b)`, not `bar(a`).
    """
    depth = 1
    in_string = False
    escape = False
    i = open_paren_index + 1
    start = i
    while i < len(text) and depth > 0:
        char = text[i]
        if in_string:
            if escape:
                escape = False
            elif char == "\\":
                escape = True
            elif char == '"':
                in_string = False
        elif char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                break
        elif char == "," and depth == 1:
            break
        i += 1
    return text[start:i].strip()


def dispatch_tool_name_violations(text: str, scanned_tools: set[str]) -> list[str]:
    """
    Scan `text` for MCP tools/call dispatch sites (`DISPATCH_CALL_PATTERN`) and return one
    human-readable violation string per problem found in the tool-name (first) argument:

    - built via string concatenation (e.g. `prefix + "_step_delete"`) -- post-sweep, every
      surviving tool is a single unconditional name, so the caller never needs to compose one from
      an engine/backend prefix; a `+` in this position is exactly the Finding-1 (#3866) bug shape.
    - a plain string literal naming a tool that is not in the live scanned `@Tool` set (i.e. a
      deleted/renamed tool referenced by exact name).

    A bare identifier or method call (e.g. `toolName`, `invocation.toolName()`) is intentionally
    left unchecked -- its value cannot be resolved statically, and letting it through is the
    documented, deliberately narrow scope for this regex-based gate (it is not a concatenation, so
    it is not the bug shape this gate targets).
    """
    violations: list[str] = []
    for match in DISPATCH_CALL_PATTERN.finditer(text):
        argument = _first_call_argument(text, match.end() - 1)
        literal_match = _FULL_LITERAL_PATTERN.match(argument)
        if literal_match:
            tool_name = literal_match.group(1)
            if tool_name and tool_name not in scanned_tools:
                violations.append(
                    f"{match.group(0).strip('(').strip()}(\"{tool_name}\") names a tool not in the "
                    "live scanned @Tool set (deleted/renamed tool referenced by stale literal name)"
                )
            continue
        without_literals = _STRING_LITERAL_PATTERN.sub('""', argument)
        if "+" in without_literals:
            violations.append(
                f"{match.group(0).strip('(').strip()}({argument}) builds its tool-name argument via "
                "string concatenation instead of an unconditional literal"
            )
    return violations


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

    """
    Gate 1d: manifest intentKeywords must name real tools with backed phrases.

    Every trigger phrase must appear in AssistantCommand.java's intent-keyword table.
    """

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


class DispatchToolNameScannerTest(unittest.TestCase):

    """
    Unit-level proof that `dispatch_tool_name_violations` actually catches the Finding-1 (#3866)
    bug shape, and does not false-positive on lookup-table dictionary keys or unresolvable
    identifiers/method calls used as the tool-name argument.
    """

    def test_flags_prefix_concatenation_building_a_stale_tool_name(self):
        # The exact shape of the landed-then-reverted GuidedWorkflowPanel bug: a backend prefix
        # string-concatenated with a literal suffix, passed straight into the dispatch call.
        text = 'invokeStepTool(prefix + "_step_delete", arguments);'
        violations = dispatch_tool_name_violations(text, scanned_tools={"capture_step_delete"})
        self.assertEqual(1, len(violations), violations)
        self.assertIn("concatenation", violations[0])

    def test_flags_a_literal_tool_name_deleted_from_the_scanned_set(self):
        text = 'Invocation.tool("playwright_step_delete", arguments);'
        violations = dispatch_tool_name_violations(text, scanned_tools={"capture_step_delete"})
        self.assertEqual(1, len(violations), violations)
        self.assertIn("playwright_step_delete", violations[0])

    def test_passes_the_unconditional_replacement_tool_name(self):
        text = 'invokeStepTool("capture_step_delete", arguments);'
        violations = dispatch_tool_name_violations(text, scanned_tools={"capture_step_delete"})
        self.assertEqual([], violations)

    def test_does_not_flag_an_intent_keywords_dictionary_key(self):
        # Map.entry/.get(...) is not one of the five dispatch call patterns, so a natural-language
        # lookup key that happens to look like a deleted tool name (e.g. AssistantCommand's
        # INTENT_KEYWORDS map) must never trip this gate.
        text = 'INTENT_KEYWORDS.get("mobile_record_start")'
        violations = dispatch_tool_name_violations(text, scanned_tools=set())
        self.assertEqual([], violations)

    def test_does_not_flag_an_unresolvable_identifier_or_method_call(self):
        text = (
            'invocationService.startTool(toolName, arguments);\n'
            'invocationService.startTool(invocation.toolName(), invocation.arguments());\n'
        )
        violations = dispatch_tool_name_violations(text, scanned_tools=set())
        self.assertEqual([], violations)


class DispatchToolNameSubsetTest(unittest.TestCase):

    """
    Gate 1e (issue #3866 A4): every literal MCP tool name dispatched from AssistantCommand.java,
    ShaftAssistantPanel.java, or GuidedWorkflowPanel.java must be a real, currently scanned tool,
    and none may be composed via string concatenation of an engine/backend prefix (the exact shape
    of the Finding-1 regression this gate exists to catch).
    """

    def test_no_dispatched_tool_name_is_stale_or_prefix_concatenated(self):
        scanned = GENERATOR.scanned_tool_names()
        violations: list[str] = []
        for path in DISPATCH_UI_CLASS_PATHS:
            for violation in dispatch_tool_name_violations(path.read_text(encoding="utf-8"), scanned):
                violations.append(f"{path.name}: {violation}")

        self.assertEqual(
            [],
            violations,
            "MCP dispatch call(s) reference a stale/deleted tool name or build one via string "
            f"concatenation: {violations}",
        )


if __name__ == "__main__":
    unittest.main()
