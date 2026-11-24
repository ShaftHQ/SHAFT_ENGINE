import json
import re
import shlex
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CLI_PLAYBOOK = ROOT / "shaft-skills" / "shaft-cli" / "references" / "playbook.md"
TOOL_INDEX = (
    ROOT
    / "shaft-mcp"
    / "src"
    / "main"
    / "resources"
    / "META-INF"
    / "shaft-mcp"
    / "tool-index.json"
)


COMMANDS = ROOT / "shaft-skills" / "references" / "shaft-cli-commands.md"


class ShaftCliCommandReferenceTest(unittest.TestCase):
    def test_reference_documents_design_report_doctor_and_tools_discovery(self):
        text = COMMANDS.read_text(encoding="utf-8")
        forms = (
            "shaft-cli tools --cached --group <group> --compact",
            "shaft-cli tools describe <tool> [--json]",
            "shaft-cli design ingest|analyze|gherkin|examples|lexicon|coverage|lint|gap-map|readiness|handoff",
            "shaft-cli report generate|open|summary|history|flake|tags|clusters|heal|mute|unmute|mutes",
            "shaft-cli doctor analyze|suggest|local-ai-status|cause-label",
            "doctor_cause_label",
        )
        for form in forms:
            self.assertIn(form, text)


class ShaftSkillCliExamplesTest(unittest.TestCase):
    def test_guide_search_examples_supply_every_required_tool_argument(self):
        playbook = CLI_PLAYBOOK.read_text(encoding="utf-8")
        commands = re.findall(r"`shaft-cli guide search ([^`]+)`", playbook)
        self.assertTrue(commands, "CLI playbook must include a guide-search example")

        tool_index = json.loads(TOOL_INDEX.read_text(encoding="utf-8"))
        guide_search = next(tool for tool in tool_index["tools"] if tool["name"] == "shaft_guide_search")
        required = {param["name"] for param in guide_search["params"] if param["required"]}

        for command in commands:
            supplied = {
                token.split("=", 1)[0]
                for token in shlex.split(command)
                if "=" in token and not token.startswith("--")
            }
            self.assertEqual(required - supplied, set(), f"invalid guide-search example: {command}")


if __name__ == "__main__":
    unittest.main()
