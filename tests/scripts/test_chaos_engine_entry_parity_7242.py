"""#7242 chunk 1: host entry files are pointers to one canonical set, never restated policy.

Copilot Chat on github.com reads only .github/copilot-instructions.md, so that file keeps
the managed ChaosEngine block; everything else in it must be a pointer to AGENTS.md.
"""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HOSTS = ROOT / "chaos-engine/hosts.py"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def outside_block(text: str, start: str, end: str) -> list[str]:
    if start in text:
        text = text[: text.index(start)] + text[text.index(end) + len(end):]
    return [line.strip() for line in text.splitlines() if line.strip()]


class EntryParityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.hosts = load(HOSTS, "ce_hosts_7242")

    def test_fresh_import_pointers_are_exactly_one_import(self):
        for relative in self.hosts.IMPORT_POINTER_FILES:
            self.assertEqual(
                b"@AGENTS.md\n",
                self.hosts.import_pointer_instruction(None, relative=relative),
                relative,
            )

    def test_fresh_copilot_file_is_only_the_managed_block(self):
        merged = self.hosts.merge_instruction(
            None, self.hosts.copilot_instruction_block(),
            relative=".github/copilot-instructions.md",
        ).decode()
        self.assertEqual([], outside_block(merged, self.hosts.START, self.hosts.END))

    def test_tracked_pointer_files_do_not_restate_policy(self):
        for relative in self.hosts.IMPORT_POINTER_FILES:
            self.assertEqual("@AGENTS.md\n", (ROOT / relative).read_text(encoding="utf-8"), relative)
        copilot = (ROOT / ".github/copilot-instructions.md").read_text(encoding="utf-8")
        extra = outside_block(copilot, self.hosts.START, self.hosts.END)
        allowed = {"# SHAFT_ENGINE Copilot Instructions",
                   "Follow `AGENTS.md` as the canonical repository policy."}
        self.assertEqual([], [line for line in extra if line not in allowed])

    def test_ethics_card_is_in_bot_entry_and_linked_from_core_card(self):
        tool = load(ROOT / "chaos-engine/tool.py", "ce_tool_7242")
        self.assertIn("companions/ethics-card.md", tool.CORE_ENTRY_FILES)
        card = ROOT / "chaos-engine/companions/ethics-card.md"
        self.assertLessEqual(len(card.read_bytes()), 600)
        self.assertIn("references/ethical-conduct.md", card.read_text(encoding="utf-8"))
        core = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("companions/ethics-card.md", core)


if __name__ == "__main__":
    unittest.main()
