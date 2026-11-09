"""#7242 chunk 1: host entry files are pointers to one canonical set, never restated policy.

Copilot Chat on github.com reads only .github/copilot-instructions.md, so that file keeps
the managed ChaosEngine block; everything else in it must be a pointer to AGENTS.md.
"""

from __future__ import annotations

import importlib.util
import re
import tempfile
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


class ShaftSkillsEntryLinkTest(unittest.TestCase):
    """#7858: ChaosEngine points at the shaft entrypoint and skips a missing tree."""

    def test_repository_entry_links_the_shaft_entrypoint_and_skips_when_absent(self):
        entry = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        self.assertIn("If that file is absent, skip this route and continue.", entry)
        match = re.search(r"\[[^\]]+\]\(([^)]*shaft-developer/SKILL\.md)\)", entry)
        self.assertIsNotNone(match)
        self.assertLess(entry.index(match.group(0)), entry.index("<!-- CHAOSENGINE:START -->"))
        target = (ROOT / match.group(1)).resolve()
        self.assertEqual(target, (ROOT / "shaft-skills/shaft-developer/SKILL.md").resolve())
        playbook = (
            ROOT / "shaft-skills/shaft-failure-analysis/references/playbook.md"
        ).read_text(encoding="utf-8")
        sentence = next(
            line.strip()
            for line in playbook.splitlines()
            if len(line.strip()) > 80 and not line.startswith("#")
        )
        self.assertNotIn(sentence, entry)
        core = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        host = (ROOT / ".grok/rules/entrypoint.md").read_text(encoding="utf-8")
        profile = (ROOT / "shaft-skills/ce-pack/entrypoint.md").read_text(encoding="utf-8")
        for text in (core, host, profile):
            self.assertNotIn(sentence, text)
            self.assertNotIn("shaft-developer/SKILL.md", text)
        for text in (core, host):
            self.assertNotIn("shaft", text.casefold())

    def test_missing_shaft_tree_reads_cleanly_and_does_not_require_the_path(self):
        source = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            entry = root / "AGENTS.md"
            entry.write_text(source, encoding="utf-8")
            self.assertFalse((root / "shaft-skills/shaft-developer").exists())
            text = entry.read_text(encoding="utf-8")
            match = re.search(r"\[[^\]]+\]\(([^)]*shaft-developer/SKILL\.md)\)", text)
            self.assertIsNotNone(match)
            self.assertFalse((entry.parent / match.group(1)).resolve().is_file())
            self.assertIn("If that file is absent, skip this route and continue.", text)

    def test_generated_host_copy_matches_the_canonical_entry(self):
        module_path = ROOT / "chaos-engine/grok_model_load.py"
        spec = importlib.util.spec_from_file_location("grok_model_load_7858", module_path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        source = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
        destination = ROOT / ".grok/rules/entrypoint.md"
        self.assertEqual(destination.read_bytes(), module._linked_bytes(source, destination))


if __name__ == "__main__":
    unittest.main()
