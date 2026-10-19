"""The technical-presentations skill is one on-demand source every host reaches (no per-host copies)."""

from __future__ import annotations

import json
import re
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import harness_reachability as reach  # noqa: E402

BASE = "chaos-engine/skills/technical-presentations"
SKILL = f"{BASE}/SKILL.md"
CARDS = [f"{BASE}/references/{name}.md"
         for name in ("plan", "slide", "technical", "teach", "review", "sources")]
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
INTELLIJ = ".github/copilot-instructions.md"
HOST_CONTEXTS = {
    "claude": ["CLAUDE.md", "AGENTS.md"], "codex": ["AGENTS.md"], "grok": ["AGENTS.md"],
    "gemini": ["GEMINI.md", "AGENTS.md"], "cursor": ["AGENTS.md"],
    "copilot": ["AGENTS.md", INTELLIJ], "copilot-intellij": [INTELLIJ],
    "grok-bot": ["chaos-engine/references/bot-entry.md", "chaos-engine/skills/chaos-engine/SKILL.md"],
}


def _text(path: str) -> str:
    return (ROOT / path).read_text(encoding="utf-8")


class TechnicalPresentationsParityTest(unittest.TestCase):
    def test_every_host_reaches_the_skill_and_every_card(self) -> None:
        for host, contexts in HOST_CONTEXTS.items():
            reached: set[str] = set()
            for context in contexts:
                reached |= reach.link_walk(ROOT, context)[0]
            with self.subTest(host=host):
                self.assertEqual([], [p for p in [SKILL, *CARDS] if p not in reached])

    def test_one_source_no_host_copies(self) -> None:
        copies = [p for p in ROOT.glob("**/technical-presentations/SKILL.md")
                  if ".chaos-engine" not in p.parts and ".git" not in p.parts]
        self.assertEqual([ROOT / SKILL], copies)

    def test_parity_row_covers_every_host(self) -> None:
        data = json.loads(_text("scripts/ci/agent_harness_parity.json"))
        row = next(c for c in data["capabilities"] if c["id"] == "technical_presentations_skill")
        self.assertEqual({SKILL}, {path for host in data["hosts"] for path in row[host]})

    def test_on_demand_only_never_in_the_always_loaded_router(self) -> None:
        self.assertNotIn("technical-presentations", ROUTER.read_text(encoding="utf-8"))
        self.assertIn("../skills/technical-presentations/SKILL.md",
                      _text("chaos-engine/references/level-1-catalog.md"))

    def test_skill_opens_with_both_retrieves(self) -> None:
        body = _text(SKILL).split("## Card index", 1)[0]
        self.assertIn("--store graphify", body)
        self.assertIn("--store mempalace", body)

    def test_hard_rule_never_removes_existing_slide_content(self) -> None:
        rules = _text(SKILL).split("## Hard rules", 1)[1].split("\n## ", 1)[0].casefold()
        self.assertIn("never remove", rules)
        self.assertIn("skip slide", rules)
        self.assertIn("speaker notes", rules)

    def test_brand_neutral_with_a_palette_hook(self) -> None:
        self.assertIn("brand palette file if present", _text(SKILL))
        for path in [SKILL, *CARDS]:
            with self.subTest(path=path):
                text = _text(path)
                self.assertNotRegex(text, r"#[0-9A-Fa-f]{6}\b")
                self.assertNotIn("automatest", text.casefold())


if __name__ == "__main__":
    unittest.main()
