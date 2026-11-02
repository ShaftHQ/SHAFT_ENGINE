"""The architect skill is one source every host reaches on demand (no per-host copies)."""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import harness_reachability as reach  # noqa: E402

SKILL = "chaos-engine/skills/architect/SKILL.md"
CARDS = [f"chaos-engine/skills/architect/references/{name}.md"
         for name in ("analyze", "design", "operate", "change", "sources")]
# The GitHub Copilot IntelliJ plugin reads repository custom instructions, the
# same file Copilot CLI reads; it gets its own row so a regression names it.
INTELLIJ = ".github/copilot-instructions.md"
HOST_CONTEXTS = {
    "claude": ["CLAUDE.md", "AGENTS.md"], "codex": ["AGENTS.md"], "grok": ["AGENTS.md"],
    "gemini": ["GEMINI.md", "AGENTS.md"], "cursor": ["AGENTS.md"],
    "copilot": ["AGENTS.md", INTELLIJ], "copilot-intellij": [INTELLIJ],
    "grok-bot": ["chaos-engine/references/bot-entry.md", "chaos-engine/skills/chaos-engine/SKILL.md"],
}


class ArchitectParityTest(unittest.TestCase):
    def test_every_host_reaches_the_skill_and_every_card(self) -> None:
        for host, contexts in HOST_CONTEXTS.items():
            reached: set[str] = set()
            for context in contexts:
                reached |= reach.link_walk(ROOT, context)[0]
            with self.subTest(host=host):
                self.assertEqual([], [p for p in [SKILL, *CARDS] if p not in reached])

    def test_one_source_no_host_copies(self) -> None:
        copies = [p for p in ROOT.glob("**/architect/SKILL.md")
                  if ".chaos-engine" not in p.parts and ".git" not in p.parts]
        self.assertEqual([ROOT / SKILL], copies)

    def test_parity_row_covers_every_host(self) -> None:
        data = json.loads((ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8"))
        row = next(c for c in data["capabilities"] if c["id"] == "architect_skill")
        self.assertEqual({SKILL}, {path for host in data["hosts"] for path in row[host]})

    def test_skill_opens_with_both_retrieves(self) -> None:
        body = (ROOT / SKILL).read_text(encoding="utf-8").split("## Tier", 1)[0]
        self.assertIn("--store graphify", body)
        self.assertIn("--store mempalace", body)


if __name__ == "__main__":
    unittest.main()
