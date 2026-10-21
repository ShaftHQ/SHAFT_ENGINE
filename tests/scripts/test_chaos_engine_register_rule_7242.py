"""#7242 chunk 5: Mohab's register rule is pinned in the always-on caveman card and the tactics card."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


class RegisterRuleTests(unittest.TestCase):
    def test_caveman_card_carries_both_registers(self):
        card = (CE / "companions/caveman-ultra.md").read_text(encoding="utf-8")
        self.assertIn("Register, ultra: bot-to-bot messages", card)
        self.assertIn("thinking and notes; keep full reasoning", card)
        self.assertIn("Register, humans: short plain full sentences", card)
        self.assertIn("not telegraphic", card)

    def test_tactics_card_carries_the_register_rule(self):
        card = (CE / "companions/token-tactics.md").read_text(encoding="utf-8")
        self.assertIn("Caveman and Ponytail ultra for bot-to-bot messages", card)
        self.assertIn("keeping\n  the full reasoning", card)
        self.assertIn("Humans get short plain full sentences", card)

    def test_harness_maintenance_links_the_tactics_card(self):
        entry = (CE / "references/harness-maintenance.md").read_text(encoding="utf-8")
        self.assertIn("../companions/token-tactics.md", entry)


if __name__ == "__main__":
    unittest.main()
