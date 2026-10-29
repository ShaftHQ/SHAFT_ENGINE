"""R01 app-recon craft card inventory + needles (method only; no vendored clone pack)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
CARD = DESIGN / "references/recon-craft/app-recon.md"
INDEX = DESIGN / "references/recon-craft/index.md"
CRAFT_INDEX = DESIGN / "references/craft/index.md"
INVENTORY = DESIGN / "INVENTORY.md"
MAX_LINES = 120
NEEDLES = ("app-recon", "features.csv", "complaint themes", "clean-room")


class AppReconCraftTests(unittest.TestCase):
    def test_card_under_budget_and_needles(self) -> None:
        lines = CARD.read_text(encoding="utf-8").splitlines()
        self.assertLessEqual(len(lines), MAX_LINES, len(lines))
        body = " ".join(lines).casefold()
        for needle in NEEDLES:
            self.assertIn(needle.casefold(), body, needle)
        self.assertIn("never copy", body)
        self.assertIn("tos", body)
        self.assertNotIn("/replica-build", body)
        self.assertNotIn("npx rea", body)

    def test_indexes_and_inventory_wire_r01(self) -> None:
        craft = CRAFT_INDEX.read_text(encoding="utf-8")
        self.assertIn("recon-craft", craft)
        self.assertIn("app-recon", craft)
        index = INDEX.read_text(encoding="utf-8")
        self.assertIn("R01", index)
        self.assertIn("app-recon.md", index)
        for needle in NEEDLES:
            self.assertIn(needle, index)
        inv = " ".join(INVENTORY.read_text(encoding="utf-8").split())
        self.assertIn("1 recon-craft card (R01, references/recon-craft)", inv)
        self.assertIn("replica-skill", inv)
        self.assertIn(
            "77c9436fb3d18c3d58169efb8caf4fe906b0dc51", inv,
        )
        self.assertIn("method re-expressed only", inv)
        self.assertIn("Replica eleven-skill clone pipeline", inv)

    def test_addon_description_mentions_recon_craft(self) -> None:
        addon = (DESIGN / "addon.json").read_text(encoding="utf-8")
        self.assertIn("recon-craft", addon)


if __name__ == "__main__":
    unittest.main()
