"""Orchestrator must not implement media generation (#7506 L6)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _flat(path: Path) -> str:
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8")).lower()


class DesignerOwnsGenerationTest(unittest.TestCase):
    def test_orchestrator_and_design_loop_forbid_self_generation(self) -> None:
        roles = _flat(ROOT / "chaos-engine/references/roles.md")
        design_loop = _flat(ROOT / "chaos-engine/references/design-loop.md")
        pdca = _flat(ROOT / "chaos-engine/references/multi-agent-pdca.md")
        for body in (roles, design_loop, pdca):
            self.assertIn("media generation", body)
        self.assertIn("never implements media generation", roles)
        self.assertIn("own executors", roles)
        self.assertIn("design-capable implementer", design_loop)
        self.assertIn("designer", pdca)


if __name__ == "__main__":
    unittest.main()
