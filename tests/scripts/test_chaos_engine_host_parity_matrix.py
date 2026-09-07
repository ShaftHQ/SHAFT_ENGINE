"""Host Parity Matrix v0 artifact (#5578)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"


class HostParityMatrixTests(unittest.TestCase):
    def test_matrix_covers_five_hosts_and_lists_gaps(self):
        text = MATRIX.read_text(encoding="utf-8")
        self.assertTrue(MATRIX.is_file())
        for host in ("Claude Code", "Codex", "Grok", "Gemini", "Copilot"):
            self.assertIn(host, text)
        for surface in (
            "Install / doctor human UX",
            "Router skill",
            "Lifecycle hooks",
            "Exit-2",
            "SessionStart",
            "Companions",
            "Headroom",
            "Self-improve",
            "Retrieval soft-degrade",
            "Learning Session",
        ):
            self.assertIn(surface, text)
        self.assertIn("Measured gaps", text)
        self.assertIn("GAP-EXIT2", text)
        self.assertIn("severity", text.casefold())


if __name__ == "__main__":
    unittest.main()

