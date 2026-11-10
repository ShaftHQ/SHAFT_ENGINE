"""#7895: a writer that finished required citations is reseeded once."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN_LOOP = ROOT / "chaos-engine/references/design-loop.md"


class DesignLoopWriterReseedTest(unittest.TestCase):
    def test_absent_design_file_reseeds_once_with_citations(self) -> None:
        text = DESIGN_LOOP.read_text(encoding="utf-8")
        self.assertIn("reseeds once with those citations", text)
        self.assertIn("write the files before any further search", text)
        self.assertIn("no numeric tool-call cap", text)
        self.assertIn("A second absence is reported to the owner", text)
        self.assertIn("still reading a file the prompt named is not stalled", text)
        self.assertIn("2 fix rounds", text)
        self.assertIn("design-capable implementer", text)


if __name__ == "__main__":
    unittest.main()
