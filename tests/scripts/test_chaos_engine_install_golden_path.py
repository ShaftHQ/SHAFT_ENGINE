"""INSTALL/README first-run golden path stays above the fold (#5577)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class InstallGoldenPathTests(unittest.TestCase):
    def test_install_golden_path_precedes_advanced_topics(self):
        text = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("## Golden path (first run)", text)
        self.assertIn("## Advanced topics", text)
        self.assertLess(text.index("## Golden path (first run)"), text.index("## Advanced topics"))
        self.assertLess(text.index("## Golden path (first run)"), text.index("## Uninstall / rollback"))
        # Reader can install without reading past golden path: one-liners appear before Advanced.
        self.assertLess(text.index("install.ps1"), text.index("## Advanced topics"))
        self.assertLess(text.index("install.sh"), text.index("## Advanced topics"))

    def test_readme_points_at_golden_path(self):
        text = (ROOT / "chaos-engine/README.md").read_text(encoding="utf-8")
        self.assertIn("Golden path", text)
        self.assertIn("INSTALL.md#golden-path-first-run", text)


if __name__ == "__main__":
    unittest.main()

