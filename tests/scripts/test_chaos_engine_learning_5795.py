"""Contract tests for ChaosEngine learning ticket #5795."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/references/installer-program.md"
GOTCHAS = ROOT / ".memory/memory/gotchas"
SLUG = "repository-install-must-sync-overlay-from-local-source-before-doctor"


class Learning5795Test(unittest.TestCase):
    def test_installer_program_cites_repository_overlay_source_sync(self) -> None:
        text = INSTALLER.read_text(encoding="utf-8")
        self.assertIn("Repository overlay SOURCE sync", text)
        self.assertIn("owned_source_files()", text)
        self.assertIn("overlay-source-mismatch", text)
        self.assertIn("overlay-handoff.md", text)
        self.assertIn("#5795", text)
        self.assertIn("#5794", text)

    def test_gotcha_md_and_json_exist_and_cite_issue(self) -> None:
        md = GOTCHAS / f"{SLUG}.md"
        js = GOTCHAS / f"{SLUG}.json"
        self.assertTrue(md.is_file(), md)
        self.assertTrue(js.is_file(), js)
        body = md.read_text(encoding="utf-8")
        self.assertIn("#5795", body)
        self.assertIn("overlay-source-mismatch", body)
        self.assertIn("owned_source_files()", body)
        sidecar = js.read_text(encoding="utf-8")
        self.assertIn("#5795", sidecar)
        self.assertIn(f"gotcha.{SLUG}", sidecar)


if __name__ == "__main__":
    unittest.main()
