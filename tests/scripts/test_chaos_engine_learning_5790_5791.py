"""Contract tests for ChaosEngine learning tickets #5790 and #5791."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/references/installer-program.md"
HOST_PARITY = ROOT / "chaos-engine/references/host-parity-matrix.md"
GOTCHAS = ROOT / ".memory/memory/gotchas"
SLUG_5790 = "auto-upgrade-recognized-ce-owned-marker-spans-on-install"
SLUG_5791 = "grok-hook-trust-is-host-gated-sync-advisory-not-install-recovery"


class Learning5790Test(unittest.TestCase):
    def test_installer_program_cites_recognized_legacy_auto_upgrade(self) -> None:
        text = INSTALLER.read_text(encoding="utf-8")
        self.assertIn("recognized legacy owned block", text)
        self.assertIn("Recognized-legacy fingerprints auto-upgrade", text)
        self.assertIn("#5790", text)

    def test_gotcha_md_and_json_exist_and_cite_issue(self) -> None:
        md = GOTCHAS / f"{SLUG_5790}.md"
        js = GOTCHAS / f"{SLUG_5790}.json"
        self.assertTrue(md.is_file(), md)
        self.assertTrue(js.is_file(), js)
        body = md.read_text(encoding="utf-8")
        self.assertIn("#5790", body)
        sidecar = js.read_text(encoding="utf-8")
        self.assertIn("#5790", sidecar)
        self.assertIn(f"gotcha.{SLUG_5790}", sidecar)


class Learning5791Test(unittest.TestCase):
    def test_host_parity_gap_hook_trust_is_sync_advisory(self) -> None:
        text = HOST_PARITY.read_text(encoding="utf-8")
        self.assertIn("GAP-HOOK-TRUST", text)
        self.assertIn("sync-advisory", text)
        self.assertIn("/hooks-trust", text)
        self.assertIn("#5791", text)
        # Learning: never treat Grok trust alone as install recovery.
        self.assertIn("recovery-required", text)
        self.assertIn("Grok trust alone", text)

    def test_gotcha_md_and_json_exist_and_cite_issue(self) -> None:
        md = GOTCHAS / f"{SLUG_5791}.md"
        js = GOTCHAS / f"{SLUG_5791}.json"
        self.assertTrue(md.is_file(), md)
        self.assertTrue(js.is_file(), js)
        body = md.read_text(encoding="utf-8")
        self.assertIn("#5791", body)
        self.assertIn("sync-advisory", body)
        sidecar = js.read_text(encoding="utf-8")
        self.assertIn("#5791", sidecar)
        self.assertIn(f"gotcha.{SLUG_5791}", sidecar)


if __name__ == "__main__":
    unittest.main()
