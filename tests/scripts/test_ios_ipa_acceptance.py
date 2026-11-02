"""Unit tests for scripts/mobile/ios_ipa_acceptance.py (#7724)."""

from __future__ import annotations

import plistlib
import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts.mobile import ios_ipa_acceptance as ipa


class IosIpaAcceptanceTest(unittest.TestCase):
    def test_iphoneos_passes(self):
        ok, message = ipa.evaluate_platform({"DTPlatformName": "iphoneos"})
        self.assertTrue(ok)
        self.assertIn("iphoneos", message)

    def test_iphonesimulator_fails(self):
        ok, message = ipa.evaluate_platform({"DTPlatformName": "iphonesimulator"})
        self.assertFalse(ok)
        self.assertIn("iphonesimulator", message)

    def test_missing_platform_fails(self):
        ok, message = ipa.evaluate_platform({})
        self.assertFalse(ok)
        self.assertIn("missing", message.lower())

    def test_blank_platform_fails(self):
        ok, _ = ipa.evaluate_platform({"DTPlatformName": "  "})
        self.assertFalse(ok)

    def test_ipa_archive_round_trip(self):
        plist_bytes = plistlib.dumps({"DTPlatformName": "iphoneos", "CFBundleIdentifier": "com.example.app"})
        with tempfile.TemporaryDirectory() as tmp:
            ipa_path = Path(tmp) / "app.ipa"
            with zipfile.ZipFile(ipa_path, "w") as archive:
                archive.writestr("Payload/Example.app/Info.plist", plist_bytes)
            raw = ipa.find_info_plist_in_ipa(ipa_path)
            data = ipa.load_plist_bytes(raw)
            ok, _ = ipa.evaluate_platform(data)
            self.assertTrue(ok)
            self.assertEqual(0, ipa.main(["--ipa", str(ipa_path)]))

    def test_simulator_ipa_cli_fails(self):
        plist_bytes = plistlib.dumps({"DTPlatformName": "iphonesimulator"})
        with tempfile.TemporaryDirectory() as tmp:
            ipa_path = Path(tmp) / "sim.ipa"
            with zipfile.ZipFile(ipa_path, "w") as archive:
                archive.writestr("Payload/Sim.app/Info.plist", plist_bytes)
            self.assertEqual(1, ipa.main(["--ipa", str(ipa_path)]))

    def test_plist_flag(self):
        with tempfile.TemporaryDirectory() as tmp:
            plist_path = Path(tmp) / "Info.plist"
            plist_path.write_bytes(plistlib.dumps({"DTPlatformName": "iphoneos"}))
            self.assertEqual(0, ipa.main(["--plist", str(plist_path)]))


if __name__ == "__main__":
    unittest.main()
