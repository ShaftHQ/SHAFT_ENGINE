"""Unit tests for scripts/mobile/android_apk_select.py (#7725)."""

from __future__ import annotations

import unittest

from scripts.mobile.android_apk_select import ApkCandidate, select_apk


class AndroidApkSelectTest(unittest.TestCase):
    def test_x86_64_prefers_universal(self):
        chosen = select_apk(
            "x86_64",
            [
                ApkCandidate("app-arm64.apk", "arm64-v8a"),
                ApkCandidate("app-universal.apk", "universal"),
            ],
        )
        self.assertEqual(chosen.path, "app-universal.apk")

    def test_x86_64_rejects_arm64_only(self):
        with self.assertRaises(ValueError) as ctx:
            select_apk("x86_64", [ApkCandidate("app-arm64.apk", "arm64-v8a")])
        self.assertIn("arm64-only", str(ctx.exception))

    def test_x86_64_accepts_x86_64_split(self):
        chosen = select_apk("x86_64", [ApkCandidate("app-x86.apk", "x86_64")])
        self.assertEqual(chosen.kind, "x86_64")

    def test_arm64_prefers_arm64_split(self):
        chosen = select_apk(
            "arm64-v8a",
            [
                ApkCandidate("app-universal.apk", "universal"),
                ApkCandidate("app-arm64.apk", "arm64-v8a"),
            ],
        )
        self.assertEqual(chosen.path, "app-arm64.apk")

    def test_arm64_falls_back_to_universal(self):
        chosen = select_apk("arm64-v8a", [ApkCandidate("app-universal.apk", "universal")])
        self.assertEqual(chosen.kind, "universal")

    def test_empty_candidates_raise(self):
        with self.assertRaises(ValueError):
            select_apk("x86_64", [])


if __name__ == "__main__":
    unittest.main()
