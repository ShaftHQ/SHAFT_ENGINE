#!/usr/bin/env python3
"""Unit tests for scripts/ci/fetch_mobile_test_apps.py (#7547 Phase 0)."""

from __future__ import annotations

import hashlib
import json
import sys
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import fetch_mobile_test_apps as fetch  # noqa: E402


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


class FetchMobileTestAppsTest(unittest.TestCase):
    def test_manifest_schema_and_pinned_digests(self):
        manifest = json.loads(
            (ROOT / "scripts/ci/mobile_test_apps.manifest.json").read_text(encoding="utf-8")
        )
        self.assertEqual(manifest["schemaVersion"], 1)
        self.assertEqual(manifest["releaseTag"], "test-fixtures-mobile-apps-v1")
        apps = {entry["asset"]: entry for entry in manifest["apps"]}
        self.assertEqual(
            apps["Android-NativeDemoApp-0.4.0.apk"]["sha256"],
            "feb1746050195ae82ba065f3c1c2b0c3a10e971e7619b523dbd7225c614b1299",
        )
        self.assertIn(
            "shaft-engine/src/test/resources/testDataFiles/apps/wdio-demo.apk",
            apps["Android-NativeDemoApp-0.4.0.apk"]["destinations"],
        )
        self.assertEqual(len(apps["ApiDemos-debug.apk"]["destinations"]), 3)

    def test_fetch_installs_from_cache_after_sha256_check(self):
        payload = b"shaft-mobile-fixture-bytes"
        digest = _sha256(payload)

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):  # noqa: N802
                if self.path.endswith("/demo.apk"):
                    body = payload
                    self.send_response(200)
                    self.send_header("Content-Length", str(len(body)))
                    self.end_headers()
                    self.wfile.write(body)
                    return
                self.send_response(404)
                self.end_headers()

            def log_message(self, *_args):
                return

        server = HTTPServer(("127.0.0.1", 0), Handler)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            with tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp) / "repo"
                cache = Path(tmp) / "cache"
                dest = "shaft-engine/src/test/resources/testDataFiles/apps/demo.apk"
                (root / "scripts" / "ci").mkdir(parents=True)
                manifest = {
                    "schemaVersion": 1,
                    "releaseTag": "test",
                    "baseUrl": f"http://127.0.0.1:{server.server_address[1]}",
                    "apps": [
                        {
                            "asset": "demo.apk",
                            "sha256": digest,
                            "destinations": [dest],
                        }
                    ],
                }
                manifest_path = root / "scripts/ci/mobile_test_apps.manifest.json"
                manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

                report = fetch.fetch_all(
                    root=root, manifest_path=manifest_path, cache_dir=cache
                )
                installed = root / dest
                self.assertTrue(installed.is_file())
                self.assertEqual(_sha256(installed.read_bytes()), digest)
                self.assertEqual(report["apps"][0]["changed"], [dest])

                # Second run is a no-op when the digest already matches.
                report2 = fetch.fetch_all(
                    root=root, manifest_path=manifest_path, cache_dir=cache
                )
                self.assertEqual(report2["apps"][0]["changed"], [])
        finally:
            server.shutdown()

    def test_sha256_mismatch_refuses_install(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "repo"
            cache = Path(tmp) / "cache"
            (root / "scripts" / "ci").mkdir(parents=True)
            # Seed cache with wrong bytes under the expected digest name so
            # _ensure_cached re-downloads; point baseUrl at a dead port.
            bad = {
                "schemaVersion": 1,
                "releaseTag": "test",
                "baseUrl": "http://127.0.0.1:1",
                "apps": [
                    {
                        "asset": "missing.apk",
                        "sha256": "0" * 64,
                        "destinations": ["apps/missing.apk"],
                    }
                ],
            }
            path = root / "scripts/ci/mobile_test_apps.manifest.json"
            path.write_text(json.dumps(bad), encoding="utf-8")
            with self.assertRaises(fetch.FetchError):
                fetch.fetch_all(root=root, manifest_path=path, cache_dir=cache)


if __name__ == "__main__":
    unittest.main()
