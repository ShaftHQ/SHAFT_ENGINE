"""Prefetch kind into the runner tool cache before helm/kind-action."""

from __future__ import annotations

import hashlib
import os
import platform
import sys
import tempfile
import threading
import unittest
from contextlib import redirect_stderr, redirect_stdout
from http.server import BaseHTTPRequestHandler, HTTPServer
from io import StringIO
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import prefetch_kind_toolcache as prefetch  # noqa: E402


def _arch() -> str:
    machine = platform.machine().lower()
    if machine in {"x86_64", "amd64"}:
        return "amd64"
    if machine in {"aarch64", "arm64"}:
        return "arm64"
    raise unittest.SkipTest(f"unsupported test arch {platform.machine()}")


def _os_name() -> str:
    system = platform.system().lower()
    if system not in {"linux", "darwin", "windows"}:
        raise unittest.SkipTest(f"unsupported test os {platform.system()}")
    return system


def _asset() -> str:
    return f"kind-{_os_name()}-{_arch()}"


def _destination(tool_cache: Path) -> Path:
    binary = "kind.exe" if _os_name() == "windows" else "kind"
    return tool_cache / "kind" / "v0.33.0" / _arch() / "kind" / "bin" / binary


class PrefetchKindToolcacheTest(unittest.TestCase):
    def setUp(self):
        self._env = {
            key: os.environ.get(key)
            for key in ("RUNNER_TOOL_CACHE", "KIND_RELEASE_BASE_URL")
        }

    def tearDown(self):
        for key, previous in self._env.items():
            if previous is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = previous

    def _serve(self, handler: type[BaseHTTPRequestHandler]) -> HTTPServer:
        server = HTTPServer(("127.0.0.1", 0), handler)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        self.addCleanup(server.server_close)
        self.addCleanup(server.shutdown)
        return server

    def test_installs_executable_at_kind_action_path(self):
        payload = b"kind-binary-bytes"
        digest = hashlib.sha256(payload).hexdigest()
        asset = _asset()
        checksum = f"{digest}  {asset}\n".encode()

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):  # noqa: N802
                if self.path == f"/{asset}":
                    body = payload
                elif self.path == f"/{asset}.sha256sum":
                    body = checksum
                else:
                    self.send_response(404)
                    self.end_headers()
                    return
                self.send_response(200)
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, *_args):
                return

        server = self._serve(Handler)
        with tempfile.TemporaryDirectory() as tmp:
            tool_cache = Path(tmp)
            os.environ["RUNNER_TOOL_CACHE"] = str(tool_cache)
            os.environ["KIND_RELEASE_BASE_URL"] = (
                f"http://127.0.0.1:{server.server_address[1]}"
            )
            self.assertEqual("v0.33.0", prefetch.KIND_VERSION)
            self.assertEqual(
                "https://github.com/kubernetes-sigs/kind/releases/download/v0.33.0",
                prefetch.DEFAULT_RELEASE_BASE_URL,
            )
            code = prefetch.main()
            dest = _destination(tool_cache)
            self.assertEqual(0, code)
            self.assertEqual(payload, dest.read_bytes())
            self.assertTrue(os.access(dest, os.X_OK))
            self.assertEqual(0o755, dest.stat().st_mode & 0o777)

    def test_http_504_exits_1_with_response_code(self):
        state = {"hits": 0}

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):  # noqa: N802
                state["hits"] += 1
                self.send_response(504)
                self.send_header("Content-Length", "0")
                self.end_headers()

            def log_message(self, *_args):
                return

        server = self._serve(Handler)
        with tempfile.TemporaryDirectory() as tmp:
            tool_cache = Path(tmp)
            os.environ["RUNNER_TOOL_CACHE"] = str(tool_cache)
            os.environ["KIND_RELEASE_BASE_URL"] = (
                f"http://127.0.0.1:{server.server_address[1]}"
            )
            stdout, stderr = StringIO(), StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = prefetch.main()
            text = stdout.getvalue() + stderr.getvalue()
            self.assertEqual(1, code)
            self.assertIn("HTTP response code: 504", text)
            self.assertFalse(_destination(tool_cache).exists())
            self.assertEqual(1, state["hits"])

    def test_http_404_is_not_a_transfer_phrase(self):
        state = {"hits": 0}

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):  # noqa: N802
                state["hits"] += 1
                self.send_response(404)
                self.send_header("Content-Length", "0")
                self.end_headers()

            def log_message(self, *_args):
                return

        server = self._serve(Handler)
        with tempfile.TemporaryDirectory() as tmp:
            tool_cache = Path(tmp)
            os.environ["RUNNER_TOOL_CACHE"] = str(tool_cache)
            os.environ["KIND_RELEASE_BASE_URL"] = (
                f"http://127.0.0.1:{server.server_address[1]}"
            )
            stdout, stderr = StringIO(), StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = prefetch.main()
            text = stdout.getvalue() + stderr.getvalue()
            self.assertEqual(1, code)
            self.assertEqual(1, state["hits"])
            self.assertIn("download refused with HTTP 404", text)
            self.assertNotIn("HTTP response code:", text)
            self.assertNotIn("Could not GET", text)

    def test_bad_checksum_exits_without_binary_or_http_phrase(self):
        payload = b"not-the-published-kind"
        asset = _asset()
        checksum = f"{'ab' * 32}  {asset}\n".encode()

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):  # noqa: N802
                if self.path == f"/{asset}":
                    body = payload
                elif self.path == f"/{asset}.sha256sum":
                    body = checksum
                else:
                    self.send_response(404)
                    self.end_headers()
                    return
                self.send_response(200)
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, *_args):
                return

        server = self._serve(Handler)
        with tempfile.TemporaryDirectory() as tmp:
            tool_cache = Path(tmp)
            os.environ["RUNNER_TOOL_CACHE"] = str(tool_cache)
            os.environ["KIND_RELEASE_BASE_URL"] = (
                f"http://127.0.0.1:{server.server_address[1]}"
            )
            stdout, stderr = StringIO(), StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = prefetch.main()
            text = stdout.getvalue() + stderr.getvalue()
            self.assertEqual(1, code)
            self.assertIn("sha256 mismatch", text)
            self.assertNotIn("HTTP response code:", text)
            self.assertFalse(_destination(tool_cache).exists())
            self.assertFalse(any(tool_cache.rglob("kind*")))


if __name__ == "__main__":
    unittest.main()
