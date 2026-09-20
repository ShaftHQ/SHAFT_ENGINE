"""Colibri loopback probe: ABSENT, UNHEALTHY, READY. Never starts ft."""

from __future__ import annotations

import json
import os
import stat
import subprocess  # nosec B404 - tests drive the fixed local probe with controlled argv.
import sys
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PROBE = ROOT / "chaos-engine/skills/colibri/scripts/probe.py"

sys.path.insert(0, str(PROBE.parent))
import probe  # noqa: E402


class _Handler(BaseHTTPRequestHandler):
    payload = b""
    status = 200
    redirect = None

    def do_GET(self):  # noqa: N802
        if self.redirect:
            self.send_response(302)
            self.send_header("Location", self.redirect)
            self.end_headers()
            return
        body = self.payload
        self.send_response(self.status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):  # noqa: A003  # pylint: disable=redefined-builtin
        """Silence request logs."""
        return


class LocalServer:
    def __init__(self, payload: bytes, status: int = 200, redirect: str | None = None):
        """Bind payload for a one-shot loopback models server."""
        self.payload = payload
        self.status = status
        self.redirect = redirect
        self.httpd = None
        self.thread = None

    def __enter__(self):
        """Start the loopback server thread."""
        handler = type(
            "Handler",
            (_Handler,),
            {"payload": self.payload, "status": self.status, "redirect": self.redirect},
        )
        self.httpd = ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)
        self.thread.start()
        host, port = self.httpd.server_address
        return f"http://{host}:{port}/v1/models"

    def __exit__(self, exc_type, exc, tb):
        """Stop the loopback server thread."""
        self.httpd.shutdown()
        self.httpd.server_close()
        self.thread.join(timeout=2)
        return False


def run_cli(
    url: str,
    extra_env: dict | None = None,
    path: str | None = None,
    extra_argv: list[str] | None = None,
) -> subprocess.CompletedProcess:
    env = os.environ.copy()
    env.pop("COLIBRI_BASE_URL", None)
    env["http_proxy"] = "http://127.0.0.1:1"
    env["https_proxy"] = "http://127.0.0.1:1"
    env["HTTP_PROXY"] = "http://127.0.0.1:1"
    env["HTTPS_PROXY"] = "http://127.0.0.1:1"
    if path is not None:
        env["PATH"] = path
    if extra_env:
        env.update(extra_env)
    command = [sys.executable, str(PROBE), "--url", url]
    if extra_argv:
        command.extend(extra_argv)
    return subprocess.run(  # nosec B603 - fixed interpreter and repository probe script.
        command,
        capture_output=True,
        text=True,
        check=False,
        env=env,
    )


class ColibriProbeTest(unittest.TestCase):
    def test_default_url_is_fixed_loopback_and_ignores_ambient_override(self):
        self.assertEqual(probe.DEFAULT_URL, "http://127.0.0.1:8000/v1/models")
        previous = os.environ.get("COLIBRI_BASE_URL")
        os.environ["COLIBRI_BASE_URL"] = "http://10.1.1.1/v1/models"
        try:
            args = probe.parse_args([])
        finally:
            if previous is None:
                os.environ.pop("COLIBRI_BASE_URL", None)
            else:
                os.environ["COLIBRI_BASE_URL"] = previous
        self.assertEqual(args.url, probe.DEFAULT_URL)

    def test_rejects_non_loopback_even_via_argv(self):
        for url in (
            "http://10.1.1.1/v1/models",
            "http://example.com/v1/models",
            "https://127.0.0.1:8000/v1/models",
            "http://127.0.0.1:8000/v1/chat",
            "http://user@127.0.0.1:8000/v1/models",
        ):
            with self.subTest(url=url):
                completed = run_cli(url)
                self.assertEqual(completed.returncode, 2, completed.stderr)
                self.assertNotIn("READY", completed.stdout)
                self.assertIn("non-loopback", completed.stderr)

    def test_absent_when_nothing_listens_and_ft_missing(self):
        completed = run_cli("http://127.0.0.1:9/v1/models", path="/usr/bin:/bin")
        self.assertEqual(completed.returncode, 0)
        self.assertEqual(completed.stdout.strip(), "ABSENT")

    def test_unhealthy_http_500_or_non_json(self):
        with LocalServer(b"nope", status=500) as url:
            completed = run_cli(url, path="/usr/bin:/bin")
        self.assertEqual(completed.returncode, 0)
        self.assertEqual(completed.stdout.strip(), "UNHEALTHY")

        with LocalServer(b"not-json", status=200) as url:
            completed = run_cli(url, path="/usr/bin:/bin")
        self.assertEqual(completed.stdout.strip(), "UNHEALTHY")

    def test_unhealthy_when_ft_exists_but_port_closed(self):
        import tempfile

        with tempfile.TemporaryDirectory() as temporary:
            marker = Path(temporary) / "must-not-run"
            binary = Path(temporary) / "coli"
            binary.write_text(
                f"#!/bin/sh\necho launched > {marker}\nexit 99\n",
                encoding="utf-8",
            )
            binary.chmod(binary.stat().st_mode | stat.S_IEXEC)
            completed = run_cli("http://127.0.0.1:9/v1/models", path=f"{temporary}:/usr/bin:/bin")
            self.assertEqual(completed.returncode, 0)
            self.assertEqual(completed.stdout.strip(), "UNHEALTHY")
            self.assertFalse(marker.exists())

    def test_ready_models_payload_and_does_not_print_model_ids(self):
        body = json.dumps({"data": [{"id": "x"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin")
        self.assertEqual(completed.returncode, 0)
        self.assertEqual(completed.stdout.strip(), "READY")
        self.assertNotIn("x", completed.stdout)
        self.assertNotIn("x", completed.stderr)

    def test_ready_accepts_models_array_object(self):
        self.assertTrue(probe.is_models_payload(b'{"models":[{"id":"hidden"}]}'))
        self.assertTrue(probe.is_models_payload(b'[{"id":"hidden"}]'))
        self.assertFalse(probe.is_models_payload(b'{"error":"no"}'))

    def test_redirect_is_rejected_as_unhealthy(self):
        with LocalServer(b"", redirect="http://127.0.0.1:9/v1/models") as url:
            completed = run_cli(url, path="/usr/bin:/bin")
        self.assertEqual(completed.stdout.strip(), "UNHEALTHY")

    def test_source_never_invokes_ft_launch_or_serve(self):
        text = PROBE.read_text(encoding="utf-8")
        self.assertNotIn("ft launch", text)
        self.assertNotIn("ft serve", text)
        self.assertNotIn("subprocess", text)



    def test_attest_json_when_ready(self):
        body = json.dumps({"data": [{"id": "secret-model"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin", extra_argv=["attest"])
        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["state"], "READY")
        self.assertFalse(payload["install"])
        self.assertFalse(payload["may_coli_serve"])
        self.assertFalse(payload["omniroute_required"])
        self.assertNotIn("secret-model", completed.stdout)

    def test_models_lists_ids_only_when_ready(self):
        body = json.dumps({"data": [{"id": "coding-moe"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin", extra_argv=["models", "--json"])
        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["models"], [{"id": "coding-moe"}])

    def test_models_json_echoes_advertised_context_length_when_present(self):
        body = json.dumps(
            {"data": [{"id": "coding-moe", "context_length": 32768}]}
        ).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin", extra_argv=["models", "--json"])
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["models"], [{"id": "coding-moe", "context_length": 32768}])

    def test_models_json_omits_context_length_when_absent(self):
        body = json.dumps({"data": [{"id": "coding-moe"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin", extra_argv=["models", "--json"])
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["models"], [{"id": "coding-moe"}])
        self.assertNotIn("context_length", payload["models"][0])


if __name__ == "__main__":
    unittest.main()
