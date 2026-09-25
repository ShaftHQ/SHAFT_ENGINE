"""Local OpenAI-compat loopback probe: ABSENT, UNHEALTHY, READY. Never starts servers."""

from __future__ import annotations

import importlib.util
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
PROBE = ROOT / "chaos-engine/skills/local-openai-compat/scripts/probe.py"

_SPEC = importlib.util.spec_from_file_location("local_openai_compat_probe", PROBE)
if _SPEC is None or _SPEC.loader is None:
    raise RuntimeError(f"unable to load probe module from {PROBE}")
probe = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(probe)


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

    def log_message(self, format, *args):  # noqa: A003
        return


class LocalServer:
    """Tiny loopback models server for probe contract tests."""

    def __init__(self, payload: bytes, status: int = 200, redirect: str | None = None):
        """Bind payload/status for one ephemeral listener."""
        self.payload = payload
        self.status = status
        self.redirect = redirect
        self.httpd = None
        self.thread = None

    def __enter__(self):
        """Start the listener and return a loopback `/v1/models` URL."""
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
        """Stop the listener."""
        self.httpd.shutdown()
        self.httpd.server_close()
        self.thread.join(timeout=2)
        return False


def run_cli(
    url: str,
    *,
    backend: str = "ollama",
    extra_env: dict | None = None,
    path: str | None = None,
    extra_argv: list[str] | None = None,
) -> subprocess.CompletedProcess:
    env = os.environ.copy()
    for key in (
        "OPENAI_BASE_URL",
        "OLLAMA_HOST",
        "OLLAMA_BASE_URL",
        "LMS_BASE_URL",
    ):
        env.pop(key, None)
    env["http_proxy"] = "http://127.0.0.1:1"
    env["https_proxy"] = "http://127.0.0.1:1"
    env["HTTP_PROXY"] = "http://127.0.0.1:1"
    env["HTTPS_PROXY"] = "http://127.0.0.1:1"
    if path is not None:
        env["PATH"] = path
    if extra_env:
        env.update(extra_env)
    command = [sys.executable, str(PROBE), "--backend", backend, "--url", url]
    if extra_argv:
        command.extend(extra_argv)
    return subprocess.run(  # nosec B603 - fixed interpreter and repository probe script.
        command,
        capture_output=True,
        text=True,
        check=False,
        env=env,
    )


class LocalOpenAICompatProbeTest(unittest.TestCase):
    def test_backend_defaults_are_fixed_loopback(self):
        self.assertEqual(probe.default_url("ollama"), "http://127.0.0.1:11434/v1/models")
        self.assertEqual(probe.default_url("lmstudio"), "http://127.0.0.1:1234/v1/models")
        self.assertEqual(probe.default_url("llamacpp"), "http://127.0.0.1:8080/v1/models")
        previous = os.environ.get("OPENAI_BASE_URL")
        os.environ["OPENAI_BASE_URL"] = "http://10.1.1.1/v1"
        try:
            args = probe.parse_args(["--backend", "ollama"])
        finally:
            if previous is None:
                os.environ.pop("OPENAI_BASE_URL", None)
            else:
                os.environ["OPENAI_BASE_URL"] = previous
        self.assertIsNone(args.url)
        self.assertEqual(args.backend, "ollama")

    def test_rejects_non_loopback_even_via_argv(self):
        for url in (
            "http://10.1.1.1/v1/models",
            "http://example.com/v1/models",
            "https://127.0.0.1:11434/v1/models",
            "http://127.0.0.1:11434/v1/chat",
            "http://user@127.0.0.1:11434/v1/models",
        ):
            with self.subTest(url=url):
                completed = run_cli(url)
                self.assertEqual(completed.returncode, 2, completed.stderr)
                self.assertNotIn("READY", completed.stdout)
                self.assertIn("non-loopback", completed.stderr)

    def test_absent_when_nothing_listens_and_cli_missing(self):
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

    def test_unhealthy_when_cli_exists_but_port_closed(self):
        import tempfile

        with tempfile.TemporaryDirectory() as temporary:  # nosec B108 - isolated test dir
            binary = Path(temporary) / "ollama"
            marker = Path(temporary) / "must-not-run"
            binary.write_text(
                f"#!/bin/sh\necho launched > {marker}\nexit 99\n",
                encoding="utf-8",
            )
            binary.chmod(binary.stat().st_mode | stat.S_IEXEC)
            completed = run_cli(
                "http://127.0.0.1:9/v1/models",
                path=f"{temporary}:/usr/bin:/bin",
            )
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

    def test_source_never_invokes_serve_or_subprocess(self):
        text = PROBE.read_text(encoding="utf-8")
        self.assertNotIn("subprocess", text)
        self.assertNotIn("ollama serve", text)
        self.assertNotIn("lms server", text)
        self.assertNotIn("ft launch", text)
        self.assertNotIn("ft serve", text)

    def test_all_backends_print_labeled_states(self):
        env = os.environ.copy()
        env["PATH"] = "/usr/bin:/bin"
        env["http_proxy"] = "http://127.0.0.1:1"
        env["https_proxy"] = "http://127.0.0.1:1"
        env["HTTP_PROXY"] = "http://127.0.0.1:1"
        env["HTTPS_PROXY"] = "http://127.0.0.1:1"
        completed = subprocess.run(  # nosec B603
            [sys.executable, str(PROBE), "--backend", "all"],
            capture_output=True,
            text=True,
            check=False,
            env=env,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        lines = completed.stdout.strip().splitlines()
        self.assertEqual(len(lines), 3)
        for backend, line in zip(probe.BACKEND_IDS, lines, strict=True):
            self.assertTrue(line.startswith(f"{backend} "))
            self.assertIn(line.split(" ", 1)[1], probe.STATES)

    def test_attest_json_when_ready(self):
        body = json.dumps({"data": [{"id": "secret-model"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(url, path="/usr/bin:/bin", extra_argv=["attest"])
        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["state"], "READY")
        self.assertEqual(payload["backend"], "ollama")
        self.assertFalse(payload["install"])
        self.assertFalse(payload["may_start_server"])
        self.assertFalse(payload["omniroute_required"])
        self.assertFalse(payload["freetoken_required"])
        self.assertNotIn("secret-model", completed.stdout)

    def test_models_lists_ids_only_when_ready(self):
        body = json.dumps({"data": [{"id": "coding-moe"}]}).encode("utf-8")
        with LocalServer(body) as url:
            completed = run_cli(
                url,
                backend="lmstudio",
                path="/usr/bin:/bin",
                extra_argv=["models", "--json"],
            )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["backend"], "lmstudio")
        self.assertEqual(payload["models"], ["coding-moe"])

    def test_skill_and_guide_declare_non_coupling(self):
        skill = (ROOT / "chaos-engine/skills/local-runtimes/references/local-openai-compat.md").read_text(
            encoding="utf-8"
        )
        guide = (ROOT / "chaos-engine/guides/local-openai-compat.md").read_text(encoding="utf-8")
        for text in (skill, guide):
            self.assertIn("Never", text)
            self.assertIn("OmniRoute", text)
            self.assertIn("FreeToken", text)
        self.assertIn("Not an OmniRoute plugin", skill)
        self.assertIn("not a FreeToken plugin", skill)


if __name__ == "__main__":
    unittest.main()
