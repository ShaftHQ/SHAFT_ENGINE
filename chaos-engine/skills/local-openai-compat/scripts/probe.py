#!/usr/bin/env python3
"""Probe optional local OpenAI-compat runtimes. Stdlib only. Never installs or starts them."""

from __future__ import annotations

import argparse
import json
import shutil
import sys
import urllib.error
import urllib.request
from urllib.parse import urlsplit

TIMEOUT_SECONDS = 2
LOOPBACK_HOSTS = frozenset({"127.0.0.1", "localhost", "::1"})
STATES = ("ABSENT", "UNHEALTHY", "READY")
BACKEND_IDS = ("ollama", "lmstudio", "llamacpp")

# Discovery defaults per product. CE never starts these servers.
BACKENDS: dict[str, dict[str, object]] = {
    "ollama": {
        "default_url": "http://127.0.0.1:11434/v1/models",
        "openai_base": "http://127.0.0.1:11434/v1",
        "binaries": ("ollama",),
    },
    "lmstudio": {
        "default_url": "http://127.0.0.1:1234/v1/models",
        "openai_base": "http://127.0.0.1:1234/v1",
        "binaries": ("lms",),
    },
    "llamacpp": {
        "default_url": "http://127.0.0.1:8080/v1/models",
        "openai_base": "http://127.0.0.1:8080/v1",
        "binaries": ("llama-server", "llama-cpp-server"),
    },
}


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    """Refuse redirects so a loopback probe cannot be sent elsewhere."""

    def redirect_request(self, req, fp, code, msg, headers, newurl):  # noqa: ANN001
        return None


def binary_present(backend: str) -> bool:
    """Return whether a known CLI for this backend is on PATH. Never executes it."""
    binaries = BACKENDS[backend]["binaries"]
    assert isinstance(binaries, tuple)
    return any(shutil.which(name) is not None for name in binaries)


def loopback_models_url(url: str) -> bool:
    """Accept only an http loopback `/v1/models` URL. Reject everything else."""
    parsed = urlsplit(url)
    if parsed.scheme != "http" or parsed.username or parsed.password:
        return False
    host = (parsed.hostname or "").lower()
    if host not in LOOPBACK_HOSTS:
        return False
    if parsed.path.rstrip("/") != "/v1/models":
        return False
    if parsed.query or parsed.fragment:
        return False
    return True


def is_models_payload(body: bytes) -> bool:
    """True for a JSON list or an object carrying a models/data array."""
    try:
        value = json.loads(body.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError):
        return False
    if isinstance(value, list):
        return True
    if isinstance(value, dict):
        return isinstance(value.get("models"), list) or isinstance(value.get("data"), list)
    return False


def fetch_models(url: str, timeout: float = TIMEOUT_SECONDS) -> tuple[bool, bytes]:
    """GET url with proxies and redirects disabled. answered, body."""
    request = urllib.request.Request(
        url,
        method="GET",
        headers={"Accept": "application/json"},
    )
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), _NoRedirect())
    try:
        with opener.open(request, timeout=timeout) as response:
            return True, response.read(65536)
    except urllib.error.HTTPError as error:
        body = error.read(65536) if error.fp is not None else b""
        return True, body
    except (urllib.error.URLError, TimeoutError, OSError):
        return False, b""


def classify(cli_on_path: bool, answered: bool, payload_ok: bool) -> str:
    if payload_ok:
        return "READY"
    if answered or cli_on_path:
        return "UNHEALTHY"
    return "ABSENT"


def default_url(backend: str) -> str:
    return str(BACKENDS[backend]["default_url"])


def openai_base(backend: str) -> str:
    return str(BACKENDS[backend]["openai_base"])


def probe_backend(
    backend: str,
    url: str | None = None,
    *,
    timeout: float = TIMEOUT_SECONDS,
) -> str:
    if backend not in BACKENDS:
        raise ValueError(f"unknown backend: {backend}")
    target = url or default_url(backend)
    if not loopback_models_url(target):
        raise ValueError("non-loopback local OpenAI-compat URL ignored")
    answered, body = fetch_models(target, timeout=timeout)
    return classify(binary_present(backend), answered, answered and is_models_payload(body))


def parse_model_ids(body: bytes) -> list[str]:
    """Extract model ids from a models payload for session stdout only."""
    value = json.loads(body.decode("utf-8"))
    rows: list = []
    if isinstance(value, list):
        rows = value
    elif isinstance(value, dict):
        if isinstance(value.get("data"), list):
            rows = value["data"]
        elif isinstance(value.get("models"), list):
            rows = value["models"]
    ids: list[str] = []
    for row in rows:
        if isinstance(row, str):
            ids.append(row)
        elif isinstance(row, dict):
            mid = row.get("id") or row.get("model") or row.get("name")
            if isinstance(mid, str) and mid:
                ids.append(mid)
    return ids


def resolve_backends(backend: str) -> tuple[str, ...]:
    if backend == "all":
        return BACKEND_IDS
    if backend not in BACKENDS:
        raise ValueError(f"unknown backend: {backend}")
    return (backend,)


def cmd_probe(args: argparse.Namespace) -> int:
    backends = resolve_backends(args.backend)
    if args.url is not None and len(backends) != 1:
        print("--url requires a single --backend", file=sys.stderr)
        return 2
    if args.url is not None and not loopback_models_url(args.url):
        print("non-loopback local OpenAI-compat URL ignored", file=sys.stderr)
        return 2
    results: list[tuple[str, str]] = []
    for backend in backends:
        url = args.url if args.url is not None else default_url(backend)
        if not loopback_models_url(url):
            print("non-loopback local OpenAI-compat URL ignored", file=sys.stderr)
            return 2
        results.append((backend, probe_backend(backend, url)))
    if len(results) == 1:
        print(results[0][1])
    else:
        for backend, state in results:
            print(f"{backend} {state}")
    return 0


def cmd_attest(args: argparse.Namespace) -> int:
    backends = resolve_backends(args.backend)
    if args.url is not None and len(backends) != 1:
        print("--url requires a single --backend", file=sys.stderr)
        return 2
    rows = []
    ready_any = False
    for backend in backends:
        url = args.url if args.url is not None else default_url(backend)
        if not loopback_models_url(url):
            print("non-loopback local OpenAI-compat URL ignored", file=sys.stderr)
            return 2
        state = probe_backend(backend, url)
        ready_any = ready_any or state == "READY"
        rows.append(
            {
                "backend": backend,
                "state": state,
                "cli_on_path": binary_present(backend),
                "openai_base_url": openai_base(backend),
                "models_url": url if args.url is not None else default_url(backend),
            }
        )
    payload = {
        "backends": rows,
        "install": False,
        "may_start_server": False,
        "omniroute_required": False,
        "freetoken_required": False,
    }
    if len(rows) == 1:
        payload["state"] = rows[0]["state"]
        payload["backend"] = rows[0]["backend"]
        payload["openai_base_url"] = rows[0]["openai_base_url"]
        payload["models_url"] = rows[0]["models_url"]
    print(json.dumps(payload, sort_keys=True))
    return 0 if ready_any else 1


def cmd_models(args: argparse.Namespace) -> int:
    backends = resolve_backends(args.backend)
    if len(backends) != 1:
        print("models requires a single --backend", file=sys.stderr)
        return 2
    backend = backends[0]
    url = args.url if args.url is not None else default_url(backend)
    if not loopback_models_url(url):
        print("non-loopback local OpenAI-compat URL ignored", file=sys.stderr)
        return 2
    answered, body = fetch_models(url)
    state = classify(binary_present(backend), answered, answered and is_models_payload(body))
    if state != "READY":
        print(state)
        return 1
    ids = parse_model_ids(body)
    if args.json:
        print(json.dumps({"backend": backend, "state": state, "models": ids}, sort_keys=True))
    else:
        for mid in ids:
            print(mid)
    return 0


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--backend",
        default="all",
        choices=(*BACKEND_IDS, "all"),
        help="runtime profile (default: %(default)s)",
    )
    parser.add_argument(
        "--url",
        default=None,
        help="loopback models URL override (single --backend only)",
    )
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("probe", help="print ABSENT|UNHEALTHY|READY (default)")
    sub.add_parser("attest", help="JSON readiness attestation for dispatch")
    models_p = sub.add_parser("models", help="list model ids when READY (session only)")
    models_p.add_argument("--json", action="store_true", help="JSON object with models array")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    # Ambient OPENAI_BASE_URL / OLLAMA_HOST / LMS_* are intentionally unread.
    command = args.command or "probe"
    if command == "probe":
        return cmd_probe(args)
    if command == "attest":
        return cmd_attest(args)
    if command == "models":
        return cmd_models(args)
    print(f"unknown command: {command}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
