#!/usr/bin/env python3
"""Probe optional local FreeToken. Stdlib only. Never installs or starts it."""

from __future__ import annotations

import argparse
import json
import shutil
import sys
import urllib.error
import urllib.request
from urllib.parse import urlsplit

DEFAULT_URL = "http://127.0.0.1:1919/v1/models"
DEFAULT_OPENAI_BASE = "http://127.0.0.1:1919/v1"
DEFAULT_ANTHROPIC_BASE = "http://127.0.0.1:1919/v1/messages"
TIMEOUT_SECONDS = 2
LOOPBACK_HOSTS = frozenset({"127.0.0.1", "localhost", "::1"})
STATES = ("ABSENT", "UNHEALTHY", "READY")


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    """Refuse redirects so a loopback probe cannot be sent elsewhere."""

    def redirect_request(self, req, fp, code, msg, headers, newurl):  # noqa: ANN001
        return None


def ft_present() -> bool:
    """Return whether `ft` is on PATH. Equivalent to `command -v ft`; never executes it."""
    return shutil.which("ft") is not None


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


def classify(ft_on_path: bool, answered: bool, payload_ok: bool) -> str:
    if payload_ok:
        return "READY"
    if answered or ft_on_path:
        return "UNHEALTHY"
    return "ABSENT"


def probe(url: str = DEFAULT_URL, *, timeout: float = TIMEOUT_SECONDS) -> str:
    if not loopback_models_url(url):
        raise ValueError("non-loopback FreeToken URL ignored")
    answered, body = fetch_models(url, timeout=timeout)
    return classify(ft_present(), answered, answered and is_models_payload(body))


def parse_model_ids(body: bytes) -> list[str]:
    """Extract model ids from a models payload for session stdout only."""
    return [item["id"] for item in parse_model_rows(body) if "id" in item]


def parse_model_rows(body: bytes) -> list[dict[str, object]]:
    """Session-only model rows. Echo advertised context_length when present; never invent."""
    value = json.loads(body.decode("utf-8"))
    rows: list = []
    if isinstance(value, list):
        rows = value
    elif isinstance(value, dict):
        if isinstance(value.get("data"), list):
            rows = value["data"]
        elif isinstance(value.get("models"), list):
            rows = value["models"]
    out: list[dict[str, object]] = []
    for row in rows:
        if isinstance(row, str):
            if row:
                out.append({"id": row})
            continue
        if not isinstance(row, dict):
            continue
        mid = row.get("id") or row.get("model") or row.get("name")
        if not isinstance(mid, str) or not mid:
            continue
        item: dict[str, object] = {"id": mid}
        advertised = row.get("context_length")
        if advertised is None:
            advertised = row.get("max_model_len")
        if isinstance(advertised, int) and not isinstance(advertised, bool) and advertised > 0:
            item["context_length"] = advertised
        out.append(item)
    return out


def cmd_probe(args: argparse.Namespace) -> int:
    if not loopback_models_url(args.url):
        print("non-loopback FreeToken URL ignored", file=sys.stderr)
        return 2
    print(probe(args.url))
    return 0


def cmd_attest(args: argparse.Namespace) -> int:
    state = probe(args.url)
    payload = {
        "state": state,
        "ft_on_path": ft_present(),
        "openai_base_url": DEFAULT_OPENAI_BASE,
        "anthropic_base_url": DEFAULT_ANTHROPIC_BASE,
        "models_url": DEFAULT_URL,
        "install": False,
        "may_ft_launch": False,
        "may_ft_serve": False,
        "omniroute_required": False,
    }
    print(json.dumps(payload, sort_keys=True))
    return 0 if state == "READY" else 1


def cmd_models(args: argparse.Namespace) -> int:
    if not loopback_models_url(args.url):
        print("non-loopback FreeToken URL ignored", file=sys.stderr)
        return 2
    answered, body = fetch_models(args.url)
    state = classify(ft_present(), answered, answered and is_models_payload(body))
    if state != "READY":
        print(state)
        return 1
    rows = parse_model_rows(body)
    if args.json:
        print(json.dumps({"state": state, "models": rows}, sort_keys=True))
    else:
        for row in rows:
            print(row["id"])
    return 0


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--url",
        default=DEFAULT_URL,
        help="loopback models URL (default: %(default)s)",
    )
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("probe", help="print ABSENT|UNHEALTHY|READY (default)")
    sub.add_parser("attest", help="JSON readiness attestation for dispatch")
    models_p = sub.add_parser("models", help="list model ids when READY (session only)")
    models_p.add_argument("--json", action="store_true", help="JSON object with models array")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    # Ambient FREETOKEN_BASE_URL is intentionally unread.
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
