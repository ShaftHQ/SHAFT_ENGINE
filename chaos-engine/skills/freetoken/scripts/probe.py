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


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--url",
        default=DEFAULT_URL,
        help="loopback models URL (default: %(default)s)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    if not loopback_models_url(args.url):
        print("non-loopback FreeToken URL ignored", file=sys.stderr)
        return 2
    # Ambient FREETOKEN_BASE_URL is intentionally unread. The probe host is fixed
    # unless a test passes an explicit loopback --url.
    print(probe(args.url))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
