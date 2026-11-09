#!/usr/bin/env python3
"""Prefetch kind into the GitHub Actions runner tool cache."""

from __future__ import annotations

import hashlib
import os
import platform
import sys
import urllib.error
import urllib.request
from pathlib import Path

# helm/kind-action v1.15.2 default. Ubuntu_JUnit_E2E does not pass a version.
KIND_VERSION = "v0.33.0"
DEFAULT_RELEASE_BASE_URL = (
    "https://github.com/kubernetes-sigs/kind/releases/download/" + KIND_VERSION
)
_RETRYABLE_HTTP = frozenset({429, 500, 502, 503, 504})
_USER_AGENT = "shaft-engine-prefetch-kind/1"


def _kind_os(system: str) -> str:
    normalized = system.lower()
    if normalized in {"linux", "darwin", "windows"}:
        return normalized
    raise ValueError(normalized)


def _kind_arch(machine: str) -> str:
    normalized = machine.lower()
    if normalized in {"x86_64", "amd64"}:
        return "amd64"
    if normalized in {"aarch64", "arm64"}:
        return "arm64"
    raise ValueError(normalized)


def _destination(tool_cache: Path, system: str, arch: str) -> Path:
    binary = "kind.exe" if system == "windows" else "kind"
    return tool_cache / "kind" / KIND_VERSION / arch / "kind" / "bin" / binary


def _read_url(url: str) -> bytes | None:
    request = urllib.request.Request(url, headers={"User-Agent": _USER_AGENT})
    try:
        with urllib.request.urlopen(  # nosec B310 - pinned HTTPS release URL
            request, timeout=120
        ) as response:
            return response.read()
    except urllib.error.HTTPError as error:
        error.close()
        if error.code in _RETRYABLE_HTTP:
            print(f"HTTP response code: {error.code}", file=sys.stderr)
        else:
            # Not a transfer phrase. "Could not GET" would make build_retry repeat a 404.
            print(f"download refused with HTTP {error.code}", file=sys.stderr)
        return None
    except (urllib.error.URLError, TimeoutError, ConnectionResetError):
        print(f"Could not GET {url}", file=sys.stderr)
        return None


def main() -> int:
    """Download kind into the runner tool cache. One attempt; the caller retries."""
    tool_cache = os.environ.get("RUNNER_TOOL_CACHE", "")
    if not tool_cache:
        print("RUNNER_TOOL_CACHE is unset", file=sys.stderr)
        return 1
    try:
        system = _kind_os(platform.system())
        arch = _kind_arch(platform.machine())
    except ValueError as error:
        print(f"unsupported kind target: {error}", file=sys.stderr)
        return 1
    base = os.environ.get("KIND_RELEASE_BASE_URL", DEFAULT_RELEASE_BASE_URL).rstrip("/")
    asset = f"kind-{system}-{arch}"
    binary = _read_url(f"{base}/{asset}")
    if binary is None:
        return 1
    checksum_blob = _read_url(f"{base}/{asset}.sha256sum")
    if checksum_blob is None:
        return 1
    fields = checksum_blob.decode("utf-8", "replace").split()
    actual = hashlib.sha256(binary).hexdigest()
    if not fields or fields[0].lower() != actual:
        print("sha256 mismatch", file=sys.stderr)
        return 1
    dest = _destination(Path(tool_cache), system, arch)
    dest.parent.mkdir(parents=True, exist_ok=True)
    partial = dest.parent / f"{dest.name}.partial"
    partial.write_bytes(binary)
    os.chmod(partial, 0o755)  # nosec B103 - kind-action skips install only when this is executable
    os.replace(partial, dest)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
