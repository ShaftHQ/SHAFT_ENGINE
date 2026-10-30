#!/usr/bin/env python3
"""Fetch pinned mobile test apk/ipa fixtures (#7547 Phase 0).

Reads ``scripts/ci/mobile_test_apps.manifest.json``, downloads each release
asset by URL into a content-addressed cache, verifies sha256, then copies the
bytes into the declared destination paths. Existing destinations that already
match the pinned digest are left untouched.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess  # nosec B404 - fixed git ls-files argv only
import sys
import tempfile
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

MANIFEST_REL = Path("scripts/ci/mobile_test_apps.manifest.json")
DEFAULT_CACHE = Path(
    os.environ.get(
        "SHAFT_MOBILE_APP_CACHE",
        str(Path.home() / ".cache" / "shaft-engine" / "mobile-test-apps"),
    )
)
_USER_AGENT = "shaft-engine-fetch-mobile-test-apps/1"


class FetchError(RuntimeError):
    """Pinned fixture could not be fetched or verified."""


# Fixture destinations externalized in #7555. Force-adding these undoes Phase 0.
TRACKED_BINARY_PATHSPECS = (
    "shaft-engine/src/test/resources/testDataFiles/apps/*.apk",
    "shaft-engine/src/test/resources/testDataFiles/apps/*.ipa",
    ":(glob)shaft-engine/src/main/resources/examples/**/testDataFiles/apk/*.apk",
    ":(glob)shaft-engine/src/main/resources/examples/**/testDataFiles/apk/*.ipa",
)


def tracked_mobile_binaries(root: Path | None = None) -> list[str]:
    """Return tracked apk/ipa paths under the Phase 0 fixture destinations."""
    repo = Path(root) if root is not None else _repo_root()
    git = shutil.which("git")
    if not git:
        raise FetchError("git is required to check tracked mobile binaries")
    completed = subprocess.run(  # nosec B603 - absolute git from shutil.which, no shell
        [git, "-C", str(repo), "ls-files", "-z", "--", *TRACKED_BINARY_PATHSPECS],
        check=False,
        capture_output=True,
    )
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout or b"").decode("utf-8", "replace").strip()
        raise FetchError(f"git ls-files failed while checking tracked mobile binaries: {detail}")
    return [item for item in completed.stdout.decode("utf-8").split("\0") if item]


def tracked_mobile_binary_failures(root: Path | None = None) -> list[str]:
    """Fail closed when apk/ipa fixtures are tracked again (#7547 Phase 0)."""
    try:
        tracked = tracked_mobile_binaries(root)
    except FetchError as error:
        return [str(error)]
    if not tracked:
        return []
    shown = ", ".join(tracked[:5])
    extra = f" (+{len(tracked) - 5} more)" if len(tracked) > 5 else ""
    return [
        "tracked mobile apk/ipa fixtures are forbidden after #7547 Phase 0 "
        f"(fetch via scripts/ci/fetch_mobile_test_apps.py): {shown}{extra}"
    ]



def _repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(1024 * 1024)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def _load_manifest(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict) or payload.get("schemaVersion") != 1:
        raise FetchError(f"unsupported manifest schema: {path}")
    apps = payload.get("apps")
    if not isinstance(apps, list) or not apps:
        raise FetchError(f"manifest apps list is empty: {path}")
    return payload


def _cache_path(cache_dir: Path, sha256: str, asset: str) -> Path:
    safe = Path(asset).name
    return cache_dir / f"{sha256}-{safe}"


def _download(url: str, destination: Path, expected_sha256: str) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    request = urllib.request.Request(url, headers={"User-Agent": _USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=120) as response:  # nosec B310 - pinned HTTPS release URL
            with tempfile.NamedTemporaryFile(
                dir=str(destination.parent), delete=False
            ) as tmp:
                tmp_path = Path(tmp.name)
                while True:
                    chunk = response.read(1024 * 1024)
                    if not chunk:
                        break
                    tmp.write(chunk)
    except urllib.error.URLError as error:
        raise FetchError(f"download failed for {url}: {error}") from error
    actual = _sha256_file(tmp_path)
    if actual != expected_sha256:
        tmp_path.unlink(missing_ok=True)
        raise FetchError(
            f"sha256 mismatch for {url}: expected {expected_sha256}, got {actual}"
        )
    tmp_path.replace(destination)


def _ensure_cached(cache_dir: Path, base_url: str, asset: str, sha256: str) -> Path:
    cached = _cache_path(cache_dir, sha256, asset)
    if cached.is_file() and _sha256_file(cached) == sha256:
        return cached
    url = f"{base_url.rstrip('/')}/{asset}"
    _download(url, cached, sha256)
    return cached


def _install(cached: Path, destinations: list[str], root: Path) -> list[str]:
    changed: list[str] = []
    for relative in destinations:
        dest = root / relative
        dest.parent.mkdir(parents=True, exist_ok=True)
        if dest.is_file() and _sha256_file(dest) == _sha256_file(cached):
            continue
        # copy via replace of a temp sibling so partial writes never linger
        with tempfile.NamedTemporaryFile(dir=str(dest.parent), delete=False) as tmp:
            tmp_path = Path(tmp.name)
            tmp.write(cached.read_bytes())
        tmp_path.replace(dest)
        changed.append(relative)
    return changed


def fetch_all(
    *,
    root: Path | None = None,
    manifest_path: Path | None = None,
    cache_dir: Path | None = None,
    dry_run: bool = False,
) -> dict[str, Any]:
    root = root or _repo_root()
    manifest_path = manifest_path or (root / MANIFEST_REL)
    cache_dir = cache_dir or DEFAULT_CACHE
    manifest = _load_manifest(manifest_path)
    base_url = str(manifest["baseUrl"])
    report: dict[str, Any] = {
        "releaseTag": manifest.get("releaseTag"),
        "cacheDir": str(cache_dir),
        "apps": [],
    }
    for entry in manifest["apps"]:
        asset = str(entry["asset"])
        sha256 = str(entry["sha256"]).lower()
        destinations = [str(item) for item in entry["destinations"]]
        if dry_run:
            report["apps"].append(
                {
                    "asset": asset,
                    "sha256": sha256,
                    "destinations": destinations,
                    "status": "dry-run",
                }
            )
            continue
        cached = _ensure_cached(cache_dir, base_url, asset, sha256)
        changed = _install(cached, destinations, root)
        report["apps"].append(
            {
                "asset": asset,
                "sha256": sha256,
                "destinations": destinations,
                "changed": changed,
                "status": "ok",
            }
        )
    return report


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        type=Path,
        default=None,
        help="repository root (default: inferred from this script)",
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        default=None,
        help="manifest path (default: scripts/ci/mobile_test_apps.manifest.json)",
    )
    parser.add_argument(
        "--cache-dir",
        type=Path,
        default=None,
        help="content-addressed cache directory",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="validate the manifest without downloading",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="print the fetch report as JSON",
    )
    parser.add_argument(
        "--check-tracked",
        action="store_true",
        help="fail if fixture apk/ipa paths are tracked in git (#7547 Phase 0)",
    )
    args = parser.parse_args(argv)
    if args.check_tracked:
        failures = tracked_mobile_binary_failures(args.root)
        if failures:
            for item in failures:
                print(f"ERROR: {item}", file=sys.stderr)
            return 1
        print("tracked-mobile-binaries: pass")
        return 0
    try:
        report = fetch_all(
            root=args.root,
            manifest_path=args.manifest,
            cache_dir=args.cache_dir,
            dry_run=args.dry_run,
        )
    except FetchError as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1
    if args.json:
        json.dump(report, sys.stdout, indent=2, sort_keys=True)
        sys.stdout.write("\n")
    else:
        for app in report["apps"]:
            changed = app.get("changed") or []
            print(
                f"{app['status']}: {app['asset']} "
                f"({len(app['destinations'])} dests, {len(changed)} written)"
            )
        print(f"cache: {report['cacheDir']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
