#!/usr/bin/env python3
"""Recompute and rewrite the pins of bundled setup npm trees (#6357, #6381).

The managed setup planners pin the SHA-256 of each bundled ``package-lock.json``
(canonicalized to LF line endings) as the approved-plan integrity check. A
Dependabot npm bump changes the lockfile but cannot change the Java constant,
so ``Unit Tests (shaft-infrastructure)`` fails with fingerprint
``setup-lock-pin-drift``. This script keeps the pin and only refreshes it:

* ``--check`` (default) prints every drifted pin and exits 1.
* ``--write`` rewrites the drifted constants in place and exits 0.

The planners also pin each planned top-level package (#6381): its version,
the SHA-256 of its registry tarball and, for some, the tarball size. Those
follow the bundled ``package.json`` and ``package-lock.json``. ``--write``
downloads a changed tarball from ``registry.npmjs.org``, verifies it against
the lockfile ``integrity`` (sha512) and only then pins its SHA-256 and size.
``--check`` reports version drift offline; ``--verify-artifacts`` also
downloads every planned tarball and fails if a pinned digest or size is wrong.

``--root`` points at the tree to inspect, so a trusted base checkout can run
this file against an untrusted pull-request checkout without executing it.
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import http.client
import json
import re
import sys
import urllib.parse
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional, Sequence

ROOT = Path(__file__).resolve().parents[2]
FINGERPRINT = "setup-lock-pin-drift"
PACKAGE_FINGERPRINT = "setup-package-pin-drift"
REGISTRY = "https://registry.npmjs.org/"
_RESOURCES = "shaft-infrastructure/src/main/resources/com/shaft/infrastructure"
_JAVA = "shaft-infrastructure/src/main/java/com/shaft/infrastructure"


class PinError(RuntimeError):
    """A configured pin cannot be located or parsed."""


@dataclass(frozen=True)
class LockPin:
    """One bundled lockfile and the Java constant that pins its digest."""

    lockfile: str
    java_file: str
    constant: str


PINS: tuple[LockPin, ...] = (
    LockPin(f"{_RESOURCES}/appium/package-lock.json", f"{_JAVA}/AndroidSetupPlanner.java", "APPIUM_LOCK_SHA256"),
    LockPin(f"{_RESOURCES}/appium-ios/package-lock.json", f"{_JAVA}/DesktopMobileSetupPlanner.java", "IOS_LOCK_SHA256"),
    LockPin(f"{_RESOURCES}/appium-windows/package-lock.json", f"{_JAVA}/DesktopMobileSetupPlanner.java",
            "WINDOWS_LOCK_SHA256"),
    LockPin(f"{_RESOURCES}/lighthouse/package-lock.json", f"{_JAVA}/LighthouseSetupPlanner.java",
            "LIGHTHOUSE_LOCK_SHA256"),
    LockPin(f"{_RESOURCES}/reporting/package-lock.json", f"{_JAVA}/ReportingSetupPlanner.java", "ALLURE_LOCK_SHA256"),
)


def canonical_digest(lockfile: Path) -> str:
    """Return the SHA-256 of the lockfile after CRLF/CR are normalized to LF."""
    text = lockfile.read_bytes().decode("utf-8").replace("\r\n", "\n").replace("\r", "\n")
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def _constant_pattern(constant: str) -> re.Pattern[str]:
    return re.compile(r"(\b" + re.escape(constant) + r'\s*=\s*"(?:sha256:)?)([0-9a-f]{64})(")')


def refresh(root: Path, pins: Sequence[LockPin] = PINS, *, write: bool) -> list[str]:
    """Return one drift line per stale pin; rewrite the constants when ``write``."""
    drift: list[str] = []
    for pin in pins:
        java_path = root / pin.java_file
        source = java_path.read_text(encoding="utf-8")
        match = _constant_pattern(pin.constant).search(source)
        if match is None:
            raise PinError(f"{pin.constant} not found as a 64-hex literal in {pin.java_file}")
        expected = canonical_digest(root / pin.lockfile)
        if match.group(2) == expected:
            continue
        drift.append(f"{FINGERPRINT}: {pin.java_file} {pin.constant} {match.group(2)} -> {expected}"
                     f" ({pin.lockfile})")
        if write:
            updated = source[:match.start(2)] + expected + source[match.end(2):]
            java_path.write_text(updated, encoding="utf-8", newline="")
    return drift


Fetch = Callable[[str], bytes]
JavaConstant = tuple[str, str]


@dataclass(frozen=True)
class PackagePin:
    """One planned top-level npm package and the Java constants that pin it."""

    package: str
    bundles: tuple[str, ...]
    version: JavaConstant
    digests: tuple[JavaConstant, ...]
    size: Optional[JavaConstant] = None


_ANDROID = f"{_JAVA}/AndroidSetupPlanner.java"
_DESKTOP = f"{_JAVA}/DesktopMobileSetupPlanner.java"
_APPIUM_BUNDLES = (f"{_RESOURCES}/appium", f"{_RESOURCES}/appium-ios", f"{_RESOURCES}/appium-windows")

PACKAGE_PINS: tuple[PackagePin, ...] = (
    PackagePin("appium", _APPIUM_BUNDLES, (_ANDROID, "APPIUM_VERSION"),
               ((_ANDROID, "APPIUM_SHA256"), (_DESKTOP, "APPIUM_SHA256"))),
    PackagePin("appium-inspector-plugin", _APPIUM_BUNDLES, (_ANDROID, "INSPECTOR_PLUGIN_VERSION"),
               ((_ANDROID, "INSPECTOR_SHA256"), (_DESKTOP, "INSPECTOR_SHA256"))),
    PackagePin("appium-uiautomator2-driver", (f"{_RESOURCES}/appium",), (_ANDROID, "UIAUTOMATOR2_VERSION"),
               ((_ANDROID, "UIAUTOMATOR2_SHA256"),)),
    PackagePin("appium-xcuitest-driver", (f"{_RESOURCES}/appium-ios",), (_DESKTOP, "XCUITEST_VERSION"),
               ((_DESKTOP, "XCUITEST_SHA256"),), (_DESKTOP, "XCUITEST_ARTIFACT_BYTES")),
    PackagePin("appium-windows-driver", (f"{_RESOURCES}/appium-windows",), (_DESKTOP, "WINDOWS_DRIVER_VERSION"),
               ((_DESKTOP, "WINDOWS_DRIVER_SHA256"),), (_DESKTOP, "WINDOWS_DRIVER_ARTIFACT_BYTES")),
    PackagePin("lighthouse", (f"{_RESOURCES}/lighthouse",), (f"{_JAVA}/LighthouseSetupPlanner.java", "LIGHTHOUSE_VERSION"),
               ((f"{_JAVA}/LighthouseSetupPlanner.java", "LIGHTHOUSE_SHA256"),)),
    PackagePin("allure", (f"{_RESOURCES}/reporting",), (f"{_JAVA}/ReportingSetupPlanner.java", "ALLURE_VERSION"),
               ((f"{_JAVA}/ReportingSetupPlanner.java", "ALLURE_SHA256"),)),
)


@dataclass(frozen=True)
class _Locked:
    version: str
    resolved: str
    integrity: str


def _read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def locked_package(root: Path, pin: PackagePin) -> _Locked:
    """Return the one version every bundle's manifest and lockfile agree on for ``pin.package``."""
    seen: set[_Locked] = set()
    for bundle in pin.bundles:
        manifest = _read_json(root / bundle / "package.json").get("dependencies", {}).get(pin.package)
        entry = _read_json(root / bundle / "package-lock.json").get("packages", {}).get(
            f"node_modules/{pin.package}", {})
        if manifest is None or manifest != entry.get("version"):
            raise PinError(f"{bundle}: package.json {pin.package}={manifest} but lockfile has {entry.get('version')}")
        seen.add(_Locked(entry["version"], entry.get("resolved", ""), entry.get("integrity", "")))
    if any(_VERSION.fullmatch(locked.version) is None for locked in seen):
        raise PinError(f"{pin.package}: refusing non-semver version {sorted(locked.version for locked in seen)}")
    if len(seen) != 1:
        raise PinError(f"{pin.package}: bundles {', '.join(pin.bundles)} disagree: "
                       f"{sorted(locked.version for locked in seen)}")
    return seen.pop()


def _string_pattern(constant: str) -> re.Pattern[str]:
    return re.compile(r"(\b" + re.escape(constant) + r'\s*=\s*"(?:sha256:)?)([^"]+)(")')


def _long_pattern(constant: str) -> re.Pattern[str]:
    return re.compile(r"(\b" + re.escape(constant) + r"\s*=\s*)([0-9_]+)(L?\s*;)")


def _find(sources: dict[str, str], root: Path, ref: JavaConstant, pattern) -> re.Match[str]:
    if ref[0] not in sources:
        sources[ref[0]] = (root / ref[0]).read_text(encoding="utf-8")
    match = pattern(ref[1]).search(sources[ref[0]])
    if match is None:
        raise PinError(f"{ref[1]} not found in {ref[0]}")
    return match


_VERSION = re.compile(r"[0-9]+\.[0-9]+\.[0-9]+(?:[-+][0-9A-Za-z.-]+)?")


def _verified_tarball(package: str, locked: _Locked, fetch: Fetch) -> bytes:
    expected = f"{REGISTRY}{package}/-/{package.rsplit('/', 1)[-1]}-{locked.version}.tgz"
    if locked.resolved != expected:
        raise PinError(f"refusing tarball URL {locked.resolved!r}; the planner downloads {expected}")
    if not locked.integrity.startswith("sha512-"):
        raise PinError(f"{locked.resolved}: lockfile integrity is not sha512")
    data = fetch(locked.resolved)
    actual = "sha512-" + base64.b64encode(hashlib.sha512(data).digest()).decode("ascii")
    if actual != locked.integrity:
        raise PinError(f"{locked.resolved}: tarball does not match lockfile integrity")
    return data


def default_fetch(url: str) -> bytes:
    """Download a registry tarball over HTTPS only; no other scheme or host is ever opened."""
    parts = urllib.parse.urlsplit(url)
    if parts.scheme != "https" or parts.hostname != urllib.parse.urlsplit(REGISTRY).hostname:
        raise PinError(f"refusing to download {url!r}")
    connection = http.client.HTTPSConnection(parts.hostname, timeout=120)
    try:
        connection.request("GET", parts.path)
        response = connection.getresponse()
        if response.status != 200:
            raise PinError(f"{url}: HTTP {response.status}")
        return response.read()
    finally:
        connection.close()


def refresh_packages(root: Path, pins: Sequence[PackagePin] = PACKAGE_PINS, *, write: bool,
                     fetch: Fetch = default_fetch, verify_artifacts: bool = False) -> list[str]:
    """Return one drift line per stale package pin; rewrite version, digest and size when ``write``."""
    sources: dict[str, str] = {}
    edits: dict[str, list[tuple[int, int, str]]] = {}
    drift: list[str] = []

    def stage(ref: JavaConstant, match: re.Match[str], value: str) -> None:
        if match.group(2).replace("_", "") == value:
            return
        drift.append(f"{PACKAGE_FINGERPRINT}: {ref[0]} {ref[1]} {match.group(2)} -> {value}")
        edits.setdefault(ref[0], []).append((match.start(2), match.end(2), value))

    for pin in pins:
        locked = locked_package(root, pin)
        version_match = _find(sources, root, pin.version, _string_pattern)
        version_changed = version_match.group(2) != locked.version
        stage(pin.version, version_match, locked.version)
        if not (version_changed and write) and not verify_artifacts:
            continue
        data = _verified_tarball(pin.package, locked, fetch)
        for ref in pin.digests:
            stage(ref, _find(sources, root, ref, _string_pattern), hashlib.sha256(data).hexdigest())
        if pin.size is not None:
            size_match = _find(sources, root, pin.size, _long_pattern)
            stage(pin.size, size_match, str(len(data)))
    if write:
        for java_file, changes in edits.items():
            text = sources[java_file]
            for start, end, value in sorted(changes, reverse=True):
                text = text[:start] + value + text[end:]
            (root / java_file).write_text(text, encoding="utf-8", newline="")
    return drift


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", type=Path, default=ROOT, help="Repository tree to inspect (default: this repo).")
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="Report drift and exit 1 if any (default).")
    mode.add_argument("--write", action="store_true", help="Rewrite drifted pins in place.")
    parser.add_argument("--verify-artifacts", action="store_true",
                        help="Download every planned tarball and verify its pinned SHA-256 and size.")
    return parser


def main(argv: Sequence[str] | None = None, pins: Sequence[LockPin] = PINS,
         package_pins: Sequence[PackagePin] | None = None, fetch: Fetch = default_fetch) -> int:
    """Run both refreshes; custom lock ``pins`` without ``package_pins`` scope the run to lock pins only."""
    arguments = build_parser().parse_args(argv)
    if package_pins is None:
        package_pins = PACKAGE_PINS if pins is PINS else ()
    try:
        drift = refresh(arguments.root, pins, write=arguments.write)
        drift += refresh_packages(arguments.root, package_pins, write=arguments.write, fetch=fetch,
                                  verify_artifacts=arguments.verify_artifacts)
    except (OSError, PinError, UnicodeDecodeError, ValueError, KeyError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    for line in drift:
        print(line)
    if not drift:
        print("setup lock and package pins match the bundled manifests and lockfiles")
        return 0
    if arguments.write:
        print(f"refreshed {len(drift)} setup pin(s)")
        return 0
    print("fix: python3 scripts/ci/refresh_setup_lock_pins.py --write")
    return 1


if __name__ == "__main__":
    sys.exit(main())
