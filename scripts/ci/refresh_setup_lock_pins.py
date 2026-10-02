#!/usr/bin/env python3
"""Recompute and rewrite the SHA-256 pins of bundled setup npm lockfiles (#6357).

The managed setup planners pin the SHA-256 of each bundled ``package-lock.json``
(canonicalized to LF line endings) as the approved-plan integrity check. A
Dependabot npm bump changes the lockfile but cannot change the Java constant,
so ``Unit Tests (shaft-infrastructure)`` fails with fingerprint
``setup-lock-pin-drift``. This script keeps the pin and only refreshes it:

* ``--check`` (default) prints every drifted pin and exits 1.
* ``--write`` rewrites the drifted constants in place and exits 0.

``--root`` points at the tree to inspect, so a trusted base checkout can run
this file against an untrusted pull-request checkout without executing it.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

ROOT = Path(__file__).resolve().parents[2]
FINGERPRINT = "setup-lock-pin-drift"
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


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", type=Path, default=ROOT, help="Repository tree to inspect (default: this repo).")
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="Report drift and exit 1 if any (default).")
    mode.add_argument("--write", action="store_true", help="Rewrite drifted pins in place.")
    return parser


def main(argv: Sequence[str] | None = None, pins: Sequence[LockPin] = PINS) -> int:
    arguments = build_parser().parse_args(argv)
    try:
        drift = refresh(arguments.root, pins, write=arguments.write)
    except (OSError, PinError, UnicodeDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    for line in drift:
        print(line)
    if not drift:
        print("setup lock pins match their lockfiles")
        return 0
    if arguments.write:
        print(f"refreshed {len(drift)} setup lock pin(s)")
        return 0
    print("fix: python3 scripts/ci/refresh_setup_lock_pins.py --write")
    return 1


if __name__ == "__main__":
    sys.exit(main())
