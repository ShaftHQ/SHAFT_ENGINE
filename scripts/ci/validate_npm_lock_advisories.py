#!/usr/bin/env python3
"""Fail when bundled npm locks declare or resolve vulnerable shell-quote (#7451).

GHSA-pqg4-j6r4-53mv patches shell-quote in 1.11.0 (range >=1.8.4 <1.11.0).
GHSA-395f-4hp3-45gv patches shell-quote in 1.9.0 (range <=1.8.4).

A resolved 1.11.0 tarball is not enough: Dependency Review and humans both
missed exact declarations like ``"shell-quote": "1.8.4"`` on @appium/support
and a bundled nested 1.10.0 under appium-windows-driver. This check reads the
committed package-lock.json files and the package.json postinstall wiring.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MIN_SHELL_QUOTE = (1, 11, 0)
EXACT = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")
LOCKS = (
    ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium/package-lock.json",
    ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium-ios/package-lock.json",
    ROOT / "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium-windows/package-lock.json",
    ROOT / "scripts/ci/npm-pins/appium-3.5.2/package-lock.json",
)
REPAIR = "scripts/ci/repair_bundled_shell_quote.js"


def parse_exact(spec: str) -> tuple[int, int, int] | None:
    match = EXACT.fullmatch(spec.strip())
    if match is None:
        return None
    return int(match.group(1)), int(match.group(2)), int(match.group(3))


def _rel(path: Path, root: Path) -> str:
    try:
        return str(path.resolve().relative_to(root.resolve()))
    except ValueError:
        return str(path)


def audit_lock(path: Path, root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    data = json.loads(path.read_text(encoding="utf-8"))
    packages = data.get("packages") or {}
    for key, meta in packages.items():
        deps = meta.get("dependencies") or {}
        declared = deps.get("shell-quote")
        if declared is not None:
            exact = parse_exact(declared)
            if exact is not None and exact < MIN_SHELL_QUOTE:
                errors.append(f"{_rel(path, root)}: {key or '(root)'} declares shell-quote@{declared}")
        if key.endswith("shell-quote") or key.endswith("/shell-quote"):
            version = meta.get("version") or ""
            exact = parse_exact(version)
            if exact is not None and exact < MIN_SHELL_QUOTE:
                kind = "bundled" if meta.get("inBundle") else "resolved"
                errors.append(
                    f"{_rel(path, root)}: {key} {kind} shell-quote@{version} (need >=1.11.0)"
                )
    return errors


def audit_postinstall(path: Path, root: Path = ROOT) -> list[str]:
    package_json = path.with_name("package.json")
    if not package_json.is_file():
        return [f"{_rel(package_json, root)}: missing"]
    data = json.loads(package_json.read_text(encoding="utf-8"))
    script = (data.get("scripts") or {}).get("postinstall", "")
    overrides = data.get("overrides") or {}
    errors: list[str] = []
    if overrides.get("shell-quote") != "1.11.0":
        errors.append(f"{_rel(package_json, root)}: overrides.shell-quote must be 1.11.0")
    repair_local = path.with_name("repair-shell-quote.js")
    if path.parent.name in {"appium", "appium-ios", "appium-windows"}:
        if "repair-shell-quote.js" not in script and "repair_bundled_shell_quote" not in script:
            errors.append(
                f"{_rel(package_json, root)}: postinstall must run repair-shell-quote.js (#7451)"
            )
        if not repair_local.is_file() and "repair_bundled_shell_quote" not in script:
            errors.append(f"{_rel(repair_local, root)}: missing repair script")
    return errors


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    args = parser.parse_args(argv)
    root = args.root.resolve()
    errors: list[str] = []
    for lock in LOCKS:
        path = root / lock.relative_to(ROOT)
        if not path.is_file():
            errors.append(f"missing {path.relative_to(root)}")
            continue
        errors.extend(audit_lock(path, root))
        if path.parent.name in {"appium", "appium-ios", "appium-windows"}:
            errors.extend(audit_postinstall(path, root))
    repair = root / REPAIR
    if not repair.is_file():
        errors.append(f"missing {REPAIR}")
    if errors:
        print("npm lock advisory check failed (#7451):", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        return 1
    print("npm lock advisory check: shell-quote declarations and resolutions are >=1.11.0")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
