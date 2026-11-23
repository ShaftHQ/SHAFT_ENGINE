#!/usr/bin/env python3
"""Copy hoisted patched lock nodes over Dependabot's vulnerable declarations.

Dependabot regenerates ``package-lock.json`` from the upstream manifests. That
restores exact ``shell-quote@1.8.4`` dependency declarations and ``inBundle``
copies below the advisory floors. npm overrides do not rewrite those bundled
copies. The install-time repair replaces extracted files but does not rewrite
the committed lock, so the next bump fails the same gate.

This is the committed-lock half of e928e92c91: when a node is below the floor,
replace that lock entry with the hoisted patched package already in the same
lock (version, resolved, integrity, and the rest) and drop ``inBundle``. The
hoisted entry is a real registry tarball. The nested ``inBundle`` tarball is
not patched; ``npm ci`` still extracts it (see ``NPM_CI_STILL_EXTRACTS``) and
``repair-shell-quote.js`` replaces the extracted tree.

The script only reads and writes JSON. A trusted checkout runs it against an
untrusted pull-request tree (``--root``). It does not execute pull-request
code and does not invent a version when the hoisted patched node is missing.
"""

from __future__ import annotations

import argparse
import copy
import json
import sys
from pathlib import Path

if __name__ == "__main__" and __package__ in {None, ""}:
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.ci.validate_npm_lock_advisories import (
    MIN_SHELL_QUOTE,
    NESTED_FLOORS,
    ROOT,
    _resolved_name,
    parse_exact,
)

# Managed bundles only. The historical npm pin is audited elsewhere and is not
# part of the Dependabot lock-pin patch allowlist.
LOCKS = (
    "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium/package-lock.json",
    "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium-ios/package-lock.json",
    "shaft-infrastructure/src/main/resources/com/shaft/infrastructure/appium-windows/package-lock.json",
)
FLOORS: dict[str, tuple[int, int, int]] = {
    "shell-quote": MIN_SHELL_QUOTE,
    **{name: floor for name, (floor, _label) in NESTED_FLOORS.items()},
}


class RewriteError(RuntimeError):
    """A below-floor node cannot be replaced from a hoisted patched package."""


def _below(version: object, floor: tuple[int, int, int]) -> bool:
    exact = parse_exact(str(version or ""))
    return exact is not None and exact < floor


def _required_floor(name: str, overrides: dict) -> tuple[int, int, int]:
    floor = FLOORS[name]
    override = overrides.get(name)
    exact = parse_exact(override) if isinstance(override, str) else None
    if exact is not None and exact > floor:
        return exact
    return floor


def _hoisted(packages: dict, name: str, floor: tuple[int, int, int]) -> dict:
    meta = packages.get(f"node_modules/{name}")
    version = meta.get("version") if isinstance(meta, dict) else None
    if not isinstance(meta, dict) or parse_exact(str(version or "")) is None or _below(version, floor):
        floor_label = ".".join(str(part) for part in floor)
        raise RewriteError(
            f"no hoisted node_modules/{name} at or above {floor_label} "
            "to copy; refusing to invent a patched lock node"
        )
    return meta


def _index_compliant_hoisted(packages: dict, overrides: dict) -> dict[str, dict]:
    hoisted: dict[str, dict] = {}
    for key, meta in list(packages.items()):
        if not isinstance(meta, dict):
            continue
        name = _resolved_name(key)
        if name not in FLOORS or key != f"node_modules/{name}":
            continue
        floor = _required_floor(name, overrides)
        version = meta.get("version")
        if parse_exact(str(version or "")) is None or _below(version, floor):
            continue
        hoisted[name] = meta
    return hoisted


def _rewrite_declarations_and_nested(
    packages: dict, overrides: dict, hoisted: dict[str, dict], notes: list[str]
) -> None:
    for key, meta in list(packages.items()):
        if not isinstance(meta, dict):
            continue
        dependencies = meta.get("dependencies")
        if isinstance(dependencies, dict) and "shell-quote" in dependencies:
            declared = dependencies["shell-quote"]
            if _below(declared, FLOORS["shell-quote"]):
                source = hoisted.get("shell-quote")
                if source is None:
                    source = _hoisted(packages, "shell-quote", _required_floor("shell-quote", overrides))
                    hoisted["shell-quote"] = source
                replacement = str(source["version"])
                if declared != replacement:
                    dependencies["shell-quote"] = replacement
                    notes.append(f"{key or '(root)'} declares shell-quote@{declared} -> {replacement}")
        name = _resolved_name(key)
        if name not in FLOORS or key == f"node_modules/{name}":
            continue
        floor = _required_floor(name, overrides)
        if not _below(meta.get("version"), floor):
            continue
        source = hoisted.get(name)
        if source is None:
            source = _hoisted(packages, name, floor)
            hoisted[name] = source
        packages[key] = copy.deepcopy(source)
        notes.append(
            f"{key} {name}@{meta.get('version')} -> {source.get('version')} (hoisted registry node, inBundle dropped)"
        )


def _drop_matching_inbundle(packages: dict, hoisted: dict[str, dict], notes: list[str]) -> None:
    for key, meta in list(packages.items()):
        if not isinstance(meta, dict) or not meta.get("inBundle") or not meta.get("resolved"):
            continue
        name = _resolved_name(key)
        if name not in FLOORS or key == f"node_modules/{name}":
            continue
        source = hoisted.get(name)
        if source is None:
            continue
        if meta.get("version") != source.get("version") or meta.get("resolved") != source.get("resolved"):
            continue
        packages[key] = copy.deepcopy(source)
        notes.append(f"{key} drops inBundle; registry node matches hoisted {name}@{source.get('version')}")


def rewrite_packages(packages: dict, overrides: dict | None = None) -> list[str]:
    """Return drift lines. Mutates ``packages``. Raises ``RewriteError`` if a copy source is missing."""
    overrides = overrides or {}
    notes: list[str] = []
    hoisted = _index_compliant_hoisted(packages, overrides)
    _rewrite_declarations_and_nested(packages, overrides, hoisted, notes)
    _drop_matching_inbundle(packages, hoisted, notes)
    return notes


def _load_overrides(lock_path: Path) -> dict:
    manifest = lock_path.with_name("package.json")
    if not manifest.is_file():
        return {}
    data = json.loads(manifest.read_text(encoding="utf-8"))
    overrides = data.get("overrides") or {}
    return overrides if isinstance(overrides, dict) else {}


def _dump(data: dict) -> str:
    return json.dumps(data, indent=2, ensure_ascii=False) + "\n"


def plan_lock(lock_path: Path) -> tuple[str, list[str]]:
    """Return ``(serialized lock, drift lines)``. Drift is empty when the lock is already clean."""
    original = json.loads(lock_path.read_text(encoding="utf-8"))
    packages = original.get("packages")
    if not isinstance(packages, dict):
        raise RewriteError(f"{lock_path}: lock has no packages object")
    notes = rewrite_packages(packages, _load_overrides(lock_path))
    if not notes:
        return lock_path.read_text(encoding="utf-8"), []
    return _dump(original), notes


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--write", action="store_true", help="Rewrite drifted locks in place.")
    parser.add_argument("--check", action="store_true", help="Report drift and exit 1 (default).")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    found = [root / relative for relative in LOCKS if (root / relative).is_file()]
    if not found:
        print("npm lock advisory rewrite: no managed locks under the root", file=sys.stderr)
        return 1
    planned: list[tuple[Path, str, list[str]]] = []
    errors: list[str] = []
    for path in found:
        try:
            text, notes = plan_lock(path)
        except (RewriteError, json.JSONDecodeError, OSError) as exc:
            errors.append(f"{path}: {exc}")
            continue
        if notes:
            planned.append((path, text, notes))
    if errors:
        print("npm lock advisory rewrite failed:", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        return 1
    if not planned:
        print("npm lock advisory rewrite: hoisted floors already recorded")
        return 0
    for _path, _text, notes in planned:
        for note in notes:
            print(note, file=sys.stderr)
    if not args.write:
        print("npm lock advisory rewrite: drift (re-run with --write)", file=sys.stderr)
        return 1
    for path, text, _notes in planned:
        path.write_text(text, encoding="utf-8", newline="\n")
    print(f"npm lock advisory rewrite: updated {len(planned)} lock(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
