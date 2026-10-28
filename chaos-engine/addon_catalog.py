#!/usr/bin/env python3
"""Optional ChaosEngine add-ons: discovery, selection and payload mapping.

An add-on is an explicit, opt-in bundle of skills and references described by an
``addon.json`` manifest. Nothing here selects an add-on by default: the caller
passes the names a user asked for (``--with-<addon>`` / ``--without-<addon>`` or
``CHAOS_ENGINE_ADDONS``) and the names an earlier install already recorded.

Manifest locations, relative to the portable tree ``source``:

* ``source/addons/<name>/addon.json``: project-neutral add-ons in the core tree.
* ``source.parent/*/ce-addons/<name>/addon.json``: add-ons a source repository
  ships beside the core.
* ``source.parent/*/ce-pack/addon.json``: an add-on that selects a project pack's
  distribution instead of copying files.

Zero LLM, stdlib only.
"""

from __future__ import annotations

import json
import os
import sys
import re
from pathlib import Path, PurePosixPath

MANIFEST = "addon.json"
DIRECTORY = "addons"
ENVIRONMENT = "CHAOS_ENGINE_ADDONS"
# Extra directories of `<name>/addon.json` external add-ons (os.pathsep-separated);
# the installer points it at kept copies of recorded add-ons a source lacks (#7248).
PATHS_ENVIRONMENT = "CHAOS_ENGINE_ADDON_PATHS"
EXTERNAL_GLOBS = ("*/ce-addons/*/addon.json", "*/ce-pack/addon.json")
NAME = re.compile(r"[a-z][a-z0-9]*(?:-[a-z0-9]+)+")
# Flags the installers already own; an add-on may never shadow one of them.
RESERVED = frozenset({
    "maven-tools", "mcp", "deja", "memory", "mempalace", "graphify", "ponytail",
    "caveman", "icm-architect", "addon", "addons",
})
FLAG = re.compile(r"--(with|without)-([a-z][a-z0-9-]*)")


def _is_link(path: Path) -> bool:
    return path.is_symlink() or bool(getattr(path, "is_junction", lambda: False)())


def _load(path: Path, internal: bool) -> dict[str, object]:
    if _is_link(path) or _is_link(path.parent):
        raise ValueError(f"ChaosEngine add-on manifest is a link: {path}")
    try:
        manifest = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as error:
        raise ValueError(f"invalid ChaosEngine add-on manifest: {path}") from error
    if not isinstance(manifest, dict) or manifest.get("schemaVersion") != 1 or manifest.get("kind") != "addon":
        raise ValueError(f"invalid ChaosEngine add-on manifest: {path}")
    name = manifest.get("name")
    if not isinstance(name, str) or NAME.fullmatch(name) is None or name in RESERVED:
        raise ValueError(f"invalid ChaosEngine add-on name in {path}")
    if internal and path.parent.name != name:
        raise ValueError(f"ChaosEngine add-on directory must match its name: {path}")
    requires = manifest.get("requires", [])
    if not isinstance(requires, list) or not all(isinstance(item, str) for item in requires):
        raise ValueError(f"invalid requires in ChaosEngine add-on: {name}")
    for key in ("description", "useWhen"):
        if not isinstance(manifest.get(key), str) or not manifest[key].strip():
            raise ValueError(f"ChaosEngine add-on {name} needs a {key}")
    distribution = manifest.get("distribution")
    if distribution is not None and (not isinstance(distribution, str) or not distribution):
        raise ValueError(f"invalid distribution in ChaosEngine add-on: {name}")
    for item in manifest.get("include", []):
        if (
            not isinstance(item, dict)
            or not isinstance(item.get("from"), str)
            or not isinstance(item.get("globs"), list)
            or not all(isinstance(glob, str) and glob for glob in item["globs"])
        ):
            raise ValueError(f"invalid include in ChaosEngine add-on: {name}")
    return manifest


_ORIGIN_RANK = {"internal": 0, "paths": 1, "sibling": 2}


def _explicit_addon_roots() -> list[Path]:
    return [
        Path(folder).resolve()
        for folder in os.environ.get(PATHS_ENVIRONMENT, "").split(os.pathsep)
        if folder.strip()
    ]


def _addon_candidates(source: Path, explicit_roots: list[Path]) -> list[tuple[Path, bool, str]]:
    """Internal, sibling-scan, then CHAOS_ENGINE_ADDON_PATHS manifests."""
    candidates: list[tuple[Path, bool, str]] = [
        (path, True, "internal")
        for path in sorted((source / DIRECTORY).glob(f"*/{MANIFEST}"))
    ]
    for pattern in EXTERNAL_GLOBS:
        candidates.extend((path, False, "sibling") for path in sorted(source.parent.glob(pattern)))
    for folder in explicit_roots:
        candidates.extend((path, False, "paths") for path in sorted(folder.glob(f"*/{MANIFEST}")))
    return candidates


def _warn_addon_choice(name: str, message: str) -> None:
    print(f"chaos-engine: {message}", file=sys.stderr)


def _record_addon(
    found: dict[str, dict[str, object]],
    name: str,
    entry: dict[str, object],
) -> None:
    """Keep one root per name; PATHS conflicts fail; sibling dups warn (#7487)."""
    prior = found.get(name)
    if prior is None:
        found[name] = entry
        return
    origin = str(entry["origin"])
    prior_origin = str(prior.get("origin") or "sibling")
    if prior_origin == "paths" and origin == "paths":
        raise ValueError(f"duplicate ChaosEngine add-on: {name}")
    prior_rank = _ORIGIN_RANK.get(prior_origin, 9)
    new_rank = _ORIGIN_RANK.get(origin, 9)
    if new_rank < prior_rank:
        if "sibling" in (prior_origin, origin):
            _warn_addon_choice(
                name,
                f"add-on {name}: preferring {origin} ({entry['root']}) "
                f"over {prior_origin} ({prior['root']})",
            )
        found[name] = entry
        return
    if new_rank > prior_rank:
        if "sibling" in (prior_origin, origin):
            _warn_addon_choice(
                name,
                f"add-on {name}: keeping {prior_origin} ({prior['root']}); "
                f"ignoring {origin} ({entry['root']})",
            )
        return
    prior_root = Path(prior["root"])  # type: ignore[arg-type]
    new_root = Path(entry["root"])  # type: ignore[arg-type]
    if new_root == prior_root:
        return
    winner, loser = min(prior_root, new_root), max(prior_root, new_root)
    _warn_addon_choice(
        name,
        f"duplicate add-on {name} in sibling scan; using {winner} (ignored {loser})",
    )
    if new_root < prior_root:
        found[name] = entry


def _drop_unsatisfied(found: dict[str, dict[str, object]]) -> None:
    """Remove dependents whose required add-ons are missing from this source tree."""
    changed = True
    while changed:
        changed = False
        for name in list(found):
            requires = found[name]["manifest"].get("requires", [])  # type: ignore[union-attr]
            if any(required not in found for required in requires):
                del found[name]
                changed = True


def discover(source: Path) -> dict[str, dict[str, object]]:
    """Every add-on shipped with ``source``, keyed by name (validated, acyclic).

    #7487: a sibling-scan name collision warns and picks the lexicographically first
    root. ``CHAOS_ENGINE_ADDON_PATHS`` beats a sibling. Two PATHS entries with the
    same name still fail. Internal trees beat both.
    """
    source = Path(source).resolve()
    found: dict[str, dict[str, object]] = {}
    for path, internal, origin in _addon_candidates(source, _explicit_addon_roots()):
        manifest = _load(path, internal)
        name = str(manifest["name"])
        _record_addon(
            found,
            name,
            {
                "manifest": manifest,
                "root": path.parent.resolve(),
                "internal": internal,
                "origin": origin,
            },
        )
    _drop_unsatisfied(found)
    for name in found:
        _closure(found, {name})
    return found


def _closure(found: dict[str, dict[str, object]], names: set[str]) -> set[str]:
    selected: set[str] = set()
    stack = [(name, ()) for name in sorted(names)]
    while stack:
        name, path = stack.pop()
        if name in path:
            raise ValueError("ChaosEngine add-on requires form a cycle: " + " -> ".join((*path, name)))
        if name in selected:
            continue
        selected.add(name)
        for required in found[name]["manifest"].get("requires", []):  # type: ignore[union-attr]
            stack.append((required, (*path, name)))
    return selected


def split_flags(arguments: list[str]) -> tuple[set[str], set[str], list[str]]:
    """Separate ``--with-<addon>`` / ``--without-<addon>`` from other unknown arguments."""
    requested: set[str] = set()
    removed: set[str] = set()
    rest: list[str] = []
    for argument in arguments:
        match = FLAG.fullmatch(argument)
        if match is None or match.group(2) in RESERVED:
            rest.append(argument)
            continue
        (requested if match.group(1) == "with" else removed).add(match.group(2))
    return requested, removed, rest


def environment_names(environ: dict[str, str] | None = None) -> set[str]:
    raw = (os.environ if environ is None else environ).get(ENVIRONMENT, "")
    return {item.strip() for item in raw.split(",") if item.strip()}


def canonical(found: dict[str, dict[str, object]], names: set[str]) -> set[str]:
    """Accept the hyphen-free spelling PowerShell switches produce (designskills)."""
    aliases = {name.replace("-", ""): name for name in found}
    return {name if name in found else aliases.get(name.replace("-", ""), name) for name in names}


def resolve(
    found: dict[str, dict[str, object]],
    requested: set[str],
    removed: set[str],
    persisted: set[str],
) -> list[str]:
    """Explicit choices win over persisted ones; requires are added, never guessed."""
    requested, removed = canonical(found, requested), canonical(found, removed)
    unknown = sorted((requested | removed) - set(found))
    if unknown:
        valid = ", ".join(sorted(found)) or "none"
        raise ValueError(f"unknown ChaosEngine add-on: {', '.join(unknown)} (available: {valid})")
    both = sorted(requested & removed)
    if both:
        raise ValueError(f"ChaosEngine add-on both added and removed: {', '.join(both)}")
    wanted = ((persisted & set(found)) - removed) | requested
    selected = _closure(found, wanted)
    blocked = sorted(selected & removed)
    if blocked:
        dependents = sorted(
            name for name in selected - removed
            if set(found[name]["manifest"].get("requires", [])) & set(blocked)  # type: ignore[union-attr]
        )
        raise ValueError(
            f"cannot remove ChaosEngine add-on {', '.join(blocked)}: required by {', '.join(dependents)}"
        )
    return sorted(selected)


def distribution(found: dict[str, dict[str, object]], selected: list[str], default: str) -> str:
    declared = sorted({
        str(found[name]["manifest"]["distribution"])
        for name in selected
        if found[name]["manifest"].get("distribution")
    })
    if len(declared) > 1:
        raise ValueError("selected ChaosEngine add-ons need different distributions: " + ", ".join(declared))
    return declared[0] if declared else default


def installed(found: dict[str, dict[str, object]], files: dict[str, object], distribution_id: str | None) -> set[str]:
    """Add-ons an existing install recorded (file ownership plus a pack distribution)."""
    names = {
        parts[1]
        for parts in (PurePosixPath(key).parts for key in files)
        if len(parts) >= 3 and parts[0] == DIRECTORY and parts[2] == MANIFEST
    }
    for name, entry in found.items():
        if distribution_id and entry["manifest"].get("distribution") == distribution_id:
            names.add(name)
    return names


def external_files(found: dict[str, dict[str, object]], name: str) -> dict[Path, Path]:
    """Source files of a repository-shipped add-on mapped to ``addons/<name>/...``."""
    entry = found[name]
    manifest = entry["manifest"]
    if entry["internal"] or manifest.get("distribution"):  # type: ignore[union-attr]
        return {}
    root = Path(entry["root"])  # type: ignore[arg-type]
    base = Path(DIRECTORY) / name
    boundary = root.parent.parent.resolve()
    mapping: dict[Path, Path] = {}
    for path in sorted(root.rglob("*")):
        if path.is_file() and "__pycache__" not in path.parts:
            mapping[path] = base / path.relative_to(root)
    for item in manifest.get("include", []):  # type: ignore[union-attr]
        origin = (root / item["from"]).resolve()
        if origin != boundary and not origin.is_relative_to(boundary):
            raise ValueError(f"ChaosEngine add-on include escapes its repository folder: {name}")
        for pattern in item["globs"]:
            for path in sorted(origin.glob(pattern)):
                if _is_link(path):
                    raise ValueError(f"ChaosEngine add-on contains a link: {path}")
                if path.is_file() and "__pycache__" not in path.parts and not path.is_relative_to(root):
                    mapping.setdefault(path, base / path.relative_to(origin))
    return mapping


def repository_paths(manifest_path: PurePosixPath, manifest: dict[str, object]) -> list[tuple[PurePosixPath, str]]:
    """(base, glob) pairs, as repository paths, that a download must fetch for an add-on."""
    root = manifest_path.parent
    pairs = [(root, "**/*")]
    for item in manifest.get("include", []):  # type: ignore[union-attr]
        base = PurePosixPath(os.path.normpath((root / item["from"]).as_posix()))
        pairs.extend((base, pattern) for pattern in item["globs"])
    return pairs


def index_rows(found: dict[str, dict[str, object]], selected: list[str]) -> list[str]:
    """Human summary lines: name, state, flag and use-when."""
    rows = []
    for name in sorted(found):
        state = "installed" if name in selected else "available"
        rows.append(f"{name}\t{state}\t--with-{name}\t{found[name]['manifest']['useWhen']}")
    return rows
