#!/usr/bin/env python3
"""Merge several allure-results folders into one, keeping every attachment file (#7178).

Allure only shows an attachment whose file sits in the results folder; copying only
``*-result.json`` gives a report where every attachment is "missed". This copies each
kept result with every ``attachments[].source`` it and its fixture containers reference,
gives each source's containers a fresh uuid, and exits non-zero when a referenced file is
missing. The first source wins per test (full name plus parameters).

Usage: allure_compose.py --out target/evidence/allure-results <results-dir> [<results-dir> ...]
"""
from __future__ import annotations

import argparse
import json
import shutil
import sys
import uuid
from pathlib import Path

SCRATCH_ROOTS = ("/tmp", "/var/tmp", "/private/tmp")  # nosec B108 - refused, never written
EXTRAS = ("environment.properties", "environment.xml", "categories.json", "executor.json")


class ComposeError(RuntimeError):
    """A compose that would produce a report with missed attachments or lost evidence."""


def _sources(node: object, found: set[str]) -> set[str]:
    if isinstance(node, dict):
        found.update(item["source"] for item in node.get("attachments") or [] if item.get("source"))
        for value in node.values():
            _sources(value, found)
    elif isinstance(node, list):
        for value in node:
            _sources(value, found)
    return found


def _is_scratch(path: Path) -> bool:
    resolved = str(path.resolve())
    return any(resolved == root or resolved.startswith(root + "/") for root in SCRATCH_ROOTS)


def _load(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def _plan_source(directory: Path, taken: set, planned: list, containers_out: list, missing: list) -> None:
    """Plan one results folder: kept results, their attachments, renamed containers and extras."""
    containers = [_load(path) for path in sorted(directory.glob("*-container.json"))]
    kept: set[str] = set()
    for result_path in sorted(directory.glob("*-result.json")):
        result = _load(result_path)
        key = (result.get("fullName"), json.dumps(result.get("parameters"), sort_keys=True))
        if key in taken:
            continue
        taken.add(key)
        kept.add(result["uuid"])
        found = _sources(result, set())
        for container in containers:
            if result["uuid"] in container.get("children", []):
                _sources(container, found)
        for name in sorted(found):
            attachment = directory / name
            if attachment.is_file():
                planned.append((attachment, name))
            else:
                missing.append(str(attachment))
        planned.append((result_path, result_path.name))
    for container in containers:
        children = [child for child in container.get("children", []) if child in kept]
        if children:
            containers_out.append(dict(container, uuid=str(uuid.uuid4()), children=children))
    names = {name for _, name in planned}
    planned.extend((directory / extra, extra) for extra in EXTRAS
                   if (directory / extra).is_file() and extra not in names)


def compose(out: Path, sources: list[Path]) -> int:
    """Compose ``sources`` into ``out``; return the number of results kept."""
    if _is_scratch(out):
        raise ComposeError("--out must be under the project (target/), not /tmp: it is wiped")
    planned: list[tuple[Path, str]] = []
    containers_out: list[dict] = []
    missing: list[str] = []
    taken: set[tuple[object, str]] = set()
    for directory in sources:
        _plan_source(directory, taken, planned, containers_out, missing)
    if missing:
        raise ComposeError(
            f"MISSING {len(missing)} attachment files (e.g. {missing[0]}); "
            "refusing to build a report with missed attachments")
    if out.exists():
        shutil.rmtree(out)
    out.mkdir(parents=True)
    for source, name in planned:
        shutil.copy2(source, out / name)
    for container in containers_out:
        (out / f"{container['uuid']}-container.json").write_text(
            json.dumps(container, ensure_ascii=False), encoding="utf-8")
    return sum(1 for _, name in planned if name.endswith("-result.json"))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("src", nargs="+", type=Path)
    args = parser.parse_args(argv)
    try:
        count = compose(args.out, args.src)
    except ComposeError as error:
        print(error, file=sys.stderr)
        return 1
    print(f"composed {count} results with their attachments into {args.out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
