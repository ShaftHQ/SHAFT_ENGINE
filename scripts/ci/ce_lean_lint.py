#!/usr/bin/env python3
"""ChaosEngine lean-harness lints: core leaks, prose hygiene, near-duplicates.

Zero LLM, stdlib only. Allowlists in ``ce_lean_allowlist.json`` may only shrink.

    python3 scripts/ci/ce_lean_lint.py          # report + exit 1 on violations
"""

from __future__ import annotations

import itertools
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CORE = "chaos-engine"
ALLOWLIST = Path(__file__).with_name("ce_lean_allowlist.json")
LEAK_TERMS = re.compile(
    r"SHAFT|ShaftHQ|shafthq|shaft-|\bshaft\b|\bMaven\b|\bmvn\b|\bJava\b|pom\.xml|[Ss]urefire|"
    r"\b[Aa]llure\b|[Cc]odacy|TestNG|IntelliJ|Mohab|F79E3F65"
)
ISSUE_TAG = re.compile(r"(?<![\w&/])#\d{4}\b")
ZERO_TAG_FILES = (
    "chaos-engine/skills/chaos-engine/SKILL.md",
    "chaos-engine/identity.md",
    "chaos-engine/skills/kanban/SKILL.md",
    "chaos-engine/companions/caveman-ultra.md",
    "chaos-engine/companions/ponytail-ultra.md",
)
PLACEHOLDER_ROUTE = "Open the file."
DUPLICATE_THRESHOLD = 0.25


def _excluded(relative: str) -> bool:
    parts = relative.split("/")
    if len(parts) > 1 and parts[1] in {"vendor", "packs"}:
        return True
    return len(parts) > 2 and parts[1] == "profiles" and parts[2] != "portable" and parts[2] != "README.md"


def core_files(root: Path = ROOT, suffixes: tuple[str, ...] | None = None) -> list[Path]:
    base = root / CORE
    files = []
    for path in sorted(base.rglob("*")):
        if not path.is_file() or "__pycache__" in path.parts:
            continue
        relative = path.relative_to(root).as_posix()
        if _excluded(relative):
            continue
        if suffixes and path.suffix not in suffixes:
            continue
        files.append(path)
    return files


def _text(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError):
        return ""


def leak_counts(root: Path = ROOT) -> dict[str, int]:
    counts = {}
    for path in core_files(root):
        hits = len(LEAK_TERMS.findall(_text(path)))
        if hits:
            counts[path.relative_to(root).as_posix()] = hits
    return counts


def issue_tag_counts(root: Path = ROOT) -> dict[str, int]:
    counts = {}
    for path in core_files(root, (".md",)):
        hits = len(ISSUE_TAG.findall(_text(path)))
        if hits:
            counts[path.relative_to(root).as_posix()] = hits
    return counts


def placeholder_routes(root: Path = ROOT) -> int:
    return _text(root / ZERO_TAG_FILES[0]).count(PLACEHOLDER_ROUTE)


def _shingles(text: str, size: int = 8) -> set[str]:
    words = re.findall(r"\w+", text.lower())
    return {" ".join(words[i:i + size]) for i in range(len(words) - size)}


def near_duplicates(root: Path = ROOT) -> dict[str, float]:
    sets = {
        path.relative_to(root).as_posix(): _shingles(_text(path))
        for path in core_files(root, (".md",))
    }
    sets = {
        key: value for key, value in sets.items()
        if len(value) > 50 and not key.endswith("references/catalog.md")
    }
    pairs = {}
    for left, right in itertools.combinations(sorted(sets), 2):
        shared = len(sets[left] & sets[right])
        ratio = shared / min(len(sets[left]), len(sets[right]))
        if ratio > DUPLICATE_THRESHOLD:
            pairs[f"{left} | {right}"] = round(ratio, 2)
    return pairs


def load_allowlist(path: Path = ALLOWLIST) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def _over(actual: dict, allowed: dict, label: str) -> list[str]:
    return [
        f"{label}: {key} has {value} (allowed {allowed.get(key, 0)})"
        for key, value in sorted(actual.items())
        if value > allowed.get(key, 0)
    ]


def violations(root: Path = ROOT, allowlist: dict | None = None) -> list[str]:
    allowed = allowlist if allowlist is not None else load_allowlist()
    errors = _over(leak_counts(root), allowed.get("leaks", {}), "core-leak")
    tags = issue_tag_counts(root)
    errors += _over(tags, allowed.get("issueTags", {}), "issue-tag")
    errors += [f"issue-tag: {name} must carry none" for name in ZERO_TAG_FILES if tags.get(name)]
    if placeholder_routes(root):
        errors.append(f"route-row: router has placeholder '{PLACEHOLDER_ROUTE}' rows")
    dupes = near_duplicates(root)
    errors += [
        f"near-duplicate: {pair} overlap {ratio}"
        for pair, ratio in sorted(dupes.items())
        if pair not in allowed.get("nearDuplicates", [])
    ]
    return errors


def main() -> int:
    errors = violations()
    print(f"core leaks: {sum(leak_counts().values())} hits in {len(leak_counts())} files")
    for error in errors:
        print(error)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
