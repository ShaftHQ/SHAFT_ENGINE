#!/usr/bin/env python3
"""ChaosEngine lean-harness lints: core leaks, prose hygiene, near-duplicates.

Zero LLM, stdlib only. Allowlists in ``ce_lean_allowlist.json`` may only shrink.

    python3 scripts/ci/ce_lean_lint.py          # report + exit 1 on violations
"""

from __future__ import annotations

import itertools
import json
import importlib.util
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CORE = "chaos-engine"
ALLOWLIST = Path(__file__).with_name("ce_lean_allowlist.json")
# SHAFT leaks: product, owner, and SHAFT-toolchain names. The shipped core must
# carry none (CE-10 enforces zero, allowlist empty).
LEAK_TERMS = re.compile(
    r"SHAFT|ShaftHQ|shafthq|shaft-|\bshaft\b|[Ss]urefire|"
    r"\b[Aa]llure\b|[Cc]odacy|TestNG|IntelliJ|Mohab|F79E3F65"
)
# Ecosystem names belong to the java pack (`packs/java/`). Core wiring that
# activates the pack still names them; that count is tracked and may only shrink.
# Build-tool command tokens (mvn, gradle, npm) are neutral detection inputs.
ECOSYSTEM_TERMS = re.compile(r"\bMaven\b|\bJava\b|pom\.xml")
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


# The distribution's own origin slug (install one-liners, source receipts) is
# the address adopters install from, not a product leak.
ORIGIN_SLUG = "ShaftHQ/SHAFT_ENGINE"


def _origin_only(root: Path):
    """The installer's own predicate for origin docs that never ship to adopters."""
    spec = importlib.util.spec_from_file_location("ce_lean_install", root / CORE / "install.py")
    if spec is None or spec.loader is None:
        raise ImportError("ChaosEngine install.py cannot load")
    module = importlib.util.module_from_spec(spec)
    previous = sys.dont_write_bytecode
    sys.dont_write_bytecode = True
    try:
        spec.loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = previous
    return module.is_origin_only


def _shipped_counts(root: Path, terms: re.Pattern[str]) -> dict[str, int]:
    origin_only = _origin_only(root)
    counts = {}
    for path in core_files(root):
        if origin_only(path.relative_to(root / CORE)):
            continue
        hits = len(terms.findall(_text(path).replace(ORIGIN_SLUG, "")))
        if hits:
            counts[path.relative_to(root).as_posix()] = hits
    return counts


def ecosystem_counts(root: Path = ROOT) -> dict[str, int]:
    """Java-pack ecosystem names left in the shipped core (allowlisted, shrink-only)."""
    return _shipped_counts(root, ECOSYSTEM_TERMS)


def leak_counts(root: Path = ROOT) -> dict[str, int]:
    """SHAFT terms in the payload the installer ships to adopters (must be zero)."""
    return _shipped_counts(root, LEAK_TERMS)


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
    errors += _over(ecosystem_counts(root), allowed.get("ecosystemTerms", {}), "ecosystem-term")
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
    leaks, ecosystem = leak_counts(), ecosystem_counts()
    print(f"core leaks: {sum(leaks.values())} hits in {len(leaks)} files")
    print(f"ecosystem terms (java pack wiring): {sum(ecosystem.values())} hits in {len(ecosystem)} files")
    for error in errors:
        print(error)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
