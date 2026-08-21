#!/usr/bin/env python3
"""Validate one semantic owner for each ChaosEngine harness duty."""

from __future__ import annotations

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "scripts/ci/agent_ownership.json"
TEXT_SUFFIXES = {".json", ".md", ".py", ".yml", ".yaml"}
IGNORED_PARTS = {".git", ".gradle", "build", "target"}
REQUIRED_DUTIES = {
    "router", "lifecycle dispatch and protocol", "repository guard policy",
    "portable guard adapter", "repository adapter", "tool dispatch",
    "plugin manifests", "Markdown harness map", "shaft skill routing",
    "CI aggregation", "cleanup policy", "host installer",
}


def _words(value: str) -> set[str]:
    words = set(re.findall(r"[a-z0-9_]+", value.casefold())) - {"a", "an", "the", "this", "file"}
    normalized = set()
    for word in words:
        if word in {"own", "owns", "owner", "ownership"}:
            normalized.add("own")
        else:
            normalized.add(word[:-1] if "_" not in word and len(word) > 4 and word.endswith("s") else word)
    return normalized


def _policy_surface(relative: str) -> bool:
    path = Path(relative)
    if len(path.parts) == 1:
        return path.suffix.casefold() == ".md" or path.suffix.casefold() == ".py"
    return path.parts[0] in {
        ".agents", ".claude", ".codex", ".github", "agent-plugins",
        "chaos-engine", "shaft-skills", "tools",
    } or path.parts[:2] in {("scripts", "agents"), ("scripts", "ci")}


def _contains_markers(text: str, phrases: list[object]) -> bool:
    lines = text.splitlines()
    windows = ["\n".join(lines[index:index + 3]) for index in range(len(lines))]
    return all(
        any(_words(str(phrase)) <= _words(window) for window in windows)
        for phrase in phrases
    )


def validate(root: Path = ROOT, manifest_path: Path | None = None) -> list[str]:
    manifest_path = manifest_path or root / "scripts/ci/agent_ownership.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        return [f"ownership manifest unreadable: {error}"]
    adapters = set(manifest.get("adapters", []))
    texts: dict[str, str] = {}
    for path in root.rglob("*"):
        if not path.is_file() or path.suffix.casefold() not in TEXT_SUFFIXES or IGNORED_PARTS & set(path.parts):
            continue
        relative = path.relative_to(root).as_posix()
        if path.resolve() == manifest_path.resolve() or not _policy_surface(relative):
            continue
        try:
            texts[relative] = path.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
    errors: list[str] = []
    missing_duties = REQUIRED_DUTIES - set(manifest.get("duties", {}))
    if missing_duties:
        errors.append("required duties missing: " + ", ".join(sorted(missing_duties)))
    for adapter in sorted(adapters):
        if adapter not in texts:
            errors.append(f"adapter missing: {adapter}")
    for duty, contract in manifest.get("duties", {}).items():
        owner = contract.get("owner", "")
        if owner not in texts:
            errors.append(f"{duty}: owner missing: {owner}")
            continue
        phrases = contract.get("terms") or contract.get("markers") or []
        if phrases and not _contains_markers(texts[owner], phrases):
            errors.append(f"{duty}: owner does not contain its semantic markers: {owner}")
        allowed = set(contract.get("allowed", []))
        for relative, text in texts.items():
            if relative == owner or relative in allowed:
                continue
            if phrases and _contains_markers(text, phrases):
                errors.append(f"{duty}: semantic owner conflict in {relative}; canonical owner is {owner}")
    return sorted(errors)


if __name__ == "__main__":
    problems = validate()
    for problem in problems:
        print(problem)
    raise SystemExit(bool(problems))
