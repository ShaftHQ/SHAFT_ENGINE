#!/usr/bin/env python3
"""Copy the entrypoint, identity, and skills into the directories Grok reads.

Grok puts `.grok/rules/*.md` in the model prompt and discovers
`.grok/skills/*/SKILL.md`. SessionStart stdout is not model context, and the
session-start locator is capped at 600 bytes, so the bodies live here.
Each copy stays byte-identical to the canonical file.
"""

from __future__ import annotations

from pathlib import Path

COMPANIONS = """# ChaosEngine load

`.grok/rules/entrypoint.md` is the ChaosEngine core card and is already in this session.
`.grok/rules/identity.md` is the identity and is already in this session.
Every skill is `.grok/skills/<name>/SKILL.md`, the same bytes as `chaos-engine/skills/<name>/SKILL.md`. Follow the skill whose description matches the task.
Companions are already on (caveman=ultra; ponytail=ultra; off only: stop caveman, stop ponytail, normal mode).
Before work, run `python3 .chaos-engine/tool.py entry`, then `python3 .chaos-engine/tool.py retrieve`.
Follow chaos-engine/references/iterative-flow.md, including its session-close checklist.
Memory, MemPalace, and Graphify are the tools that entry reports.
"""


def _canonical_root(project: Path) -> Path | None:
    """Source tree wins so a stale install cannot overwrite the canonical card."""
    source = project / "chaos-engine"
    installed = project / ".chaos-engine"
    for root in (source, installed):
        if (root / "skills" / "chaos-engine" / "SKILL.md").is_file():
            return root
    return None


def _write_same(destination: Path, payload: bytes) -> str | None:
    if destination.is_symlink():
        return None
    if destination.is_file() and destination.read_bytes() == payload:
        return None
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(payload)
    return destination.as_posix()


def publish_from(start: Path) -> list[str]:
    """Publish for the project that contains `start`, walking parents."""
    start = Path(start).resolve()
    for candidate in (start, *start.parents):
        if _canonical_root(candidate) is not None:
            return publish(candidate)
    return []


def publish(project: Path) -> list[str]:
    """Write Grok rules and skill copies. Return the paths that changed."""
    project = Path(project)
    root = _canonical_root(project)
    if root is None:
        return []
    written: list[str] = []
    rules = project / ".grok" / "rules"
    identity = root / "identity.md"
    entry = root / "skills" / "chaos-engine" / "SKILL.md"
    if identity.is_file():
        changed = _write_same(rules / "identity.md", identity.read_bytes())
        if changed:
            written.append(changed)
    if entry.is_file():
        changed = _write_same(rules / "entrypoint.md", entry.read_bytes())
        if changed:
            written.append(changed)
    changed = _write_same(rules / "companions.md", COMPANIONS.encode("utf-8"))
    if changed:
        written.append(changed)
    skills = root / "skills"
    if skills.is_dir():
        for skill in sorted(skills.glob("*/SKILL.md")):
            if not skill.is_file() or skill.is_symlink():
                continue
            destination = project / ".grok" / "skills" / skill.parent.name / "SKILL.md"
            changed = _write_same(destination, skill.read_bytes())
            if changed:
                written.append(changed)
    return written
