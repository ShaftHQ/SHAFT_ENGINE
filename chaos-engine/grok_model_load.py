#!/usr/bin/env python3
"""Copy the entrypoint and identity into the rules directory Grok reads.

Grok puts `.grok/rules/*.md` in the model prompt. SessionStart stdout is not
model context, and the session-start locator is capped at 600 bytes, so those
two bodies live here. Relative links are retargeted so they resolve from
`.grok/rules`. Skill bodies stay one source at
`chaos-engine/skills/<name>/SKILL.md`. The entrypoint, which this publisher
puts in the prompt, routes them.
"""

from __future__ import annotations

import os
import re
from pathlib import Path

_LINK = re.compile(r"(!?)\[([^]]*)\]\(([^)]*)\)")
_SCHEME = re.compile(r"^[a-z][a-z0-9+.-]*:", re.IGNORECASE)

COMPANIONS = """\
# ChaosEngine load

`.grok/rules/entrypoint.md` is the ChaosEngine core card and is already in this session. Follow it.
`.grok/rules/identity.md` is the identity and is already in this session. Follow it.
Skills are one source at `chaos-engine/skills/<name>/SKILL.md`. The entrypoint routes them. Read that file when its description matches the task.
Roles are one source at `chaos-engine/references/roles.md`. Before the first action, choose one workflow from `chaos-engine/references/execution-workflows.md` and one of those roles, then read that role. Use `SOLO` when another agent would cost more, or when the owner wants a headed browser.
A headed owner request overrides the Tester headless rule. Say so before the browser starts.
A role name that file does not contain is mapped to the nearest role or reported absent. Product skill routing is additional to that role.
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


def _split_link_target(raw: str) -> tuple[str, str] | None:
    """Return the path and any title suffix, or None when the target stays."""
    text = raw.strip()
    if not text or text.startswith("#"):
        return None
    suffix = ""
    if text.startswith("<") and ">" in text:
        path, _, after = text[1:].partition(">")
        if after.strip():
            suffix = " " + after.strip()
    else:
        path, separator, title = text.partition(" ")
        if separator:
            suffix = separator + title
    if not path or path.startswith("#") or _SCHEME.match(path):
        return None
    return path, suffix


def _retarget(source: Path, destination: Path, raw: str) -> str:
    """Point one link at the same file from the rules directory."""
    parts = _split_link_target(raw)
    if parts is None:
        return raw
    path, suffix = parts
    file_path, hash_sep, fragment = path.partition("#")
    if not file_path:
        return raw
    resolved = (source.parent / file_path).resolve()
    relative = Path(os.path.relpath(resolved, destination.parent.resolve())).as_posix()
    fragment_text = f"{hash_sep}{fragment}" if hash_sep else ""
    return f"{relative}{fragment_text}{suffix}"


def _linked_bytes(source: Path, destination: Path) -> bytes:
    text = source.read_text(encoding="utf-8")

    def replace(match: re.Match[str]) -> str:
        return f"{match.group(1)}[{match.group(2)}]({_retarget(source, destination, match.group(3))})"

    return _LINK.sub(replace, text).encode("utf-8")


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
    """Write the Grok rules Grok actually reads. Return the paths that changed."""
    project = Path(project)
    root = _canonical_root(project)
    if root is None:
        return []
    written: list[str] = []
    rules = project / ".grok" / "rules"
    identity = root / "identity.md"
    entry = root / "skills" / "chaos-engine" / "SKILL.md"
    if identity.is_file():
        changed = _write_same(rules / "identity.md", _linked_bytes(identity, rules / "identity.md"))
        if changed:
            written.append(changed)
    if entry.is_file():
        changed = _write_same(rules / "entrypoint.md", _linked_bytes(entry, rules / "entrypoint.md"))
        if changed:
            written.append(changed)
    changed = _write_same(rules / "companions.md", COMPANIONS.encode("utf-8"))
    if changed:
        written.append(changed)
    return written
