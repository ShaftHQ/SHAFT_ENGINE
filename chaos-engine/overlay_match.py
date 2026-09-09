#!/usr/bin/env python3
"""Origin overlay must byte-match SOURCE for installer-owned files."""

from __future__ import annotations

import hashlib
from pathlib import Path

SOURCE_DIR = "chaos-engine"
OVERLAY_DIR = ".chaos-engine"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def is_repository_checkout(project: Path) -> bool:
    return (project / SOURCE_DIR / "skills/chaos-engine/SKILL.md").is_file()


def core_matches_source(project: Path) -> dict[str, object]:
    """Compare overlay owned files to SOURCE. Adopters have no SOURCE tree."""
    root = project.resolve()
    if not is_repository_checkout(root):
        return {"coreMatchesSource": True, "scope": "adopter"}
    source = root / SOURCE_DIR
    overlay = root / OVERLAY_DIR
    if not overlay.is_dir():
        return {
            "coreMatchesSource": False,
            "scope": "repository",
            "detail": "overlay-absent",
        }
    mismatches: list[str] = []
    for path in sorted(source.rglob("*")):
        if not path.is_file():
            continue
        relative = path.relative_to(source)
        if "__pycache__" in relative.parts or path.suffix == ".pyc":
            continue
        if relative.as_posix() == "distributions.json":
            continue
        other = overlay / relative
        if not other.is_file():
            continue
        if _sha256(path) != _sha256(other):
            mismatches.append(relative.as_posix())
    return {
        "coreMatchesSource": not mismatches,
        "scope": "repository",
        "mismatches": mismatches[:8],
    }
