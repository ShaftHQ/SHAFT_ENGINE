"""Fail fast when generated-project workflow templates trail the repository's own action versions.

Python mirror of shaft-mcp's GeneratedWorkflowActionVersionFreshnessTest (#3798). Dependabot only scans
``.github/workflows``, so the templates under ``shaft-engine/src/main/resources/examples/.github/workflows``
drift on every action bump; running this before the push replaces a failed CI round (#6839).
"""

from __future__ import annotations

import re
from pathlib import Path

MAIN_WORKFLOWS = ".github/workflows"
TEMPLATE_WORKFLOWS = "shaft-engine/src/main/resources/examples/.github/workflows"
_USES = re.compile(r"^\s*(?:-\s*)?uses:\s*([^\s#]+)\s*(?:#.*)?$")


def touches_workflows(path: str) -> bool:
    """True when a changed path is a main or template workflow file."""
    normalized = path.replace("\\", "/").removeprefix("./")
    return normalized.startswith((MAIN_WORKFLOWS + "/", TEMPLATE_WORKFLOWS + "/"))


def _version(raw: str) -> tuple[int, ...] | None:
    segments = raw.removeprefix("v").split(".")
    if not all(segment.isdigit() for segment in segments):
        return None  # a commit SHA or branch name is not comparable
    return tuple(int(segment) for segment in segments)


def _older(left: tuple[int, ...], right: tuple[int, ...]) -> bool:
    width = max(len(left), len(right))
    return left + (0,) * (width - len(left)) < right + (0,) * (width - len(right))


def action_versions(directory: Path) -> dict[str, tuple[str, tuple[int, ...]]]:
    """Newest comparable ``uses: owner/action@version`` per action under one workflows directory."""
    newest: dict[str, tuple[str, tuple[int, ...]]] = {}
    if not directory.is_dir():
        return newest
    for workflow in sorted([*directory.glob("*.yml"), *directory.glob("*.yaml")]):
        for line in workflow.read_text(encoding="utf-8").splitlines():
            match = _USES.match(line)
            if not match:
                continue
            reference = match.group(1)
            if reference.startswith(("./", "docker://")) or "@" not in reference:
                continue
            action, _, raw = reference.rpartition("@")
            parsed = _version(raw)
            if parsed is None:
                continue
            current = newest.get(action)
            if current is None or _older(current[1], parsed):
                newest[action] = (raw, parsed)
    return newest


def stale_template_actions(root: Path) -> list[str]:
    """One line per template action that pins an older version than the repository workflows."""
    main = action_versions(root / MAIN_WORKFLOWS)
    stale: list[str] = []
    for action, (raw, parsed) in sorted(action_versions(root / TEMPLATE_WORKFLOWS).items()):
        current = main.get(action)
        if current is not None and _older(parsed, current[1]):
            stale.append(f"{action}: template pins {raw} but the repository now uses {current[0]}")
    return stale
