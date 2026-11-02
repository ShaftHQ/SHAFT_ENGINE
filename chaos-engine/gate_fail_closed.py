"""Fail-closed helpers for observation gates."""
from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def require_file(path: Path | str | None, *, hit: str = "missing-input") -> list[str]:
    """Return ``[hit]`` when path is missing or not a file."""
    if path is None or str(path).strip() == "":
        return [hit]
    if not Path(str(path)).is_file():
        return [hit]
    return []


def require_observations(
    path: Path | str | None,
    *,
    missing_hit: str = "missing-observations",
    bad_hit: str = "bad-observations",
) -> tuple[dict[str, Any] | None, list[str]]:
    """Load observations JSON object; fail closed on any problem."""
    hits = require_file(path, hit=missing_hit)
    if hits:
        return None, hits
    try:
        raw = json.loads(Path(str(path)).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None, [bad_hit]
    if not isinstance(raw, dict) or len(raw) == 0:
        return None, [missing_hit if not isinstance(raw, dict) or raw == {} else bad_hit]
    return raw, []


def fail_hits(*groups: list[str]) -> list[str]:
    """Flatten hit groups preserving order."""
    out: list[str] = []
    for group in groups:
        out.extend(group)
    return out
