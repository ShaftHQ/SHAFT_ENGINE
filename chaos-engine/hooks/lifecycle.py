#!/usr/bin/env python3
"""Shared lifecycle context owned by ChaosEngine source and installed launchers."""

from __future__ import annotations

from pathlib import Path

COMPANION_NAMES = ("caveman", "ponytail")
ULTRA_SELECTOR = (
    "ChaosEngine companion intensity: caveman=ultra; ponytail=ultra. "
    "Off only: stop caveman, stop ponytail, or normal mode."
)


def _skill_relatives(name: str) -> tuple[str, ...]:
    return (
        f"vendor/{name}/skills/{name}/SKILL.md",
        f"plugins/{name}/skills/{name}/SKILL.md",
        f"{name}/skills/{name}/SKILL.md",
        f"chaos-engine/vendor/{name}/skills/{name}/SKILL.md",
    )


def _search_roots() -> list[Path]:
    here = Path(__file__).resolve().parent
    candidates = [here, *here.parents]
    try:
        cwd = Path.cwd().resolve()
    except OSError:
        cwd = None
    if cwd is not None:
        candidates.extend((cwd, *cwd.parents))
    return list(dict.fromkeys(candidates))


def _read_companion(name: str) -> str | None:
    for root in _search_roots():
        for relative in _skill_relatives(name):
            path = root / relative
            try:
                if path.is_file():
                    return path.read_text(encoding="utf-8")
            except OSError:
                continue
    return None


def session_start_context(token: str | None, activation: str) -> str:
    """Return the shared activation and exact, once-only companion payload."""
    parts = [f"ChaosEngine: {activation}"]
    if token:
        parts.append(f"Reflection session token (never track it): {token}")
    parts.append(ULTRA_SELECTOR)
    for name in COMPANION_NAMES:
        text = _read_companion(name)
        if text:
            parts.append(text)
    return "\n\n".join(parts)
