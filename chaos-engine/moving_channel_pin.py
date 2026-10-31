"""Moving-channel version pin policy."""
from __future__ import annotations

from typing import Literal

PinMode = Literal["exact", "min"]


def _parse_dotted(version: str) -> tuple[int, ...]:
    parts: list[int] = []
    for piece in str(version).strip().split("."):
        if not piece.isdigit():
            raise ValueError(f"non-numeric version piece: {version!r}")
        parts.append(int(piece))
    if not parts:
        raise ValueError("empty version")
    return tuple(parts)


def channel_pin_allows(
    installed: str,
    pinned: str,
    *,
    mode: PinMode = "min",
) -> bool:
    """Return whether ``installed`` satisfies ``pinned`` under ``mode``."""
    got = _parse_dotted(installed)
    need = _parse_dotted(pinned)
    if mode == "exact":
        return got == need
    if mode != "min":
        raise ValueError(f"unknown mode: {mode!r}")
    # minimum within same major (first component)
    if got[0] != need[0]:
        return False
    # pad to equal length
    width = max(len(got), len(need))
    got_p = got + (0,) * (width - len(got))
    need_p = need + (0,) * (width - len(need))
    return got_p >= need_p
