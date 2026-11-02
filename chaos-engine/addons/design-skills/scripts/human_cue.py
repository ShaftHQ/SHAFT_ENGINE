"""Shared human-cue attestation helpers (L63 + L64).

L63 (ASS karaoke-human-frag) accepts Style/Name containing ``human`` or
inline ``human=true`` in ASS text. L64 (uncaptioned speech coverage) counts
only JSON cues with ``human: true`` (bool). Callers pick the predicate that
matches their input shape; this module documents both and hosts the shared
checks so the two gates cannot silently diverge.
"""
from __future__ import annotations

from typing import Any


def is_human_json_cue(cue: dict[str, Any] | None) -> bool:
    """True when a JSON cue carries explicit ``human: true`` (L64 coverage)."""
    if not isinstance(cue, dict):
        return False
    return cue.get("human") is True


def is_human_ass_dialogue(
    *,
    style: str = "",
    name: str = "",
    text: str = "",
) -> bool:
    """True when an ASS Dialogue line is human-attested (L63).

    Matches Style/Name containing ``human`` (case-insensitive) or inline
    ``human=true`` in the override text. Does **not** accept JSON-only cues —
    use :func:`is_human_json_cue` for those.
    """
    style_cf = (style or "").casefold()
    name_cf = (name or "").casefold()
    text_cf = (text or "").casefold()
    return "human" in style_cf or name_cf == "human" or "human=true" in text_cf


def human_predicate_note() -> str:
    """One-line doc for lesson Verify sections."""
    return (
        "L63 human = ASS Style/Name 'Human' or inline human=true; "
        "L64 coverage = JSON human: true only (non-hall text without the flag does not count)."
    )
