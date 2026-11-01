"""L79: multi-block CMS edits require current-block readback == expected pre-text."""
from __future__ import annotations

from typing import Any


def _norm(text: object) -> str:
    return " ".join(str(text or "").split())


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """Fail-closed: paste/save without matching block identity → FAIL."""
    if obs is None or not isinstance(obs, dict):
        return {
            "ok": False,
            "hits": ["missing-input"],
            "error": "cms block identity receipt required",
        }
    hits: list[str] = []
    current = obs.get("current_block_text")
    expected = obs.get("expected_pre_text")
    if current is None:
        hits.append("missing-current-block-text")
    if expected is None:
        hits.append("missing-expected-pre-text")
    if current is not None and expected is not None:
        if _norm(current) != _norm(expected):
            hits.append("block-identity-mismatch")
    paste = obs.get("paste") is True or obs.get("saving") is True or obs.get("save") is True
    if paste:
        if obs.get("readback_before_paste") is not True and obs.get("paste") is True:
            hits.append("missing-readback-before-paste")
        if (obs.get("saving") is True or obs.get("save") is True) and (
            obs.get("readback_before_save") is not True
        ):
            hits.append("missing-readback-before-save")
        if hits and "block-identity-mismatch" in hits:
            hits.append("paste-or-save-with-wrong-block")
    block_id = obs.get("block_id") or obs.get("block_key")
    if paste and not block_id:
        hits.append("missing-block-id")
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L79 CMS block identity failed",
        "block_id": block_id,
    }
