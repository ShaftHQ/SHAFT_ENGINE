"""Advisory reminder when one tool repeats with the same arguments.

The reminder is context for the model. It is never a deny. Bookkeeping tools
in EXCLUDED_TOOLS neither increment nor reset the chain. State is in-memory
for this process; a new user turn clears one session.
"""

from __future__ import annotations

import json
import sys

EXCLUDED_TOOLS = frozenset({"retrieve"})
DEFAULT_THRESHOLDS = (3, 5, 8)


class RepeatReminderError(ValueError):
    """Raised when a threshold list is empty, too small, or has duplicates."""


def load_thresholds(raw: object) -> tuple[int, ...]:
    """Return sorted thresholds. Invalid configuration fails here, not later."""
    if raw is None or raw == "" or raw == () or raw == []:
        raise RepeatReminderError("repeat thresholds are empty")
    parts: list[object]
    if isinstance(raw, str):
        parts = [item.strip() for item in raw.split(",") if item.strip()]
    elif isinstance(raw, (list, tuple)):
        parts = list(raw)
    else:
        raise RepeatReminderError("repeat thresholds are not a list")
    if not parts:
        raise RepeatReminderError("repeat thresholds are empty")
    try:
        values = tuple(int(item) for item in parts)
    except (TypeError, ValueError) as exc:
        raise RepeatReminderError("repeat thresholds must be integers") from exc
    if any(item < 2 for item in values) or len(set(values)) != len(values):
        raise RepeatReminderError("repeat thresholds must be unique and at least 2")
    return tuple(sorted(values))


THRESHOLDS = load_thresholds(DEFAULT_THRESHOLDS)


def canonical_args(tool_input: object) -> str:
    """Stable identity. Key order does not matter. Near-duplicates stay distinct."""
    return json.dumps(tool_input, sort_keys=True, separators=(",", ":"), default=str)


class _Chain:
    def __init__(self) -> None:
        self.key: tuple[str, str] | None = None
        self.count = 0


_SESSIONS: dict[str, _Chain] = {}


def reset_session(session_id: str) -> None:
    """Drop one session chain. A new user turn calls this."""
    _SESSIONS.pop(str(session_id or ""), None)


def allow_context(advice: str | None) -> dict[str, str] | None:
    """Post-tool context. No decision field, so the call is not denied."""
    if not advice:
        return None
    return {"additionalContext": advice}


def denied_with_advice(reason: str, advice: str | None) -> dict[str, str]:
    """Keep a safety deny. Advice is extra context and does not change the decision."""
    payload = {"decision": "block", "reason": reason}
    if advice:
        payload["additionalContext"] = advice
    return payload


def _advice(tool_name: str, count: int) -> str:
    return (
        f"Repeat reminder: {tool_name} ran {count} times with the same arguments. "
        "Inspect the previous result and change approach or finish. "
        "This is advice, not a block."
    )


def _observe(
    session_id: str,
    tool_name: str,
    tool_input: object,
    thresholds: tuple[int, ...],
) -> str | None:
    name = str(tool_name or "")
    if name in EXCLUDED_TOOLS:
        return None
    chain = _SESSIONS.setdefault(str(session_id or ""), _Chain())
    key = (name, canonical_args(tool_input))
    if chain.key != key:
        chain.key = key
        chain.count = 0
    chain.count += 1
    if chain.count in thresholds:
        return _advice(name, chain.count)
    return None


def observe(
    session_id: str,
    tool_name: str,
    tool_input: object,
    *,
    thresholds: tuple[int, ...] | None = None,
) -> str | None:
    """Count one call. Returns reminder text, or None. Failures return None."""
    try:
        return _observe(session_id, tool_name, tool_input, thresholds or THRESHOLDS)
    except Exception:
        print("repeat reminder failed open", file=sys.stderr)
        return None
