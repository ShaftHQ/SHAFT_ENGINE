"""
L74: prove each new gate FAIL on a known-bad fixture first."""

from __future__ import annotations

from typing import Any, Callable


def prove_red(
    gate: Callable[[Any], dict[str, Any]],
    bad_fixture: Any,
    *,
    expected_hit_substr: str | None = None,
) -> dict[str, Any]:
    """Run *gate* on *bad_fixture*; raise AssertionError unless FAIL.

    Returns the gate payload for the test to assert hit codes.
    """
    result = gate(bad_fixture)
    if not isinstance(result, dict):
        raise AssertionError("gate must return a dict payload")
    ok = result.get("ok")
    hits = result.get("hits") or []
    if ok is True or not hits:
        raise AssertionError(
            "L74 gate-proven-red: expected FAIL on known-bad fixture; "
            f"got ok={ok!r} hits={hits!r}"
        )
    if expected_hit_substr and not any(expected_hit_substr in str(h) for h in hits):
        raise AssertionError(
            f"L74: expected hit containing {expected_hit_substr!r}; got {hits!r}"
        )
    return result
