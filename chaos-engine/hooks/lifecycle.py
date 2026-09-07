#!/usr/bin/env python3
"""Shared lifecycle context, dispatch, and protocol for every launcher."""

from __future__ import annotations

import contextlib
import os
import io
import json
import sys
from collections.abc import Callable, Mapping
from pathlib import Path

COMPANION_NAMES = ("caveman", "ponytail")
# Hard budget for SessionStart additionalContext (#5580). Locators only.
SESSION_START_MAX_BYTES = 4096
TOKEN_BUDGET_DEFAULT = "balanced"
TOKEN_BUDGET_MODES = {
    "ultra-lean": {
        "read_line_budget": 80,
        "guidance": (
            "Token budget ultra-lean: ≤80-line excerpts; one search; script-first. "
            "Details: chaos-engine/references/token-budget-modes.md"
        ),
    },
    "balanced": {
        "read_line_budget": 200,
        "guidance": (
            "Token budget balanced (default): ≤200-line excerpts; narrow once after "
            "truncation; prefer path+excerpt over dumps; script-first when multi-hop. "
            "Details: chaos-engine/references/token-budget-modes.md"
        ),
    },
    "deep": {
        "read_line_budget": 400,
        "guidance": (
            "Token budget deep: ≤400-line excerpts; allow a second discriminating pass "
            "before deciding; spill large tool output to disk; still prefer script-first "
            "for mechanical transforms; keep safety and negation intact. "
            "Details: chaos-engine/references/token-budget-modes.md"
        ),
    },
}


def resolve_token_budget_mode(environ: Mapping[str, str] | None = None) -> str:
    """Return the owner-selected token budget mode (default balanced)."""
    env = environ if environ is not None else os.environ
    raw = str(env.get("CHAOS_ENGINE_TOKEN_BUDGET") or TOKEN_BUDGET_DEFAULT).strip().casefold()
    # Accept common aliases
    aliases = {"lean": "ultra-lean", "ultra_lean": "ultra-lean", "default": "balanced"}
    raw = aliases.get(raw, raw)
    if raw not in TOKEN_BUDGET_MODES:
        return TOKEN_BUDGET_DEFAULT
    return raw


def token_budget_guidance(mode: str | None = None) -> str:
    """Return the compact guidance string for a mode (fixture + SessionStart)."""
    selected = mode or TOKEN_BUDGET_DEFAULT
    if selected not in TOKEN_BUDGET_MODES:
        selected = TOKEN_BUDGET_DEFAULT
    return str(TOKEN_BUDGET_MODES[selected]["guidance"])


HEADROOM_PROFILE_BY_BUDGET = {
    "ultra-lean": "agent-90",
    "balanced": "balanced",
    "deep": "coding",
}

# Triage (blast radius) → default token budget when env unset (#5621).
# Env CHAOS_ENGINE_TOKEN_BUDGET remains the owner override.
TRIAGE_TO_TOKEN_BUDGET = {
    "one-file": "ultra-lean",
    "one-module": "balanced",
    "module": "balanced",
    "public-contract": "deep",
}



def triage_token_budget(triage: str | None) -> str:
    """Map triage blast-radius label to the default token budget mode."""
    raw = str(triage or "").strip().casefold().replace("_", "-").replace(" ", "-")
    aliases = {
        "onefile": "one-file",
        "file": "one-file",
        "onemodule": "one-module",
        "public": "public-contract",
        "contract": "public-contract",
        "hard-to-reverse": "public-contract",
    }
    raw = aliases.get(raw, raw)
    return TRIAGE_TO_TOKEN_BUDGET.get(raw, TOKEN_BUDGET_DEFAULT)


def zero_llm_session_guidance() -> str:
    """Prefer doctor/repair catalog before chat discovery (locator-only)."""
    return (
        "Zero-LLM first: references/zero-llm-catalog.md "
        "(doctor / repair --component / --fix-next-only) before chat discovery."
    )


def level1_catalog_guidance() -> str:
    """Point at the Level-1 progressive-disclosure surface catalog."""
    return "Level-1 catalog: references/level-1-catalog.md."


def heal_route_guidance() -> str:
    """Router Heal surface always reachable by file path."""
    return "Heal: references/heal-route.md (install one-liner / repair --component)."

def headroom_session_guidance(mode: str | None = None) -> str:
    """Compact SessionStart line for Headroom profile (locator-only)."""
    selected = mode or TOKEN_BUDGET_DEFAULT
    if selected not in HEADROOM_PROFILE_BY_BUDGET:
        selected = TOKEN_BUDGET_DEFAULT
    profile = HEADROOM_PROFILE_BY_BUDGET[selected]
    return f"Headroom {profile}/{selected}; beacon=off."


def self_improve_session_guidance() -> str:
    """Cheap SessionStart locator — full protocol runs at Learning Session."""
    return "Learning: skills/self-improve/SKILL.md."


ULTRA_SELECTOR = (
    "ChaosEngine companion intensity: caveman=ultra; ponytail=ultra. "
    "Off only: stop caveman, stop ponytail, or normal mode."
)
LIFECYCLE_EVENTS = (
    "SessionStart",
    "UserPromptSubmit",
    "PreToolUse",
    "PostToolUse",
    "PostToolUseFailure",
    "Stop",
    "SubagentStop",
    "PreCompact",
    "SessionEnd",
)
HOOK_PROTOCOL_ERROR = "Lifecycle hook produced invalid JSON output."


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


def _workspace_locator(path: Path) -> str:
    """Return a companion path resolvable from the active project root."""
    anchors = (
        Path(".agents/skills/chaos-engine/SKILL.md"),
        Path(".chaos-engine/skills/chaos-engine/SKILL.md"),
        Path("chaos-engine/skills/chaos-engine/SKILL.md"),
    )
    for root in _search_roots():
        if not any((root / anchor).is_file() for anchor in anchors):
            continue
        try:
            return path.resolve().relative_to(root.resolve()).as_posix()
        except (OSError, ValueError):
            continue
    return path.as_posix()


def session_start_context(token: str | None, activation: str) -> str:
    """Return compact activation; agents load canonical skills from owned paths."""
    parts = [f"ChaosEngine: {activation}"]
    if token:
        parts.append(f"Reflection session token (never track it): {token}")
    parts.append(ULTRA_SELECTOR)
    budget = resolve_token_budget_mode()
    parts.append(token_budget_guidance(budget))
    parts.append(headroom_session_guidance(budget))
    parts.append(zero_llm_session_guidance())
    parts.append(level1_catalog_guidance())
    parts.append(heal_route_guidance())
    parts.append(self_improve_session_guidance())
    for name in COMPANION_NAMES:
        for root in _search_roots():
            path = next(
                (root / candidate for candidate in _skill_relatives(name) if (root / candidate).is_file()),
                None,
            )
            if path is not None:
                locator = _workspace_locator(path)
                parts.append(f"Required companion: read and follow `{locator}` before responding.")
                break
    return "\n\n".join(parts)


def _reject_json_constant(value: str):
    raise ValueError(f"non-standard JSON constant: {value}")


def _strict_json_loads(rendered: str):
    return json.loads(rendered, parse_constant=_reject_json_constant)


def _write_json(output: dict, stream=None) -> None:
    target = sys.stdout if stream is None else stream
    target.write(json.dumps(output, separators=(",", ":"), allow_nan=False) + "\n")


def run_hook_protocol(
    raw: str,
    callbacks: Mapping[str, Callable[[dict, str], int]],
    *,
    normalize: Callable[[dict], dict] = dict,
    host_for_input: Callable[[dict], str] = lambda _raw: "portable",
    prepare: Callable[[dict], None] = lambda _event: None,
    adapt_output: Callable[[dict, str, str], dict] = lambda output, _event, _host: output,
    fallback: Callable[[str, str], dict] = lambda event, _host: (
        {"decision": "block", "reason": HOOK_PROTOCOL_ERROR}
        if event in {"PreToolUse", "Stop", "SubagentStop"}
        else {}
    ),
) -> int:
    """Parse, dispatch, contain callback output, and emit one JSON object."""
    if not raw.strip():
        _write_json({})
        return 0
    try:
        raw_event = _strict_json_loads(raw)
    except (json.JSONDecodeError, ValueError, RecursionError):
        _write_json({})
        return 0
    if not isinstance(raw_event, dict):
        _write_json({})
        return 0
    event = normalize(raw_event)
    event_name = event.get("hook_event_name", "PreToolUse")
    if not isinstance(event_name, str) or event_name not in LIFECYCLE_EVENTS:
        _write_json({})
        return 0
    callback = callbacks.get(event_name)
    if callback is None:
        _write_json({})
        return 0
    host = host_for_input(raw_event)
    captured = io.StringIO()
    result = 0
    try:
        prepare(event)
        with contextlib.redirect_stdout(captured):
            result = callback(event, host)
        rendered = captured.getvalue().strip()
        output = {} if not rendered else _strict_json_loads(rendered)
        if not isinstance(output, dict):
            raise ValueError("hook output is not a JSON object")
        output = adapt_output(output, event_name, host)
        if not isinstance(output, dict):
            raise ValueError("adapted hook output is not a JSON object")
        json.dumps(output, allow_nan=False)
    except (Exception, KeyboardInterrupt, SystemExit) as error:
        print(f"Hook protocol error: {error}", file=sys.stderr)
        try:
            output = adapt_output(fallback(event_name, host), event_name, host)
            if not isinstance(output, dict):
                raise ValueError("adapted fallback output is not a JSON object")
            json.dumps(output, allow_nan=False)
        except (Exception, KeyboardInterrupt, SystemExit) as fallback_error:
            print(f"Hook fallback error: {fallback_error}", file=sys.stderr)
            output = {}
        result = 0
    _write_json(output, sys.stderr if host == "claude" and result == 2 else sys.stdout)
    return result
