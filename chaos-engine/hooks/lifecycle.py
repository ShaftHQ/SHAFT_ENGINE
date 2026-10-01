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
ADVISORY_COMPANION_NAMES = ("icm-architect",)
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


SESSION_START_LEAN_MAX_BYTES = 600
COMPANION_CARDS = {
    "caveman": "companions/caveman-ultra.md",
    "ponytail": "companions/ponytail-ultra.md",
}


def _locate(relatives: tuple[str, ...]) -> str | None:
    for root in _search_roots():
        for relative in relatives:
            path = root / relative
            if path.is_file():
                return _workspace_locator(path)
    return None


def session_start_context(token: str | None, activation: str) -> str:
    """Return the compact SessionStart locator line set (<= 600 bytes)."""
    parts = [f"ChaosEngine: {activation}"]
    if token:
        parts.append(f"Reflection session token (never track it): {token}")
    cards = [
        _locate((card, f".chaos-engine/{card}", f"chaos-engine/{card}"))
        or f".chaos-engine/{card}"
        for card in COMPANION_CARDS.values()
    ]
    parts.append(
        "Companions (caveman=ultra; ponytail=ultra; off only: stop caveman, stop ponytail, "
        "normal mode): " + ", ".join(f"`{card}`" for card in cards)
    )
    for name in ADVISORY_COMPANION_NAMES:
        path = next(
            (
                root / candidate
                for root in _search_roots()
                for candidate in _skill_relatives(name)
                if (root / candidate).is_file()
            ),
            None,
        )
        if path is not None:
            parts.append(
                f"Advisory companion (design/structure): load `{_workspace_locator(path)}` "
                "for ICM / workspace-structure work."
            )
    identity = _locate(("identity.md", ".chaos-engine/identity.md", "chaos-engine/identity.md"))
    parts.append(f"Identity: `{identity or '.chaos-engine/identity.md'}`")
    parts.append(f"Token budget: {resolve_token_budget_mode()}")
    rendered = "\n".join(parts)
    with contextlib.suppress(Exception):
        counters_path = Path(__file__).resolve().parents[1] / "learning_counters.py"
        if counters_path.is_file():
            import importlib.util as _ilu

            _spec = _ilu.spec_from_file_location("ce_learning_counters_ss", counters_path)
            if _spec is not None and _spec.loader is not None:
                _mod = _ilu.module_from_spec(_spec)
                _spec.loader.exec_module(_mod)
                _mod.record_session_start_bytes(len(rendered.encode("utf-8")))
    return rendered


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
