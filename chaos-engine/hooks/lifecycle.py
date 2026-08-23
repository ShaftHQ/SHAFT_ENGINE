#!/usr/bin/env python3
"""Shared lifecycle context, dispatch, and protocol for every launcher."""

from __future__ import annotations

import contextlib
import io
import json
import sys
from collections.abc import Callable, Mapping
from pathlib import Path

COMPANION_NAMES = ("caveman", "ponytail")
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


def _write_json(output: dict) -> None:
    sys.stdout.write(json.dumps(output, separators=(",", ":"), allow_nan=False) + "\n")


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
        output = fallback(event_name, host)
        result = 0
    _write_json(output)
    return result
