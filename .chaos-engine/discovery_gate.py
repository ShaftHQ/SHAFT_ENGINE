#!/usr/bin/env python3
"""PreToolUse retrieve + graphify gate before broad discovery."""

from __future__ import annotations

import json
import os
import re
from pathlib import Path
from typing import Mapping

BROAD_TOOLS = frozenset({
    "Grep",
    "Glob",
    "GrepSearch",
    "grep",
    "list_dir",
    "ListDir",
    "GlobTool",
})
RETRIEVE_MARK = re.compile(
    r"tool\.py\s+retrieve|retrieve\.py|graphify\s+query|graphify\s+path|graphify\s+explain",
    re.I,
)


def _state_dir(root: Path | None = None) -> Path:
    base = root or Path.cwd()
    return base / ".chaos-engine-state" / "discovery"


def receipt_path(session_id: str, root: Path | None = None) -> Path:
    return _state_dir(root) / f"{session_id or 'unknown'}.json"


def record_retrieve(session_id: str, *, root: Path | None = None) -> None:
    path = receipt_path(session_id, root)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"session_id": session_id, "retrieve": True, "graphify": True}
    path.write_text(json.dumps(payload) + "\n", encoding="utf-8")


def has_retrieve(session_id: str, *, root: Path | None = None) -> bool:
    path = receipt_path(session_id, root)
    if not path.is_file():
        return False
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return False
    return bool(data.get("retrieve") or data.get("graphify"))


def graph_present(root: Path | None = None) -> bool:
    base = root or Path.cwd()
    return (base / "graphify-out" / "graph.json").is_file()


def enabled(environ: Mapping[str, str] | None = None) -> bool:
    env = environ if environ is not None else os.environ
    flag = str(env.get("CHAOS_ENGINE_ENFORCE_DISCOVERY_GATE") or "1").strip().casefold()
    return flag not in {"0", "false", "off", "no"}


def note_command(session_id: str, command: str, *, root: Path | None = None) -> None:
    if session_id and RETRIEVE_MARK.search(command or ""):
        record_retrieve(session_id, root=root)


def deny_reason(
    *,
    event_name: str,
    tool_name: str,
    commands: tuple[str, ...],
    session_id: str,
    root: Path | None = None,
    environ: Mapping[str, str] | None = None,
) -> str | None:
    if event_name != "PreToolUse" or not enabled(environ):
        return None
    joined = "\n".join(commands)
    if RETRIEVE_MARK.search(joined):
        note_command(session_id, joined, root=root)
        return None
    if tool_name not in BROAD_TOOLS:
        return None
    if has_retrieve(session_id, root=root):
        return None
    if not graph_present(root):
        return None
    tree = "chaos-engine" if (root or Path.cwd()).joinpath(
        "chaos-engine/tool.py"
    ).is_file() else ".chaos-engine"
    return (
        "Retrieve+graphify required before broad search. "
        f"Run `python3 {tree}/tool.py retrieve \"…\"` then `graphify query \"…\"`."
    )
