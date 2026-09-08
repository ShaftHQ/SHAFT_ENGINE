#!/usr/bin/env python3
"""Owned MCP uniqueness across project and user/host layers. No Headroom."""

from __future__ import annotations

import json
import os
import re
from collections.abc import Iterable
from pathlib import Path

ALIAS_GROUPS: tuple[frozenset[str], ...] = (
    frozenset({"github", "github-gh", "github_gh"}),
)

HEAL_PROMPT = (
    "Duplicate MCP servers. Keep one GitHub MCP. "
    "Repair: disable extras in host MCP config."
)

_SERVER_HEADER = re.compile(
    r"^\[mcp_servers\.([^\]]+)\]\s*$", re.MULTILINE
)


def duplicate_groups(server_ids: Iterable[str]) -> list[tuple[str, ...]]:
    names = {str(item).strip().casefold() for item in server_ids if str(item).strip()}
    found: list[tuple[str, ...]] = []
    for group in ALIAS_GROUPS:
        hit = sorted(names & group)
        if len(hit) > 1:
            found.append(tuple(hit))
    counts: dict[str, int] = {}
    for item in server_ids:
        key = str(item).strip().casefold()
        if not key:
            continue
        counts[key] = counts.get(key, 0) + 1
    for key, count in sorted(counts.items()):
        if count > 1 and (key, key) not in found:
            found.append((key, key))
    return found


def uniqueness_error(server_ids: Iterable[str]) -> str | None:
    groups = duplicate_groups(server_ids)
    if not groups:
        return None
    rendered = ", ".join("+".join(group) for group in groups)
    return f"{HEAL_PROMPT} Seen: {rendered}."


def server_ids_from_text(text: str) -> list[str]:
    stripped = text.strip()
    if not stripped:
        return []
    if stripped[0] in "{[":
        try:
            parsed = json.loads(stripped)
        except json.JSONDecodeError:
            parsed = None
        if isinstance(parsed, dict):
            for key in ("mcpServers", "servers"):
                servers = parsed.get(key)
                if isinstance(servers, dict):
                    return [str(name) for name in servers]
    return [match.group(1).strip() for match in _SERVER_HEADER.finditer(text)]


def user_mcp_paths(home: Path | None = None) -> tuple[tuple[str, Path], ...]:
    """Host user/global MCP files. Same heal prompt on every adapter."""
    root = home or Path.home()
    xdg = Path(os.environ.get("XDG_CONFIG_HOME") or root / ".config")
    named = (
        ("claude", Path(os.environ.get("CLAUDE_CONFIG") or root / ".claude.json")),
        ("codex", Path(os.environ.get("CODEX_HOME") or root / ".codex") / "config.toml"),
        ("grok", Path(os.environ.get("GROK_HOME") or root / ".grok") / "config.toml"),
        ("gemini", Path(os.environ.get("GEMINI_HOME") or root / ".gemini") / "settings.json"),
        ("copilot", Path(os.environ.get("COPILOT_HOME") or root / ".copilot") / "mcp-config.json"),
        (
            "copilot-intellij",
            xdg / "github-copilot" / "intellij" / "mcp.json",
        ),
    )
    return named


def collect_server_ids(
    project: Path,
    home: Path | None = None,
) -> list[str]:
    names: list[str] = []
    project_mcp = project / ".mcp.json"
    if project_mcp.is_file():
        names.extend(server_ids_from_text(project_mcp.read_text(encoding="utf-8")))
    for _host, path in user_mcp_paths(home):
        if path.is_file():
            names.extend(server_ids_from_text(path.read_text(encoding="utf-8")))
    return names
