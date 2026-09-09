#!/usr/bin/env python3
"""Owned MCP uniqueness across project and user/host layers. No proxy."""

from __future__ import annotations

import json
import os
import re
from collections.abc import Iterable
from pathlib import Path

ALIAS_GROUPS: tuple[frozenset[str], ...] = (
    frozenset({"github", "github-gh", "github_gh"}),
)

# When a CLI is the owned client, these MCP server ids must not be enabled.
CLI_OWNED_MCP: dict[str, frozenset[str]] = {
    "github": frozenset({"github", "github-gh", "github_gh"}),
    "graphify": frozenset({"graphify", "graphifyy"}),
}

HEAL_PROMPT = (
    "No duplicate GitHub MCP. Repair: disable extras in host MCP config."
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
    cli_error = cli_owned_conflict_error(server_ids)
    if cli_error:
        return cli_error
    groups = duplicate_groups(server_ids)
    if not groups:
        return None
    rendered = ", ".join("+".join(group) for group in groups)
    return f"{HEAL_PROMPT} Seen: {rendered}."


def cli_owned_conflict_error(server_ids: Iterable[str]) -> str | None:
    """Fail when an MCP duplicates an owned CLI (gh, graphify)."""
    names = {str(item).strip().casefold() for item in server_ids if str(item).strip()}
    seen: list[str] = []
    for owned, aliases in CLI_OWNED_MCP.items():
        hit = sorted(names & aliases)
        if hit:
            seen.append(f"{owned} CLI owns {','.join(hit)}")
    if not seen:
        return None
    return f"{HEAL_PROMPT} Seen: {'; '.join(seen)}."


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


def collect_project_server_ids(project: Path) -> list[str]:
    """Server ids published by the project overlay `.mcp.json` (install-owned)."""
    project_mcp = project / ".mcp.json"
    if not project_mcp.is_file():
        return []
    return server_ids_from_text(project_mcp.read_text(encoding="utf-8"))


def collect_user_server_ids(home: Path | None = None) -> list[str]:
    """Server ids from user/global host MCP files (host-environment)."""
    names: list[str] = []
    for _host, path in user_mcp_paths(home):
        if path.is_file():
            names.extend(server_ids_from_text(path.read_text(encoding="utf-8")))
    return names


def collect_server_ids(
    project: Path,
    home: Path | None = None,
) -> list[str]:
    return [
        *collect_project_server_ids(project),
        *collect_user_server_ids(home),
    ]


def project_mcp_policy_error(project: Path) -> str | None:
    """Install-blocking / doctor-failing when the project overlay published the conflict."""
    return uniqueness_error(collect_project_server_ids(project))


def user_mcp_policy_finding(
    project: Path,
    home: Path | None = None,
) -> str | None:
    """Host-environment advisory when only user/global (or cross-layer) MCP policy fails.

    Project-overlay conflicts remain install-blocking via project_mcp_policy_error.
    """
    if project_mcp_policy_error(project) is not None:
        return None
    user_ids = collect_user_server_ids(home)
    user_error = uniqueness_error(user_ids)
    if user_error:
        return user_error
    return uniqueness_error([*collect_project_server_ids(project), *user_ids])


_INSTRUCTION_START = "<!-- CHAOSENGINE:START -->"
_INSTRUCTION_END = "<!-- CHAOSENGINE:END -->"
_CONTRACT_MARKERS = (
    "## Iron laws",
    "### Ethical conduct",
    "EC1: Tell the truth",
    "## Operating contract",
)


def user_instruction_paths(home: Path | None = None) -> tuple[Path, ...]:
    root = home or Path.home()
    return (
        Path(os.environ.get("CLAUDE_CONFIG") or root / ".claude") / "CLAUDE.md",
        Path(os.environ.get("CODEX_HOME") or root / ".codex") / "AGENTS.md",
        Path(os.environ.get("GROK_HOME") or root / ".grok") / "AGENTS.md",
        Path(os.environ.get("GEMINI_HOME") or root / ".gemini") / "GEMINI.md",
    )


def user_instruction_conflict_error(
    project: Path,
    home: Path | None = None,
) -> str | None:
    """Fail when user/machine guidance duplicates or restates the project contract."""
    project_blocks: list[str] = []
    for relative in ("AGENTS.md", "CLAUDE.md", "GEMINI.md", ".github/copilot-instructions.md"):
        path = project / relative
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8")
        if _INSTRUCTION_START in text and _INSTRUCTION_END in text:
            project_blocks.append(
                text.split(_INSTRUCTION_START, 1)[1].split(_INSTRUCTION_END, 1)[0].strip()
            )
    if not project_blocks:
        return None
    for path in user_instruction_paths(home):
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8")
        for marker in _CONTRACT_MARKERS:
            if marker in text:
                return (
                    "User/machine host config restates the ChaosEngine contract. "
                    f"Repair: remove the second copy from {path}."
                )
        if _INSTRUCTION_START not in text:
            continue
        user_block = text.split(_INSTRUCTION_START, 1)[1].split(_INSTRUCTION_END, 1)[0].strip()
        for project_block in project_blocks:
            if user_block and user_block == project_block:
                return (
                    "User/machine host config duplicates the project instruction block. "
                    f"Repair: remove the CHAOSENGINE block from {path}."
                )
    return None
