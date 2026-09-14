#!/usr/bin/env python3
"""Owned MCP uniqueness across project and user/host layers. No proxy."""

from __future__ import annotations

import json
import os
import re
from collections.abc import Callable, Iterable
from pathlib import Path

GITHUB_MCP_IDS = frozenset({"github", "github-gh", "github_gh"})

# GitHub MCP is never a default catalog entry. When gh is healthy, doctor/repair
# strips GitHub MCP from user host MCP files. When gh is missing/unauthenticated,
# leave existing GitHub MCP in place. Graphify MCP still conflicts with the
# owned Graphify CLI.
ALIAS_GROUPS: tuple[frozenset[str], ...] = ()

CLI_OWNED_MCP: dict[str, frozenset[str]] = {
    "graphify": frozenset({"graphify", "graphifyy"}),
}

HEAL_PROMPT = (
    "Prefer gh for GitHub when gh exists and is configured; "
    "when gh auth status succeeds, disable user-host GitHub MCP. "
    "When gh is missing or unauthenticated, leave existing GitHub MCP in place. "
    "CLI over MCP when both exist. "
    "Default MCP catalog never includes GitHub MCP."
)

COLLIDING_USER_SKILL_NAMES = frozenset(
    {
        "graphify",
        "find-docs",
        "caveman",
        "ponytail",
        "chaos-engine",
        "learn",
        "learn-traces",
        "deep-research",
    }
)

# Grok product office/game bundles must never be vendored into the overlay (#5785).
FORBIDDEN_GROK_BUNDLED_SKILL_NAMES = frozenset(
    {
        "pdf",
        "pptx",
        "imagine",
    }
)
FORBIDDEN_GROK_BUNDLED_SKILL_PREFIXES = ("game-",)

# CE-owned plugin enable list (#5783). Extra enabled plugins fail/heal.
CE_ENABLED_PLUGIN_NAMES = frozenset({"chaos-engine", "caveman", "ponytail"})

# Host-product limits that CE cannot delete from the Grok install tree (#5780).
GROK_HOST_PRODUCT_LIMIT = (
    "GAP-GROK-BUNDLED: Grok product bundled skills (pdf/pptx/imagine/game-*) and "
    "session GitHub MCP injection cannot be deleted from the Grok install tree. "
    "CE policy: load overlay-only skills; prefer gh; never vendor those bundles; "
    "never add GitHub MCP to the default catalog. Tracking: #5780 #5785."
)

_SERVER_HEADER = re.compile(
    r"^\[mcp_servers\.([^\]]+)\]\s*$", re.MULTILINE
)
_ABSOLUTE_PATH = re.compile(
    r"(?:^|[\\/])(?:[A-Za-z]:[\\/]|/)(?:Users|home|tmp|var|opt|usr|Windows|Program Files)",
    re.IGNORECASE,
)
_JAR_ABS = re.compile(
    r'(?i)(?:^|[\\"\'\s])((?:[A-Za-z]:[\\/]|/)[^\s"\']*maven-tools-mcp[^\s"\']*\.jar)'
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


def is_github_mcp_id(name: str) -> bool:
    return str(name).strip().casefold() in GITHUB_MCP_IDS


def omit_github_from_defaults(servers: dict[str, object]) -> dict[str, object]:
    """Default install catalog must never publish GitHub MCP ids."""
    return {
        key: value
        for key, value in servers.items()
        if not is_github_mcp_id(str(key))
    }


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
    """Fail when an MCP duplicates an owned CLI (Graphify)."""
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
    return [match.group(1).strip().strip('"') for match in _SERVER_HEADER.finditer(text)]


def user_skill_roots(home: Path | None = None) -> tuple[tuple[str, Path], ...]:
    """User-home skill trees that must not collide with the overlay catalog."""
    root = home or Path.home()
    return (
        ("claude", root / ".claude" / "skills"),
        ("agents", root / ".agents" / "skills"),
        ("codex", Path(os.environ.get("CODEX_HOME") or root / ".codex") / "skills"),
        ("grok", Path(os.environ.get("GROK_HOME") or root / ".grok") / "skills"),
        ("gemini", Path(os.environ.get("GEMINI_HOME") or root / ".gemini") / "skills"),
    )


def colliding_user_skills(home: Path | None = None) -> list[str]:
    found: list[str] = []
    for _host, root in user_skill_roots(home):
        if not root.is_dir():
            continue
        for child in sorted(root.iterdir()):
            if child.is_dir() and child.name.casefold() in COLLIDING_USER_SKILL_NAMES:
                found.append(str(child))
    return found


def user_skill_collision_error(home: Path | None = None) -> str | None:
    hits = colliding_user_skills(home)
    if not hits:
        return None
    rendered = ", ".join(hits)
    return f"{HEAL_PROMPT} User-home skills collide with ChaosEngine overlay. Seen: {rendered}."


def gh_is_configured(
    runner: Callable[..., object] | None = None,
) -> bool:
    """Probe `gh auth status`. Injectable runner for tests."""
    try:
        import subprocess

        run = runner or subprocess.run
        completed = run(  # nosec B603 B607 - fixed gh probe
            ["gh", "auth", "status"],
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return False
    code = getattr(completed, "returncode", 1)
    return code == 0


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
    uniqueness = uniqueness_error(collect_project_server_ids(project))
    if uniqueness:
        return uniqueness
    return project_absolute_maven_error(project)


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


def _is_absolute_local_path(value: object) -> bool:
    if not isinstance(value, str) or not value.strip():
        return False
    text = value.strip().replace("\\", "/")
    if text.startswith("/") or re.match(r"^[A-Za-z]:/", text):
        # Relative project launchers and bare commands are fine.
        if text in {".", "./"} or not any(ch in text for ch in "/\\"):
            return False
        # tool.py / docker image refs are not workstation jar paths.
        if "maven-tools-mcp" in text.casefold() and text.casefold().endswith(".jar"):
            return True
        if text.casefold().endswith(".jar"):
            return True
        # Absolute java home binaries used only as maven command.
        base = text.rsplit("/", 1)[-1].casefold()
        if base in {"java", "java.exe"}:
            return True
    return False


def maven_server_has_absolute_local_paths(server: object) -> bool:
    """True when a maven-tools-mcp entry embeds workstation-absolute java/jar paths."""
    if not isinstance(server, dict):
        return False
    command = server.get("command")
    args = server.get("args")
    if _is_absolute_local_path(command):
        # docker absolute path is ok if args are image-only; jar/java are not.
        if isinstance(command, str) and Path(str(command)).name.casefold() in {
            "docker",
            "docker.exe",
        }:
            return False
        return True
    if isinstance(args, list):
        for item in args:
            if _is_absolute_local_path(item):
                return True
            if isinstance(item, str) and _JAR_ABS.search(item):
                return True
    return False


def project_absolute_maven_error(project: Path) -> str | None:
    """Fail doctor/install when git-tracked project MCP embeds machine-local jar paths."""
    path = project / ".mcp.json"
    if not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    if not isinstance(payload, dict):
        return None
    servers = payload.get("mcpServers")
    if not isinstance(servers, dict):
        return None
    maven = servers.get("maven-tools-mcp")
    if maven is None:
        return None
    if maven_server_has_absolute_local_paths(maven):
        return (
            "Project .mcp.json publishes workstation-absolute maven-tools-mcp "
            "java/jar paths. Use the shared ChaosEngine cache launcher via "
            ".chaos-engine/tool.py maven-tools-mcp, docker image mode, or omit "
            "maven-tools-mcp from the committed overlay."
        )
    return None


def strip_github_mcp_from_json_text(text: str) -> tuple[str, list[str]]:
    """Remove GitHub MCP ids from a JSON MCP config. Returns (text, removed ids)."""
    try:
        payload = json.loads(text)
    except json.JSONDecodeError:
        return text, []
    if not isinstance(payload, dict):
        return text, []
    removed: list[str] = []
    changed = False
    for key in ("mcpServers", "servers"):
        servers = payload.get(key)
        if not isinstance(servers, dict):
            continue
        keep: dict[str, object] = {}
        for name, server in servers.items():
            if is_github_mcp_id(str(name)):
                removed.append(str(name))
                changed = True
            else:
                keep[str(name)] = server
        if changed:
            payload[key] = keep
    if not changed:
        return text, []
    return json.dumps(payload, indent=2, sort_keys=True) + "\n", removed


def strip_github_mcp_from_toml_text(text: str) -> tuple[str, list[str]]:
    """Remove [mcp_servers.github...] tables from Codex/Grok-style TOML."""
    lines = text.splitlines(keepends=True)
    kept: list[str] = []
    removed: list[str] = []
    skipping = False
    for line in lines:
        bare = line.splitlines()[0] if line else ""
        match = _SERVER_HEADER.match(bare.strip() if bare.startswith("[") else bare)
        if match is not None:
            raw = match.group(1).strip().strip('"')
            if is_github_mcp_id(raw):
                skipping = True
                removed.append(raw)
                continue
            skipping = False
            kept.append(line)
            continue
        if skipping:
            # Stop skipping at next top-level table that is not nested under mcp_servers.
            if bare.startswith("[") and not bare.startswith("[mcp_servers."):
                skipping = False
                kept.append(line)
            continue
        kept.append(line)
    if not removed:
        return text, []
    return "".join(kept), removed


def strip_github_mcp_from_text(text: str) -> tuple[str, list[str]]:
    stripped = text.lstrip()
    # JSON objects only — TOML mcp_servers tables also start with "[".
    if stripped.startswith("{"):
        return strip_github_mcp_from_json_text(text)
    return strip_github_mcp_from_toml_text(text)


def repair_user_github_mcp(
    home: Path | None = None,
    *,
    gh_healthy: bool | None = None,
    gh_probe: Callable[[], bool] | None = None,
) -> dict[str, object]:
    """When gh is healthy, strip GitHub MCP from user host MCP files.

    When gh is missing/unauthenticated, leave GitHub MCP unchanged.
    """
    if gh_healthy is None:
        healthy = bool(gh_probe()) if gh_probe is not None else gh_is_configured()
    else:
        healthy = bool(gh_healthy)
    result: dict[str, object] = {
        "ghHealthy": healthy,
        "stripped": [],
        "preserved": [],
        "unchangedFiles": [],
    }
    if not healthy:
        for host, path in user_mcp_paths(home):
            if not path.is_file():
                continue
            ids = [
                name
                for name in server_ids_from_text(path.read_text(encoding="utf-8"))
                if is_github_mcp_id(name)
            ]
            if ids:
                result["preserved"].append({"host": host, "path": str(path), "ids": ids})
        return result
    stripped_rows: list[dict[str, object]] = []
    unchanged: list[str] = []
    for host, path in user_mcp_paths(home):
        if not path.is_file():
            continue
        original = path.read_text(encoding="utf-8")
        updated, removed = strip_github_mcp_from_text(original)
        if not removed:
            unchanged.append(str(path))
            continue
        path.write_text(updated, encoding="utf-8")
        stripped_rows.append({"host": host, "path": str(path), "ids": removed})
    result["stripped"] = stripped_rows
    result["unchangedFiles"] = unchanged
    return result


def plugin_base_name(plugin_id: str) -> str:
    """Return `chaos-engine` from `chaos-engine@marketplace`."""
    text = str(plugin_id).strip()
    return text.split("@", 1)[0].casefold()


def extra_enabled_plugins(enabled: dict[str, object]) -> list[str]:
    """Return enabled plugin ids outside the CE trio (true-valued only)."""
    extras: list[str] = []
    for key, value in enabled.items():
        if value is not True:
            continue
        if plugin_base_name(str(key)) not in CE_ENABLED_PLUGIN_NAMES:
            extras.append(str(key))
    return sorted(extras)


def pin_enabled_plugins(
    enabled: dict[str, object],
    *,
    marketplace_name: str,
) -> dict[str, object]:
    """Return enabledPlugins with only the CE trio set true for this marketplace."""
    pinned = {
        key: value
        for key, value in enabled.items()
        if plugin_base_name(str(key)) in CE_ENABLED_PLUGIN_NAMES
    }
    for name in ("chaos-engine", "caveman", "ponytail"):
        pinned[f"{name}@{marketplace_name}"] = True
    return pinned


def enabled_plugins_policy_error(enabled: dict[str, object] | None) -> str | None:
    if not isinstance(enabled, dict):
        return None
    extras = extra_enabled_plugins(enabled)
    if not extras:
        return None
    return (
        "Enabled plugins include non-ChaosEngine entries. "
        f"Pin to {', '.join(sorted(CE_ENABLED_PLUGIN_NAMES))} only. "
        f"Seen: {', '.join(extras)}."
    )


def is_forbidden_grok_bundled_skill(name: str) -> bool:
    key = str(name).strip().casefold()
    if key in FORBIDDEN_GROK_BUNDLED_SKILL_NAMES:
        return True
    return any(key.startswith(prefix) for prefix in FORBIDDEN_GROK_BUNDLED_SKILL_PREFIXES)


def overlay_forbidden_bundled_skills(overlay_skills_root: Path) -> list[str]:
    """Return forbidden Grok office/game skill names present under an overlay skills tree."""
    if not overlay_skills_root.is_dir():
        return []
    found: list[str] = []
    for child in sorted(overlay_skills_root.iterdir()):
        if child.is_dir() and is_forbidden_grok_bundled_skill(child.name):
            found.append(child.name)
    return found


def overlay_bundled_skill_error(project: Path) -> str | None:
    roots = (
        project / ".chaos-engine" / "skills",
        project / "chaos-engine" / "skills",
    )
    hits: list[str] = []
    for root in roots:
        hits.extend(overlay_forbidden_bundled_skills(root))
    if not hits:
        return None
    return (
        "Overlay catalog must never vendor Grok office/game bundled skills "
        f"(pdf, pptx, imagine, game-*). Seen: {', '.join(sorted(set(hits)))}."
    )


def grok_host_product_status() -> dict[str, object]:
    """Report Grok host-product extras that CE cannot delete (#5780)."""
    return {
        "status": "documented-limit",
        "extrasDisabled": False,
        "limit": GROK_HOST_PRODUCT_LIMIT,
        "policy": (
            "Overlay-only skills; prefer gh; strip user GitHub MCP when gh healthy; "
            "never vendor pdf/pptx/imagine/game-* into ChaosEngine."
        ),
    }


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
