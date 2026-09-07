#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
import runpy
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}
# Memory writes the shared origin/main store; advisory tools may still query when doctor-healthy.
MEMORY_ORIGIN_MAIN_TOOLS = frozenset({"memory", "memory-mcp"})
ADVISORY_ORIGIN_MAIN_TOOLS = frozenset({"mempalace", "mempalace-mcp", "graphify"})
ORIGIN_MAIN_SYNC_FIX_NEXT = "git fetch origin main && git merge --ff-only origin/main"


def shared_project_root(project: Path) -> Path:
    """Resolve the primary checkout that owns shared MemPalace / Memory / Graphify state."""
    if not (project / "tools/repository-map/resolve_mempalace.py").is_file():
        return project.resolve()
    completed = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        ["git", "rev-parse", "--git-common-dir"],
        cwd=project,
        capture_output=True,
        text=True,
        check=True,
    )
    common = Path(completed.stdout.strip())
    if not common.is_absolute():
        common = (project / common).resolve()
    return common.parent.resolve()


def origin_main_revisions(root: Path) -> tuple[str, str]:
    """Return (HEAD, refs/remotes/origin/main) for the primary checkout."""
    revisions = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        ["git", "rev-parse", "HEAD", "refs/remotes/origin/main"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.splitlines()
    if len(revisions) != 2:
        raise ValueError(
            "primary checkout HEAD and origin/main could not both be resolved "
            f"(not synchronized with origin/main). fix-next: {ORIGIN_MAIN_SYNC_FIX_NEXT}"
        )
    return revisions[0], revisions[1]


def origin_main_desync_message(head: str, origin_main: str) -> str:
    """Name HEAD != origin/main and print the fast-forward fix-next (#5591)."""
    return (
        f"primary checkout HEAD ({head}) != origin/main ({origin_main}) "
        f"(not synchronized with origin/main). fix-next: {ORIGIN_MAIN_SYNC_FIX_NEXT}"
    )


def enforce_tool_origin_main_policy(project: Path, tool: str) -> None:
    """Hard-fail Memory tools when HEAD != origin/main; soft-warn advisory tools."""
    if not (project / "tools/repository-map/resolve_mempalace.py").is_file():
        return
    root = shared_project_root(project)
    head, origin_main = origin_main_revisions(root)
    if head == origin_main:
        return
    message = origin_main_desync_message(head, origin_main)
    if tool in MEMORY_ORIGIN_MAIN_TOOLS:
        raise ValueError(message)
    if tool in ADVISORY_ORIGIN_MAIN_TOOLS:
        print(f"warning: {message}", file=sys.stderr)


def load_host_controller(installed_root: Path):
    """Load the colocated state classifier without requiring a Python package."""
    path = installed_root / "hosts.py"
    if not path.is_file():
        raise ValueError("ChaosEngine host controller could not be loaded")
    return runpy.run_path(str(path), run_name="_chaos_engine_runtime_hosts")


def mempalace_mcp_arguments(installed_root: Path, arguments: list[str]) -> list[str]:
    """Resolve the one owned palace and return its native MCP arguments."""
    project = installed_root.resolve().parent
    if arguments:
        raise ValueError(
            "MemPalace MCP does not accept host-supplied storage arguments"
        )
    resolver = project / "tools/repository-map/resolve_mempalace.py"
    palace = project / ".chaos-engine-state/mempalace"
    if resolver.is_file():
        namespace = runpy.run_path(str(resolver), run_name="_chaos_engine_mempalace_resolver")
        palace = Path(namespace["find_shared_mempalace"](project))
    if not palace.is_absolute():
        raise ValueError("MemPalace MCP resolver returned a relative path")
    palace = palace.resolve()
    controller = load_host_controller(installed_root)
    state = controller["mempalace_directory_status"](palace)
    if state.get("status") != "healthy":
        raise ValueError(
            f"MemPalace runtime is {state.get('status', 'unknown')}: "
            f"{state.get('detail', 'operator action required')}"
        )
    return ["--palace", str(palace), "--backend", "sqlite_exact"]


def guard_mempalace_mcp(installed_root: Path, arguments: list[str]) -> None:
    """Compatibility wrapper for callers that only need validation."""
    mempalace_mcp_arguments(installed_root, arguments)


def resolve_command(
    installed_root: Path, tool: str, arguments: list[str] | None = None
) -> list[str]:
    if tool not in TOOLS:
        raise ValueError(f"unsupported ChaosEngine tool: {tool}")
    installed_project = installed_root.resolve().parent
    project = shared_project_root(installed_project)
    enforce_tool_origin_main_policy(installed_project, tool)
    path = installed_root / "dependencies.py"
    if not path.is_file():
        cli = "py -3" if os.name == "nt" else "python3"
        raise ValueError(
            "ChaosEngine dependency controller could not be loaded "
            f"(wiped or incomplete `.chaos-engine` runtime). fix-next: run the "
            f"ChaosEngine install one-liner from INSTALL.md, then "
            f"`{cli} .chaos-engine/install.py doctor --project .`"
        )
    controller = runpy.run_path(str(path), run_name="_chaos_engine_runtime_dependencies")
    return controller["active_dispatch"](project, tool, arguments or [])


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: tool.py <tool|retrieve> [args...]", file=sys.stderr)
        return 2
    try:
        installed_root = Path(__file__).resolve().parent
        tool = sys.argv[1]
        arguments = sys.argv[2:]
        if tool == "retrieve":
            path = installed_root / "retrieve.py"
            if not path.is_file():
                print("retrieve.py missing from installed core", file=sys.stderr)
                return 1
            # Re-exec as retrieve CLI (argv[0] style via runpy is awkward; call main).
            spec_mod = runpy.run_path(str(path), run_name="_chaos_engine_retrieve")
            return int(spec_mod["main"](arguments))
        if tool == "mempalace-mcp":
            arguments = mempalace_mcp_arguments(installed_root, arguments)
        command = resolve_command(installed_root, tool, arguments)
        environment = os.environ.copy()
        environment["PYTHONDONTWRITEBYTECODE"] = "1"
        invocation = (
            command
            if isinstance(command, list)
            else [str(command), *arguments]  # Compatibility for injected legacy tests.
        )
        return subprocess.call(  # nosec B603
            invocation,
            env=environment,
            cwd=shared_project_root(installed_root.resolve().parent),
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
