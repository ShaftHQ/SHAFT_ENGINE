#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
import runpy
import shutil
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}


def shared_runtime_project(project: Path) -> Path:
    """Resolve the primary checkout that owns one runtime for all worktrees."""
    git = shutil.which("git")
    if git is None:
        return project
    completed = subprocess.run(  # nosec B603
        [git, "rev-parse", "--git-common-dir"], cwd=project,
        capture_output=True, text=True, check=False,
    )
    if completed.returncode != 0 or not completed.stdout.strip():
        return project
    common = Path(completed.stdout.strip())
    if not common.is_absolute():
        common = (project / common).resolve()
    return common.parent


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
        palace = Path(namespace["find_shared_mempalace"](project)).resolve()
    if not palace.is_absolute():
        raise ValueError("MemPalace MCP resolver returned a relative path")
    controller = load_host_controller(installed_root)
    state = controller["mempalace_runtime_status"](project)
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
    project = installed_root.resolve().parent
    path = installed_root / "dependencies.py"
    if not path.is_file():
        raise ValueError("ChaosEngine dependency controller could not be loaded")
    controller = runpy.run_path(str(path), run_name="_chaos_engine_runtime_dependencies")
    return controller["active_dispatch"](
        shared_runtime_project(project), tool, arguments or []
    )


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: tool.py <tool> [args...]", file=sys.stderr)
        return 2
    try:
        installed_root = Path(__file__).resolve().parent
        tool = sys.argv[1]
        arguments = sys.argv[2:]
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
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
