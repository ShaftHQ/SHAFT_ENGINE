#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
import runpy
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}


def shared_project_root(project: Path) -> Path:
    """Resolve the primary checkout pinned to its current origin/main."""
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
    root = common.parent.resolve()
    revisions = subprocess.run(  # nosec B603 - fixed Git query, no shell.
        ["git", "rev-parse", "HEAD", "refs/remotes/origin/main"],
        cwd=root,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.splitlines()
    if len(revisions) != 2 or revisions[0] != revisions[1]:
        raise ValueError("shared project Memory is not synchronized with origin/main")
    return root


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
    project = shared_project_root(installed_root.resolve().parent)
    path = installed_root / "dependencies.py"
    if not path.is_file():
        raise ValueError("ChaosEngine dependency controller could not be loaded")
    controller = runpy.run_path(str(path), run_name="_chaos_engine_runtime_dependencies")
    return controller["active_dispatch"](project, tool, arguments or [])


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
            cwd=shared_project_root(installed_root.resolve().parent),
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
