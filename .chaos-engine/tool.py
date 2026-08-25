#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
import runpy
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {"uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"}


def load_host_controller(installed_root: Path):
    """Load the colocated state classifier without requiring a Python package."""
    path = installed_root / "hosts.py"
    if not path.is_file():
        raise ValueError("ChaosEngine host controller could not be loaded")
    return runpy.run_path(str(path), run_name="_chaos_engine_runtime_hosts")


def guard_mempalace_mcp(installed_root: Path, arguments: list[str]) -> None:
    """Refuse the real MCP launch unless project-local SQLite-exact state is healthy."""
    project = installed_root.resolve().parent
    expected_arguments = [
        "--palace",
        ".chaos-engine-state/mempalace",
        "--backend",
        "sqlite_exact",
    ]
    if arguments != expected_arguments:
        raise ValueError(
            "MemPalace MCP requires the exact project-local sqlite_exact launch contract"
        )
    controller = load_host_controller(installed_root)
    state = controller["mempalace_runtime_status"](project)
    if state.get("status") != "healthy":
        raise ValueError(
            f"MemPalace runtime is {state.get('status', 'unknown')}: "
            f"{state.get('detail', 'operator action required')}"
        )


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
            guard_mempalace_mcp(installed_root, arguments)
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
