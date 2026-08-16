#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
import runpy
import subprocess  # nosec B404 - executes only fixed owned tool names.
import sys
from pathlib import Path


TOOLS = {
    "uv": ("bootstrap", "uv"),
    "mempalace": ("bin", "mempalace"),
    "mempalace-mcp": ("bin", "mempalace-mcp"),
    "graphify": ("bin", "graphify"),
    "memory": ("npm/node_modules/.bin", "memory"),
    "memory-mcp": ("npm/node_modules/.bin", "memory-mcp"),
}


def load_host_controller(installed_root: Path):
    """Load the colocated state classifier without requiring a Python package."""
    path = installed_root / "hosts.py"
    if not path.is_file():
        raise ValueError("ChaosEngine host controller could not be loaded")
    return runpy.run_path(str(path), run_name="_chaos_engine_runtime_hosts")


def shared_mempalace_path(project: Path) -> Path:
    """Resolve one repository palace shared by the primary checkout and worktrees."""
    try:
        completed = subprocess.run(  # nosec B603 - fixed read-only git command.
            ["git", "rev-parse", "--git-common-dir"],
            cwd=project,
            check=True,
            capture_output=True,
            text=True,
        )
        common = Path(completed.stdout.strip())
        if not common.is_absolute():
            common = project / common
        return common.resolve() / "chaos-engine" / "mempalace"
    except (OSError, subprocess.SubprocessError):
        return project / ".chaos-engine-state" / "mempalace"


def canonical_mempalace_mcp_arguments(
    installed_root: Path, arguments: list[str]
) -> list[str]:
    """Replace the portable logical palace with the repository-shared path."""
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
    return [
        "--palace",
        str(shared_mempalace_path(project)),
        "--backend",
        "sqlite_exact",
    ]


def resolve_command(installed_root: Path, tool: str) -> Path:
    if tool not in TOOLS:
        raise ValueError(f"unsupported ChaosEngine tool: {tool}")
    project = installed_root.resolve().parent
    directory, name = TOOLS[tool]
    if tool == "uv":
        directory = f"bootstrap/{'Scripts' if os.name == 'nt' else 'bin'}"
    suffix = ".cmd" if os.name == "nt" and tool.startswith("memory") else ""
    if os.name == "nt" and tool in {"uv", "mempalace", "mempalace-mcp", "graphify"}:
        suffix = ".exe"
    command = project / ".chaos-engine-runtime" / directory / f"{name}{suffix}"
    if not command.is_file():
        raise ValueError(f"ChaosEngine tool is not installed: {command}")
    return command


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: tool.py <tool> [args...]", file=sys.stderr)
        return 2
    try:
        installed_root = Path(__file__).resolve().parent
        tool = sys.argv[1]
        arguments = sys.argv[2:]
        if tool == "mempalace-mcp":
            arguments = canonical_mempalace_mcp_arguments(installed_root, arguments)
        command = resolve_command(installed_root, tool)
        environment = os.environ.copy()
        environment["PYTHONDONTWRITEBYTECODE"] = "1"
        return subprocess.call(  # nosec B603
            [str(command), *arguments],
            env=environment,
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
