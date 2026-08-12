#!/usr/bin/env python3
"""Resolve and run a ChaosEngine project-local tool."""

from __future__ import annotations

import os
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
        command = resolve_command(Path(__file__).resolve().parent, sys.argv[1])
        return subprocess.call([str(command), *sys.argv[2:]])  # nosec B603
    except (OSError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
