#!/usr/bin/env python3
"""Validate agent guidance, repository memory, and their deterministic checks."""

from __future__ import annotations

import argparse
import json
import math
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci.validate_agent_guidance import (  # noqa: E402
    expand_globs,
    load_budget,
    parse_frontmatter,
    validate_repository as validate_guidance,
)
from scripts.ci.validate_documentation_boundaries import (  # noqa: E402
    validate_repository as validate_documentation,
)

MEMORY_PACKAGE = "@aictx/memory@0.1.55"
MEMORY_TOOLS = {
    "inspect_memory",
    "load_memory",
    "remember_memory",
    "search_memory",
}
GENERATED_MEMORY_PATHS = {
    ".memory/index/",
    ".memory/context/",
    ".memory/exports/",
    ".memory/recovery/",
    ".memory/.backup/",
    ".memory/.lock",
}


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable setup issue."""
    return {"code": code, "path": path, "message": message}


def read_json(path: Path) -> dict:
    """Read one UTF-8 JSON object."""
    return json.loads(path.read_text(encoding="utf-8"))


def toml_section(content: str, name: str) -> str:
    """Return one top-level TOML table body from the small project config."""
    match = re.search(
        rf"(?ms)^\[{re.escape(name)}\]\s*$\n(.*?)(?=^\[|\Z)", content
    )
    return match.group(1) if match else ""


def validate_memory_setup(root: Path = ROOT) -> list[dict[str, str]]:
    """Validate pinned Aictx storage and the restricted Codex MCP surface."""
    errors: list[dict[str, str]] = []
    required_files = [
        ".memory/config.json",
        ".memory/events.jsonl",
        ".memory/schema/config.schema.json",
        ".memory/schema/event.schema.json",
        ".memory/schema/object.schema.json",
        ".memory/schema/patch.schema.json",
        ".memory/schema/relation.schema.json",
        ".memory/memory/project.md",
        ".memory/memory/project.json",
        ".memory/memory/architecture.md",
        ".memory/memory/architecture.json",
        ".codex/config.toml",
    ]
    for configured_path in required_files:
        if not (root / configured_path).is_file():
            errors.append(issue("memory-file", configured_path, "required file is missing"))
    if errors:
        return errors

    try:
        config = read_json(root / ".memory/config.json")
    except (OSError, json.JSONDecodeError) as error:
        return [issue("memory-config", ".memory/config.json", str(error))]

    expected_config = {
        "version": 4,
        "project": {"id": "project.shaft-engine", "name": "Shaft Engine"},
        "memory": {
            "autoIndex": True,
            "defaultTokenBudget": 600,
            "saveContextPacks": False,
        },
        "git": {"trackContextPacks": False},
    }
    if config != expected_config:
        errors.append(
            issue(
                "memory-config",
                ".memory/config.json",
                "configuration must use schema v4, project.shaft-engine, and a 600-token budget",
            )
        )

    ignored = {
        line.strip()
        for line in (root / ".gitignore").read_text(encoding="utf-8").splitlines()
        if line.strip()
    }
    for generated_path in sorted(GENERATED_MEMORY_PATHS - ignored):
        errors.append(
            issue("memory-ignore", ".gitignore", f"generated path is not ignored: {generated_path}")
        )

    codex_content = (root / ".codex/config.toml").read_text(encoding="utf-8")
    server_content = toml_section(codex_content, "mcp_servers.shaft-memory")
    remember_content = toml_section(
        codex_content, "mcp_servers.shaft-memory.tools.remember_memory"
    )
    list_values: dict[str, list[str]] = {}
    for key in ("args", "enabled_tools"):
        match = re.search(rf"(?m)^{key}\s*=\s*(\[[^\n]+\])\s*$", server_content)
        if match:
            try:
                list_values[key] = json.loads(match.group(1))
            except json.JSONDecodeError:
                list_values[key] = []
    checks = {
        "server section": bool(server_content),
        "command": re.search(r'(?m)^command\s*=\s*"npx"\s*$', server_content)
        is not None,
        "args": list_values.get("args")
        == ["--yes", "--package", MEMORY_PACKAGE, "--", "memory-mcp"],
        "cwd": re.search(r'(?m)^cwd\s*=\s*"\.\."\s*$', server_content)
        is not None,
        "enabled_tools": set(list_values.get("enabled_tools", [])) == MEMORY_TOOLS,
        "default_tools_approval_mode": re.search(
            r'(?m)^default_tools_approval_mode\s*=\s*"auto"\s*$', server_content
        )
        is not None,
        "required": re.search(r"(?m)^required\s*=\s*false\s*$", server_content)
        is not None,
        "remember approval": (
            bool(remember_content)
            and re.search(r'(?m)^approval_mode\s*=\s*"prompt"\s*$', remember_content)
            is not None
        ),
    }
    for name, valid in checks.items():
        if not valid:
            errors.append(
                issue("memory-mcp", ".codex/config.toml", f"invalid shaft-memory {name}")
            )
    return errors


def run_command(root: Path, command: list[str], code: str) -> list[dict[str, str]]:
    """Run one external check and return a concise issue on failure."""
    executable = shutil.which(command[0])
    if executable is None:
        return [issue(code, command[0], "required executable is unavailable")]
    completed = subprocess.run(
        [executable, *command[1:]],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    if completed.returncode == 0:
        return []
    detail = (completed.stderr or completed.stdout).strip().replace("\n", " ")
    return [issue(code, command[0], detail[:500] or f"exit code {completed.returncode}")]


def collect_metrics(root: Path = ROOT) -> dict:
    """Collect stable context and memory size metrics."""
    budget = load_budget(root / "scripts/ci/agent_guidance_budget.json")
    guidance_paths = expand_globs(root, budget.get("total_guidance_globs", []))
    host_tokens: dict[str, int] = {}
    for host, configured_paths in budget.get("host_contexts", {}).items():
        characters = sum(
            len((root / path).read_text(encoding="utf-8"))
            for path in configured_paths
            if (root / path).is_file()
        )
        for path in expand_globs(
            root, budget.get("host_skill_metadata_globs", {}).get(host, [])
        ):
            frontmatter = parse_frontmatter(path.read_text(encoding="utf-8")) or {}
            characters += len(frontmatter.get("name", ""))
            characters += len(frontmatter.get("description", ""))
        host_tokens[host] = math.ceil(characters / 4)
    baseline = budget.get("reduction_baseline_bytes", 0)
    guidance_bytes = sum(path.stat().st_size for path in guidance_paths)
    reduction = 0 if not baseline else round((1 - guidance_bytes / baseline) * 100, 2)
    memory_config = root / ".memory/config.json"
    memory_budget = None
    if memory_config.is_file():
        try:
            memory_budget = read_json(memory_config).get("memory", {}).get(
                "defaultTokenBudget"
            )
        except (OSError, json.JSONDecodeError):
            pass
    memory_root = root / ".memory/memory"
    return {
        "guidance_bytes": guidance_bytes,
        "guidance_reduction_percent": reduction,
        "estimated_host_tokens": host_tokens,
        "memory_objects": len(list(memory_root.rglob("*.json")))
        if memory_root.is_dir()
        else 0,
        "memory_default_token_budget": memory_budget,
        "codex_memory_tools": sorted(MEMORY_TOOLS),
    }


def validate_repository(
    root: Path = ROOT, *, run_external: bool = True
) -> tuple[list[dict[str, str]], dict]:
    """Run all agent setup checks."""
    errors = [
        *validate_guidance(root),
        *[
            issue("documentation-boundary", "documentation", message)
            for message in validate_documentation(root)
        ],
        *validate_memory_setup(root),
    ]
    if run_external:
        errors.extend(
            run_command(
                root,
                [
                    "npx",
                    "--yes",
                    "--package",
                    MEMORY_PACKAGE,
                    "--",
                    "memory",
                    "check",
                ],
                "memory-check",
            )
        )
        errors.extend(run_command(root, ["git", "diff", "--check"], "diff-check"))
    return (
        sorted(errors, key=lambda item: (item["path"], item["code"], item["message"])),
        collect_metrics(root),
    )


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument(
        "--skip-external",
        action="store_true",
        help="Skip pinned Memory CLI and git diff checks.",
    )
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    errors, metrics = validate_repository(
        args.root.resolve(), run_external=not args.skip_external
    )
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors, "metrics": metrics}, indent=2))
    elif errors:
        for error in errors:
            print(f"{error['code']}: {error['path']}: {error['message']}", file=sys.stderr)
    else:
        print(
            "Agent setup is valid: "
            f"{metrics['guidance_bytes']} guidance bytes, "
            f"{metrics['guidance_reduction_percent']}% reduction, "
            f"{metrics['memory_objects']} memory objects."
        )
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
