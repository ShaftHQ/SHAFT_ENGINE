#!/usr/bin/env python3
"""Write the compact on-demand MCP and CLI caches from tool-index.json.

Compact cache rule: name, when to use, and essential arguments only. Do not
embed a JSON schema. Load this file only when the shaft skills entrypoint
selects this skill. Full schemas stay on the server until the agent calls one
tool. The CLI cache is the command index, so ordinary discovery does not run
--help.

    python3 scripts/mcp/generate_shaft_skill_tool_caches.py
    python3 scripts/mcp/generate_shaft_skill_tool_caches.py --check
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
TOOL_INDEX = (
    REPO_ROOT / "shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json"
)
MCP_CACHE = REPO_ROOT / "shaft-skills/shaft-mcp/references/cache.md"
CLI_CACHE = REPO_ROOT / "shaft-skills/shaft-cli/references/cache.md"
CACHE_RULE = (
    "Compact cache rule: name, when to use, and essential arguments only. "
    "Do not embed a JSON schema. Load this file only when the shaft skills "
    "entrypoint selects this skill."
)
CALL_FORM = (
    "shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]"
)
WHEN_LIMIT = 96


def _when(description: str) -> str:
    text = description.replace("|", "/").replace("`", "'").replace("\n", " ").strip()
    if len(text) <= WHEN_LIMIT:
        return text
    return text[: WHEN_LIMIT - 3].rstrip() + "..."


def _args(params: list[dict]) -> str:
    if not params:
        return "-"
    names = []
    for param in params:
        name = str(param.get("name") or "").strip()
        if not name:
            continue
        if not param.get("required"):
            name += "?"
        names.append(name)
    return ", ".join(names) if names else "-"


def _table(tools: list[dict]) -> str:
    lines = ["| Tool | When | Args |", "| --- | --- | --- |"]
    for tool in tools:
        lines.append(
            f"| `{tool['name']}` | {_when(str(tool.get('description') or ''))} | {_args(tool.get('params') or [])} |"
        )
    return "\n".join(lines)


def render_mcp(index: dict) -> str:
    tools = index["tools"]
    body = "\n".join(
        [
            "# MCP cache",
            "",
            CACHE_RULE,
            "",
            "Defer every full schema until the one tool you will call. "
            "Do not paste this table's rows into the ChaosEngine entry.",
            "",
            _table(tools),
            "",
        ]
    )
    return body


def render_cli(index: dict) -> str:
    tools = index["tools"]
    forms = "\n".join(
        [
            "## Command forms",
            "",
            "Do not run `--help` for ordinary discovery.",
            "",
            f"- `{CALL_FORM}`",
            "- `shaft-cli session start`",
            "- `shaft-cli session status`",
            "- `shaft-cli session stop`",
            "- `shaft-cli tools --cached [--json]`",
            "- `shaft-cli codegen [--session <path>]`",
            "- `shaft-cli setup catalog|profiles [--json]`",
            "- `shaft-cli setup doctor|status|verify --profile REPORTING [--json]`",
            "- `shaft-cli setup plan --profile REPORTING --mode MANAGED --output <absolute-plan.json>`",
            "- `shaft-cli setup install|apply|update --plan <absolute-plan.json> --approve <sha256:digest>`",
            "- `shaft-cli setup start|stop --profile <profile>`",
            "- `shaft-cli setup logs --profile REPORTING`",
            "- `shaft-cli browser navigate|screenshot|dom|url`",
            "- `shaft-cli element click|type|hover`",
            "- `shaft-cli capture start|stop|status|code|step-delete|step-reorder`",
            "- `shaft-cli guide search`",
            "- `shaft-cli doctor analyze|suggest|local-ai-status`",
            "",
            "Browser, element, capture, and driver commands need `shaft-cli session start` "
            "unless the call is a deliberately disposable one-shot.",
            "",
            "## Tools",
            "",
            "Pass required names as `key=value`. A trailing `?` is optional.",
            "",
            _table(tools),
        ]
    )
    return "\n".join(
        [
            "# CLI cache",
            "",
            CACHE_RULE,
            "",
            forms,
            "",
        ]
    )


def _load(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--tool-index", type=Path, default=TOOL_INDEX)
    args = parser.parse_args(argv)
    index = _load(args.tool_index)
    expected = {MCP_CACHE: render_mcp(index), CLI_CACHE: render_cli(index)}
    if args.check:
        stale = [
            path.relative_to(REPO_ROOT).as_posix()
            for path, content in expected.items()
            if not path.is_file() or path.read_text(encoding="utf-8") != content
        ]
        if stale:
            print("stale shaft skill tool cache: " + ", ".join(stale), file=sys.stderr)
            return 1
        print("shaft skill tool caches are up to date.")
        return 0
    for path, content in expected.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
        print(path.relative_to(REPO_ROOT).as_posix())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
