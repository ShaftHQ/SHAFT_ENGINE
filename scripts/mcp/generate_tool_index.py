#!/usr/bin/env python3
"""Merges the Java-dumped mechanical tool schema with the curated overlay into tool-index.json.

Design doc (tool architecture sweep) Decision 4 / amendment A5: the canonical
shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json is NOT produced by a regex scan --
that approach was explicitly ruled out because it cannot see the actual live Spring-AI JSON
schemas (policy-driven defaults, generated `required`/`type` shapes). Instead:

  1. tool-index-mechanical.json is dumped by shaft-mcp's ToolIndexMechanicalDumpTest straight from
     the live Spring-registered ToolCallback/@McpTool schemas (names, services, descriptions,
     param schemas).
  2. tool-index-overlay.json is hand-curated (mutation/sensitive/intentKeywords/slashAlias/
     cliCommand/example/paramDefaults), keyed by tool name.
  3. This script merges the two into tool-index.json -- the single file shaft-cli, the IntelliJ
     plugin, and skills all read.

This is a deterministic, offline generator: no LLM, no Maven, no network -- it only reads the two
already-materialized JSON files.

Usage:
    python3 scripts/mcp/generate_tool_index.py            # (re)write tool-index.json
    python3 scripts/mcp/generate_tool_index.py --check    # verify no drift; no write
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
RESOURCE_DIR = REPO_ROOT / "shaft-mcp" / "src" / "main" / "resources" / "META-INF" / "shaft-mcp"
DEFAULT_MECHANICAL_PATH = RESOURCE_DIR / "tool-index-mechanical.json"
DEFAULT_OVERLAY_PATH = RESOURCE_DIR / "tool-index-overlay.json"
DEFAULT_OUTPUT_PATH = RESOURCE_DIR / "tool-index.json"
GENERATOR_RELATIVE_PATH = "scripts/mcp/generate_tool_index.py"


class ToolIndexMergeError(RuntimeError):
    """Raised when the mechanical dump and the curated overlay disagree on which tools exist."""


def _merge_param(param: dict, curated_defaults: dict) -> dict:
    return {
        "name": param["name"],
        "type": param["type"],
        "required": param["required"],
        "default": curated_defaults.get(param["name"]),
        "description": param.get("description"),
    }


def merge(mechanical: dict, overlay: dict) -> dict:
    """Merges a mechanical-dump document and a curated-overlay document into one tool-index
    document. Raises ToolIndexMergeError if the two disagree on the tool-name set -- a mechanical
    tool with no overlay entry (curated metadata never written for a newly added tool) or an
    overlay entry with no mechanical tool (curated metadata left behind after a tool was renamed
    or deleted) are both treated as errors, not silently dropped."""
    mechanical_tools = mechanical["tools"]
    overlay_tools = overlay["tools"]

    mechanical_names = {tool["name"] for tool in mechanical_tools}
    overlay_names = set(overlay_tools.keys())

    missing_from_overlay = sorted(mechanical_names - overlay_names)
    unknown_in_overlay = sorted(overlay_names - mechanical_names)
    if missing_from_overlay or unknown_in_overlay:
        problems = []
        if missing_from_overlay:
            problems.append(f"tool(s) with no curated overlay entry: {missing_from_overlay}")
        if unknown_in_overlay:
            problems.append(f"overlay entry/entries for tool(s) that no longer exist: {unknown_in_overlay}")
        raise ToolIndexMergeError("tool-index-overlay.json is out of sync with tool-index-mechanical.json -- "
                                   + "; ".join(problems))

    merged_tools = []
    for tool in mechanical_tools:
        curated = overlay_tools[tool["name"]]
        curated_defaults = curated.get("paramDefaults", {})
        merged_tools.append({
            "name": tool["name"],
            "service": tool["service"],
            "description": tool["description"],
            "params": [_merge_param(param, curated_defaults) for param in tool["params"]],
            "mutation": curated["mutation"],
            "sensitive": curated["sensitive"],
            "intentKeywords": curated.get("intentKeywords", []),
            "slashAlias": curated.get("slashAlias"),
            "cliCommand": curated.get("cliCommand"),
            "example": curated.get("example"),
        })

    return {"schemaVersion": "1.0", "tools": merged_tools}


def render(document: dict) -> str:
    return json.dumps(document, indent=2) + "\n"


def build(mechanical_path: Path, overlay_path: Path) -> str:
    mechanical = json.loads(mechanical_path.read_text(encoding="utf-8"))
    overlay = json.loads(overlay_path.read_text(encoding="utf-8"))
    return render(merge(mechanical, overlay))


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Merge the mechanical tool-index dump with the curated overlay into tool-index.json.",
    )
    parser.add_argument("--check", action="store_true",
                         help="Verify the checked-in tool-index.json matches the merge output; do not write.")
    parser.add_argument("--mechanical-path", type=Path, default=DEFAULT_MECHANICAL_PATH)
    parser.add_argument("--overlay-path", type=Path, default=DEFAULT_OVERLAY_PATH)
    parser.add_argument("--output-path", type=Path, default=DEFAULT_OUTPUT_PATH)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    try:
        content = build(args.mechanical_path, args.overlay_path)
    except (ToolIndexMergeError, FileNotFoundError, json.JSONDecodeError) as exc:
        print(f"generate_tool_index: {exc}", file=sys.stderr)
        return 1

    if args.check:
        if not args.output_path.is_file():
            print(f"generate_tool_index: {args.output_path} does not exist; run "
                  f"'python3 {GENERATOR_RELATIVE_PATH}' to generate it.", file=sys.stderr)
            return 1
        # Compare newline-insensitively: git autocrlf checkouts materialize the committed index
        # with CRLF on Windows, and that is not content drift.
        existing = args.output_path.read_bytes().replace(b"\r\n", b"\n")
        expected = content.encode("utf-8")
        if existing != expected:
            print(f"generate_tool_index: {args.output_path} is out of date with "
                  f"{args.mechanical_path.name}/{args.overlay_path.name}. Re-run "
                  f"'python3 {GENERATOR_RELATIVE_PATH}' and commit the result.", file=sys.stderr)
            return 1
        print(f"{args.output_path} is up to date.")
        return 0

    args.output_path.parent.mkdir(parents=True, exist_ok=True)
    args.output_path.write_text(content, encoding="utf-8", newline="\n")
    print(f"Generated {args.output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
