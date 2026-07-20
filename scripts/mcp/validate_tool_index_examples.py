#!/usr/bin/env python3
"""Offline example-schema staleness gate (tool architecture sweep design doc, amendment A6).

Every specialist shaft-skills/*/SKILL.md file has a `## Example calls` section with real
request/response JSON pairs (recorded per the design's Decision 8 E2E suite). Those examples are
prose, hand-copied into markdown -- nothing stops a future tool-schema change (a renamed param, a
deleted tool) from silently leaving a stale example behind. This script parses every `## Example
calls` request block, and cross-checks its top-level JSON keys against that tool's actual params
in the canonical tool-index.json (itself Java-dumped from the live schemas, see
scripts/mcp/generate_tool_index.py) -- catching drift without needing a live MCP run.

This is a deterministic, offline generator: no LLM, no Maven, no network.

Usage:
    python3 scripts/mcp/validate_tool_index_examples.py
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_SKILLS_ROOT = REPO_ROOT / "shaft-skills"
DEFAULT_TOOL_INDEX_PATH = (
    REPO_ROOT / "shaft-mcp" / "src" / "main" / "resources" / "META-INF" / "shaft-mcp" / "tool-index.json"
)

# A marker line such as "`tool_name` — request" or "`tool_name` - request" (em dash or hyphen).
MARKER_PATTERN = re.compile(r"`([a-z][a-z0-9_]*)`\s*[—-]\s*request\b")
FENCED_JSON_PATTERN = re.compile(r"```json\s*\n(.*?)```", re.DOTALL)
INLINE_JSON_PATTERN = re.compile(r"`(\{.*?\})`")


@dataclass(frozen=True)
class Example:
    tool_name: str
    request: dict
    source: str


def extract_examples(text: str, source_name: str = "<text>") -> list[Example]:
    """
    Returns every `` `tool_name` — request `` example's parsed request JSON, in document order.

    Each example's search window is bounded by the next marker (request or response) or the next
    heading, so an example never accidentally captures a later example's fenced block or inline
    JSON as its own.
    """
    marker_matches = list(MARKER_PATTERN.finditer(text))
    boundary_pattern = re.compile(r"`[a-z][a-z0-9_]*`\s*[—-]\s*(?:request|response)\b|^#{1,6} ", re.MULTILINE)

    examples = []
    for match in marker_matches:
        window_start = match.end()
        next_boundary = boundary_pattern.search(text, window_start)
        window_end = next_boundary.start() if next_boundary else len(text)
        window = text[window_start:window_end]

        fenced_match = FENCED_JSON_PATTERN.search(window)
        if fenced_match:
            request_text = fenced_match.group(1)
        else:
            inline_match = INLINE_JSON_PATTERN.search(window)
            if not inline_match:
                line_number = text.count("\n", 0, match.start()) + 1
                raise ValueError(
                    f"{source_name}:{line_number}: '{match.group(1)}' — request has no fenced ```json "
                    "block or inline `{...}` JSON before the next example/heading"
                )
            request_text = inline_match.group(1)

        line_number = text.count("\n", 0, match.start()) + 1
        try:
            request = json.loads(request_text)
        except json.JSONDecodeError as exc:
            raise ValueError(f"{source_name}:{line_number}: '{match.group(1)}' request JSON does not parse: {exc}")
        examples.append(Example(tool_name=match.group(1), request=request, source=f"{source_name}:{line_number}"))
    return examples


def check_example(example: Example, tool_index: dict) -> list[str]:
    """Returns human-readable problem descriptions for one example, or [] if it matches the
    tool's actual param schema."""
    tool = next((candidate for candidate in tool_index["tools"] if candidate["name"] == example.tool_name), None)
    if tool is None:
        return [f"{example.source}: references tool '{example.tool_name}', which does not exist in tool-index.json "
                 "(renamed, deleted, or a typo)"]

    if not isinstance(example.request, dict):
        return []  # a bare non-object request (uncommon) has no keys to cross-check

    known_params = {param["name"] for param in tool["params"]}
    unknown_keys = sorted(set(example.request.keys()) - known_params)
    if unknown_keys:
        return [f"{example.source}: '{example.tool_name}' example request references key(s) not in its actual "
                 f"param schema: {unknown_keys} (known params: {sorted(known_params)})"]
    return []


def validate_all(skills_root: Path, tool_index: dict) -> list[str]:
    problems = []
    for skill_file in sorted(skills_root.glob("*/SKILL.md")):
        text = skill_file.read_text(encoding="utf-8")
        if "## Example calls" not in text:
            continue
        source_name = str(skill_file.relative_to(REPO_ROOT)) if skill_file.is_relative_to(REPO_ROOT) else str(skill_file)
        try:
            examples = extract_examples(text, source_name)
        except ValueError as exc:
            problems.append(str(exc))
            continue
        for example in examples:
            problems.extend(check_example(example, tool_index))
    return problems


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Validate shaft-skills Example calls request JSON against tool-index.json params.",
    )
    parser.add_argument("--skills-root", type=Path, default=DEFAULT_SKILLS_ROOT)
    parser.add_argument("--tool-index-path", type=Path, default=DEFAULT_TOOL_INDEX_PATH)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if not args.tool_index_path.is_file():
        print(f"validate_tool_index_examples: {args.tool_index_path} does not exist; run "
              "'python3 scripts/mcp/generate_tool_index.py' first.", file=sys.stderr)
        return 1
    tool_index = json.loads(args.tool_index_path.read_text(encoding="utf-8"))

    problems = validate_all(args.skills_root, tool_index)
    if problems:
        print("validate_tool_index_examples: stale or broken Example calls found:", file=sys.stderr)
        for problem in problems:
            print(f"  - {problem}", file=sys.stderr)
        return 1

    print("All shaft-skills Example calls request JSON match tool-index.json.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
