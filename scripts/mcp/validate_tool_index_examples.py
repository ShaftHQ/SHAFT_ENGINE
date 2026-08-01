#!/usr/bin/env python3
"""Validate delivered SHAFT skills and their canonical MCP tool references.

Specialist skills and their linked playbooks can carry `## Example calls` sections with real
request/response JSON pairs. Those examples are prose copied into Markdown, so a renamed param or
deleted tool can silently leave a stale example behind. This script parses every delivered
`## Example calls` request block and cross-checks its top-level JSON keys against that tool's
actual params in canonical tool-index.json (see scripts/mcp/generate_tool_index.py).

The same offline pass also enforces the installed skill-family contract: every identifier is
``shaft-*`` hyphen-case, folder/frontmatter names agree, only ``name`` and ``description`` appear
in frontmatter, the ``shaft-developer`` hub directly links every routed specialist, local links
resolve, and each specialist directly links its hub and playbook. Literal MCP names are checked
against the canonical tool index, so retired names fail even outside ``## Example calls``.

This is deterministic and offline: no LLM, no Maven, no network.

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
from urllib.parse import unquote

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_SKILLS_ROOT = REPO_ROOT / "shaft-skills"
DEFAULT_TOOL_INDEX_PATH = (
    REPO_ROOT / "shaft-mcp" / "src" / "main" / "resources" / "META-INF" / "shaft-mcp" / "tool-index.json"
)
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.ci.validate_agent_guidance import parse_frontmatter  # noqa: E402

HUB_SKILL_NAME = "shaft-developer"
SKILL_NAME_PATTERN = re.compile(r"shaft-[a-z0-9]+(?:-[a-z0-9]+)*$")
ALLOWED_FRONTMATTER_KEYS = {"name", "description"}
MIN_DESCRIPTION_CHARS = 20
MARKDOWN_LINK_PATTERN = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
FRONTMATTER_KEY_PATTERN = re.compile(r"(?m)^([A-Za-z][A-Za-z0-9_-]*):")
EXAMPLE_SECTION_PATTERN = re.compile(
    r"(?ms)^## .*?(?:[Ee]xamples|[Uu]se cases)\s*$\n(.*?)(?=^## |\Z)"
)
EXAMPLE_ITEM_PATTERN = re.compile(r"(?m)^\s*(?:[-*]|\d+[.)])\s+\S|^###\s+\S")
CATALOG_TOOL_PATTERN = re.compile(r"(?m)^- `([a-z][a-z0-9_]*)`\s+—")
EXPLICIT_TOOL_PATTERNS = (
    re.compile(r"shaft-mcp:([a-z][a-z0-9_]*)"),
    re.compile(r"mcp__shaft-mcp__([a-z][a-z0-9_]*)"),
    re.compile(r"shaft-cli\s+call\s+`?([a-z][a-z0-9_]*)`?"),
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


def validate_examples(skills_root: Path, tool_index: dict) -> list[str]:
    """Validate request examples against canonical tool names and parameter schemas."""
    problems = []
    for skill_file in sorted(skills_root.rglob("*.md")):
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


def _display(skills_root: Path, path: Path) -> str:
    try:
        return path.relative_to(skills_root).as_posix()
    except ValueError:
        return str(path)


def _frontmatter_keys(content: str) -> set[str]:
    if not content.startswith("---\n"):
        return set()
    marker = content.find("\n---\n", 4)
    if marker < 0:
        return set()
    return set(FRONTMATTER_KEY_PATTERN.findall(content[4:marker]))


def _section(content: str, heading: str) -> str | None:
    match = re.search(
        rf"(?ms)^## {re.escape(heading)}\s*$\n(.*?)(?=^## |\Z)",
        content,
    )
    return match.group(1) if match else None


def _local_link_targets(path: Path, content: str, skills_root: Path) -> list[Path]:
    targets: list[Path] = []
    for raw_target in MARKDOWN_LINK_PATTERN.findall(content):
        target = unquote(raw_target.strip().strip("<>"))
        if not target or target.startswith(("#", "http://", "https://", "mailto:")):
            continue
        target = target.split("#", 1)[0]
        resolved = (path.parent / target).resolve()
        try:
            resolved.relative_to(skills_root.resolve())
        except ValueError:
            continue
        targets.append(resolved)
    return targets


def _direct_playbooks(skill_path: Path, content: str, skills_root: Path) -> list[Path]:
    references_root = skill_path.parent / "references"
    return [
        target
        for target in _local_link_targets(skill_path, content, skills_root)
        if target.suffix.lower() == ".md" and target.is_file() and target.is_relative_to(references_root)
    ]


def _example_count(playbooks: list[Path]) -> int:
    return sum(
        len(EXAMPLE_ITEM_PATTERN.findall(section)) + len(MARKER_PATTERN.findall(section))
        for playbook in playbooks
        for section in EXAMPLE_SECTION_PATTERN.findall(playbook.read_text(encoding="utf-8"))
    )


def _literal_tool_names(path: Path, content: str, skills_root: Path) -> set[str]:
    names = {
        name
        for pattern in EXPLICIT_TOOL_PATTERNS
        for name in pattern.findall(content)
    }
    names.update(match.group(1) for match in MARKER_PATTERN.finditer(content))
    if path == skills_root / "references" / "shaft-mcp-tools.md":
        names.update(CATALOG_TOOL_PATTERN.findall(content))
    return names


def validate_delivery(skills_root: Path, tool_index: dict) -> list[str]:
    """Validate the portable shaft-skills tree and every literal MCP reference."""
    problems: list[str] = []
    if not skills_root.is_dir():
        return [f"{skills_root}: shaft-skills directory is missing"]

    canonical_names = {tool["name"] for tool in tool_index.get("tools", []) if tool.get("name")}
    skill_dirs = sorted(
        path for path in skills_root.iterdir() if path.is_dir() and path.name != "references"
    )
    skill_names = {path.name for path in skill_dirs}
    specialist_names = skill_names - {HUB_SKILL_NAME}

    for skill_dir in skill_dirs:
        skill_path = skill_dir / "SKILL.md"
        display = _display(skills_root, skill_path)
        if not SKILL_NAME_PATTERN.fullmatch(skill_dir.name):
            problems.append(f"{display}: skill identifier must use shaft-* lowercase hyphen-case")
        if not skill_path.is_file():
            problems.append(f"{display}: SKILL.md is required")
            continue
        content = skill_path.read_text(encoding="utf-8")
        frontmatter = parse_frontmatter(content)
        if frontmatter is None:
            problems.append(f"{display}: valid YAML frontmatter is required")
        else:
            if frontmatter.get("name") != skill_dir.name:
                problems.append(
                    f"{display}: frontmatter name {frontmatter.get('name')!r} must match folder {skill_dir.name!r}"
                )
            description = frontmatter.get("description", "")
            if not description:
                problems.append(f"{display}: frontmatter description is required")
            elif len(description.strip()) < MIN_DESCRIPTION_CHARS:
                problems.append(
                    f"{display}: frontmatter description must be a meaningful trigger description "
                    f"({MIN_DESCRIPTION_CHARS}+ characters)"
                )
            extra_keys = sorted(_frontmatter_keys(content) - ALLOWED_FRONTMATTER_KEYS)
            if extra_keys:
                problems.append(f"{display}: unsupported frontmatter key(s): {extra_keys}")

        if skill_dir.name != HUB_SKILL_NAME:
            hub_target = (skills_root / HUB_SKILL_NAME / "SKILL.md").resolve()
            if hub_target not in _local_link_targets(skill_path, content, skills_root):
                problems.append(f"{display}: specialist must include a direct hub link to {HUB_SKILL_NAME}")
            playbooks = _direct_playbooks(skill_path, content, skills_root)
            if not playbooks:
                problems.append(f"{display}: specialist must include one direct playbook link under references/")
            elif _example_count(playbooks) < 2:
                problems.append(
                    f"{display}: direct playbook must provide at least two valid examples or use cases"
                )

    hub_path = skills_root / HUB_SKILL_NAME / "SKILL.md"
    if not hub_path.is_file():
        problems.append(f"{HUB_SKILL_NAME}/SKILL.md is required as the installed SHAFT skill hub")
    else:
        hub_content = hub_path.read_text(encoding="utf-8")
        routing_path = hub_path.parent / "references" / "routing.md"
        if routing_path not in _local_link_targets(hub_path, hub_content, skills_root):
            problems.append(f"{HUB_SKILL_NAME}/SKILL.md: direct link to references/routing.md is required")
        if not routing_path.is_file():
            problems.append(f"{HUB_SKILL_NAME}/references/routing.md is required")
        else:
            route_counts: dict[str, int] = {}
            for target in _local_link_targets(
                routing_path, routing_path.read_text(encoding="utf-8"), skills_root
            ):
                if target.name != "SKILL.md" or not target.parent.name.startswith("shaft-"):
                    continue
                route_name = target.parent.name
                route_counts[route_name] = route_counts.get(route_name, 0) + 1
                if route_name not in specialist_names:
                    problems.append(
                        f"{HUB_SKILL_NAME}/references/routing.md: orphan route targets missing specialist {route_name!r}"
                    )
            for missing in sorted(specialist_names - route_counts.keys()):
                problems.append(
                    f"{HUB_SKILL_NAME}/references/routing.md: missing route for specialist {missing!r}"
                )
            for duplicate, count in sorted(route_counts.items()):
                if count > 1:
                    problems.append(
                        f"{HUB_SKILL_NAME}/references/routing.md: duplicate route for specialist {duplicate!r} ({count} links)"
                    )
            hub_links = {
                target.parent.name
                for target in _local_link_targets(hub_path, hub_content, skills_root)
                if target.name == "SKILL.md" and target.parent.name.startswith("shaft-")
            }
            for route_name in sorted(route_counts):
                if route_name not in hub_links:
                    problems.append(
                        f"{HUB_SKILL_NAME}/SKILL.md: missing direct link for routed specialist {route_name!r}"
                    )

    for markdown_path in sorted(skills_root.rglob("*.md")):
        content = markdown_path.read_text(encoding="utf-8")
        display = _display(skills_root, markdown_path)
        for raw_target in MARKDOWN_LINK_PATTERN.findall(content):
            target = unquote(raw_target.strip().strip("<>"))
            if not target or target.startswith(("#", "http://", "https://", "mailto:")):
                continue
            target = target.split("#", 1)[0]
            resolved = (markdown_path.parent / target).resolve()
            try:
                resolved.relative_to(skills_root.resolve())
            except ValueError:
                problems.append(f"{display}: local reference leaves delivered shaft-skills: {raw_target}")
                continue
            if not resolved.exists():
                problems.append(f"{display}: missing local reference: {raw_target}")

        for tool_name in sorted(_literal_tool_names(markdown_path, content, skills_root)):
            if tool_name not in canonical_names:
                problems.append(
                    f"{display}: literal MCP tool {tool_name!r} is absent from canonical tool-index.json"
                )

    catalog_path = skills_root / "references" / "shaft-mcp-tools.md"
    if not catalog_path.is_file():
        problems.append("references/shaft-mcp-tools.md: generated MCP catalog is required")
    else:
        catalog_names = CATALOG_TOOL_PATTERN.findall(catalog_path.read_text(encoding="utf-8"))
        missing = sorted(canonical_names - set(catalog_names))
        extra = sorted(set(catalog_names) - canonical_names)
        duplicates = sorted(name for name in set(catalog_names) if catalog_names.count(name) > 1)
        if missing or extra or duplicates:
            problems.append(
                "references/shaft-mcp-tools.md: generated catalog differs from canonical tool-index.json "
                f"(missing={missing}, extra={extra}, duplicates={duplicates})"
            )

    return sorted(set(problems))


def validate_all(skills_root: Path, tool_index: dict) -> list[str]:
    return sorted(validate_delivery(skills_root, tool_index) + validate_examples(skills_root, tool_index))


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Validate delivered shaft-skills structure and MCP references against tool-index.json.",
    )
    parser.add_argument("--skills-root", type=Path, default=DEFAULT_SKILLS_ROOT)
    parser.add_argument("--tool-index-path", type=Path, default=DEFAULT_TOOL_INDEX_PATH)
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    skills_root = args.skills_root.resolve()
    tool_index_path = args.tool_index_path.resolve()
    if not tool_index_path.is_file():
        print(f"validate_tool_index_examples: {tool_index_path} does not exist; run "
              "'python3 scripts/mcp/generate_tool_index.py' first.", file=sys.stderr)
        return 1
    tool_index = json.loads(tool_index_path.read_text(encoding="utf-8"))

    problems = validate_all(skills_root, tool_index)
    if problems:
        print("validate_tool_index_examples: delivered skill contract problems found:", file=sys.stderr)
        for problem in problems:
            print(f"  - {problem}", file=sys.stderr)
        return 1

    print("Delivered shaft-skills structure, links, examples, and MCP references match tool-index.json.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
