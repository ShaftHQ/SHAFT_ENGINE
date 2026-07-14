#!/usr/bin/env python3
"""Generates the SHAFT MCP tool catalog markdown from @Tool annotations in shaft-mcp Java sources.

This is a deterministic, offline generator: no LLM, no Maven, no network. It scans every
*.java file under shaft-mcp/src/main/java/com/shaft/mcp/ for Spring AI @Tool(...) annotations,
extracts each tool's name and description, and writes a compact agent-facing catalog to
shaft-skills/references/shaft-mcp-tools.md grouped by service class.

Usage:
    python3 scripts/mcp/generate_shaft_skills_tool_catalog.py            # (re)write the catalog
    python3 scripts/mcp/generate_shaft_skills_tool_catalog.py --check    # verify no drift; no write
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCE_DIR = REPO_ROOT / "shaft-mcp" / "src" / "main" / "java" / "com" / "shaft" / "mcp"
OUTPUT_PATH = REPO_ROOT / "shaft-skills" / "references" / "shaft-mcp-tools.md"
GENERATOR_RELATIVE_PATH = "scripts/mcp/generate_shaft_skills_tool_catalog.py"
TOOL_MARKER = "@Tool("
# Spring AI exposes MCP tools via two annotations: the plain @Tool (registered through
# ToolCallbacks.from) and @McpTool (annotation-scanned; the only form that can receive an
# McpSyncServerExchange / @McpProgressToken and emit notifications/progress — see #3546). Both
# carry the same name/description attributes, so the catalog scans them identically. "@McpTool("
# never contains "@Tool(" as a substring, so the two markers count disjointly.
MCP_TOOL_MARKER = "@McpTool("
TOOL_MARKERS = (TOOL_MARKER, MCP_TOOL_MARKER)
SERVICE_CLASS_SUFFIX = "Service"


def count_tool_markers(text: str) -> int:
    """Total @Tool( plus @McpTool( literal occurrences in one file."""
    return sum(text.count(marker) for marker in TOOL_MARKERS)

# Basic Java escape sequences that can legally appear inside a string literal in these
# annotations. Anything not listed here is passed through unescaped (the backslash is dropped
# and the following character is kept literally), matching javac's behavior closely enough for
# the plain ASCII descriptions used in shaft-mcp tool annotations.
JAVA_STRING_ESCAPES = {'"': '"', "\\": "\\", "n": "\n", "t": "\t", "r": "\r"}


class CatalogError(RuntimeError):
    """Raised when the Java sources cannot be parsed into a consistent tool catalog."""


@dataclass(frozen=True)
class ToolEntry:
    name: str
    description: str


@dataclass(frozen=True)
class ServiceSection:
    class_name: str
    tools: tuple[ToolEntry, ...]


def log(message: str) -> None:
    print(message, file=sys.stderr)


def fail(message: str) -> None:
    raise CatalogError(message)


def display_path(path: Path) -> str:
    """Path relative to the repo root when possible (e.g. for real sources), otherwise the raw
    path (e.g. for temporary test fixtures that live outside the repo)."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def find_source_files(source_dir: Path) -> list[Path]:
    """
    Every *.java file directly under source_dir, plus any @Tool-bearing file nested deeper.

    The 15 shaft-mcp service classes currently live flat in source_dir, so the non-recursive
    scan covers them all; the recursive fallback only exists so a future @Tool annotation added
    to a nested file is not silently skipped.
    """
    if not source_dir.is_dir():
        fail(f"shaft-mcp source directory not found: {source_dir}")
    flat_files = sorted(source_dir.glob("*.java"))
    flat_set = set(flat_files)
    nested_files = []
    for path in sorted(source_dir.rglob("*.java")):
        if path in flat_set:
            continue
        if count_tool_markers(path.read_text(encoding="utf-8")) > 0:
            nested_files.append(path)
    return flat_files + nested_files


def find_tool_annotations(text: str) -> list[int]:
    """Returns the index right after each literal "@Tool(" / "@McpTool(" occurrence, in source order."""
    positions = []
    for marker in TOOL_MARKERS:
        index = 0
        while True:
            index = text.find(marker, index)
            if index == -1:
                break
            positions.append(index + len(marker))
            index += len(marker)
    return sorted(positions)


def extract_annotation_body(text: str, start: int) -> str | None:
    """
    Scans from just after "@Tool(" to the matching close paren, tracking string-literal state
    so parens or commas inside a description string never confuse the match. Returns the body
    text (exclusive of the closing paren), or None if the annotation is unterminated.
    """
    depth = 1
    in_string = False
    i = start
    n = len(text)
    while i < n:
        char = text[i]
        if in_string:
            if char == "\\":
                i += 2
                continue
            if char == '"':
                in_string = False
            i += 1
            continue
        if char == '"':
            in_string = True
            i += 1
            continue
        if char == "(":
            depth += 1
            i += 1
            continue
        if char == ")":
            depth -= 1
            i += 1
            if depth == 0:
                return text[start:i - 1]
            continue
        i += 1
    return None


def split_top_level_arguments(body: str) -> list[str]:
    """Splits an annotation body on top-level commas, ignoring commas inside string literals
    or inside any nested bracket/paren/brace."""
    parts: list[str] = []
    current: list[str] = []
    depth = 0
    in_string = False
    i = 0
    n = len(body)
    while i < n:
        char = body[i]
        if in_string:
            current.append(char)
            if char == "\\" and i + 1 < n:
                current.append(body[i + 1])
                i += 2
                continue
            if char == '"':
                in_string = False
            i += 1
            continue
        if char == '"':
            in_string = True
            current.append(char)
            i += 1
            continue
        if char in "([{":
            depth += 1
            current.append(char)
            i += 1
            continue
        if char in ")]}":
            depth -= 1
            current.append(char)
            i += 1
            continue
        if char == "," and depth == 0:
            parts.append("".join(current))
            current = []
            i += 1
            continue
        current.append(char)
        i += 1
    if current:
        parts.append("".join(current))
    return [part for part in parts if part.strip()]


def parse_java_string_concatenation(expression: str) -> str | None:
    """
    Parses a Java expression that must be one or more string literals joined by '+' (the shape
    every @Tool name/description attribute takes in this codebase, including the multi-line
    concatenation style). Returns the joined, unescaped text, or None if the expression contains
    anything other than quoted string literals and '+'/whitespace between them (for example a
    bare constant reference), which the caller treats as unparseable.
    """
    result: list[str] = []
    found_string = False
    i = 0
    n = len(expression)
    while i < n:
        char = expression[i]
        if char.isspace() or char == "+":
            i += 1
            continue
        if char != '"':
            return None
        i += 1
        buffer: list[str] = []
        closed = False
        while i < n:
            char = expression[i]
            if char == "\\" and i + 1 < n:
                escaped = expression[i + 1]
                buffer.append(JAVA_STRING_ESCAPES.get(escaped, escaped))
                i += 2
                continue
            if char == '"':
                closed = True
                i += 1
                break
            buffer.append(char)
            i += 1
        if not closed:
            return None
        result.append("".join(buffer))
        found_string = True
    return "".join(result) if found_string else None


def parse_tool_attributes(body: str) -> dict[str, str] | None:
    """Parses an @Tool(...) body into an attribute-name -> string-value mapping, or None if any
    attribute's value is not a pure string-literal concatenation."""
    attributes: dict[str, str] = {}
    for argument in split_top_level_arguments(body):
        if "=" not in argument:
            return None
        key, _, raw_value = argument.partition("=")
        key = key.strip()
        value = parse_java_string_concatenation(raw_value.strip())
        if value is None:
            return None
        attributes[key] = value
    return attributes


def parse_file(path: Path, text: str) -> tuple[list[ToolEntry], list[str]]:
    """Returns (tool entries in source order, parse-error messages) for one Java file."""
    entries: list[ToolEntry] = []
    errors: list[str] = []
    for start in find_tool_annotations(text):
        line_number = text.count("\n", 0, start) + 1
        body = extract_annotation_body(text, start)
        if body is None:
            errors.append(f"{path.name}:{line_number}: unterminated @Tool(...) annotation")
            continue
        attributes = parse_tool_attributes(body)
        if attributes is None:
            snippet = " ".join(body.split())[:120]
            errors.append(
                f"{path.name}:{line_number}: could not parse @Tool attributes as string "
                f"literals (body: {snippet!r})"
            )
            continue
        name = attributes.get("name")
        description = attributes.get("description")
        if not name or not description:
            errors.append(
                f"{path.name}:{line_number}: @Tool annotation is missing a name or description attribute"
            )
            continue
        entries.append(ToolEntry(name=name, description=description))
    return entries, errors


def build_catalog(source_dir: Path) -> tuple[list[ServiceSection], int]:
    """Parses every source file and returns (service sections sorted by class name, total tool
    count), or raises CatalogError if the sources cannot be parsed consistently."""
    files = find_source_files(source_dir)
    services: list[ServiceSection] = []
    mismatched_files: list[str] = []
    all_errors: list[str] = []
    total_parsed = 0
    name_to_files: dict[str, list[str]] = {}

    for path in files:
        text = path.read_text(encoding="utf-8")
        raw_count = count_tool_markers(text)
        if raw_count == 0:
            continue
        entries, errors = parse_file(path, text)
        if errors or len(entries) != raw_count:
            mismatched_files.append(
                f"{display_path(path)}: found {raw_count} @Tool(/@McpTool( occurrence(s), parsed {len(entries)}"
            )
            all_errors.extend(errors)
            continue
        total_parsed += len(entries)
        for entry in entries:
            name_to_files.setdefault(entry.name, []).append(path.name)
        services.append(ServiceSection(class_name=path.stem, tools=tuple(entries)))

    if mismatched_files:
        detail = "\n".join(f"  - {line}" for line in mismatched_files)
        message = f"@Tool annotation parsing mismatch in the following file(s):\n{detail}"
        if all_errors:
            error_detail = "\n".join(f"  - {line}" for line in all_errors)
            message += f"\nParse errors:\n{error_detail}"
        fail(message)

    if total_parsed == 0:
        fail(f"No @Tool annotations were found under {source_dir}.")

    duplicates = {name: paths for name, paths in name_to_files.items() if len(paths) > 1}
    if duplicates:
        detail = "\n".join(f"  - {name}: {', '.join(paths)}" for name, paths in sorted(duplicates.items()))
        fail(f"Duplicate tool name(s) found:\n{detail}")

    services.sort(key=lambda service: service.class_name)
    return services, total_parsed


def scanned_tool_names(source_dir: Path = SOURCE_DIR) -> set[str]:
    """
    Returns the set of @Tool names scanned from shaft-mcp Java sources.

    This is the reusable half of the catalog generator's static scan: it lets other drift gates
    (for example tests/scripts/test_mcp_tool_catalog_sync.py, which checks the MCP tool manifest,
    ToolTemplates.java, and the shaft-cli command ACTIONS maps against the same tool-name set)
    share one source of truth instead of re-implementing the @Tool parser.
    """
    services, _ = build_catalog(source_dir)
    return {tool.name for service in services for tool in service.tools}


def human_heading(class_name: str) -> str:
    """Derives an H2 heading label from a service class name, e.g. "BrowserService" -> "Browser",
    "CodingPartnerService" -> "Coding Partner"."""
    base = class_name
    if base.endswith(SERVICE_CLASS_SUFFIX) and len(base) > len(SERVICE_CLASS_SUFFIX):
        base = base[: -len(SERVICE_CLASS_SUFFIX)]
    return re.sub(r"(?<!^)(?=[A-Z])", " ", base)


def collapse_whitespace(value: str) -> str:
    return " ".join(value.split())


def render_catalog(services: list[ServiceSection], total: int) -> str:
    lines = [
        "# SHAFT MCP Tool Catalog",
        "",
        f"_This file is GENERATED by `{GENERATOR_RELATIVE_PATH}` from the shaft-mcp @Tool "
        "annotations. Do not edit it by hand; re-run the generator instead._",
        "",
        f"Total tools: {total}",
        "",
        "Agents should use these exact tool names instead of guessing or listing tools at "
        "runtime. Client tool-name prefixes vary: some clients expose `shaft-mcp:<name>`, "
        "others expose `mcp__shaft-mcp__<name>` (for example Claude Code). On clients that "
        "defer tool schemas until requested, batch-load only the tools you need in a single "
        "lookup instead of one at a time, e.g. Claude Code "
        '`ToolSearch("select:mcp__shaft-mcp__<a>,mcp__shaft-mcp__<b>")`.',
        "",
    ]
    for service in services:
        lines.append(f"## {human_heading(service.class_name)} ({service.class_name})")
        lines.append("")
        for tool in service.tools:
            description = collapse_whitespace(tool.description)
            lines.append(f"- `{tool.name}` — {description}")
        lines.append("")
    while lines and lines[-1] == "":
        lines.pop()
    return "\n".join(lines) + "\n"


def build(source_dir: Path = SOURCE_DIR) -> tuple[str, int, int]:
    """Returns (rendered markdown, total tool count, service count)."""
    services, total = build_catalog(source_dir)
    content = render_catalog(services, total)
    return content, total, len(services)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate the SHAFT MCP tool catalog markdown from @Tool annotations.",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify the checked-in catalog matches the generated output; do not write.",
    )
    return parser.parse_args(argv)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    try:
        content, total, service_count = build(SOURCE_DIR)
    except CatalogError as exc:
        print(f"generate_shaft_skills_tool_catalog: {exc}", file=sys.stderr)
        return 1

    output_display = display_path(OUTPUT_PATH)
    if args.check:
        if not OUTPUT_PATH.is_file():
            print(
                f"generate_shaft_skills_tool_catalog: {output_display} does not exist; run "
                f"'python3 {GENERATOR_RELATIVE_PATH}' to generate it.",
                file=sys.stderr,
            )
            return 1
        # Compare newline-insensitively: git autocrlf checkouts materialize the
        # committed catalog with CRLF on Windows, and that is not content drift.
        existing = OUTPUT_PATH.read_bytes().replace(b"\r\n", b"\n")
        expected = content.encode("utf-8")
        if existing != expected:
            print(
                f"generate_shaft_skills_tool_catalog: {output_display} is out of date with the "
                f"shaft-mcp sources ({total} tools across {service_count} services expected). "
                f"Re-run 'python3 {GENERATOR_RELATIVE_PATH}' and commit the result.",
                file=sys.stderr,
            )
            return 1
        print(f"{output_display} is up to date ({total} tools across {service_count} services).")
        return 0

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT_PATH.write_text(content, encoding="utf-8", newline="\n")
    print(f"Generated {total} tools across {service_count} services -> {output_display}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
