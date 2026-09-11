#!/usr/bin/env python3
"""Fail fast when JavaDoc ``@param`` names do not match method arity (#5748)."""
# Codacy runs both D212 and D213, so multi-line module docstrings always flag
# one of them; detail therefore lives in this comment block.
#
# Learning from Wave D: a bogus ``@param replaceAllowed`` on a 3-arg
# ``typeMobileText`` overload made ErrorProne fail ``TypeStrategies.java`` and
# cascaded unit/CodeQL red even though the Java logic was fine.
#
# Portable ChaosEngine local / PR-gate script (agent before-push checklist +
# harness surface). By default discovers
# ``*/src/main/java/**/gui/element/internal/interaction/*.java`` under the
# project root (skip silently when absent). Reports every javadoc block-tag
# ``@param`` whose name is not a parameter of the following method.
# Type-parameter tags (``@param <T>``) and prose mentions of ``@param`` are
# ignored.
#
# Usage:
#     python3 chaos-engine/check_javadoc_param_arity.py
#     python3 chaos-engine/check_javadoc_param_arity.py --paths file1.java
#
# Exit codes: 0 clean / no sources; 1 mismatches; 2 usage / IO error.

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

# Product-neutral discovery: module/src/main/java/**/gui/element/internal/interaction
DEFAULT_INTERACTION_GLOB = (
    "*/src/main/java/**/gui/element/internal/interaction"
)

METHOD_START = re.compile(
    r"^\s*(?:(?:public|protected|private|static|final|synchronized|native|abstract|default)\s+)+"
    r"(?:[\w.<>,\[\]?\s]+?\s+)?"
    r"(?P<name>\w+)\s*\(",
    re.MULTILINE,
)
# Javadoc block tags only: leading * / whitespace, then @param.
PARAM_TAG = re.compile(
    r"(?m)^\s*(?:\*\s*)?@param\s+(?P<name><[^>]+>|\w+)\b"
)
IDENT = re.compile(r"\b([A-Za-z_][A-Za-z0-9_]*)\b")
_NON_METHODS = frozenset({"if", "for", "while", "switch", "catch", "return"})


@dataclass(frozen=True)
class Finding:
    path: Path
    line: int
    method: str
    param: str
    signature_params: tuple[str, ...]

    def format(self) -> str:
        expected = ", ".join(self.signature_params) or "(none)"
        return (
            f"{self.path}:{self.line}: @param '{self.param}' is not a parameter of "
            f"{self.method}({expected})"
        )


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / "chaos-engine" / "install.py").is_file() or (
            candidate / ".chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def _collect_java(paths: list[Path]) -> list[Path]:
    found: list[Path] = []
    for path in paths:
        if path.is_file() and path.suffix == ".java":
            found.append(path.resolve())
        elif path.is_dir():
            found.extend(sorted(path.rglob("*.java")))
    return found


def default_interaction_roots(root: Path) -> list[Path]:
    """Locate interaction strategy packages without hardcoding a product module."""
    return sorted(
        path for path in root.glob(DEFAULT_INTERACTION_GLOB) if path.is_dir()
    )


def discover_sources(
    root: Path,
    *,
    relative_roots: tuple[str, ...] | None = None,
    paths: list[Path] | None = None,
) -> list[Path]:
    if paths:
        resolved = [
            path if path.is_absolute() else (root / path) for path in paths
        ]
        return _collect_java(resolved)

    if relative_roots:
        bases = []
        for relative in relative_roots:
            base = root / relative
            if base.exists():
                bases.append(base)
        return _collect_java(bases)

    return _collect_java(default_interaction_roots(root))


def _strip_line_comments(text: str) -> str:
    return re.sub(r"//.*?$", "", text, flags=re.MULTILINE)


def _skip_blank(lines: list[str], index: int) -> int:
    while index < len(lines) and not lines[index].strip():
        index += 1
    return index


def _read_javadoc_block(lines: list[str], index: int) -> tuple[str, int] | None:
    if index >= len(lines) or not lines[index].lstrip().startswith("/**"):
        return None
    block_lines = [lines[index]]
    if "*/" in lines[index]:
        return "\n".join(block_lines), index + 1
    index += 1
    while index < len(lines):
        block_lines.append(lines[index])
        if "*/" in lines[index]:
            return "\n".join(block_lines), index + 1
        index += 1
    return "\n".join(block_lines), index


def _paren_delta(text: str) -> int:
    return text.count("(") - text.count(")")


def _skip_annotations(lines: list[str], index: int) -> int:
    while index < len(lines) and lines[index].lstrip().startswith("@"):
        depth = 0
        while index < len(lines):
            depth += _paren_delta(lines[index])
            index += 1
            if depth <= 0:
                break
    return index


def _read_method_header(lines: list[str], index: int) -> tuple[str, int] | None:
    header_parts: list[str] = []
    depth = 0
    seen_paren = False
    while index < len(lines):
        line = lines[index]
        header_parts.append(line)
        depth += _paren_delta(line)
        if "(" in line:
            seen_paren = True
        index += 1
        if seen_paren and depth <= 0:
            return "\n".join(header_parts), index
        if not seen_paren and "{" in line:
            return None
    return None


def _iter_javadoc_method_pairs(source: str) -> list[tuple[int, str, str]]:
    """Return (javadoc_start_line, javadoc, method_header) pairs."""
    pairs: list[tuple[int, str, str]] = []
    lines = source.splitlines()
    index = 0
    while index < len(lines):
        start = index
        block = _read_javadoc_block(lines, index)
        if block is None:
            index += 1
            continue
        javadoc, index = block
        index = _skip_blank(lines, index)
        index = _skip_annotations(lines, index)
        index = _skip_blank(lines, index)
        if index >= len(lines):
            break
        header = _read_method_header(lines, index)
        if header is None:
            index += 1
            continue
        text, index = header
        pairs.append((start + 1, javadoc, text))
    return pairs


def _param_tags(javadoc: str) -> list[tuple[str, int]]:
    """Return (param_name, line_offset_within_javadoc) for value params only."""
    tags: list[tuple[str, int]] = []
    for offset, line in enumerate(javadoc.splitlines()):
        for match in PARAM_TAG.finditer(line):
            name = match.group("name")
            if name.startswith("<") and name.endswith(">"):
                continue
            tags.append((name, offset))
    return tags


def _split_param_list(inside: str) -> list[str]:
    params: list[str] = []
    depth = 0
    current: list[str] = []
    for char in inside:
        if char in "<([":
            depth += 1
            current.append(char)
        elif char in ">)]":
            depth = max(0, depth - 1)
            current.append(char)
        elif char == "," and depth == 0:
            chunk = "".join(current).strip()
            if chunk:
                params.append(chunk)
            current = []
        else:
            current.append(char)
    chunk = "".join(current).strip()
    if chunk:
        params.append(chunk)
    return params


def _matching_close(text: str, open_index: int) -> int:
    depth = 0
    for index, char in enumerate(text[open_index:], start=open_index):
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return index
    return -1


def _method_name_and_params(header: str) -> tuple[str, tuple[str, ...]] | None:
    cleaned = _strip_line_comments(header).split("{", 1)[0]
    match = METHOD_START.search(cleaned)
    if match is None:
        return None
    name = match.group("name")
    open_paren = cleaned.find("(", match.end() - 1)
    if open_paren < 0:
        return None
    close = _matching_close(cleaned, open_paren)
    if close < 0:
        return None
    inside = cleaned[open_paren + 1 : close].strip()
    if not inside:
        return name, ()
    return name, tuple(_param_name(chunk) for chunk in _split_param_list(inside))


def _param_name(declaration: str) -> str:
    text = declaration.strip()
    while text.startswith("@"):
        head = text[: text.find(" ") if " " in text else len(text)]
        if "(" in head:
            close = _matching_close(text, text.find("("))
            text = text[close + 1 :].strip() if close >= 0 else ""
        else:
            parts = text.split(None, 1)
            text = parts[1].strip() if len(parts) > 1 else ""
    text = text.replace("...", " ")
    tokens = IDENT.findall(text)
    return tokens[-1] if tokens else text


def check_source(path: Path, text: str | None = None) -> list[Finding]:
    source = text if text is not None else path.read_text(encoding="utf-8")
    findings: list[Finding] = []
    for javadoc_line, javadoc, header in _iter_javadoc_method_pairs(source):
        parsed = _method_name_and_params(header)
        if parsed is None:
            continue
        method_name, signature_params = parsed
        if method_name in _NON_METHODS:
            continue
        allowed = set(signature_params)
        for param, offset in _param_tags(javadoc):
            if param not in allowed:
                findings.append(
                    Finding(
                        path=path,
                        line=javadoc_line + offset,
                        method=method_name,
                        param=param,
                        signature_params=signature_params,
                    )
                )
    return findings


def check_paths(paths: list[Path]) -> list[Finding]:
    findings: list[Finding] = []
    for path in paths:
        findings.extend(check_source(path))
    return findings


def parser() -> argparse.ArgumentParser:
    command = argparse.ArgumentParser(
        description="Fail when JavaDoc @param names do not match method parameters (#5748)."
    )
    command.add_argument(
        "--project",
        type=Path,
        default=None,
        help="project root (default: discover from cwd)",
    )
    command.add_argument(
        "--root",
        action="append",
        default=[],
        metavar="REL_OR_ABS",
        help="extra relative/absolute scan root (repeatable); replaces defaults when set",
    )
    command.add_argument(
        "--paths",
        nargs="*",
        default=[],
        help="explicit .java files or directories to scan",
    )
    return command


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    root = project_root(args.project)
    explicit_paths = [Path(p) for p in args.paths] if args.paths else None
    relative_roots = tuple(args.root) if args.root else None
    try:
        sources = discover_sources(
            root, relative_roots=relative_roots, paths=explicit_paths
        )
    except OSError as error:
        sys.stderr.write(f"check_javadoc_param_arity: {error}\n")
        return 2
    if not sources:
        return 0
    findings = check_paths(sources)
    if not findings:
        return 0
    for finding in findings:
        sys.stderr.write(finding.format() + "\n")
    sys.stderr.write(
        f"check_javadoc_param_arity: {len(findings)} @param arity mismatch(es) "
        f"across {len({f.path for f in findings})} file(s)\n"
    )
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
