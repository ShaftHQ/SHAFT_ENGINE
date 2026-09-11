#!/usr/bin/env python3
"""Fail fast when JavaDoc ``@param`` names do not match method arity (#5748).

Learning from Wave D: a bogus ``@param replaceAllowed`` on a 3-arg
``typeMobileText`` overload made ErrorProne fail ``TypeStrategies.java`` and
cascaded unit/CodeQL red even though the Java logic was fine.

This checker is a portable ChaosEngine pre-push / PR-gate script. It scans
``shaft-engine`` interaction packages by default (skip silently when absent)
and reports every ``@param`` whose name is not a parameter of the following
method declaration. Type-parameter tags (``@param <T>``) are ignored.

Usage:
    python3 chaos-engine/check_javadoc_param_arity.py
    python3 chaos-engine/check_javadoc_param_arity.py --root shaft-engine/.../interaction
    python3 chaos-engine/check_javadoc_param_arity.py --paths file1.java file2.java

Exit codes:
    0  clean (or no matching sources)
    1  one or more ``@param`` / signature mismatches
    2  usage / IO error
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

DEFAULT_RELATIVE_ROOTS = (
    "shaft-engine/src/main/java/com/shaft/gui/element/internal/interaction",
)

METHOD_START = re.compile(
    r"(?:(?:public|protected|private|static|final|synchronized|native|abstract|default)\s+)+"
    r"(?:[\w.<>,\[\]?\s]+?\s+)?"
    r"(?P<name>\w+)\s*\(",
)
PARAM_TAG = re.compile(r"@param\s+(?P<name><[^>]+>|\w+)")
IDENT = re.compile(r"\b([A-Za-z_][A-Za-z0-9_]*)\b")


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


def discover_sources(
    root: Path,
    *,
    relative_roots: tuple[str, ...] = DEFAULT_RELATIVE_ROOTS,
    paths: list[Path] | None = None,
) -> list[Path]:
    if paths:
        found: list[Path] = []
        for path in paths:
            resolved = path if path.is_absolute() else (root / path)
            if resolved.is_file() and resolved.suffix == ".java":
                found.append(resolved.resolve())
            elif resolved.is_dir():
                found.extend(sorted(resolved.rglob("*.java")))
        return found

    found = []
    for relative in relative_roots:
        base = root / relative
        if base.is_dir():
            found.extend(sorted(base.rglob("*.java")))
        elif base.is_file() and base.suffix == ".java":
            found.append(base)
    return found


def _strip_line_comments(text: str) -> str:
    return re.sub(r"//.*?$", "", text, flags=re.MULTILINE)


def _iter_javadoc_method_pairs(source: str) -> list[tuple[int, str, int, str]]:
    """Return (javadoc_start_line, javadoc, method_start_line, method_header)."""
    pairs: list[tuple[int, str, int, str]] = []
    lines = source.splitlines()
    index = 0
    while index < len(lines):
        stripped = lines[index].lstrip()
        if not stripped.startswith("/**"):
            index += 1
            continue
        start = index
        block_lines = [lines[index]]
        if "*/" not in lines[index]:
            index += 1
            while index < len(lines):
                block_lines.append(lines[index])
                if "*/" in lines[index]:
                    break
                index += 1
        javadoc = "\n".join(block_lines)
        index += 1
        while index < len(lines) and not lines[index].strip():
            index += 1
        # Skip annotations between javadoc and the method signature.
        while index < len(lines):
            stripped = lines[index].lstrip()
            if not stripped.startswith("@"):
                break
            depth = 0
            while index < len(lines):
                for char in lines[index]:
                    if char == "(":
                        depth += 1
                    elif char == ")":
                        depth -= 1
                index += 1
                if depth <= 0:
                    break
        while index < len(lines) and not lines[index].strip():
            index += 1
        if index >= len(lines):
            break
        header_start = index
        header_parts: list[str] = []
        depth = 0
        seen_paren = False
        while index < len(lines):
            line = lines[index]
            header_parts.append(line)
            for char in line:
                if char == "(":
                    depth += 1
                    seen_paren = True
                elif char == ")":
                    depth -= 1
            index += 1
            if seen_paren and depth <= 0:
                break
            if not seen_paren and "{" in line:
                break
        if not seen_paren:
            continue
        header = "\n".join(header_parts)
        pairs.append((start + 1, javadoc, header_start + 1, header))
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


def _method_name_and_params(header: str) -> tuple[str, tuple[str, ...]] | None:
    cleaned = _strip_line_comments(header)
    # Drop throws clause and body opener for matching.
    cleaned = cleaned.split("{", 1)[0]
    match = METHOD_START.search(cleaned)
    if match is None:
        return None
    name = match.group("name")
    # Reject constructors named like types only when preceded by `new` — keep all.
    open_paren = cleaned.find("(", match.end() - 1)
    if open_paren < 0:
        return None
    depth = 0
    close = -1
    for index, char in enumerate(cleaned[open_paren:], start=open_paren):
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                close = index
                break
    if close < 0:
        return None
    inside = cleaned[open_paren + 1 : close].strip()
    if not inside:
        return name, ()
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
                params.append(_param_name(chunk))
            current = []
        else:
            current.append(char)
    chunk = "".join(current).strip()
    if chunk:
        params.append(_param_name(chunk))
    return name, tuple(params)


def _param_name(declaration: str) -> str:
    text = declaration.strip()
    # Drop annotations (@Nullable Type name).
    while text.startswith("@"):
        # annotation may be @Foo or @Foo(...)
        if "(" in text[: text.find(" ") if " " in text else len(text)]:
            depth = 0
            end = 0
            for index, char in enumerate(text):
                if char == "(":
                    depth += 1
                elif char == ")":
                    depth -= 1
                    if depth == 0:
                        end = index + 1
                        break
            text = text[end:].strip()
        else:
            parts = text.split(None, 1)
            text = parts[1].strip() if len(parts) > 1 else ""
    text = text.replace("...", " ")
    tokens = IDENT.findall(text)
    if not tokens:
        return text
    # Last identifier is the parameter name.
    return tokens[-1]


def check_source(path: Path, text: str | None = None) -> list[Finding]:
    source = text if text is not None else path.read_text(encoding="utf-8")
    findings: list[Finding] = []
    for javadoc_line, javadoc, method_line, header in _iter_javadoc_method_pairs(source):
        parsed = _method_name_and_params(header)
        if parsed is None:
            continue
        method_name, signature_params = parsed
        # Skip obvious non-methods (enum constants etc. rarely match METHOD_START well).
        if method_name in {"if", "for", "while", "switch", "catch", "return"}:
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
    relative_roots = tuple(args.root) if args.root else DEFAULT_RELATIVE_ROOTS
    try:
        sources = discover_sources(
            root, relative_roots=relative_roots, paths=explicit_paths
        )
    except OSError as error:
        sys.stderr.write(f"check_javadoc_param_arity: {error}\n")
        return 2
    if not sources:
        # Portable overlay / empty tree: nothing to enforce.
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
