#!/usr/bin/env python3
"""One citation ledger for MemPalace and Graphify checks.

File reads and file searches are allowed only when a prior MemPalace or
Graphify check cited the path. The ledger is the portable core; every host
hook calls this module. Host adapters do not keep a second copy.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

STORES = frozenset({"mempalace", "graphify"})
RECEIPT_NAME = ".chaos-engine-state/retrieve-justification.json"
CITATION_LIMIT = 200
BLOCK_REASON = (
    "File read or search requires a MemPalace or Graphify check that cites "
    "this path. Run `python3 .chaos-engine/tool.py retrieve --store graphify "
    "\"<what calls or depends on this>\"` or `--store mempalace "
    "\"<what happened around this>\"`, then read only the cited paths. "
    "The same rule is `chaos-engine/references/host-parity-matrix.md`."
)

_PATH = re.compile(r"(?<![\w.@])((?:[\w.-]+/)+[\w.-]+\.[\w.]+)")
_SOURCE_LINE = re.compile(r"(?m)^[ \t]*Source:\s*(\S+)")
_READ_TOOLS = frozenset({"Read", "Grep", "Glob"})
_FILE_HEADS = frozenset(
    {"rg", "grep", "ag", "ack", "fd", "find", "cat", "head", "tail", "less", "more", "bat", "nl"}
)
_STORE_HEADS = frozenset({"mempalace", "graphify"})
_PY = frozenset({"py", "python", "python3"})
_SPLIT = re.compile(r"\s*(?:&&|\|\||;)\s*")


def _norm(value: str) -> str:
    text = value.replace("\\", "/").strip()
    while text.startswith("./"):
        text = text[2:]
    return text


def extract_citations(text: str) -> list[str]:
    """Return repo-relative paths named by one store result."""
    found: list[str] = []
    for match in _PATH.finditer(text or ""):
        path = _norm(match.group(1))
        if "://" in path or path.startswith("."):
            continue
        found.append(path)
    for match in _SOURCE_LINE.finditer(text or ""):
        name = _norm(match.group(1).strip("`,;"))
        if name and "://" not in name:
            found.append(name)
    unique: list[str] = []
    for path in found:
        if path not in unique:
            unique.append(path)
    return unique[:CITATION_LIMIT]


def receipt_path(project: Path) -> Path:
    return Path(project) / RECEIPT_NAME


def load_citations(project: Path) -> list[str]:
    path = receipt_path(project)
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return []
    raw = payload.get("citations") if isinstance(payload, dict) else None
    if not isinstance(raw, list):
        return []
    return [item for item in raw if isinstance(item, str)]


def record_citations(project: Path, store: str, text: str) -> list[str]:
    """Append paths from a MemPalace or Graphify result. Other stores no-op."""
    chosen = str(store or "").casefold()
    if chosen not in STORES:
        return []
    fresh = extract_citations(text)
    if not fresh:
        return []
    current = load_citations(project)
    for path in fresh:
        if path not in current:
            current.append(path)
    current = current[-CITATION_LIMIT:]
    destination = receipt_path(project)
    destination.parent.mkdir(parents=True, exist_ok=True)
    payload = {"schemaVersion": 1, "store": chosen, "citations": current}
    destination.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return fresh


def cites(project: Path, target: str) -> bool:
    """True when a recorded check names this file or its basename-only hit."""
    wanted = _norm(target)
    if not wanted:
        return False
    base = wanted.rsplit("/", 1)[-1]
    for citation in load_citations(project):
        cited = _norm(citation)
        if not cited:
            continue
        if wanted == cited or wanted.endswith("/" + cited) or cited.endswith("/" + wanted):
            return True
        if "/" not in cited and cited == base:
            return True
    return False


def _command_head(tokens: list[str]) -> tuple[str, list[str]]:
    index = 0
    while index < len(tokens):
        item = tokens[index]
        if "=" in item and not item.startswith("-") and index == 0:
            index += 1
            continue
        head = item.replace("\\", "/").rsplit("/", 1)[-1].casefold()
        return head, tokens[index + 1 :]
    return "", []


def _segments(command: str) -> list[str]:
    return [segment.strip() for segment in _SPLIT.split(command) if segment.strip()]


def _is_store_segment(segment: str) -> bool:
    try:
        import shlex

        tokens = shlex.split(segment, posix=True)
    except ValueError:
        return False
    head, arguments = _command_head(tokens)
    if head in _STORE_HEADS:
        return True
    if head not in _PY:
        return False
    scripts = [item.replace("\\", "/").casefold() for item in arguments]
    joined = " ".join(scripts)
    if any(item.endswith("tool.py") for item in scripts) and any(
        name in scripts for name in ("mempalace", "graphify", "retrieve", "--help", "-h")
    ):
        return True
    if any(item.endswith("retrieve.py") for item in scripts):
        return True
    if "knowledge_stores.py" in joined and "search" in scripts:
        return True
    return False


def _paths_in_segment(segment: str) -> list[str]:
    try:
        import shlex

        tokens = shlex.split(segment, posix=True)
    except ValueError:
        return []
    _head, arguments = _command_head(tokens)
    return [item for item in arguments if not item.startswith("-") and ("/" in item or "." in item)]


def _shell_block(project: Path, commands: tuple[str, ...]) -> str | None:
    for command in commands:
        for segment in _segments(command):
            if _is_store_segment(segment):
                continue
            try:
                import shlex

                tokens = shlex.split(segment, posix=True)
            except ValueError:
                continue
            head, _arguments = _command_head(tokens)
            if head not in _FILE_HEADS:
                continue
            paths = _paths_in_segment(segment)
            if paths and all(cites(project, path) for path in paths):
                continue
            return BLOCK_REASON
    return None


def _input_paths(tool_input: dict) -> list[str]:
    paths: list[str] = []
    for key in ("file_path", "filePath", "path", "target_file", "target_directory", "notebook_path"):
        value = tool_input.get(key)
        if isinstance(value, str) and value.strip():
            paths.append(value.strip())
    return paths


def file_read_block_reason(
    *,
    project: Path,
    event_name: str,
    tool_name: str,
    tool_input: dict,
    commands: tuple[str, ...],
) -> str | None:
    """Return the shared deny reason, or None when this call is not a file read."""
    if event_name != "PreToolUse":
        return None
    if tool_name in _READ_TOOLS:
        paths = _input_paths(tool_input)
        if paths and all(cites(project, path) for path in paths):
            return None
        return BLOCK_REASON
    return _shell_block(project, commands)
