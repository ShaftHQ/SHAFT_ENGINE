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
    "The same rule is `chaos-engine/references/host-parity-matrix.md`. "
    "When retrieve is degraded, doctor prints blocking row `retrieve-mempalace` "
    "or `retrieve-graphify` with the backend reason. Repair with "
    "`python3 .chaos-engine/install.py repair --project . --component mempalace` "
    "or `--component graphify`. Do not auto-migrate ~/.mempalace."
)

_PATH = re.compile(r"(?<![\w.@])((?:[\w.-]+/)+[\w.-]+\.[\w.]+)")
_SOURCE_LINE = re.compile(r"(?m)^[ \t]*Source:\s*(\S+)")
_READ_TOOLS = frozenset({"Read", "Grep", "Glob"})
_FILE_HEADS = frozenset(
    {
        "rg",
        "grep",
        "ag",
        "ack",
        "fd",
        "find",
        "cat",
        "head",
        "tail",
        "less",
        "more",
        "bat",
        "nl",
        "sed",
    }
)
_HOST_POINTERS = frozenset(
    {
        "AGENTS.md",
        "CLAUDE.md",
        "GEMINI.md",
        ".github/copilot-instructions.md",
    }
)
_INSTRUCTION_MARKERS = (
    "chaos-engine/references/",
    "chaos-engine/skills/",
    "chaos-engine/profiles/",
    ".chaos-engine/references/",
    ".chaos-engine/skills/",
    ".chaos-engine/profiles/",
)
_STORE_HEADS = frozenset({"mempalace", "graphify"})
_PY = frozenset({"py", "python", "python3"})
_SHELL_PATH = re.compile(r"(?<![\w@])(\.?[\w.-]+(?:/[\w.-]+)+\.[\w.]+)")


def _norm(value: str) -> str:
    text = value.replace("\\", "/").strip()
    while text.startswith("./"):
        text = text[2:]
    return text


def _project_relative(project: Path, target: str) -> str:
    """Host reads pass absolute paths. The ledger stores repo-relative ones."""
    wanted = _norm(target)
    if not wanted:
        return ""
    roots = {_norm(str(Path(project))), _norm(str(Path(project).resolve()))}
    for root in roots:
        if root and (wanted == root or wanted.startswith(root + "/")):
            return wanted[len(root) :].lstrip("/")
    return wanted


def _instruction_markdown(wanted: str) -> bool:
    if not wanted.endswith(".md"):
        return False
    if wanted in _HOST_POINTERS:
        return True
    return any(marker in wanted for marker in _INSTRUCTION_MARKERS)


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
    raw = _load_payload(project).get("citations")
    if not isinstance(raw, list):
        return []
    return [item for item in raw if isinstance(item, str)]


HEAL_ARTIFACTS = frozenset({
    ".chaos-engine-state/heal-handoff.md",
    ".chaos-engine-state/doctor-failure.json",
    ".chaos-engine-state/install-trace.json",
    ".chaos-engine-state/install-console.log",
})
HEAL_ARTIFACT_CAP = 65536
ROUTER_SUFFIXES = (
    "skills/chaos-engine/SKILL.md",
    "chaos-engine/identity.md",
    ".chaos-engine/identity.md",
    "chaos-engine/bootstrap.py",
)
_FAIL_OPEN = frozenset({"degraded", "skipped"})


def _load_payload(project: Path) -> dict:
    path = receipt_path(project)
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _write_payload(project: Path, payload: dict) -> None:
    destination = receipt_path(project)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def record_citations(project: Path, store: str, text: str) -> list[str]:
    """Append paths from a MemPalace or Graphify result. Other stores no-op."""
    return record_store_outcome(project, store, "used", "", text)


def record_store_outcome(
    project: Path, store: str, status: str, query: str, text: str = "", reason: str = ""
) -> list[str]:
    """Record a store attempt. Degraded or skipped attempts fail open for query paths."""
    chosen = str(store or "").casefold()
    if chosen not in STORES:
        return []
    payload = _load_payload(project)
    citations = payload.get("citations")
    current = [item for item in citations if isinstance(item, str)] if isinstance(citations, list) else []
    fresh = extract_citations(text) if status == "used" else []
    for path in fresh:
        if path not in current:
            current.append(path)
    current = current[-CITATION_LIMIT:]
    outcomes = payload.get("outcomes")
    kept = [item for item in outcomes if isinstance(item, dict)] if isinstance(outcomes, list) else []
    if status == "used" and chosen == "mempalace":
        kept = [
            item
            for item in kept
            if not (
                isinstance(item, dict)
                and item.get("store") == "mempalace"
                and item.get("reason") == "backend-mismatch"
            )
        ]
    if status in _FAIL_OPEN:
        outcome = {"store": chosen, "status": status, "query": str(query or "")[:500]}
        if reason:
            outcome["reason"] = str(reason)[:120]
        kept.append(outcome)
        kept = kept[-32:]
    payload = {"schemaVersion": 1, "store": chosen, "citations": current, "outcomes": kept}
    _write_payload(project, payload)
    return fresh


def backend_mismatch_recorded(project: Path) -> bool:
    """True when MemPalace backend-mismatch was already recorded for this project."""
    outcomes = _load_payload(project).get("outcomes")
    if not isinstance(outcomes, list):
        return False
    for item in outcomes:
        if not isinstance(item, dict):
            continue
        if item.get("store") != "mempalace" or item.get("status") != "degraded":
            continue
        if item.get("reason") == "backend-mismatch":
            return True
    return False



def _allowlisted(project: Path, target: str) -> bool:
    """Router files, named memory topics, and small heal artifacts need no citation."""
    wanted = _norm(target)
    if not wanted:
        return False
    if wanted.endswith(ROUTER_SUFFIXES) or wanted in ROUTER_SUFFIXES:
        return True
    if _instruction_markdown(wanted):
        return True
    lowered = wanted.casefold()
    if "/memory-v2/" in f"/{lowered}" and "/topics/" in f"/{lowered}" and lowered.endswith(".md"):
        return True
    if wanted not in HEAL_ARTIFACTS and not any(wanted.endswith("/" + name) for name in HEAL_ARTIFACTS):
        return False
    relative = wanted if wanted in HEAL_ARTIFACTS else next(
        name for name in HEAL_ARTIFACTS if wanted.endswith("/" + name)
    )
    candidate = Path(project) / relative
    try:
        if candidate.is_symlink() or not candidate.is_file():
            return True
        return candidate.stat().st_size <= HEAL_ARTIFACT_CAP
    except OSError:
        return True


def _prefix_allowed(project: Path, target: str) -> bool:
    """A cited file unlocks its directory for the rest of this ledger."""
    wanted = _norm(target)
    if not wanted or wanted.endswith("/"):
        return False
    for citation in load_citations(project):
        cited = _norm(citation)
        if "/" not in cited:
            continue
        prefix = cited.rsplit("/", 1)[0] + "/"
        if wanted.startswith(prefix):
            return True
    return False


def _path_named_in_query(target: str, query: str) -> bool:
    wanted = _norm(target)
    if not wanted:
        return False
    text = _norm(query)
    if wanted in text:
        return True
    base = wanted.rsplit("/", 1)[-1]
    return bool(base) and base in query.replace("\\", "/").split()


def _fail_open(project: Path, target: str) -> bool:
    outcomes = _load_payload(project).get("outcomes")
    if not isinstance(outcomes, list):
        return False
    for item in outcomes:
        if not isinstance(item, dict) or item.get("status") not in _FAIL_OPEN:
            continue
        if _path_named_in_query(target, str(item.get("query") or "")):
            return True
    return False


def read_allowed(project: Path, target: str) -> bool:
    """True when this path may be read without another store round trip."""
    relative = _project_relative(project, target)
    return (
        _allowlisted(project, relative)
        or cites(project, relative)
        or _prefix_allowed(project, relative)
        or _fail_open(project, relative)
    )


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
    """Split on &&, ||, and ; outside quotes. A quoted semicolon stays in one command."""
    parts: list[str] = []
    buf: list[str] = []
    quote = ""
    index = 0
    text = command or ""
    while index < len(text):
        character = text[index]
        if quote:
            buf.append(character)
            if character == quote and text[index - 1] != "\\":
                quote = ""
            index += 1
            continue
        if character in {"'", '"'}:
            quote = character
            buf.append(character)
            index += 1
            continue
        if text.startswith("&&", index) or text.startswith("||", index):
            parts.append("".join(buf).strip())
            buf = []
            index += 2
            continue
        if character == ";":
            parts.append("".join(buf).strip())
            buf = []
            index += 1
            continue
        buf.append(character)
        index += 1
    tail = "".join(buf).strip()
    if tail:
        parts.append(tail)
    return [part for part in parts if part]


def _shell_read_paths(text: str) -> list[str]:
    """Slash paths, including `.chaos-engine/...`, that a shell command can open."""
    found: list[str] = []
    for match in _SHELL_PATH.finditer(text or ""):
        path = _norm(match.group(1))
        if "://" in path or path.startswith(".."):
            continue
        if path not in found:
            found.append(path)
    return found


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
    if "knowledge_stores.py" in joined and any(
        name in scripts for name in ("search", "--help", "-h")
    ):
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


def _only_store_scripts(paths: list[str]) -> bool:
    scripts = ("tool.py", "retrieve.py", "knowledge_stores.py")
    return bool(paths) and all(path.endswith(scripts) for path in paths)


def _shell_block(project: Path, commands: tuple[str, ...]) -> str | None:
    for command in commands:
        if re.search(r"\b(python3?|py|sed)\b", command):
            opened = _shell_read_paths(command)
            if opened and not all(read_allowed(project, path) for path in opened):
                if not _only_store_scripts(opened):
                    return BLOCK_REASON
        for segment in _segments(command):
            if _is_store_segment(segment):
                continue
            try:
                import shlex

                tokens = shlex.split(segment, posix=True)
            except ValueError:
                tokens = []
            head, _arguments = _command_head(tokens)
            if head in _PY or (not tokens and "python" in segment.casefold()):
                paths = _shell_read_paths(segment)
                if paths and all(read_allowed(project, path) for path in paths):
                    continue
                if paths:
                    return BLOCK_REASON
                continue
            if head not in _FILE_HEADS:
                continue
            paths = _paths_in_segment(segment)
            if paths and all(read_allowed(project, path) for path in paths):
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
        if paths and all(read_allowed(project, path) for path in paths):
            return None
        return BLOCK_REASON
    return _shell_block(project, commands)
