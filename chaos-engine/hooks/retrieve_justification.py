#!/usr/bin/env python3
"""One citation ledger for MemPalace and Graphify checks.

Project file reads and searches are allowed only when a prior MemPalace or
Graphify check cited the path. Running a script is not reading it, harness
files (everything `harness-index.json` names) are exempt, the project root is
walked up like `retrieve.project_root()`, and a graph with no project nodes
fails open as `skipped(no-project-index)` (#6174). The ledger is the portable
core; every host hook calls this module. Host adapters keep no second copy.
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

_PATH = re.compile(r"(?<![\w@])((?:[\w.-]+/)+[\w.-]+\.[\w.]+)")
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
_RUNNERS = frozenset(
    {"node", "bash", "sh", "zsh", "pwsh", "powershell", "npx", "deno", "bun", "uv", "java", "mvn", "gradle", "make"}
)
_SEARCH_HEADS = frozenset({"rg", "grep", "ag", "ack", "fd", "find"})
_FALLBACK_ROOTS = (
    ".chaos-engine/", "chaos-engine/", ".chaos-engine-state/", "plugins/", ".agents/", ".claude/",
    ".claude-plugin/", ".codex/", ".codex-plugin/", ".gemini/", ".grok/", ".opencode/", ".cursor/",
    ".github/hooks/", ".github/skills/", ".memory/",
)
_FALLBACK_FILES = (
    "AGENTS.md", "CLAUDE.md", "GEMINI.md", ".github/copilot-instructions.md", ".mcp.json",
    "mempalace.yaml", ".graphifyignore",
)
_LOCATOR = re.compile(r"`([^`\s]+\.md)`")
_RECEIPT_FIELD = re.compile(
    r"(?mi)^\s*(?:[-*]\s*)?retrieve:\s*(used|skipped\([^)\n]+\)|exempt\(harness\))\s*$"
)
NO_PROJECT_INDEX = "no-project-index"


def _norm(value: str) -> str:
    text = value.replace("\\", "/").strip()
    while text.startswith("./"):
        text = text[2:]
    return text


def _index_payload() -> dict:
    path = Path(__file__).resolve().parent.parent / "harness-index.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def harness_roots() -> tuple[tuple[str, ...], frozenset[str]]:
    """Harness roots and files from the generated index (fallback: built-in copy)."""
    payload = _index_payload()
    roots = payload.get("harnessRoots")
    files = payload.get("harnessFiles")
    if not (isinstance(roots, list) and roots and all(isinstance(item, str) for item in roots)):
        roots = list(_FALLBACK_ROOTS)
    if not (isinstance(files, list) and all(isinstance(item, str) for item in files)):
        files = list(_FALLBACK_FILES)
    return tuple(roots), frozenset(files)


def is_harness_path(path: str, project: Path | None = None) -> bool:
    """True for ChaosEngine harness paths: always readable, never gated."""
    wanted = _project_relative(project, path) if project is not None else _norm(path)
    if not wanted or wanted.startswith("../"):
        return False
    roots, files = harness_roots()
    if wanted in files:
        return True
    return any(wanted == root.rstrip("/") or wanted.startswith(root) for root in roots)


def project_root(start: Path | None = None) -> Path:
    """Same walk-up as `retrieve.project_root()`: citations are visible from subdirectories."""
    try:
        here = (start or Path.cwd()).resolve()
    except OSError:
        return Path(start or ".")
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def session_start_locators(text: str) -> list[str]:
    """Backticked markdown paths SessionStart tells the agent to read."""
    found: list[str] = []
    for match in _LOCATOR.finditer(text or ""):
        path = _norm(match.group(1))
        if "/" in path and path not in found:
            found.append(path)
    return found


def retrieve_receipt_field(text: str) -> str | None:
    """Instruction-only hosts record `retrieve: used|skipped(reason)|exempt(harness)`."""
    match = _RECEIPT_FIELD.search(text or "")
    return match.group(1) if match else None


def _graph_json(project: Path) -> Path:
    import os

    configured = os.environ.get("CHAOS_ENGINE_GRAPHIFY_OUT")
    base = Path(configured) if configured else Path(project) / "graphify-out"
    return base / "graph.json"


def _node_path(node: object) -> str:
    if not isinstance(node, dict):
        return ""
    for key in ("source_file", "file", "path", "source"):
        value = node.get(key)
        if isinstance(value, str) and value.strip():
            return value
    return ""


def project_index_state(project: Path) -> str:
    """`ok`, `missing`, or `no-project-index` when the graph holds only harness nodes."""
    graph = _graph_json(project)
    try:
        stat = graph.stat()
    except OSError:
        return "missing"
    stamp = [int(stat.st_mtime_ns), int(stat.st_size)]
    cached = _load_payload(project).get("indexState")
    if isinstance(cached, dict) and cached.get("stamp") == stamp and isinstance(cached.get("state"), str):
        return cached["state"]
    try:
        payload = json.loads(graph.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return "missing"
    nodes = payload.get("nodes") if isinstance(payload, dict) else None
    nodes = nodes if isinstance(nodes, list) else []
    state = NO_PROJECT_INDEX
    for node in nodes:
        path = _node_path(node)
        if path and not is_harness_path(path, project):
            state = "ok"
            break
    record = _load_payload(project)
    if record or (Path(project) / RECEIPT_NAME).parent.is_dir() or state == NO_PROJECT_INDEX:
        record["indexState"] = {"stamp": stamp, "state": state}
        record.setdefault("schemaVersion", 1)
        _write_payload(project, record)
    return state


def _record_no_project_index(project: Path) -> None:
    payload = _load_payload(project)
    outcomes = payload.get("outcomes")
    kept = [item for item in outcomes if isinstance(item, dict)] if isinstance(outcomes, list) else []
    if any(item.get("reason") == NO_PROJECT_INDEX for item in kept):
        return
    kept.append({"store": "graphify", "status": "skipped", "query": "", "reason": NO_PROJECT_INDEX})
    payload["outcomes"] = kept[-32:]
    payload.setdefault("schemaVersion", 1)
    payload.setdefault("citations", [])
    _write_payload(project, payload)


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
        if "://" in path:
            continue
        if path.startswith(".") and not is_harness_path(path):
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


def _citable(project: Path, path: str) -> bool:
    """#6179: only existing repository files enter the ledger (no tqdm or cache noise)."""
    wanted = _norm(path)
    if not wanted or wanted.startswith(("../", "/")) or ".." in wanted.split("/"):
        return False
    try:
        return (Path(project) / wanted).is_file()
    except OSError:
        return False


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
    fresh = [
        path for path in (extract_citations(text) if status == "used" else [])
        if _citable(project, path)
    ]
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
    previous = _load_payload(project).get("indexState")
    if isinstance(previous, dict):
        payload["indexState"] = previous
    _write_payload(project, payload)
    return fresh


def clear_backend_mismatch(project: Path) -> None:
    """Forget a recorded MemPalace backend mismatch so the next retrieve re-probes (#6212)."""
    payload = _load_payload(project)
    outcomes = payload.get("outcomes")
    if not isinstance(outcomes, list):
        return
    payload["outcomes"] = [
        item
        for item in outcomes
        if not (
            isinstance(item, dict)
            and item.get("store") == "mempalace"
            and item.get("reason") == "backend-mismatch"
        )
    ]
    _write_payload(project, payload)


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


def _oversize_heal_artifact(project: Path, relative: str) -> bool:
    """#6219: harness exemption never covers a heal artifact above the read cap."""
    if relative not in HEAL_ARTIFACTS:
        return False
    candidate = Path(project) / relative
    try:
        return candidate.is_file() and not candidate.is_symlink() and candidate.stat().st_size > HEAL_ARTIFACT_CAP
    except OSError:
        return False


def read_allowed(project: Path, target: str) -> bool:
    """True when this path may be read without another store round trip."""
    relative = _project_relative(project, target)
    if is_harness_path(relative) and not _oversize_heal_artifact(project, relative):
        return True
    return (
        _allowlisted(project, relative)
        or cites(project, relative)
        or _prefix_allowed(project, relative)
        or _fail_open(project, relative)
        or _no_project_index(project)
    )


def _no_project_index(project: Path) -> bool:
    if project_index_state(project) != NO_PROJECT_INDEX:
        return False
    _record_no_project_index(project)
    return True


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
    if "-c" in arguments or "-" in arguments:
        # #6219: an inline/stdin program runs before any trailing store script.
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


def _tokens(segment: str) -> list[str]:
    try:
        import shlex

        return shlex.split(segment, posix=True)
    except ValueError:
        return []


def segment_kind(segment: str) -> str:
    """`store`, `run` (executing a script is not reading it), `read`, or `other`."""
    if _is_store_segment(segment):
        return "store"
    tokens = _tokens(segment)
    head, arguments = _command_head(tokens)
    if head in _PY:
        if "-c" in arguments or "-" in arguments:  # #6219: stdin/heredoc scripts read too
            return "read"
        if "-m" in arguments:
            return "run"
        return "run" if any(item.endswith(".py") for item in arguments if not item.startswith("-")) else "other"
    if head in _RUNNERS:
        return "run"
    if head in _FILE_HEADS:
        return "read"
    if not tokens and "python" in segment.casefold() and " -c" in segment:
        return "read"
    return "other"


def _pipeline_parts(command: str) -> list[str]:
    parts: list[str] = []
    for segment in _segments(command):
        parts.extend(piece.strip() for piece in re.split(r"(?<!\|)\|(?!\|)", segment) if piece.strip())
    return parts


def _read_segment_block(project: Path, segment: str) -> bool:
    head, _arguments = _command_head(_tokens(segment))
    if head in _PY or not head:
        paths = _shell_read_paths(segment)
        return bool(paths) and not all(read_allowed(project, path) for path in paths)
    paths = _paths_in_segment(segment)
    if paths:
        return not all(read_allowed(project, path) for path in paths)
    return head in _SEARCH_HEADS and not _no_project_index(project)


_PY_HEREDOC = re.compile(r"(?:^|[\s;&|(])(?:py|python3?)(?:\.exe)?\s+-\s*<<")


def _shell_block(project: Path, commands: tuple[str, ...]) -> str | None:
    for command in commands:
        if _PY_HEREDOC.search(command or ""):
            # #6219: a heredoc body is one program; `;` inside it is not a shell split.
            paths = _shell_read_paths(command)
            if paths and not all(read_allowed(project, path) for path in paths):
                return BLOCK_REASON
            continue
        for segment in _pipeline_parts(command):
            if segment_kind(segment) == "read" and _read_segment_block(project, segment):
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
