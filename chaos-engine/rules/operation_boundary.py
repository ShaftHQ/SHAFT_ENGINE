"""Operation-boundary guards for push, remotes, deletes, and secret-bearing output.

Callers pass the real command and the project root. Approval is a file on disk,
not a boolean the caller asserts.
"""

from __future__ import annotations

import importlib.util
import json
import re
import sys
from pathlib import Path
from typing import Any

# `set +e` is a separate shell statement, so it is not part of the git push
# statement. Any plus argument on the push statement is a force refspec.
# A short option cluster that contains f counts only on that statement (#7901).
# The cluster may end at a substitution closer or a redirect, not only at whitespace.
_FORCE = re.compile(
    r"(--force\b|--force-with-lease\b"
    r"|(?:^|\s)-[A-Za-z]*f[A-Za-z]*(?=[\s)|`<>]|$)"
    r"|\s\+\S+)"
)
_REMOTE_ADD = re.compile(r"\bgit\s+remote\s+add\b")
_DELETE = re.compile(r"\b(?:delete|cleanup)\b", re.I)
_DELETE_CODE = re.compile(r"(?:--delete-code|--code)\s+\S+|DELETE_CODE=\S+", re.I)
_PUSH = re.compile(r"\bgit\s+push\b")
_FORCE_WORD = re.compile(r"--force(?:-with-lease)?\b")
_ASSIGN = re.compile(
    r"^(?:set\s+--(?=\s|$)|"
    r"(?:(?:declare|export|local|readonly|typeset)(?:\s+-[A-Za-z]+)*\s+)?"
    r"[A-Za-z_][A-Za-z0-9_]*\+?=)"
)
_APPROVE_ONLY = re.compile(
    r"^(?:python3?|py(?:\s+-3)?)\s+\S*tool\.py\s+force-push-approve\s+--\s+\S"
)
_UNSAFE_ARG = re.compile(r"[$`\\;&|<>\n\r()]")


def _load(path: Path, name: str) -> Any:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def _rules() -> Any:
    return _load(Path(__file__).resolve().with_name("lessons_2026_11_06.py"), "ce_boundary_lessons")


def _pdca() -> Any:
    return _load(Path(__file__).resolve().with_name("lessons_repeat_failure_pdca.py"), "ce_boundary_pdca")


def _normalized(command: str) -> str:
    return " ".join((command or "").split())


def _shell_statements(command: str) -> list[str]:
    """Split on ``&&``, ``||``, ``;``, ``|``, and ``&`` outside quotes and substitutions.

    A quoted ref, ``$(...)``, or a backtick command may contain those separators.
    """
    parts: list[str] = []
    buf: list[str] = []
    quote = ""
    index = 0

    def append_quoted(mark: str, cursor: int) -> int:
        buf.append(command[cursor])
        cursor += 1
        while cursor < len(command):
            buf.append(command[cursor])
            if command[cursor] == mark and command[cursor - 1] != "\\":
                return cursor + 1
            cursor += 1
        return cursor

    while index < len(command):
        char = command[index]
        if quote:
            buf.append(char)
            if char == quote and command[index - 1] != "\\":
                quote = ""
            index += 1
            continue
        if char in {"'", '"'}:
            quote = char
            buf.append(char)
            index += 1
            continue
        if command.startswith("$(", index):
            depth = 1
            buf.append("$(")
            index += 2
            while index < len(command) and depth:
                if command[index] in {"'", '"'}:
                    index = append_quoted(command[index], index)
                    continue
                buf.append(command[index])
                if command[index] == "(":
                    depth += 1
                elif command[index] == ")":
                    depth -= 1
                index += 1
            continue
        if char == "`":
            index = append_quoted("`", index)
            continue
        if command.startswith("&&", index) or command.startswith("||", index):
            parts.append("".join(buf))
            buf = []
            index += 2
            continue
        if char in {"|", "&", ";"}:
            parts.append("".join(buf))
            buf = []
            index += 1
            continue
        buf.append(char)
        index += 1
    parts.append("".join(buf))
    return [part.strip() for part in parts if part.strip()]


def _strip_unquoted_comment(part: str) -> str:
    """Drop a bash comment. A hash starts one only at the beginning of a word."""
    quote = ""
    out: list[str] = []
    for index, char in enumerate(part):
        if quote:
            out.append(char)
            if char == quote and part[index - 1] != "\\":
                quote = ""
            continue
        if char in {"'", '"'}:
            quote = char
            out.append(char)
            continue
        if char == "#" and (index == 0 or part[index - 1] in " \t"):
            break
        out.append(char)
    return "".join(out).strip()


def _comment_is_open(part: str) -> bool:
    """True when an unquoted word-starting hash has already started a comment."""
    quote = ""
    for index, char in enumerate(part):
        if quote:
            if char == quote and part[index - 1] != "\\":
                quote = ""
            continue
        if char in {"'", '"'}:
            quote = char
            continue
        if char == "#" and (index == 0 or part[index - 1] in " \t"):
            return True
    return False


def _heredoc_delimiter(text: str, index: int) -> tuple[str, int] | None:
    """Return the delimiter and the index after ``<<word`` when one starts here."""
    if not text.startswith("<<", index):
        return None
    cursor = index + 2
    if cursor < len(text) and text[cursor] == "-":
        cursor += 1
    while cursor < len(text) and text[cursor] in " \t":
        cursor += 1
    if cursor >= len(text) or text[cursor] in "\n\r;&|<>()":
        return None
    if text[cursor] in {"'", '"'}:
        mark = text[cursor]
        cursor += 1
        start = cursor
        while cursor < len(text) and text[cursor] != mark:
            cursor += 1
        delim = text[start:cursor]
        if cursor < len(text):
            cursor += 1
        return delim, cursor
    start = cursor
    while cursor < len(text) and text[cursor] not in " \t\n\r;&|<>()":
        cursor += 2 if text[cursor] == "\\" and cursor + 1 < len(text) else 1
    delim = text[start:cursor].replace("\\", "")
    if not delim:
        return None
    return delim, cursor


def _consume_heredoc(text: str, index: int, delim: str, buf: list[str]) -> int:
    """Append one here-document body. ``index`` is the first body character."""
    line: list[str] = []
    while index < len(text):
        char = text[index]
        buf.append(char)
        index += 1
        if char == "\r" and index < len(text) and text[index] == "\n":
            buf.append("\n")
            index += 1
            char = "\n"
        if char in "\n\r":
            if "".join(line) == delim:
                return index
            line = []
            continue
        line.append(char)
    return index


def _matching_brace(text: str, index: int) -> int:
    """Index after the ``${...}`` that starts at ``index``. Unclosed runs to the end."""
    depth = 1
    quote = ""
    cursor = index + 2
    while cursor < len(text):
        char = text[cursor]
        if quote:
            if char == quote and text[cursor - 1] != "\\":
                quote = ""
            cursor += 1
            continue
        if char in {"'", '"'}:
            quote = char
            cursor += 1
            continue
        if text.startswith("${", cursor) or char == "{":
            depth += 1
            cursor += 2 if text.startswith("${", cursor) else 1
            continue
        if char == "}" and depth:
            depth -= 1
            cursor += 1
            if depth == 0:
                return cursor
            continue
        cursor += 1
    return cursor


def _fold_command_syntax(
    text: str,
    index: int,
    buf: list[str],
    pieces: list[str],
    pending: list[str],
    quote: str,
    ticks: bool,
    depth: int,
) -> int | None:
    """Advance past a continuation or here-doc. None means take this character."""
    if ticks or quote == "'":
        return None
    if text.startswith("\\\r\n", index) or text.startswith("\\\n", index):
        step = 3 if text.startswith("\\\r\n", index) else 2
        if _comment_is_open("".join(buf)):
            if depth == 0:
                pieces.append("".join(buf))
                buf.clear()
            else:
                buf.append("\n")
        return index + step
    if quote:
        return None
    if text[index] == "#" and (not buf or buf[-1] in " \t"):
        end = index
        while end < len(text) and text[end] not in "\n\r":
            end += 1
        buf.append(text[index:end])
        return end
    if text.startswith("${", index):
        end = _matching_brace(text, index)
        buf.append(text[index:end])
        return end
    found = _heredoc_delimiter(text, index)
    if found is not None:
        delim, end = found
        buf.append(text[index:end])
        pending.append(delim)
        return end
    if pending and text[index] in "\n\r":
        buf.append(text[index])
        index += 1
        if text[index - 1] == "\r" and index < len(text) and text[index] == "\n":
            buf.append("\n")
            index += 1
        while pending:
            index = _consume_heredoc(text, index, pending.pop(0), buf)
        return index
    return None


def _is_approve_invocation(part: str) -> bool:
    """True only for a literal approval command. Shell syntax still runs a push."""
    code = _strip_unquoted_comment(part).strip()
    if _APPROVE_ONLY.match(code) is None:
        return False
    marker = code.find(" -- ")
    if marker < 0:
        return False
    return _UNSAFE_ARG.search(code[marker + 4 :]) is None


def _command_pieces(command: str) -> list[str]:
    """One shell statement per piece.

    A backslash-newline is removed, except inside a comment, where the next
    line is a new command. A newline inside quotes, ``$(...)``, backticks,
    parentheses, or a here-document stays in that command.
    """
    text = command or ""
    pieces: list[str] = []
    buf: list[str] = []
    pending: list[str] = []
    quote = ""
    depth = 0
    ticks = False
    index = 0
    while index < len(text):
        folded = _fold_command_syntax(text, index, buf, pieces, pending, quote, ticks, depth)
        if folded is not None:
            index = folded
            continue
        char = text[index]
        if ticks:
            buf.append(char)
            if char == "`":
                ticks = False
            index += 1
            continue
        if quote:
            buf.append(char)
            if char == quote and text[index - 1] != "\\":
                quote = ""
            index += 1
            continue
        if char in {"'", '"'}:
            quote = char
            buf.append(char)
            index += 1
            continue
        if char == "`":
            ticks = True
            buf.append(char)
            index += 1
            continue
        if (
            text.startswith("$(", index)
            or text.startswith("<(", index)
            or text.startswith(">(", index)
        ):
            depth += 1
            buf.append(text[index : index + 2])
            index += 2
            continue
        if char == "(":
            depth += 1
            buf.append(char)
            index += 1
            continue
        if char == ")" and depth:
            depth -= 1
            buf.append(char)
            index += 1
            continue
        if char in {"\n", "\r"} and depth == 0:
            pieces.append("".join(buf))
            buf = []
            index += 1
            continue
        buf.append(char)
        index += 1
    pieces.append("".join(buf))
    statements: list[str] = []
    for piece in pieces:
        statements.extend(_shell_statements(piece))
    return statements


def _approval_root(root: Path) -> Path:
    """Git toplevel, so a subdirectory tool call sees the recorded approval."""
    current = Path(root)
    for candidate in (current, *current.parents):
        if (candidate / ".git").exists():
            return candidate
    return current


def force_push_block(root: Path, command: str) -> str | None:
    text = _normalized(command)
    pieces = _command_pieces(command or "")
    segments = [
        part
        for part in pieces
        if _PUSH.search(part) and not _is_approve_invocation(part)
    ]
    borrowed = any(re.search(r"[`$]", part) for part in segments) and any(
        _ASSIGN.match(part.strip()) is not None and _FORCE.search(part) is not None
        for part in pieces
    )
    if (
        not any(_FORCE.search(part) for part in segments)
        and not (segments and _FORCE_WORD.search(text))
        and not borrowed
    ):
        return None
    path = _approval_root(root) / ".chaos-engine-state" / "force-push-approval.txt"
    recorded: list[str] = []
    if path.is_file():
        recorded = [line.strip() for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]
    if text in recorded:
        recorded.remove(text)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("".join(f"{line}\n" for line in recorded), encoding="utf-8")
        return None
    return "force-push-without-approval"


def force_push_hint(command: str) -> str:
    """User-facing approval step. The denial token itself stays exact."""
    text = _normalized(command)
    lease = re.search(r"--force-with-lease=\S+:[0-9a-fA-F]{7,}", text) is not None
    form = "--force-with-lease=<ref>:<sha>" if lease else "bare --force"
    return (
        f"{form}. Ask the user to run `python3 .chaos-engine/tool.py force-push-approve -- {text}`"
    )


def private_remote_block(root: Path, command: str) -> str | None:
    if not _REMOTE_ADD.search(command or ""):
        return None
    marker = Path(root) / ".chaos-engine-state" / "private-working-copy"
    if marker.is_file():
        return "remote-on-private-copy"
    return None


def relayed_delete_block(command: str) -> str | None:
    """A delete code inside the agent command was relayed, not typed by the human."""
    text = command or ""
    if _DELETE.search(text) and _DELETE_CODE.search(text):
        return "relayed-delete-code"
    return None


def secret_failures(text: str, *, kind: str = "output") -> list[str]:
    result = _rules().evaluate_secrets_text(text or "", kind=kind)
    return list(result.get("hits") or [])


_STAGE = re.compile(r"\b(?:pytest|unittest)\b|\bmvn\b[^\n]*\btest\b|\bnpm\b[^\n]*\btest\b")
_STAGE_FLAG = re.compile(r"--stage(?:=|\s+)([A-Za-z0-9_.-]+)")
_EMPTY_TREE = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
# #6216: origin/HEAD is the configured default. These names are only a fallback
# when that symref was never written, which a fetch-only checkout often skips.
_REMOTE_BRANCH_FALLBACKS = ("main", "master")


def stage_of_command(command: str) -> str | None:
    flagged = _STAGE_FLAG.search(command or "")
    if flagged:
        return flagged.group(1)
    if _STAGE.search(command or ""):
        return "test"
    return None


def _pdca_dir(root: Path) -> Path:
    return Path(root) / ".chaos-engine-state" / "repeat-failure-pdca"


def _replay_events(path: Path) -> dict[str, int]:
    counts: dict[str, int] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        item = json.loads(line)
        if not isinstance(item, dict):
            continue
        name = str(item.get("stage") or "").strip()
        if not name:
            continue
        counts[name] = counts.get(name, 0) + 1 if item.get("failed") is True else 0
    return counts


def record_stage_event(root: Path, stage: str, *, failed: bool) -> str | None:
    """Append one observed failure or reset. The events file is the source of truth."""
    folder = _pdca_dir(root)
    try:
        folder.mkdir(parents=True, exist_ok=True)
        events = folder / "events.jsonl"
        with events.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps({"stage": stage, "failed": bool(failed)}) + "\n")
        counts = _replay_events(events)
        (folder / "failures.json").write_text(json.dumps({"stages": counts}), encoding="utf-8")
    except (OSError, json.JSONDecodeError, TypeError, ValueError):
        return "pdca-state-unavailable"
    return None


def observe_commands(root: Path, commands: list[str] | tuple[str, ...], *, failed: bool) -> str | None:
    """Persist a stage event for a command the host actually ran."""
    for command in commands:
        stage = stage_of_command(command)
        if not stage:
            continue
        problem = record_stage_event(root, stage, failed=failed)
        if problem:
            return problem
    return None


def output_secret_block(result: object, root: Path | None = None) -> str | None:
    """Scan tool stdout/stderr/output. The command string is not this evidence."""
    if not isinstance(result, dict):
        return None
    text = "\n".join(str(result.get(key) or "") for key in ("stdout", "stderr", "output"))
    hits = secret_failures(text, kind="output") if text.strip() else []
    if root is not None and text.strip() and not hits:
        guard = _load(Path(__file__).resolve().parents[1] / "secret_guard.py", "ce_boundary_secret_guard")
        names = [part.strip() for part in str(guard.DEFAULT_GUARDED_FILES).split(",") if part.strip()]
        encoded = text.encode("utf-8", errors="ignore")
        for value in guard.secret_values(Path(root), names):
            if value and value in encoded:
                hits.append("secret-in-output")
                break
    if not hits:
        return None
    code = result.get("exit_code", result.get("exitCode", 0))
    return f"{hits[0]}; output withheld; the command already finished with exit code {code}"


def _git(root: Path, args: list[str]):
    import subprocess  # nosec B404 - fixed git argv, no shell.

    try:
        return subprocess.run(  # nosec B603 B607 - git from PATH, argument list, no shell.
            ["git", *args],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return None


def _remote_candidates(root: Path) -> list[str] | None:
    head = _git(root, ["symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"])
    if head is None:
        return None
    names: list[str] = []
    text = (head.stdout or "").strip()
    if head.returncode == 0 and text:
        names.append(text)
    names.append("origin/HEAD")
    names.extend(f"refs/remotes/origin/{name}" for name in _REMOTE_BRANCH_FALLBACKS)
    return names


def _remote_base(root: Path) -> str | None:
    """Merge base with the remote default branch. ``''`` means no such branch."""
    names = _remote_candidates(root)
    if names is None:
        return None
    for name in names:
        verified = _git(root, ["rev-parse", "--verify", "--quiet", name])
        if verified is None:
            return None
        if verified.returncode != 0 or not (verified.stdout or "").strip():
            continue
        merged = _git(root, ["merge-base", "HEAD", name])
        if merged is None or merged.returncode != 0 or not (merged.stdout or "").strip():
            return None
        return merged.stdout.strip()
    return ""


def _outgoing_base(root: Path) -> str | None:
    """Commit range being published. ``None`` means the range cannot be read."""
    upstream = _git(root, ["rev-parse", "--verify", "--quiet", "@{upstream}"])
    if upstream is not None and upstream.returncode == 0 and (upstream.stdout or "").strip():
        base = _git(root, ["merge-base", "HEAD", "@{upstream}"])
        if base is not None and base.returncode == 0 and (base.stdout or "").strip():
            return base.stdout.strip()
        return None
    remote = _remote_base(root)
    if remote is None:
        return None
    if remote:
        return remote
    inside = _git(root, ["rev-parse", "--is-inside-work-tree"])
    if inside is None or inside.returncode != 0:
        return ""
    count = _git(root, ["rev-list", "--count", "HEAD"])
    if count is None or count.returncode != 0:
        return None
    try:
        commits = int((count.stdout or "0").strip() or "0")
    except ValueError:
        return None
    if commits > 40:
        return None
    return _EMPTY_TREE


def _added_diff_text(patch: str) -> str:
    """Text introduced by a patch. ``+++`` file headers are not additions.

    Deletions of lines that already existed on the outgoing base are not new
    commits. An add that a later commit removes stays visible in ``git log -p``.
    """
    added: list[str] = []
    for line in (patch or "").splitlines():
        if not line.startswith("+") or line.startswith("+++"):
            continue
        added.append(line[1:])
    return "\n".join(added)


def committed_secret_failures(root: Path) -> list[str]:
    """Scan outgoing commit and index blobs. A clean worktree does not clear a committed secret."""
    if not (Path(root) / ".git").exists():
        return []
    base = _outgoing_base(Path(root))
    if base is None:
        return ["outgoing-diff-unavailable"]
    if not base:
        return []
    exclude = [".", ":(exclude)tests", ":(exclude)tests/**"]
    # Every outgoing commit, including merge resolutions. Default `git log -p`
    # omits merge patches, so a secret that exists only in a merge commit would
    # disappear after a later merge removes it. Only added lines count: deleting
    # a line that already existed on the base is not a new secret.
    history = _git(
        Path(root),
        ["log", "-p", "--diff-merges=first-parent", "--no-color", f"{base}..HEAD", "--", *exclude],
    )
    head = _git(Path(root), ["diff", "--no-color", f"{base}..HEAD", "--", *exclude])
    staged = _git(Path(root), ["diff", "--cached", "--no-color", "--", *exclude])
    scans = (history, head, staged)
    if any(item is None or item.returncode != 0 for item in scans):
        return ["outgoing-diff-unavailable"]
    text = "\n".join(_added_diff_text(item.stdout or "") for item in scans)
    return secret_failures(text, kind="commit")


def pdca_push_failures(root: Path) -> list[str]:
    """A second consecutive observed failure blocks push until that stage's note exists."""
    folder = _pdca_dir(root)
    events = folder / "events.jsonl"
    summary = folder / "failures.json"
    if not folder.exists() and not events.exists() and not summary.exists():
        return []
    if events.exists():
        try:
            return _pdca_stage_hits(Path(root), _replay_events(events))
        except (OSError, json.JSONDecodeError, TypeError, ValueError):
            return ["pdca-state-unavailable"]
    if summary.is_file():
        return _summary_stage_hits(Path(root), summary)
    return [] if folder.is_dir() else ["pdca-state-unavailable"]


def _summary_stage_hits(root: Path, summary: Path) -> list[str]:
    try:
        payload = json.loads(summary.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return ["pdca-state-unavailable"]
    stages = payload.get("stages") if isinstance(payload, dict) else None
    if not isinstance(stages, dict):
        return ["pdca-state-unavailable"]
    return _pdca_stage_hits(root, stages)


def _pdca_stage_hits(root: Path, stages: dict[str, Any]) -> list[str]:
    notes = _pdca()
    hits: list[str] = []
    for stage, count in stages.items():
        try:
            consecutive = int(count)
        except (TypeError, ValueError):
            continue
        if consecutive < 2:
            continue
        note_path = root / ".chaos-engine-state" / "repeat-failure-pdca" / f"{stage}.json"
        note = None
        if note_path.is_file():
            try:
                loaded = json.loads(note_path.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError):
                loaded = None
            note = loaded if isinstance(loaded, dict) else None
        if not notes.note_covers_stage(note, str(stage)):
            hits.append(f"push-without-rca:{stage}")
    return hits


def command_blocks(root: Path, command: str) -> list[str]:
    """Guards that read the command about to run and the files already on disk."""
    hits: list[str] = []
    for item in (
        force_push_block(root, command),
        private_remote_block(root, command),
        relayed_delete_block(command),
    ):
        if item:
            hits.append(item)
    hits.extend(secret_failures(command, kind="command"))
    return hits


def _watcher_loop_failures(root: Path, changed: list[str]) -> list[str]:
    interesting = [relative for relative in changed if relative.replace("\\", "/").endswith("watch_pr_checks.py")]
    if not interesting:
        return []
    watcher = _load(
        Path(__file__).resolve().parents[2] / "scripts" / "agents" / "watch_pr_checks.py",
        "ce_boundary_watch",
    )
    hits: list[str] = []
    for relative in interesting:
        normal = relative.replace("\\", "/")
        if not normal.endswith("watch_pr_checks.py"):
            continue
        path = Path(root) / relative
        if not path.is_file():
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        for item in watcher.preflight_unbounded_loops(text):
            hits.append(f"watcher:{item}")
    return hits


def disk_blocks(root: Path, changed: list[str] | None = None) -> list[str]:
    """Push-time evidence: PDCA events, outgoing blobs, bounded watcher loops."""
    hits = list(pdca_push_failures(root))
    hits.extend(committed_secret_failures(root))
    hits.extend(_watcher_loop_failures(root, list(changed or [])))
    return hits
