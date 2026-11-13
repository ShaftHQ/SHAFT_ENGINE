#!/usr/bin/env python3
"""Store.

Manual SQLite store for tool output. The database is
``.chaos-engine-state/tool-output/output.db`` under the project directory,
never a home-directory path. Nothing here replaces host tool output and
nothing here registers a hook.

FTS5 indexes the stored body when ``CREATE VIRTUAL TABLE ... USING fts5``
succeeds. When this Python cannot create that table, search uses a SQL LIKE
query against the same stored body. The optional index helper stores a body
only when it is over 32768 bytes, or over 8192 bytes for names ending in
``.xml``, ``.html``, ``.log``, or ``.har``.

Previews keep whole sentences, fenced blocks, and stack frames. A unit that
does not fit the budget is omitted, and the preview then keeps one pointer.
``output-purge`` asks delete-check before it unlinks the database. A refusal
leaves the file in place.
"""

from __future__ import annotations

import runpy
import shlex
import sqlite3
import sys
import time
from pathlib import Path

TRIGGER_BYTES = 32768
DUMP_TRIGGER_BYTES = 8192
DUMP_SUFFIXES = (".xml", ".html", ".log", ".har")
BUDGETS = {"ultra-lean": 1024, "balanced": 4096, "deep": 8192}
_LOADED: dict[str, dict] = {}


def db_path(project: Path) -> Path:
    return project.resolve() / ".chaos-engine-state" / "tool-output" / "output.db"


def has_row(project: Path | None = None) -> bool:
    """True when the store file exists and has a row. Does not create the file."""
    path = db_path(project or Path.cwd())
    if not path.is_file():
        return False
    try:
        conn = sqlite3.connect(path, timeout=1)
    except sqlite3.Error:
        return False
    try:
        conn.execute("PRAGMA query_only=ON")
        row = conn.execute("SELECT 1 FROM outputs LIMIT 1").fetchone()
    except sqlite3.Error:
        return False
    finally:
        conn.close()
    return row is not None


def trigger_for(name: str) -> int:
    if Path(name).suffix.lower() in DUMP_SUFFIXES:
        return DUMP_TRIGGER_BYTES
    return TRIGGER_BYTES


def _module(filename: str) -> dict:
    loaded = _LOADED.get(filename)
    if loaded is None:
        path = Path(__file__).resolve().with_name(filename)
        loaded = runpy.run_path(str(path), run_name="_ce_" + filename.removesuffix(".py"))
        _LOADED[filename] = loaded
    return loaded


def secret_labels(text: str) -> list[str]:
    """Labels from secret_guard, never the secret value itself."""
    return list(
        _module("secret_guard.py")["findings"](
            "tool-output.txt", text.encode("utf-8"), set(), []
        )
    )


def _receipt(text: str, labels: list[str]) -> str:
    listed = ", ".join(labels) if labels else "secret-shaped"
    return (
        "redaction-receipt: tool output was not stored\n"
        f"findings: {listed}\n"
        f"bytes: {len(text.encode('utf-8'))}\n"
    )


def fts5_available() -> bool:
    """True when this sqlite build can create an FTS5 table. Otherwise LIKE."""
    probe = sqlite3.connect(":memory:")
    try:
        probe.execute("CREATE VIRTUAL TABLE probe USING fts5(body)")
        return True
    except sqlite3.OperationalError:
        return False
    finally:
        probe.close()


def connect(project: Path) -> sqlite3.Connection:
    path = db_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path, timeout=5)
    conn.execute("PRAGMA busy_timeout=5000")
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute(
        """CREATE TABLE IF NOT EXISTS outputs (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            body TEXT NOT NULL,
            bytes INTEGER NOT NULL,
            created TEXT NOT NULL,
            redacted INTEGER NOT NULL
        )"""
    )
    if fts5_available():
        conn.execute("CREATE VIRTUAL TABLE IF NOT EXISTS outputs_fts USING fts5(body)")
    conn.commit()
    return conn


def _engine(conn: sqlite3.Connection) -> str:
    row = conn.execute(
        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='outputs_fts'"
    ).fetchone()
    return "fts5" if row else "like"


def store_text(text: str, *, name: str, project: Path | None = None, redacted: bool = False) -> tuple[int, str]:
    """Persist text. A secret-shaped body is replaced with a redaction receipt."""
    labels = [] if redacted else secret_labels(text)
    if labels:
        text = _receipt(text, labels)
        redacted = True
    root = (project or Path.cwd()).resolve()
    conn = connect(root)
    try:
        conn.execute("BEGIN")
        cursor = conn.execute(
            "INSERT INTO outputs(name, body, bytes, created, redacted) VALUES (?,?,?,?,?)",
            (name, text, len(text.encode("utf-8")), time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()), int(redacted)),
        )
        row_id = int(cursor.lastrowid)
        if _engine(conn) == "fts5":
            conn.execute("INSERT INTO outputs_fts(rowid, body) VALUES (?, ?)", (row_id, text))
        conn.commit()
        return row_id, text
    except Exception:
        conn.rollback()
        raise
    finally:
        conn.close()


def index_if_over_trigger(text: str, *, name: str = "tool-output.txt", project: Path | None = None) -> int | None:
    """Store text only when it is over the trigger. Return the row id or None."""
    if len(text.encode("utf-8")) <= trigger_for(name):
        return None
    row_id, _stored = store_text(text, name=name, project=project)
    return row_id


def _boundary(text: str, index: int) -> bool:
    nxt = index + 1
    if nxt >= len(text):
        return True
    if text[nxt].isdigit():
        return False
    return text[nxt].isspace()


def _frame_line(line: str) -> bool:
    stripped = line.strip()
    if stripped.startswith('File "') and '", line ' in stripped and ", in " in stripped:
        return True
    return stripped.startswith("at ") and "(" in stripped and stripped.endswith(")")


def _traceback_end(text: str, start: int) -> int:
    newline = text.find("\n", start)
    if newline < 0:
        return len(text)
    index = newline + 1
    size = len(text)
    while index < size:
        line_end = text.find("\n", index)
        line_end = size if line_end < 0 else line_end + 1
        line = text[index:line_end]
        if line.startswith((" ", "\t")) or _frame_line(line):
            index = line_end
            continue
        return index
    return index


def _sentence_end(text: str, start: int) -> int:
    index = start
    size = len(text)
    while index < size:
        if index > start and (
            text.startswith("```", index)
            or text.startswith("Traceback (most recent call last):", index)
        ):
            return index
        if text[index] in ".!?" and _boundary(text, index):
            index += 1
            while index < size and text[index] in " \t":
                index += 1
            if index < size and text[index] == "\n":
                index += 1
            return max(index, start + 1)
        index += 1
    return size


def units(text: str) -> list[str]:
    """Sentences, fenced blocks, and stack frames. Never a partial unit."""
    parts: list[str] = []
    index = 0
    size = len(text)
    while index < size:
        if text.startswith("```", index):
            close = text.find("```", index + 3)
            end = size if close < 0 else close + 3
            if end < size and text[end] == "\n":
                end += 1
            parts.append(text[index:end])
            index = end
            continue
        if text.startswith("Traceback (most recent call last):", index):
            end = _traceback_end(text, index)
            parts.append(text[index:end])
            index = end
            continue
        newline = text.find("\n", index)
        line_end = size if newline < 0 else newline + 1
        line = text[index:line_end]
        if _frame_line(line):
            parts.append(line)
            index = line_end
            continue
        end = _sentence_end(text, index)
        if end <= index:
            end = index + 1
        parts.append(text[index:end])
        index = end
    return parts


def build_preview(text: str, budget: int, pointer: str) -> str:
    """Fit whole units into ``budget`` bytes. Omit the rest and keep one pointer."""
    pointer_size = len(pointer.encode("utf-8"))
    room = budget - pointer_size
    if room < 0:
        return pointer
    kept: list[str] = []
    used = 0
    for chunk in units(text):
        size = len(chunk.encode("utf-8"))
        if used + size <= room:
            kept.append(chunk)
            used += size
            continue
        return "".join(kept) + pointer
    return "".join(kept)


def replacement_preview(
    text: str,
    *,
    name: str = "tool-output.txt",
    budget: str = "balanced",
    project: Path | None = None,
) -> str:
    """Preview after a successful store. On a store or FTS error, return ``text``."""
    try:
        if len(text.encode("utf-8")) <= trigger_for(name):
            return text
        row_id, stored = store_text(text, name=name, project=project)
        if stored != text:
            return stored
        limit = BUDGETS.get(budget, BUDGETS["balanced"])
        return build_preview(text, limit, f" [tool-output id={row_id}]")
    except (sqlite3.Error, OSError):
        return text


def _project(flags: dict[str, str]) -> Path:
    raw = flags.get("project")
    return Path(raw).resolve() if raw else Path.cwd().resolve()


def _flags(arguments: list[str]) -> tuple[dict[str, str], list[str]] | str:
    flags: dict[str, str] = {}
    rest: list[str] = []
    index = 0
    while index < len(arguments):
        arg = arguments[index]
        if arg in {"--project", "--budget"}:
            if index + 1 >= len(arguments):
                return f"missing value for {arg}"
            flags[arg[2:]] = arguments[index + 1]
            index += 2
            continue
        rest.append(arg)
        index += 1
    if flags.get("budget") and flags["budget"] not in BUDGETS:
        return "budget must be ultra-lean, balanced, or deep"
    return flags, rest


def _matching_lines(body: str, query: str) -> list[str]:
    lines = body.splitlines()
    direct = [line for line in lines if query in line]
    if direct:
        return direct
    tokens = [token for token in "".join(ch if ch.isalnum() else " " for ch in query).split() if token]
    if not tokens:
        return []
    return [line for line in lines if all(token in line for token in tokens)]


def _like_pattern(query: str) -> str:
    escaped = query.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
    return f"%{escaped}%"


def _search_rows(conn: sqlite3.Connection, query: str) -> list[tuple[int, str]]:
    if _engine(conn) == "fts5":
        phrase = '"' + query.replace('"', '""') + '"'
        try:
            return list(
                conn.execute(
                    """SELECT outputs.id, outputs.body
                       FROM outputs JOIN outputs_fts ON outputs.id = outputs_fts.rowid
                       WHERE outputs_fts MATCH ?""",
                    (phrase,),
                )
            )
        except sqlite3.OperationalError:
            pass  # MATCH can raise; the LIKE query below is the fallback
    return list(
        conn.execute(
            "SELECT id, body FROM outputs WHERE body LIKE ? ESCAPE '\\'",
            (_like_pattern(query),),
        )
    )


def _open(project: Path) -> sqlite3.Connection:
    path = db_path(project)
    if not path.is_file():
        raise FileNotFoundError(path)
    conn = sqlite3.connect(path, timeout=5)
    conn.execute("PRAGMA busy_timeout=5000")
    return conn


def _cmd_search(project: Path, query: str) -> int:
    try:
        conn = _open(project)
    except FileNotFoundError:
        print("output-search: no store", file=sys.stderr)
        return 1
    try:
        rows = _search_rows(conn, query)
    finally:
        conn.close()
    printed = False
    for row_id, body in rows:
        for line in _matching_lines(body, query):
            print(f"{row_id}\t{line}")
            printed = True
    if not printed:
        print("output-search: no match", file=sys.stderr)
        return 1
    return 0


def _cmd_resume(project: Path, raw_id: str) -> int:
    try:
        row_id = int(raw_id)
    except ValueError:
        print("output-resume: id must be an integer", file=sys.stderr)
        return 2
    try:
        conn = _open(project)
    except FileNotFoundError:
        print("output-resume: no store", file=sys.stderr)
        return 1
    try:
        row = conn.execute("SELECT body FROM outputs WHERE id=?", (row_id,)).fetchone()
    finally:
        conn.close()
    if row is None:
        print("output-resume: absent", file=sys.stderr)
        return 1
    sys.stdout.write(row[0])
    return 0


def _cmd_stats(project: Path) -> int:
    path = db_path(project)
    if not path.is_file():
        print(f"path: {path}")
        print("engine: absent")
        print("rows: 0")
        print("bytes: 0")
        print("redacted: 0")
        return 0
    conn = _open(project)
    try:
        count, total, redacted = conn.execute(
            "SELECT COUNT(*), COALESCE(SUM(bytes), 0), COALESCE(SUM(redacted), 0) FROM outputs"
        ).fetchone()
        engine = _engine(conn)
    finally:
        conn.close()
    print(f"path: {path}")
    print(f"engine: {engine}")
    print(f"rows: {count}")
    print(f"bytes: {total}")
    print(f"redacted: {redacted}")
    return 0


def _cmd_purge(project: Path) -> int:
    path = db_path(project)
    targets = [item for item in (path, Path(str(path) + "-wal"), Path(str(path) + "-shm")) if item.exists()]
    if not targets:
        print("output-purge: absent")
        return 0
    command = "rm -f -- " + " ".join(shlex.quote(str(item)) for item in targets)
    reason, _warnings = _module("delete_guard.py")["check"](command, project, project)
    if reason:
        print(reason, file=sys.stderr)
        print("output-purge: refused", file=sys.stderr)
        return 1
    for item in targets:
        item.unlink()
    print("output-purge: removed")
    return 0


def main(arguments: list[str] | None = None, tool: str | None = None) -> int:
    """Dispatch one output-* verb. ``tool`` is the tool.py verb name."""
    arguments = list(sys.argv[1:] if arguments is None else arguments)
    parsed = _flags(arguments)
    if isinstance(parsed, str):
        print(f"output-store: {parsed}", file=sys.stderr)
        return 2
    flags, rest = parsed
    project = _project(flags)
    verb = tool or (rest[0] if rest else "")
    if verb in {"output-search", "output-resume", "output-stats", "output-purge"} and tool:
        operands = rest
    else:
        operands = rest[1:] if rest else []
        if rest:
            verb = rest[0]
    if verb == "output-search" and len(operands) == 1:
        return _cmd_search(project, operands[0])
    if verb == "output-resume" and len(operands) == 1:
        return _cmd_resume(project, operands[0])
    if verb == "output-stats" and not operands:
        return _cmd_stats(project)
    if verb == "output-purge" and not operands:
        return _cmd_purge(project)
    print(
        "usage: tool.py output-search QUERY [--project PATH]\n"
        "       tool.py output-resume ID [--project PATH]\n"
        "       tool.py output-stats [--project PATH]\n"
        "       tool.py output-purge [--project PATH]",
        file=sys.stderr,
    )
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
