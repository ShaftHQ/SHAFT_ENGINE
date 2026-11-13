"""PreToolUse decision for an unranged Read of a large file.

A dump suffix (.html, .xml, .log, .har) at or above 8192 bytes is denied.
Any file at or above 262144 bytes is denied. offset, limit, or a host range
key allows the Read. This does not spill and does not replace a PostToolUse
body.
"""

from __future__ import annotations

DUMP_BYTES = 8192
ANY_BYTES = 262144
DUMP_SUFFIXES = frozenset({".html", ".xml", ".log", ".har"})
READ_NAMES = frozenset({"read", "view", "readfile", "readmanyfiles"})
PATH_KEYS = ("file_path", "filePath", "path", "target_file")
RANGE_KEYS = frozenset({
    "offset",
    "limit",
    "start_line",
    "end_line",
    "startLine",
    "endLine",
    "line_offset",
    "line_limit",
    "head_limit",
    "view_range",
    "range",
})


def _read_tool(tool_name: object) -> bool:
    compact = "".join(character for character in str(tool_name or "").casefold() if character.isalpha())
    return compact in READ_NAMES


def _present(value: object) -> bool:
    if value is None or value is False:
        return False
    if isinstance(value, str):
        return bool(value.strip())
    if isinstance(value, (list, tuple, dict)):
        return bool(value)
    return True


def _ranged(tool_input: dict) -> bool:
    return any(_present(tool_input.get(key)) for key in RANGE_KEYS if key in tool_input)


def _absolute(path: str) -> bool:
    if path.startswith(("/", "\\")):
        return True
    return len(path) > 2 and path[1] == ":"


def _join(cwd: str | None, path: str) -> str:
    if _absolute(path) or not cwd:
        return path
    return cwd.rstrip("/\\") + "/" + path


def _file(tool_input: dict, cwd: str | None) -> str | None:
    for key in PATH_KEYS:
        value = tool_input.get(key)
        if isinstance(value, str) and value.strip():
            return _join(cwd, value.strip())
    return None


def _suffix(path: str) -> str:
    name = path.replace("\\", "/").rsplit("/", 1)[-1]
    dot = name.rfind(".")
    if dot <= 0:
        return ""
    return name[dot:].casefold()


def _size(path: str) -> int | None:
    try:
        handle = open(path, "rb")
    except OSError:
        return None
    try:
        return handle.seek(0, 2)
    except OSError:
        return None
    finally:
        handle.close()


def deny_reason(tool_name: object, tool_input: object, cwd: str | None = None) -> str | None:
    """Return a deny string, or None when this Read may run."""
    if not _read_tool(tool_name) or not isinstance(tool_input, dict) or _ranged(tool_input):
        return None
    path = _file(tool_input, cwd)
    if path is None:
        return None
    size = _size(path)
    if size is None:
        return None
    dump = _suffix(path) in DUMP_SUFFIXES and size >= DUMP_BYTES
    if not dump and size < ANY_BYTES:
        return None
    name = path.replace("\\", "/").rsplit("/", 1)[-1] or path
    return (
        f"Read denied: {name} is {size} bytes. "
        "Use a ranged Read, rg or tail, or a script that prints only the answer."
    )
