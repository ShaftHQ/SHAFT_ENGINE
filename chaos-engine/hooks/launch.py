#!/usr/bin/env python3
"""
Host hook launcher (#7667).

Host hook entries point here so a block returns only its reason, not an 11KB
inline polyglot. Finds guard.py by walking parents (and CLAUDE_PROJECT_DIR /
payload cwd), then runs it.
"""

from __future__ import annotations

import errno
import io
import json
import os
import pathlib
import re
import runpy
import sys
import tempfile
import time

HOOK_PAYLOAD_ENV = "CHAOS_ENGINE_HOOK_PAYLOAD"
HOOK_PYTHON_POINTER = ".chaos-engine-state/hook-python"
HOOK_PYTHON_FLOOR = (3, 11)

CWD_UNAVAILABLE = (
    "repository working directory unavailable; restore the original mount or checkout, then retry"
)
GUARD_UNAVAILABLE = (
    "ChaosEngine guard unavailable; repair the original installation, then retry"
)
GUARD_TEMPORARILY_UNAVAILABLE = (
    "ChaosEngine guard temporarily unavailable while install rewrites the harness; "
    "retry shortly; do not repair from this child"
)
PYTHON_UNAVAILABLE = "Python 3.11+ required for ChaosEngine hooks"

_CANDS = (
    ".chaos-engine/hooks/guard.py",
    "plugins/chaos-engine/hooks/guard.py",
    "chaos-engine/hooks/guard.py",
)
_E_CODES = {
    errno.ENOENT,
    getattr(errno, "ESTALE", 116),
    getattr(errno, "ENOTCONN", 107),
}


def deny(message: str) -> None:
    print(json.dumps({"decision": "block", "reason": message}))
    raise SystemExit(2)


def locate(start: pathlib.Path):
    return next(
        (
            (root, root / rel)
            for root in (start, *start.parents)
            for rel in _CANDS
            if (root / rel).is_file()
        ),
        None,
    )


def usable_cwd() -> pathlib.Path:
    try:
        start = pathlib.Path.cwd()
    except OSError as error:
        if error.errno in _E_CODES:
            deny(CWD_UNAVAILABLE)
        raise
    try:
        missing = not start.exists()
    except OSError as error:
        if error.errno in _E_CODES:
            deny(CWD_UNAVAILABLE)
        raise
    if missing:
        deny(CWD_UNAVAILABLE)
    return start


def read_payload(event: str | None) -> str | None:
    data = None
    src = os.environ.pop(HOOK_PAYLOAD_ENV, "")
    if src:
        try:
            data = pathlib.Path(src).read_bytes().decode("utf-8", "replace").lstrip(chr(65279))
        except OSError:
            data = None  # payload path unreadable; fall through to stdin
    if event is not None and data is None:
        try:
            data = sys.stdin.buffer.read().decode("utf-8", "replace").lstrip(chr(65279))
        except OSError:
            data = ""  # stdin unavailable; treat as empty payload
    return data


def find_guard(data: str | None):
    try:
        found = locate(usable_cwd())
    except OSError as error:
        if error.errno in _E_CODES:
            deny(CWD_UNAVAILABLE)
        raise
    if found is not None:
        return found, data
    hints = [os.environ.get("CLAUDE_PROJECT_DIR", "")]
    try:
        if data is None:
            data = sys.stdin.buffer.read().decode("utf-8", "replace").lstrip(chr(65279))
        hints.append(str(json.loads(data or "{}").get("cwd") or ""))
    except (OSError, ValueError, AttributeError):
        pass  # bad/missing payload cwd hint; keep walking CLAUDE_PROJECT_DIR only
    for hint in filter(None, hints):
        try:
            found = locate(pathlib.Path(hint))
        except OSError:
            found = None  # unreadable hint path; try the next hint
        if found is not None:
            return found, data
    return None, data


def installing(start: pathlib.Path) -> bool:
    for root in (start, *start.parents):
        try:
            if (root / ".chaos-engine.transaction.json").is_file():
                return True
            if (root / ".chaos-engine.backup.next").exists():
                return True
            if next(root.glob(".chaos-engine-stage-*"), None) is not None:
                return True
        except OSError:
            pass  # transient FS errors while probing install markers
    return False


def wait_budget_s() -> float:
    raw = os.environ.get("CHAOS_ENGINE_GUARD_WAIT_MS", "60000")
    try:
        value = float(raw)
    except (TypeError, ValueError):
        value = 60000.0
    if value != value:
        value = 60000.0
    return max(0.0, min(value, 600000.0)) / 1000.0


def wait_for_guard(data: str | None):
    start = usable_cwd()
    if not installing(start):
        return None, data
    deadline = time.monotonic() + wait_budget_s()
    found = None
    while found is None and time.monotonic() < deadline:
        time.sleep(0.2)
        try:
            found = locate(usable_cwd())
        except OSError as error:
            if error.errno in _E_CODES:
                deny(CWD_UNAVAILABLE)
            raise
        if found is None:
            for hint in filter(
                None,
                [os.environ.get("CLAUDE_PROJECT_DIR", ""), os.environ.get("GEMINI_PROJECT_DIR", "")],
            ):
                try:
                    found = locate(pathlib.Path(hint))
                except OSError:
                    found = None  # unreadable env hint during wait
                if found is not None:
                    break
    return found, data


def apply_matchers(path: pathlib.Path, data: str | None, event: str | None) -> str | None:
    try:
        ev = json.loads(data or "{}")
    except ValueError:
        return data
    if not isinstance(ev, dict):
        return data
    if event is not None and not ev.get("hook_event_name") and not ev.get("hookEventName"):
        ev["hook_event_name"] = event
        data = json.dumps(ev)
    name = str(ev.get("hook_event_name") or ev.get("hookEventName") or "")
    tool = str(ev.get("tool_name") or ev.get("toolName") or "")
    key = (
        "preventive"
        if name in ("PreToolUse", "preToolUse", "BeforeTool")
        else "observational"
        if name
        in (
            "PostToolUse",
            "postToolUse",
            "PostToolUseFailure",
            "postToolUseFailure",
            "AfterTool",
        )
        else ""
    )
    try:
        pol = (
            json.loads((path.parent / "matchers.json").read_text(encoding="utf-8"))
            if key
            else None
        )
    except (OSError, ValueError):
        pol = None  # matchers.json missing or invalid; do not filter tools
    if pol and not re.fullmatch("(?:" + "|".join(pol[key]) + ")", tool, re.I):
        print("{}")
        raise SystemExit(0)
    return data


def run_guard(base: pathlib.Path, path: pathlib.Path, data: str | None) -> None:
    ptr = base / HOOK_PYTHON_POINTER
    py = (
        ptr.read_text(encoding="utf-8").strip()
        if os.name != "nt" and ptr.is_file()
        else ""
    )
    if py and os.path.isfile(py) and os.path.realpath(py) != os.path.realpath(sys.executable):
        if data is not None:
            tmp = tempfile.TemporaryFile()
            tmp.write(data.encode("utf-8"))
            tmp.flush()
            tmp.seek(0)
            os.dup2(tmp.fileno(), 0)
        os.execv(py, [py, str(path)])  # nosec B606 - argv is fixed managed python + guard path
    if data is not None:
        sys.stdin = io.StringIO(data)
    try:
        runpy.run_path(str(path), run_name="__main__")
    except OSError as error:
        if error.errno in _E_CODES:
            deny(CWD_UNAVAILABLE)
        raise
def main(argv: list[str]) -> None:
    if sys.version_info < HOOK_PYTHON_FLOOR:
        deny(PYTHON_UNAVAILABLE)
    if len(argv) < 2 or not re.fullmatch(r"[a-z0-9-]+", argv[1] or ""):
        deny("ChaosEngine launch.py requires host name (e.g. claude)")
    host = argv[1]
    event = argv[2] if len(argv) > 2 else None
    if event is not None and not re.fullmatch(r"[A-Za-z]+", event):
        deny("ChaosEngine launch.py event must be alphanumeric")
    os.environ["CHAOS_ENGINE_HOST"] = host
    data = read_payload(event)
    found, data = find_guard(data)
    if found is None:
        found, data = wait_for_guard(data)
    if found is None:
        start = usable_cwd()
        deny(GUARD_TEMPORARILY_UNAVAILABLE if installing(start) else GUARD_UNAVAILABLE)
    base, path = found
    data = apply_matchers(path, data, event)
    run_guard(base, path, data)


if __name__ == "__main__":
    main(sys.argv)
