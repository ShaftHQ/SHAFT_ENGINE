"""Optional watcher for a backups parking directory.

Prefer ``inotifywait`` when installed; otherwise poll mtime/inode snapshots.
Logs JSON lines to ``~/.local/state/chaos-engine/backups-watch.log``.
Doctor documents how to start it; tests never auto-start a daemon.

Set ``CHAOS_BACKUPS_WATCH_PATH`` (or pass ``--path``) to the backups root.
There is no machine-specific default in the portable core.

Usage:
  python3 .chaos-engine/tool.py backups-watch --path DIR [--poll-seconds N]
  python3 .chaos-engine/backups_watch.py --self-test
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess  # nosec B404 - fixed argv for inotifywait/self.
import sys
import time
from pathlib import Path

DEFAULT_PATH = ""  # require --path or CHAOS_BACKUPS_WATCH_PATH (portable core)
LOG_NAME = "backups-watch.log"
AGENT_ENV_KEYS = (
    "CURSOR_AGENT",
    "CURSOR_CONVERSATION_ID",
    "CURSOR_REQUEST_ID",
    "CHAOS_AGENT_ID",
    "AGENT_ID",
)


def state_dir() -> Path:
    return Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local/state") / "chaos-engine"


def log_path() -> Path:
    return state_dir() / LOG_NAME


def _cmdline(pid: int) -> str:
    try:
        raw = Path(f"/proc/{pid}/cmdline").read_bytes()
    except OSError:
        return ""
    return raw.replace(b"\x00", b" ").decode("utf-8", "replace").strip()


def _agent_env() -> dict[str, str]:
    return {key: os.environ[key] for key in AGENT_ENV_KEYS if os.environ.get(key)}


def _context() -> dict[str, object]:
    pid = os.getpid()
    ppid = os.getppid()
    return {
        "pid": pid,
        "ppid": ppid,
        "cmdline": _cmdline(pid) or " ".join(sys.argv),
        "parent_cmdline": _cmdline(ppid),
        "agent_env": _agent_env(),
    }


def append_event(event: str, path: str, **extra: object) -> None:
    """Append one JSON log line (create the state dir as needed)."""
    destination = log_path()
    destination.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
        "epoch": int(time.time()),
        "event": event,
        "path": path,
        **_context(),
        **extra,
    }
    with destination.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(payload, sort_keys=True) + "\n")


def doctor_row() -> tuple[str, str]:
    """Advisory doctor tip: never fails install; documents how to start the watcher."""
    script = Path(__file__).resolve()
    if not script.is_file():
        return "warn", "backups_watch.py missing"
    inotify = "inotifywait" if shutil.which("inotifywait") else "poll-fallback"
    return (
        "pass",
        f"optional ({inotify}); start: tool.py backups-watch --path <backups-root> (or CHAOS_BACKUPS_WATCH_PATH)",
    )


def _snapshot(root: Path) -> dict[str, tuple[int, int, bool]]:
    """path -> (ino, mtime_ns, is_dir) for one level + recursive files."""
    out: dict[str, tuple[int, int, bool]] = {}
    if not root.is_dir():
        return out
    for current, dirs, files in os.walk(root):
        base = Path(current)
        for name in dirs + files:
            path = base / name
            try:
                st = path.lstat()
            except OSError:
                continue
            out[str(path)] = (st.st_ino, getattr(st, "st_mtime_ns", int(st.st_mtime * 1e9)), path.is_dir())
    return out


def _snapshot_store(root: Path) -> Path:
    digest = hashlib.sha256(str(root.resolve()).encode("utf-8")).hexdigest()[:16]
    return state_dir() / f"backups-watch-snap-{digest}.json"


def _load_snapshot(root: Path) -> dict[str, tuple[int, int, bool]]:
    store = _snapshot_store(root)
    if not store.is_file():
        return {}
    try:
        raw = json.loads(store.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {}
    out: dict[str, tuple[int, int, bool]] = {}
    for key, value in raw.items():
        if isinstance(value, list) and len(value) == 3:
            out[key] = (int(value[0]), int(value[1]), bool(value[2]))
    return out


def _save_snapshot(root: Path, snap: dict[str, tuple[int, int, bool]]) -> None:
    store = _snapshot_store(root)
    store.parent.mkdir(parents=True, exist_ok=True)
    store.write_text(json.dumps({key: list(value) for key, value in snap.items()}, sort_keys=True), encoding="utf-8")


def _diff_events(before: dict[str, tuple[int, int, bool]], after: dict[str, tuple[int, int, bool]]) -> list[tuple[str, str]]:
    events: list[tuple[str, str]] = []
    gone = set(before) - set(after)
    appeared = set(after) - set(before)
    # Heuristic moves: same inode left one path and appeared at another.
    by_ino_gone = {before[p][0]: p for p in gone}
    for path in list(appeared):
        ino = after[path][0]
        if ino in by_ino_gone:
            src = by_ino_gone.pop(ino)
            events.append(("MOVED", f"{src} -> {path}"))
            gone.discard(src)
            appeared.discard(path)
    for path in sorted(gone):
        events.append(("DELETE", path))
    return events


def poll_loop(root: Path, seconds: float, once: bool = False) -> int:
    previous = _load_snapshot(root) or _snapshot(root)
    if once and not _snapshot_store(root).is_file():
        # First --once seeds the baseline without sleeping, so a later --once can diff.
        _save_snapshot(root, previous)
        return 0
    while True:
        time.sleep(max(0.2, seconds))
        current = _snapshot(root)
        for event, path in _diff_events(previous, current):
            append_event(event, path, backend="poll")
        _save_snapshot(root, current)
        previous = current
        if once:
            return 0


def inotify_loop(root: Path) -> int:
    command = [
        "inotifywait",
        "-m",
        "-r",
        "-e",
        "delete,delete_self,moved_from,moved_to",
        "--format",
        "%e|%w|%f",
        str(root),
    ]
    process = subprocess.Popen(  # nosec B603 - fixed inotifywait argv, path from caller.
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    if process.stdout is None:
        print("backups-watch: inotifywait produced no stdout pipe", file=sys.stderr)
        process.terminate()
        return 1
    try:
        for line in process.stdout:
            line = line.strip()
            if not line or "|" not in line:
                continue
            kind, watch, name = line.split("|", 2)
            target = str(Path(watch) / name) if name else watch
            upper = kind.upper()
            if "MOVED_FROM" in upper or "MOVED_TO" in upper:
                event = "MOVED"
            elif "DELETE" in upper:
                event = "DELETE"
            else:
                event = upper
            append_event(event, target, backend="inotifywait", inotify_events=kind)
    finally:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
    return process.returncode or 0


def self_test() -> int:
    """No daemon: write a synthetic event and prove the log line shape."""
    append_event("SELF_TEST", "backups/.chaos-engine-self-test", backend="self-test")
    last = log_path().read_text(encoding="utf-8").splitlines()[-1]
    payload = json.loads(last)
    required = {"at", "epoch", "event", "path", "pid", "cmdline", "agent_env"}
    missing = required - set(payload)
    if missing:
        print(f"backups-watch self-test missing keys: {sorted(missing)}", file=sys.stderr)
        return 1
    if payload["event"] != "SELF_TEST":
        print("backups-watch self-test bad event", file=sys.stderr)
        return 1
    print(f"backups-watch self-test ok log={log_path()}")
    return 0


def main(arguments: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--path", default=os.environ.get("CHAOS_BACKUPS_WATCH_PATH") or DEFAULT_PATH)
    parser.add_argument("--poll-seconds", type=float, default=2.0)
    parser.add_argument("--force-poll", action="store_true")
    parser.add_argument("--once", action="store_true", help="poll one cycle then exit (tests)")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(arguments)
    if args.self_test:
        return self_test()
    if not args.path:
        print("backups-watch: set --path or CHAOS_BACKUPS_WATCH_PATH to the backups root", file=sys.stderr)
        return 2
    root = Path(args.path).expanduser()
    if not root.exists():
        root.mkdir(parents=True, exist_ok=True)
    if not args.force_poll and shutil.which("inotifywait") and not args.once:
        append_event("WATCH_START", str(root), backend="inotifywait")
        return inotify_loop(root)
    append_event("WATCH_START", str(root), backend="poll")
    return poll_loop(root, args.poll_seconds, once=args.once)


if __name__ == "__main__":
    raise SystemExit(main())
