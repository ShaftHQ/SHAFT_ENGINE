#!/usr/bin/env python3
"""Durable jobs: long work that outlives the agent session (#6524).

Hosts kill an agent session at a hard limit (about 50 minutes has been seen).
A durable job runs detached from the session under a supervisor. The supervisor
keeps a lease (PID, process group, command, heartbeat) and a step checkpoint
log, so any later session or watchdog can tell live from stale without a doc
mtime or ``pgrep``. A live lease means nobody starts a second worker.

    tool.py job start NAME [--heartbeat S] [--stale S] [--part-dir D]... [--max-runs N] -- CMD...
    tool.py job status NAME [--json]   exit 0 live/done, 3 stale, 4 failed, 5 stopped, 6 absent
    tool.py job resume NAME            no-op while live or done; restarts a stale/failed run
    tool.py job stop NAME
    tool.py job checkpoint NAME STEP [--note TEXT] [--check]

State lives in ``$CHAOS_ENGINE_JOBS_DIR`` or ``./.chaos-engine-state/jobs/NAME``.
Stdlib only; the same on every host.
"""

from __future__ import annotations

import argparse
import contextlib
import json
import os
import re
import signal
import socket
import subprocess  # nosec B404 - runs the owner's own job command, no shell.
import sys
import time
import uuid
from pathlib import Path
from typing import Iterator

SCHEMA_VERSION = 1
NAME_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$")
DEFAULT_HEARTBEAT = 30.0
STALE_FACTOR = 4
DEFAULT_MAX_RUNS = 3
START_LOCK_STALE = 30.0
RUN_ENV = "CHAOS_ENGINE_JOB_RUN"
NAME_ENV = "CHAOS_ENGINE_JOB"
ROOT_ENV = "CHAOS_ENGINE_JOBS_DIR"
PART_SUFFIX = ".part"
EXIT = {"live": 0, "done": 0, "stale": 3, "failed": 4, "stopped": 5, "absent": 6}
REFUSED = 3


# ---------------------------------------------------------------- state files

def jobs_root(root: str | os.PathLike | None = None) -> Path:
    if root:
        return Path(root)
    if os.environ.get(ROOT_ENV):
        return Path(os.environ[ROOT_ENV])
    return Path.cwd() / ".chaos-engine-state" / "jobs"


def job_dir(root: Path, name: str) -> Path:
    if not NAME_RE.match(name):
        raise ValueError(f"invalid job name {name!r}: use letters, digits, '.', '_' or '-'")
    return root / name


def write_json(path: Path, payload: dict) -> None:
    """Atomic JSON write: a reader sees the old or the new lease, never half."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with atomic_output(path) as part:
        part.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def read_lease(directory: Path) -> dict | None:
    try:
        payload = json.loads((directory / "lease.json").read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    return payload if isinstance(payload, dict) else None


def update_lease(directory: Path, **changes) -> dict:
    lease = read_lease(directory) or {}
    lease.update(changes)
    write_json(directory / "lease.json", lease)
    return lease


@contextlib.contextmanager
def atomic_output(path: str | os.PathLike) -> Iterator[Path]:
    """Yield ``<path>.part``; on success flush it and rename it over ``path``.

    A kill or an exception leaves the previous file (or nothing), never a
    partial output. Resume deletes leftover ``.part`` files (#6526).
    """
    target = Path(path)
    part = target.with_name(target.name + PART_SUFFIX)
    try:
        yield part
        if part.exists():
            with open(part, "rb+") as handle:
                os.fsync(handle.fileno())
            os.replace(part, target)
    finally:
        with contextlib.suppress(FileNotFoundError):
            part.unlink()


@contextlib.contextmanager
def start_lock(directory: Path, wait: float = 10.0) -> Iterator[None]:
    """Serialize start/resume/stop of one job (two watchdogs firing together)."""
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / "start.lock"
    deadline = time.monotonic() + wait
    while True:
        try:
            handle = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
            os.write(handle, str(os.getpid()).encode())
            os.close(handle)
            break
        except FileExistsError:
            with contextlib.suppress(OSError):
                if time.time() - path.stat().st_mtime > START_LOCK_STALE:
                    path.unlink()
                    continue
            if time.monotonic() > deadline:
                raise RuntimeError(f"another start/stop of this job holds {path}")
            time.sleep(0.1)
    try:
        yield
    finally:
        with contextlib.suppress(FileNotFoundError):
            path.unlink()


# ------------------------------------------------------------------ processes

def _proc_marker(pid: int) -> str | None:
    """The job run id in a Linux process environment, or None when unreadable."""
    try:
        environ = Path(f"/proc/{pid}/environ").read_bytes()
    except OSError:
        return None
    for item in environ.split(b"\0"):
        if item.startswith(RUN_ENV.encode() + b"="):
            return item.split(b"=", 1)[1].decode(errors="replace")
    return ""


def _zombie(pid: int) -> bool:
    try:
        stat = Path(f"/proc/{pid}/stat").read_text(encoding="utf-8", errors="replace")
    except OSError:
        return False
    return stat.rsplit(")", 1)[-1].split()[:1] == ["Z"]


def pid_alive(pid: object, run_id: str | None = None) -> bool:
    """True while ``pid`` runs; on Linux it must also carry ``run_id`` (PID reuse)."""
    if not isinstance(pid, int) or pid <= 0:
        return False
    if os.name == "nt":
        return _windows_pid_alive(pid)
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    if _zombie(pid):
        return False
    if run_id:
        marker = _proc_marker(pid)
        if marker is not None and marker != run_id:
            return False
    return True


def _windows_pid_alive(pid: int) -> bool:  # pragma: no cover - Windows only
    import ctypes

    kernel = ctypes.windll.kernel32
    handle = kernel.OpenProcess(0x1000, False, pid)  # PROCESS_QUERY_LIMITED_INFORMATION
    if not handle:
        return False
    code = ctypes.c_ulong()
    try:
        kernel.GetExitCodeProcess(handle, ctypes.byref(code))
    finally:
        kernel.CloseHandle(handle)
    return code.value == 259  # STILL_ACTIVE


def run_members(run_id: str) -> list[int]:
    """Every live process of one run, found by its inherited run id (Linux).

    This catches children of dead parents, even ones that left the job's
    process group, without ever matching an unrelated process by name.
    """
    members = []
    proc = Path("/proc")
    if not run_id or not proc.is_dir():
        return members
    for entry in proc.iterdir():
        if entry.name.isdigit() and int(entry.name) != os.getpid():
            if _proc_marker(int(entry.name)) == run_id and not _zombie(int(entry.name)):
                members.append(int(entry.name))
    return members


def _signal(pid: int, sig: int) -> None:
    with contextlib.suppress(ProcessLookupError, PermissionError):
        os.kill(pid, sig)


def kill_run(lease: dict, grace: float = 5.0) -> list[int]:
    """Terminate every surviving process of the lease's run. Returns the PIDs hit."""
    run_id = str(lease.get("run_id") or "")
    if os.name == "nt":  # pragma: no cover - Windows only
        hit = [pid for pid in (lease.get("child_pid"), lease.get("pid")) if pid_alive(pid)]
        taskkill = os.path.join(os.environ.get("SystemRoot", r"C:\Windows"), "System32", "taskkill.exe")
        for pid in hit:
            subprocess.run([taskkill, "/PID", str(pid), "/T", "/F"], capture_output=True, check=False)  # nosec B603
        return hit
    group = lease.get("pgid")
    targets = set(run_members(run_id))
    group_alive = isinstance(group, int) and group > 1 and any(
        pid_alive(pid, run_id) for pid in (group, lease.get("child_pid"))
    )
    if not targets and not group_alive:
        return []
    if group_alive:
        with contextlib.suppress(ProcessLookupError, PermissionError):
            os.killpg(group, signal.SIGTERM)
    for pid in targets:
        _signal(pid, signal.SIGTERM)
    deadline = time.monotonic() + grace
    while time.monotonic() < deadline:
        if not [pid for pid in targets if pid_alive(pid)] and not (
            group_alive and pid_alive(group, run_id)
        ):
            break
        time.sleep(0.1)
    for pid in targets:
        if pid_alive(pid):
            _signal(pid, signal.SIGKILL)
    if group_alive:
        with contextlib.suppress(ProcessLookupError, PermissionError):
            os.killpg(group, signal.SIGKILL)
    return sorted(targets | ({group} if group_alive else set()))


def clean_parts(part_dirs: list[str], cwd: str | None) -> list[str]:
    """Delete ``*.part`` orphans under the job's declared part dirs (worker is dead)."""
    removed = []
    for raw in part_dirs:
        base = Path(raw) if os.path.isabs(raw) else Path(cwd or ".") / raw
        if not base.is_dir():
            continue
        for part in sorted(base.rglob("*" + PART_SUFFIX)):
            if part.is_file() or part.is_symlink():
                with contextlib.suppress(FileNotFoundError):
                    part.unlink()
                    removed.append(str(part))
    return removed


# ------------------------------------------------------------------- the lease

def classify(lease: dict | None, now: float | None = None) -> str:
    """live | stale | done | failed | stopped | absent. Never reads a doc mtime."""
    if not lease:
        return "absent"
    state = lease.get("state")
    if state in {"done", "failed", "stopped"}:
        return str(state)
    now = time.time() if now is None else now
    heartbeat = float(lease.get("heartbeat_at") or 0)
    stale_after = float(lease.get("stale_s") or DEFAULT_HEARTBEAT * STALE_FACTOR)
    if now - heartbeat > stale_after:
        return "stale"
    pid = lease.get("pid")
    if pid is None:  # supervisor still starting; the fresh heartbeat covers it
        return "live"
    if lease.get("host") not in (None, socket.gethostname()):
        return "live"  # another machine's worker: only its heartbeat can tell
    return "live" if pid_alive(pid, lease.get("run_id")) else "stale"


def last_checkpoint(directory: Path) -> dict | None:
    try:
        lines = (directory / "checkpoint.jsonl").read_text(encoding="utf-8").splitlines()
    except OSError:
        return None
    for line in reversed(lines):
        with contextlib.suppress(ValueError):
            item = json.loads(line)
            if isinstance(item, dict):
                return item
    return None


def describe(name: str, directory: Path, now: float | None = None) -> dict:
    lease = read_lease(directory)
    now = time.time() if now is None else now
    state = classify(lease, now)
    report: dict = {"job": name, "state": state, "dir": str(directory)}
    if lease:
        report.update({
            key: lease.get(key)
            for key in ("pid", "child_pid", "command", "cwd", "exit_code", "runs", "run_id", "log")
        })
        if lease.get("heartbeat_at"):
            report["heartbeat_age_s"] = round(now - float(lease["heartbeat_at"]), 1)
        if lease.get("started_at"):
            report["running_s"] = round(float(lease.get("ended_at") or now) - float(lease["started_at"]), 1)
    checkpoint = last_checkpoint(directory)
    if checkpoint:
        report["last_checkpoint"] = checkpoint
    return report


def status_line(report: dict) -> str:
    parts = [f"job {report['job']}: {report['state']}"]
    if report.get("pid"):
        parts.append(f"pid {report['pid']}")
    if "heartbeat_age_s" in report and report["state"] in {"live", "stale"}:
        parts.append(f"heartbeat {report['heartbeat_age_s']:.0f}s ago")
    if report.get("exit_code") is not None and report["state"] != "live":
        parts.append(f"exit {report['exit_code']}")
    checkpoint = report.get("last_checkpoint")
    if checkpoint:
        parts.append(f"last step {checkpoint.get('step')!r}")
    return ", ".join(parts)


# ------------------------------------------------------------------ operations

def _spawn_supervisor(root: Path, name: str, run_id: str, cwd: str, log: Path) -> subprocess.Popen:
    environment = dict(os.environ, **{RUN_ENV: run_id, NAME_ENV: name, ROOT_ENV: str(root)})
    options: dict = {}
    if os.name == "nt":  # pragma: no cover - Windows only
        options["creationflags"] = 0x00000008 | 0x00000200  # DETACHED_PROCESS | NEW_PROCESS_GROUP
    else:
        options["start_new_session"] = True  # setsid: survives the session's death
    with open(log, "ab") as output:
        return subprocess.Popen(  # nosec B603 - this script, fixed argv.
            [sys.executable, str(Path(__file__).resolve()), "--root", str(root), "_supervise", name],
            cwd=cwd, env=environment, stdin=subprocess.DEVNULL, stdout=output, stderr=output, **options,
        )


def take_over(directory: Path, lease: dict | None, part_dirs: list[str]) -> dict:
    """Clear a dead run: kill its survivors, then delete its .part orphans."""
    report = {"killed": [], "removed_parts": []}
    if lease:
        report["killed"] = kill_run(lease)
        dirs = list(dict.fromkeys([*(lease.get("part_dirs") or []), *part_dirs]))
        report["removed_parts"] = clean_parts(dirs, lease.get("cwd"))
    else:
        report["removed_parts"] = clean_parts(part_dirs, None)
    return report


def launch(root: Path, name: str, command: list[str], *, cwd: str, heartbeat: float, stale: float | None,
           part_dirs: list[str], max_runs: int, runs: int) -> dict:
    directory = job_dir(root, name)
    run_id = uuid.uuid4().hex
    log = directory / "output.log"
    now = time.time()
    lease = {
        "schema": SCHEMA_VERSION, "job": name, "run_id": run_id, "state": "running",
        "command": command, "cwd": cwd, "part_dirs": part_dirs, "host": socket.gethostname(),
        "heartbeat_s": heartbeat, "stale_s": stale or heartbeat * STALE_FACTOR,
        "max_runs": max_runs, "runs": runs, "started_at": now, "heartbeat_at": now,
        "pid": None, "child_pid": None, "pgid": None, "exit_code": None, "ended_at": None, "log": str(log),
    }
    write_json(directory / "lease.json", lease)
    supervisor = _spawn_supervisor(root, name, run_id, cwd, log)
    deadline = time.monotonic() + 10
    while time.monotonic() < deadline:
        current = read_lease(directory) or {}
        if current.get("run_id") != run_id or current.get("child_pid") or supervisor.poll() is not None:
            break
        time.sleep(0.05)
    current = read_lease(directory) or lease
    if current.get("run_id") == run_id and current.get("state") == "running" and not current.get("pid") \
            and supervisor.poll() is not None:
        current = update_lease(directory, state="failed", exit_code=supervisor.returncode, ended_at=time.time())
    return current


def start(root: Path, name: str, command: list[str], *, cwd: str | None = None,
          heartbeat: float = DEFAULT_HEARTBEAT, stale: float | None = None,
          part_dirs: list[str] | None = None, max_runs: int = DEFAULT_MAX_RUNS) -> tuple[int, str]:
    if not command:
        return 2, "job start: give the command after --"
    directory = job_dir(root, name)
    with start_lock(directory):
        lease = read_lease(directory)
        state = classify(lease)
        if state == "live":
            return REFUSED, f"refused: {status_line(describe(name, directory))} (one worker per job)"
        cleanup = take_over(directory, lease, list(part_dirs or []))
        lease = launch(root, name, list(command), cwd=str(Path(cwd or os.getcwd()).resolve()),
                       heartbeat=heartbeat, stale=stale, part_dirs=list(part_dirs or []),
                       max_runs=max_runs, runs=1)
    return 0, _started_text(name, lease, state, cleanup)


def resume(root: Path, name: str, *, force: bool = False) -> tuple[int, str]:
    """Watchdog entry point: never starts a second worker."""
    directory = job_dir(root, name)
    with start_lock(directory):
        lease = read_lease(directory)
        state = classify(lease)
        report = describe(name, directory)
        if state in {"live", "done"} and not (force and state == "done"):
            return 0, f"leave alone: {status_line(report)}"
        if state == "absent":
            return EXIT["absent"], f"job {name}: absent; start it with `job start {name} -- CMD`"
        if state == "stopped" and not force:
            return EXIT["stopped"], f"leave alone: {status_line(report)} (stopped by an owner; --force restarts)"
        runs = int(lease.get("runs") or 1)
        max_runs = int(lease.get("max_runs") or DEFAULT_MAX_RUNS)
        if runs >= max_runs and not force:
            return EXIT["failed"], f"needs a human: {status_line(report)} after {runs} runs (--force restarts)"
        cleanup = take_over(directory, lease, [])
        lease = launch(root, name, list(lease["command"]), cwd=str(lease.get("cwd") or os.getcwd()),
                       heartbeat=float(lease.get("heartbeat_s") or DEFAULT_HEARTBEAT),
                       stale=float(lease.get("stale_s") or 0) or None,
                       part_dirs=list(lease.get("part_dirs") or []), max_runs=max_runs, runs=runs + 1)
    return 0, _started_text(name, lease, state, cleanup)


def _started_text(name: str, lease: dict, previous: str, cleanup: dict) -> str:
    text = f"job {name}: started pid {lease.get('pid')} (run {lease.get('runs')}, log {lease.get('log')})"
    if previous not in {"absent"}:
        text += f"; took over {previous} run"
    if cleanup.get("killed"):
        text += f"; killed survivors {cleanup['killed']}"
    if cleanup.get("removed_parts"):
        text += f"; removed {len(cleanup['removed_parts'])} .part orphan(s)"
    return text


def stop(root: Path, name: str) -> tuple[int, str]:
    directory = job_dir(root, name)
    with start_lock(directory):
        lease = read_lease(directory)
        if not lease:
            return EXIT["absent"], f"job {name}: absent"
        if classify(lease) in {"done", "failed", "stopped"} and not run_members(str(lease.get("run_id") or "")):
            return 0, status_line(describe(name, directory))
        update_lease(directory, state="stopped", ended_at=time.time())
        killed = kill_run(lease)
        clean_parts(list(lease.get("part_dirs") or []), lease.get("cwd"))
        # A heartbeat racing the first write must not leave "running" behind,
        # or a watchdog would restart a job its owner stopped.
        if (read_lease(directory) or {}).get("run_id") == lease.get("run_id"):
            update_lease(directory, state="stopped")
    return 0, f"job {name}: stopped (killed {killed})"


def checkpoint(root: Path, name: str, step: str, note: str = "", check: bool = False) -> tuple[int, str]:
    """Append a finished step, or with ``check`` exit 0 only if it is recorded."""
    directory = job_dir(root, name)
    path = directory / "checkpoint.jsonl"
    if check:
        try:
            lines = path.read_text(encoding="utf-8").splitlines()
        except OSError:
            lines = []
        for line in lines:
            with contextlib.suppress(ValueError):
                if json.loads(line).get("step") == step:
                    return 0, ""
        return 1, ""
    directory.mkdir(parents=True, exist_ok=True)
    entry = {"at": time.time(), "step": step, "note": note, "run_id": os.environ.get(RUN_ENV, "")}
    with open(path, "a", encoding="utf-8") as handle:
        handle.write(json.dumps(entry, sort_keys=True) + "\n")
        handle.flush()
        os.fsync(handle.fileno())
    return 0, ""


def supervise(root: Path, name: str) -> int:
    """Detached supervisor: run the command, heartbeat the lease, record the end."""
    directory = job_dir(root, name)
    lease = read_lease(directory) or {}
    run_id = os.environ.get(RUN_ENV, "")
    if lease.get("run_id") != run_id:
        return 1
    options: dict = {}
    if os.name != "nt":
        options["start_new_session"] = True  # own group: stop and takeover kill it whole
    try:
        child = subprocess.Popen(  # nosec B603 - the owner's recorded job argv, no shell.
            list(lease["command"]), cwd=lease.get("cwd") or None, stdin=subprocess.DEVNULL, **options
        )
    except OSError as error:
        print(f"job {name}: cannot start command: {error}", file=sys.stderr)
        update_lease(directory, pid=os.getpid(), state="failed", exit_code=127, ended_at=time.time())
        return 1
    stopping = {"flag": False}

    def on_term(_signum, _frame):
        stopping["flag"] = True
        with contextlib.suppress(ProcessLookupError, PermissionError, OSError):
            if os.name == "nt":  # pragma: no cover
                child.terminate()
            else:
                os.killpg(child.pid, signal.SIGTERM)

    signal.signal(signal.SIGTERM, on_term)
    update_lease(directory, pid=os.getpid(), child_pid=child.pid, pgid=child.pid, heartbeat_at=time.time())
    heartbeat = float(lease.get("heartbeat_s") or DEFAULT_HEARTBEAT)
    while True:
        try:
            code = child.wait(timeout=heartbeat)
            break
        except subprocess.TimeoutExpired:
            current = read_lease(directory) or {}
            if current.get("run_id") != run_id or current.get("state") != "running":
                on_term(None, None)
                continue
            update_lease(directory, heartbeat_at=time.time())
    current = read_lease(directory) or {}
    if current.get("run_id") == run_id:
        state = "stopped" if stopping["flag"] or current.get("state") == "stopped" else (
            "done" if code == 0 else "failed")
        update_lease(directory, state=state, exit_code=code, ended_at=time.time(), heartbeat_at=time.time())
    return 0


# ------------------------------------------------------------------------- CLI

def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="tool.py job", description="Durable jobs that outlive the agent session.")
    parser.add_argument("--root", help=f"state dir (default ${ROOT_ENV} or ./.chaos-engine-state/jobs)")
    commands = parser.add_subparsers(dest="action", required=True)
    begin = commands.add_parser("start", help="start NAME detached; refused while its lease is live")
    begin.add_argument("name")
    begin.add_argument("--heartbeat", type=float, default=DEFAULT_HEARTBEAT, help="seconds between heartbeats")
    begin.add_argument("--stale", type=float, help=f"heartbeat age that makes the lease stale (default {STALE_FACTOR}x)")
    begin.add_argument("--part-dir", action="append", default=[], help="dir whose *.part files a takeover deletes")
    begin.add_argument("--max-runs", type=int, default=DEFAULT_MAX_RUNS, help="resume gives up after this many runs")
    begin.add_argument("--cwd", help="working directory of the command")
    for action, text in (("status", "live|stale|done|failed|stopped|absent"),
                         ("resume", "no-op while live or done; restart a stale or failed run once"),
                         ("stop", "terminate the whole run and record stopped")):
        sub = commands.add_parser(action, help=text)
        sub.add_argument("name")
        if action == "status":
            sub.add_argument("--json", action="store_true")
        if action == "resume":
            sub.add_argument("--force", action="store_true")
    mark = commands.add_parser("checkpoint", help="record a finished step (or --check one)")
    mark.add_argument("name")
    mark.add_argument("step")
    mark.add_argument("--note", default="")
    mark.add_argument("--check", action="store_true", help="exit 0 only if STEP is recorded")
    hidden = commands.add_parser("_supervise")
    hidden.add_argument("name")
    return parser


def main(argv: list[str] | None = None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    command: list[str] = []
    if "--" in argv:  # everything after -- is the job command, never job options
        split = argv.index("--")
        argv, command = argv[:split], argv[split + 1:]
    arguments = build_parser().parse_args(argv)
    root = jobs_root(arguments.root)
    try:
        if arguments.action == "_supervise":
            return supervise(root, arguments.name)
        if arguments.action == "status":
            report = describe(arguments.name, job_dir(root, arguments.name))
            print(json.dumps(report, sort_keys=True) if arguments.json else status_line(report))
            return EXIT[report["state"]]
        if arguments.action == "start":
            code, text = start(root, arguments.name, command, cwd=arguments.cwd, heartbeat=arguments.heartbeat,
                               stale=arguments.stale, part_dirs=arguments.part_dir, max_runs=arguments.max_runs)
        elif arguments.action == "resume":
            code, text = resume(root, arguments.name, force=arguments.force)
        elif arguments.action == "stop":
            code, text = stop(root, arguments.name)
        else:
            code, text = checkpoint(root, arguments.name, arguments.step, arguments.note, arguments.check)
    except (OSError, RuntimeError, ValueError) as error:
        print(f"job: {error}", file=sys.stderr)
        return 1
    if text:
        print(text, file=sys.stderr if code not in (0,) and arguments.action != "resume" else sys.stdout)
    return code


if __name__ == "__main__":
    raise SystemExit(main())
