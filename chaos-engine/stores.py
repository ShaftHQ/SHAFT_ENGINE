#!/usr/bin/env python3
"""Shared MemPalace and Graphify locations for every ChaosEngine checkout.

One Git repository has one palace and one graphify-out. Linked worktrees and
other branches resolve those same paths. Refresh builds a detached snapshot of
the default-branch tip and does not reset a checkout. ``~/.mempalace`` is never
read or migrated.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess  # nosec B404 - fixed git and store CLIs, no shell.
import sys
import tempfile
from contextlib import contextmanager
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable, Iterator


PALACE_BACKEND = "sqlite_exact"
GENERIC_MARKER = ".chaos-engine-source-revision.json"
LEGACY_MARKER = ".sha" + "ft-source-revision.json"
LOCK_NAME = "stores.lock"
ATTEMPT_STAMP = "stores-refresh-attempted"
GRAPHIFY_GRAPH_COMMANDS = frozenset({
    "query",
    "path",
    "explain",
    "diagnose",
    "affected",
    "god-nodes",
    "cluster-only",
})
Runner = Callable[[list[str], Path], object]


def _git_executable() -> str:
    git = shutil.which("git")
    if git is None:
        raise RuntimeError("git is not on PATH")
    return git


def _git(cwd: Path, *args: str) -> str:
    completed = subprocess.run(  # nosec B603 - fixed git argv, no shell.
        [_git_executable(), *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout or "git failed").strip()
        raise RuntimeError(detail)
    return completed.stdout.strip()


def _override(names: tuple[str, ...]) -> Path | None:
    for name in names:
        if name not in os.environ:
            continue
        configured = os.environ[name].strip()
        if not configured:
            raise RuntimeError(f"{name} must not be blank")
        path = Path(configured).expanduser()
        if not path.is_absolute():
            raise RuntimeError(f"{name} must be absolute")
        return path.resolve()
    return None


def resolve_common_dir(cwd: Path) -> Path | None:
    """Return the shared git directory, or None when cwd is not a repository."""
    try:
        raw = _git(cwd, "rev-parse", "--git-common-dir")
    except RuntimeError:
        return None
    if not raw:
        return None
    common = Path(raw)
    if not common.is_absolute():
        common = (cwd / common).resolve()
    return common.resolve()


def resolve_primary_root(cwd: Path) -> Path:
    """Return the main worktree that owns shared store files."""
    common = resolve_common_dir(cwd)
    if common is None:
        return cwd.resolve()
    return common.parent.resolve()


def resolve_palace(cwd: Path) -> Path:
    """Return the one sqlite_exact palace for this repository."""
    configured = _override(("CHAOS_ENGINE_MEMPALACE", "SHA" + "FT_MEMPALACE"))
    if configured is not None:
        return configured
    common = resolve_common_dir(cwd)
    if common is not None:
        return common / "chaos-engine" / "mempalace"
    return cwd.resolve() / ".chaos-engine-state" / "mempalace"


def resolve_graph_out(cwd: Path) -> Path:
    """Return the one graphify-out directory for this repository."""
    configured = _override(("CHAOS_ENGINE_GRAPHIFY_OUT", "SHA" + "FT_GRAPHIFY_OUT"))
    if configured is not None:
        return configured
    common = resolve_common_dir(cwd)
    if common is not None:
        return common.parent / "graphify-out"
    return cwd.resolve() / "graphify-out"


def inject_mempalace_arguments(arguments: list[str], palace: Path) -> list[str]:
    """Place ``--palace`` and ``--backend`` before the MemPalace subcommand.

    A caller-supplied ``--palace`` keeps its path but still gets the pinned
    backend, so an ambient backend selection cannot pick another one (#6212).
    """
    if "--backend" in arguments:
        return list(arguments)
    if "--palace" in arguments:
        return ["--backend", PALACE_BACKEND, *arguments]
    if arguments and arguments[0].startswith("-"):
        return list(arguments)
    return [
        "--palace",
        str(palace),
        "--backend",
        PALACE_BACKEND,
        *arguments,
    ]


def inject_graphify_arguments(arguments: list[str], graph_json: Path) -> list[str]:
    """Point graph-reading Graphify subcommands at the shared graph.json."""
    if not arguments or arguments[0] not in GRAPHIFY_GRAPH_COMMANDS:
        return list(arguments)
    if "--graph" in arguments:
        return list(arguments)
    return [*arguments, "--graph", str(graph_json)]


DEFAULT_BRANCH_FALLBACKS = ("main", "master")


def default_branch_commit(cwd: Path) -> str:
    """Return the local default-branch tip. Does not fetch."""
    candidates: list[str] = []
    try:
        symbolic = _git(cwd, "symbolic-ref", "--quiet", "refs/remotes/origin/HEAD")
    except RuntimeError:
        symbolic = ""
    if symbolic:
        candidates.append(symbolic)
    # #6216: conventional names only as a fallback when origin/HEAD is unset.
    candidates.extend(f"refs/remotes/origin/{name}" for name in DEFAULT_BRANCH_FALLBACKS)
    seen: set[str] = set()
    for candidate in candidates:
        if candidate in seen:
            continue
        seen.add(candidate)
        try:
            sha = _git(cwd, "rev-parse", "--verify", "--quiet", candidate)
        except RuntimeError:
            continue
        if len(sha) == 40 and all(character in "0123456789abcdef" for character in sha):
            return sha
    raise RuntimeError(
        "default-branch commit is not available locally. fix-next: git fetch"
    )


def _marker_path(graph_out: Path) -> Path | None:
    generic = graph_out / GENERIC_MARKER
    if generic.is_file():
        return generic
    legacy = graph_out / LEGACY_MARKER
    if legacy.is_file():
        return legacy
    return None


def _read_marker(graph_out: Path) -> dict[str, object] | None:
    path = _marker_path(graph_out)
    if path is None:
        return None
    import json

    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    return payload if isinstance(payload, dict) else None


def _manifest_digest(graph_out: Path) -> str | None:
    manifest = graph_out / "manifest.json"
    if not manifest.is_file():
        return None
    return hashlib.sha256(manifest.read_bytes()).hexdigest()


def graph_freshness(cwd: Path) -> tuple[bool, str]:
    """Report whether the shared graph matches the local default-branch tip."""
    graph_out = resolve_graph_out(cwd)
    graph_json = graph_out / "graph.json"
    if not graph_json.is_file():
        return False, "absent - shared Graphify cache is not built"
    digest = _manifest_digest(graph_out)
    if digest is None:
        return False, "stale - shared Graphify cache has no manifest.json"
    marker = _read_marker(graph_out)
    if not isinstance(marker, dict):
        return False, "stale - shared Graphify cache has no indexed revision marker"
    indexed = marker.get("indexed_revision")
    expected = marker.get("manifest_sha256")
    if not isinstance(indexed, str) or len(indexed) != 40:
        return False, "stale - indexed revision marker has no valid revision"
    if not isinstance(expected, str) or expected != digest:
        return False, "stale - Graphify manifest changed after revision marker"
    try:
        requested = default_branch_commit(cwd)
    except RuntimeError as error:
        return False, str(error)
    if indexed != requested:
        return False, f"stale - indexed={indexed} requested={requested}"
    return True, str(graph_out)


def write_marker(graph_out: Path, revision: str) -> Path:
    """Bind the shared cache to the revision that was indexed."""
    import json

    digest = _manifest_digest(graph_out)
    if digest is None:
        raise RuntimeError("Graphify manifest.json is absent; extract did not finish")
    graph_out.mkdir(parents=True, exist_ok=True)
    marker = graph_out / GENERIC_MARKER
    temporary = marker.with_suffix(marker.suffix + ".tmp")
    temporary.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "indexed_revision": revision,
                "manifest_sha256": digest,
            },
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )
    temporary.replace(marker)
    return marker


def graphify_doctor_row(cwd: Path) -> dict[str, str] | None:
    """Doctor row for an existing shared graph. Missing caches stay the setup plan."""
    graph_out = resolve_graph_out(cwd)
    if not graph_out.exists():
        return None
    fresh, message = graph_freshness(cwd)
    if fresh:
        return {"status": "healthy", "detail": message}
    return {
        "status": "degraded",
        "detail": message,
        "fixNext": (
            "python3 .chaos-engine/install.py repair --project . --component graphify"
        ),
    }


def home_palace_note() -> str:
    """Name a Chroma home palace without opening or migrating it."""
    chroma = Path.home() / ".mempalace" / "palace" / "chroma.sqlite3"
    if chroma.is_file():
        return "ignored-chroma"
    return ""


def read_wing(root: Path) -> str | None:
    """Return the single wing declared in mempalace.yaml, when present."""
    path = root / "mempalace.yaml"
    if not path.is_file():
        return None
    found: list[str] = []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return None
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("wing:"):
            continue
        value = stripped.split(":", 1)[1].strip()
        if value:
            found.append(value)
    if len(found) == 1:
        return found[0]
    return None


@contextmanager
def refresh_lock(common_dir: Path) -> Iterator[None]:
    """Hold the repository store lock. A second refresh exits without building."""
    lock_dir = common_dir / "chaos-engine"
    lock_dir.mkdir(parents=True, exist_ok=True)
    lock_path = lock_dir / LOCK_NAME
    lock_file = lock_path.open("a+b")
    if lock_file.seek(0, os.SEEK_END) == 0:
        lock_file.write(b"\0")
        lock_file.flush()
    lock_file.seek(0)
    try:
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel

            msvcrt.locking(lock_file.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel

            fcntl.flock(lock_file.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        lock_file.close()
        raise RuntimeError("store refresh is already running") from error
    try:
        yield
    finally:
        lock_file.seek(0)
        if os.name == "nt":
            msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)
        lock_file.close()


def _run(runner: Runner, command: list[str], cwd: Path) -> None:
    if runner is subprocess.run:
        completed = subprocess.run(  # nosec B603 - caller-built argv, no shell.
            command,
            cwd=cwd,
            capture_output=True,
            text=True,
            check=False,
        )
        if completed.returncode != 0:
            detail = (completed.stderr or completed.stdout or "command failed").strip()
            raise RuntimeError(detail or "command failed")
        return
    result = runner(command, cwd)
    if isinstance(result, int) and result != 0:
        raise RuntimeError(f"command failed with exit {result}")


def _component_current(cwd: Path, component: str) -> bool:
    if component == "graphify":
        fresh, _message = graph_freshness(cwd)
        return fresh
    if component == "mempalace":
        return (resolve_palace(cwd) / "sqlite_exact.sqlite3").is_file()
    raise RuntimeError(f"unsupported store component: {component}")


def _refresh_graphify(
    cwd: Path,
    *,
    snapshot: Path,
    scratch: Path,
    revision: str,
    invoke: Runner,
) -> None:
    """Extract Graphify into the shared graphify-out for one detached snapshot."""
    graphify = shutil.which("graphify")
    if graphify is None:
        raise RuntimeError("graphify is not on PATH")
    staging = scratch / "graph-out"
    _run(
        invoke,
        [
            graphify,
            "extract",
            str(snapshot),
            "--code-only",
            "--no-cluster",
            "--out",
            str(staging),
        ],
        snapshot,
    )
    produced = staging / "graphify-out"
    if not (produced / "graph.json").is_file() or not (produced / "manifest.json").is_file():
        raise RuntimeError("graphify extract did not write graph.json and manifest.json")
    target = resolve_graph_out(cwd)
    target.parent.mkdir(parents=True, exist_ok=True)
    backup = target.with_name(target.name + ".replacing")
    if backup.exists():
        shutil.rmtree(backup)
    if target.exists():
        target.rename(backup)
    try:
        shutil.move(str(produced), str(target))
        write_marker(target, revision)
    except (OSError, RuntimeError):
        if target.exists():
            shutil.rmtree(target, ignore_errors=True)
        if backup.exists():
            backup.rename(target)
        raise
    if backup.exists():
        shutil.rmtree(backup, ignore_errors=True)


def _refresh_mempalace(
    cwd: Path,
    *,
    snapshot: Path,
    primary: Path,
    invoke: Runner,
) -> None:
    """Mine MemPalace into the shared palace for one detached snapshot."""
    mempalace = shutil.which("mempalace")
    if mempalace is None:
        raise RuntimeError("mempalace is not on PATH")
    palace = resolve_palace(cwd)
    mine = [
        mempalace,
        "--palace",
        str(palace),
        "--backend",
        PALACE_BACKEND,
        "mine",
        str(snapshot),
        "--agent",
        "chaos-engine-stores",
    ]
    wing = read_wing(snapshot) or read_wing(primary)
    if wing:
        mine.extend(["--wing", wing])
    _run(invoke, mine, snapshot)


def refresh(
    cwd: Path,
    *,
    if_stale: bool = False,
    components: frozenset[str] | set[str] | None = None,
    runner: Runner | None = None,
) -> int:
    """Index the default-branch tip into the shared stores.

    Allowed from any worktree. Does not fetch, reset, or clean a checkout.
    """
    selected = frozenset(components or {"graphify", "mempalace"})
    unknown = selected - {"graphify", "mempalace"}
    if unknown:
        raise RuntimeError(f"unsupported store component: {sorted(unknown)[0]}")
    common = resolve_common_dir(cwd)
    if common is None:
        raise RuntimeError("store refresh requires a git repository")
    if if_stale and all(_component_current(cwd, name) for name in selected):
        return 0
    revision = default_branch_commit(cwd)
    invoke = runner or subprocess.run
    with refresh_lock(common):
        if if_stale and all(_component_current(cwd, name) for name in selected):
            return 0
        primary = resolve_primary_root(cwd)
        scratch = Path(tempfile.mkdtemp(prefix="chaos-engine-stores-"))
        snapshot = scratch / "source"
        try:
            _git(cwd, "worktree", "add", "--detach", str(snapshot), revision)
            if "graphify" in selected and not (
                if_stale and _component_current(cwd, "graphify")
            ):
                _refresh_graphify(
                    cwd,
                    snapshot=snapshot,
                    scratch=scratch,
                    revision=revision,
                    invoke=invoke,
                )
            if "mempalace" in selected and not (
                if_stale and _component_current(cwd, "mempalace")
            ):
                _refresh_mempalace(
                    cwd, snapshot=snapshot, primary=primary, invoke=invoke
                )
        finally:
            try:
                _git(cwd, "worktree", "remove", "--force", str(snapshot))
            except RuntimeError:
                # The snapshot worktree may already be gone. Cleanup must not fail the refresh.
                pass
            shutil.rmtree(scratch, ignore_errors=True)
    return 0


def _spawn_enabled() -> bool:
    flag = os.environ.get("CHAOS_ENGINE_STORE_REFRESH")
    if flag is not None:
        return flag.strip().casefold() not in {"", "0", "false", "no", "off"}
    argv = " ".join(sys.argv)
    return not any(token in argv for token in ("unittest", "pytest", "tests/scripts", "/tests/"))


def maybe_spawn_refresh(cwd: Path, *, popen: Callable[..., object] = subprocess.Popen) -> str:
    """Start one detached refresh when the shared store is stale.

    A same-day attempt stamp stops a failing refresh from respawning on every
    later session. The daily timer and an explicit repair ignore the stamp.
    Unit-test processes do not spawn unless ``CHAOS_ENGINE_STORE_REFRESH=1``.
    """
    if not _spawn_enabled():
        return "skipped"
    try:
        if _component_current(cwd, "graphify") and _component_current(cwd, "mempalace"):
            return "fresh"
        common = resolve_common_dir(cwd)
    except (OSError, RuntimeError):
        return "skipped"
    if common is None:
        return "skipped"
    stamp = common / "chaos-engine" / ATTEMPT_STAMP
    today = datetime.now(timezone.utc).date().isoformat()
    try:
        if stamp.is_file() and stamp.read_text(encoding="utf-8").strip() == today:
            return "cooldown"
        stamp.parent.mkdir(parents=True, exist_ok=True)
        stamp.write_text(today + "\n", encoding="utf-8")
    except OSError:
        return "skipped"
    tool = Path(__file__).resolve().with_name("tool.py")
    popen(  # nosec B603 - owned tool.py argv, no shell.
        [sys.executable, str(tool), "stores", "refresh", "--if-stale"],
        cwd=str(resolve_primary_root(cwd)),
        start_new_session=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return "spawned"


def _schedule_tool(project: Path) -> Path:
    tool = project / ".chaos-engine" / "tool.py"
    if tool.is_file():
        return tool
    return project / "chaos-engine" / "tool.py"


def install_schedule(project: Path, *, home: Path | None = None) -> Path:
    """Write the user-level daily refresh timer. Does not start it."""
    root = (home or Path.home()).resolve()
    project = project.resolve()
    tool = _schedule_tool(project)
    command = f"{sys.executable} {tool} stores refresh --if-stale"
    if sys.platform == "darwin":
        destination = root / "Library/LaunchAgents/com.chaosengine.stores.refresh.plist"
        text = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" '
            '"http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n'
            '<plist version="1.0"><dict>\n'
            "<key>Label</key><string>com.chaosengine.stores.refresh</string>\n"
            "<key>ProgramArguments</key><array>\n"
            f"<string>{sys.executable}</string>\n"
            f"<string>{tool}</string>\n"
            "<string>stores</string><string>refresh</string><string>--if-stale</string>\n"
            "</array>\n"
            f"<key>WorkingDirectory</key><string>{project}</string>\n"
            "<key>StartCalendarInterval</key><dict><key>Hour</key><integer>3</integer>"
            "<key>Minute</key><integer>15</integer></dict>\n"
            "</dict></plist>\n"
        )
    elif os.name == "nt":
        destination = root / "AppData/Local/ChaosEngine/stores-refresh.cmd"
        text = (
            "@echo off\r\n"
            f"cd /d \"{project}\"\r\n"
            f"\"{sys.executable}\" \"{tool}\" stores refresh --if-stale\r\n"
        )
    else:
        destination = root / ".config/systemd/user/chaosengine-stores.timer"
        service = destination.with_name("chaosengine-stores.service")
        service.parent.mkdir(parents=True, exist_ok=True)
        service.write_text(
            "[Unit]\n"
            "Description=ChaosEngine shared MemPalace and Graphify refresh\n"
            "\n"
            "[Service]\n"
            "Type=oneshot\n"
            f"WorkingDirectory={project}\n"
            f"ExecStart={command}\n",
            encoding="utf-8",
        )
        text = (
            "[Unit]\n"
            "Description=Daily ChaosEngine store refresh\n"
            "\n"
            "[Timer]\n"
            "OnCalendar=daily\n"
            "Persistent=true\n"
            "\n"
            "[Install]\n"
            "WantedBy=timers.target\n"
        )
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(text, encoding="utf-8")
    return destination


def schedule_installed(home: Path | None = None) -> bool:
    """Return whether a user-level timer file is present. Does not query the OS."""
    root = (home or Path.home()).resolve()
    candidates = (
        root / ".config/systemd/user/chaosengine-stores.timer",
        root / "Library/LaunchAgents/com.chaosengine.stores.refresh.plist",
        root / "AppData/Local/ChaosEngine/stores-refresh.cmd",
    )
    return any(path.is_file() for path in candidates)
