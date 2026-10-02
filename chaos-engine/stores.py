#!/usr/bin/env python3
"""Shared MemPalace and Graphify locations for every ChaosEngine checkout.

One Git repository has one palace and one graphify-out. Linked worktrees and
other branches resolve those same paths. Refresh builds a detached snapshot of
the default-branch tip and does not reset a checkout. ``~/.mempalace`` is never
read or migrated.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess  # nosec B404 - fixed git and store CLIs, no shell.
import sys
import tempfile
import time
from contextlib import contextmanager
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Iterator


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


def legacy_palace(cwd: Path) -> Path:
    """Project-local palace written by older host MCP args. Do not delete it."""
    return cwd.resolve() / ".chaos-engine-state" / "mempalace"


def palace_migration_hint(cwd: Path) -> str | None:
    """Hint when the legacy directory exists and is not the shared palace."""
    legacy = legacy_palace(cwd)
    canonical = resolve_palace(cwd)
    try:
        if legacy.is_symlink() or not legacy.is_dir():
            return None
        if legacy.resolve() == canonical.resolve():
            return None
    except OSError:
        return None
    return (
        "legacy MemPalace directory "
        f"{legacy} differs from the shared palace {canonical}; "
        "leave that data in place and point MCP at the shared palace"
    )


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
        completed = run_until_stalled(
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


def palace_drawer_count(palace: Path) -> int | None:
    """Documents in a sqlite_exact palace; None when absent or unreadable (#6377)."""
    database = palace / "sqlite_exact.sqlite3"
    if not database.is_file():
        return None
    import sqlite3

    try:
        connection = sqlite3.connect(f"file:{database.as_posix()}?mode=ro", uri=True)
        try:
            row = connection.execute("select count(*) from documents").fetchone()
        finally:
            connection.close()
    except sqlite3.Error:
        return None
    return int(row[0]) if row else 0


def _component_current(cwd: Path, component: str) -> bool:
    if component == "graphify":
        fresh, _message = graph_freshness(cwd)
        return fresh
    if component == "mempalace":
        # #6377: an initialized but never-mined palace is not current.
        return bool(palace_drawer_count(resolve_palace(cwd)))
    raise RuntimeError(f"unsupported store component: {component}")


_DOC_SUFFIXES = {".md", ".mdx"}
_HEADING_RE = re.compile(r"^(#{1,6})\s+(.+?)\s*$")
_LINK_RE = re.compile(r"\[[^\]]*\]\(([^)\s]+)\)")
_SKIP_DOC_PARTS = frozenset({".git", "node_modules", "target"})


def _documentation_files(root: Path) -> list[Path]:
    files: list[Path] = []
    if not root.is_dir():
        return files
    for path in root.rglob("*"):
        if not path.is_file() or path.suffix.lower() not in _DOC_SUFFIXES:
            continue
        relative = path.relative_to(root)
        if any(part in _SKIP_DOC_PARTS for part in relative.parts):
            continue
        files.append(path)
    return sorted(files)


def index_documentation(root: Path) -> dict:
    """File Markdown and MDX pages and extract their headings and links.

    This is the zero-LLM documentation pass. External mine and code-only
    extract commands still run; refresh merges this result afterward.
    """
    pages: list[dict] = []
    for path in _documentation_files(root):
        headings: list[str] = []
        links: list[str] = []
        for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
            heading = _HEADING_RE.match(line)
            if heading:
                headings.append(heading.group(2).strip())
            links.extend(match.group(1) for match in _LINK_RE.finditer(line))
        pages.append({
            "path": path.relative_to(root).as_posix(),
            "headings": headings,
            "links": links,
        })
    return {"pages": pages}


def merge_documentation_graph(graph_json: Path, indexed: dict) -> dict:
    """Add documentation pages to a Graphify graph.json."""
    try:
        graph = json.loads(graph_json.read_text(encoding="utf-8"))
    except (OSError, ValueError, UnicodeError):
        graph = {}
    if not isinstance(graph, dict):
        graph = {}
    nodes = graph.get("nodes")
    edges = graph.get("edges")
    if not isinstance(nodes, list):
        nodes = []
    if not isinstance(edges, list):
        edges = []
    for page in indexed.get("pages") or []:
        if not isinstance(page, dict):
            continue
        page_path = str(page.get("path") or "")
        if not page_path:
            continue
        node_id = "docs:" + page_path
        label = page_path
        headings = page.get("headings") or []
        if headings:
            label = str(headings[0])
        nodes.append({
            "id": node_id,
            "label": label,
            "kind": "document",
            "path": page_path,
            "headings": list(headings),
        })
        for link in page.get("links") or []:
            edges.append({"source": node_id, "target": str(link), "kind": "link"})
    graph["nodes"] = nodes
    graph["edges"] = edges
    graph_json.parent.mkdir(parents=True, exist_ok=True)
    graph_json.write_text(json.dumps(graph, indent=2) + "\n", encoding="utf-8")
    return graph


def write_documentation_filing(directory: Path, indexed: dict) -> Path:
    """Record the pages a documentation mine files, including .mdx."""
    directory.mkdir(parents=True, exist_ok=True)
    destination = directory / "documentation-pages.json"
    destination.write_text(json.dumps(indexed, indent=2) + "\n", encoding="utf-8")
    return destination


def _apply_documentation_index(source: Path, *destinations: Path) -> dict:
    indexed = index_documentation(source)
    if not indexed["pages"]:
        return indexed
    for destination in destinations:
        if destination.name == "graph.json":
            merge_documentation_graph(destination, indexed)
        else:
            write_documentation_filing(destination, indexed)
    return indexed


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
    _apply_documentation_index(snapshot, produced / "graph.json", produced)
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
    _apply_documentation_index(snapshot, palace)


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
    """Explicit refresh wins. A repo-guarded test run does not spawn (#6249)."""
    if os.environ.get("CHAOS_ENGINE_TEST_REPO_GUARD", "").strip():
        flag = os.environ.get("CHAOS_ENGINE_STORE_REFRESH", "")
        return flag.strip().casefold() in {"1", "true", "yes", "on"}
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


DEFAULT_STALL_SECONDS = 120
STALL_ENV = "CHAOS_ENGINE_STALL_SECONDS"
_POLL_SECONDS = 0.5


def stall_seconds() -> int:
    """Seconds without progress before a store command counts as stalled (#6377)."""
    raw = os.environ.get(STALL_ENV, "").strip()
    return int(raw) if raw.isdigit() and int(raw) > 0 else DEFAULT_STALL_SECONDS


def _descendant_cpu_ticks(pid: int) -> int | None:
    """CPU ticks of a process tree from /proc; None where /proc is unavailable."""
    proc = Path("/proc")
    if not (proc / str(pid) / "stat").is_file():
        return None
    parents: dict[int, list[int]] = {}
    ticks: dict[int, int] = {}
    for entry in proc.iterdir():
        if not entry.name.isdigit():
            continue
        try:
            fields = (entry / "stat").read_text(encoding="ascii").rsplit(")", 1)[1].split()
        except (OSError, IndexError, UnicodeDecodeError):
            continue
        child = int(entry.name)
        parents.setdefault(int(fields[1]), []).append(child)
        ticks[child] = int(fields[11]) + int(fields[12])
    total, pending = 0, [pid]
    while pending:
        current = pending.pop()
        total += ticks.get(current, 0)
        pending.extend(parents.get(current, ()))
    return total


def _ps_cpu_ticks(pid: int) -> int | None:
    """CPU seconds of a process tree via psutil or POSIX ``ps`` (macOS/BSD); None if neither works."""
    try:
        import psutil  # type: ignore[import-not-found]

        root = psutil.Process(pid)
        total = 0.0
        for proc in (root, *root.children(recursive=True)):
            try:
                times = proc.cpu_times()
                total += times.user + times.system
            except psutil.Error:
                continue
        return int(total * 100)
    except ImportError:
        pass
    except Exception:  # noqa: BLE001 - process gone or inaccessible: unknown, not progress
        return None
    ps = shutil.which("ps")
    if not ps or os.name == "nt":
        return None
    try:
        listing = subprocess.run(  # nosec B603 - fixed ps argv, no shell.
            [ps, "-A", "-o", "pid=,ppid=,time="], capture_output=True, text=True, check=False
        ).stdout
    except OSError:
        return None
    parents: dict[int, list[int]] = {}
    ticks: dict[int, int] = {}
    for line in listing.splitlines():
        fields = line.split()
        if len(fields) != 3 or not fields[0].isdigit() or not fields[1].isdigit():
            continue
        clock = fields[2].replace("-", ":").split(":")
        try:
            seconds = 0.0
            for part in clock:
                seconds = seconds * 60 + float(part)
        except ValueError:
            continue
        parents.setdefault(int(fields[1]), []).append(int(fields[0]))
        ticks[int(fields[0])] = int(seconds * 100)
    if pid not in ticks:
        return None
    total, pending = 0, [pid]
    while pending:
        current = pending.pop()
        total += ticks.get(current, 0)
        pending.extend(parents.get(current, ()))
    return total


def process_tree_cpu(pid: int) -> int | None:
    """CPU ticks of a process tree: /proc, then psutil or ``ps``; None when unmeasurable."""
    ticks = _descendant_cpu_ticks(pid)
    return ticks if ticks is not None else _ps_cpu_ticks(pid)


OUTPUT_ONLY_STALL_FACTOR = 5


def run_until_stalled(
    args: list[str], *, stall_seconds: float | None = None, **kwargs: Any
) -> subprocess.CompletedProcess:
    """Drop-in for ``subprocess.run`` without a wall-clock cap (#6377).

    The command runs as long as it progresses: new output or CPU time in its
    process tree. Only ``stall_seconds`` with neither raises
    ``subprocess.TimeoutExpired``. A ``timeout`` argument is ignored. Where CPU
    time cannot be read (no /proc, psutil or ``ps``), only output counts and the
    stall window is ``OUTPUT_ONLY_STALL_FACTOR`` times longer, so a hang still ends.
    """
    kwargs.pop("timeout", None)
    check = kwargs.pop("check", False)
    text = bool(kwargs.pop("text", False) or kwargs.pop("universal_newlines", False))
    capture = kwargs.pop("capture_output", False)
    window = float(stall_seconds or globals()["stall_seconds"]())
    with tempfile.TemporaryFile() as out_file, tempfile.TemporaryFile() as err_file:
        if capture:
            kwargs["stdout"], kwargs["stderr"] = out_file, err_file
        process = subprocess.Popen(args, **kwargs)  # nosec B603 - caller-built argv, no shell.
        last_mark, last_progress = None, time.monotonic()
        while process.poll() is None:
            time.sleep(_POLL_SECONDS)
            cpu = process_tree_cpu(process.pid)
            mark = (os.fstat(out_file.fileno()).st_size, os.fstat(err_file.fileno()).st_size, cpu)
            # Failsafe: when CPU is unmeasurable, only output counts and the window widens.
            limit = window if cpu is not None else window * OUTPUT_ONLY_STALL_FACTOR
            if mark != last_mark:
                last_mark, last_progress = mark, time.monotonic()
            elif time.monotonic() - last_progress >= limit:
                process.kill()
                process.wait()
                raise subprocess.TimeoutExpired(args, window)
        out_file.seek(0)
        err_file.seek(0)
        stdout, stderr = (out_file.read(), err_file.read()) if capture else (None, None)
    if text and capture:
        stdout = stdout.decode("utf-8", errors="replace")
        stderr = stderr.decode("utf-8", errors="replace")
    completed = subprocess.CompletedProcess(args, process.returncode, stdout, stderr)
    if check:
        completed.check_returncode()
    return completed
