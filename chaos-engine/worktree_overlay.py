#!/usr/bin/env python3
"""Keep ChaosEngine loaded in every checkout a host can start in (#6178).

The overlay (`.chaos-engine/`) and generated host files are untracked, so a
linked git worktree or a fresh clone starts without them. This module:

* `verify_ce_loads(cwd)` reports, per supported host, whether a session that
  starts in `cwd` (or any subdirectory) would load ChaosEngine.
* `materialize(primary, session)` copies the overlay and missing untracked
  host files from the primary checkout into a linked worktree. Existing files
  that still match the last materialized hash (or the current source) are left
  alone; stale pure-overlay copies are upgraded in place. Local edits that
  diverge from both refuse with a clear message. Idempotent and hash-based.
* `upgrade_session(primary, session)` is the one fast command that brings a
  worktree to the primary CE version: transactional materialize (preflight,
  backup, VERSION last) plus host/plugin repair. CE-owned host adapters are
  derived from the installer ownership list (not a hand list).
* `recover_session(primary, session)` repairs half-applied upgrades.
* `linked_worktree_reason(path)` returns a one-line refusal for installers
  run inside a linked worktree, naming the primary checkout.

Timing budget: prefer `upgrade_session` / `materialize` over
`CHAOS_ENGINE_ALLOW_LINKED_WORKTREE=1` + full `install.py install` (observed
~227s across 17 worktrees). Target: well under 5s per worktree when VERSION
matches (hash skip); under ~15s when overlay files need upgrade without a
plugin marketplace round-trip.

Standard library only; runs on Windows, macOS, and Linux.
"""

from __future__ import annotations

import argparse
import os
import hashlib
import importlib.util
import json
import shutil
import subprocess  # nosec B404 - fixed git arguments only.
import sys
import time
from pathlib import Path

TREE = ".chaos-engine"
ROUTER = f"{TREE}/skills/chaos-engine/SKILL.md"
GUARD = f"{TREE}/hooks/guard.py"
MANIFEST_REL = f"{TREE}/.overlay-materialize.json"
VERSION_REL = f"{TREE}/VERSION"
HOOK_HOSTS = ("claude", "codex", "copilot", "gemini", "grok")
INSTRUCTION_ONLY_HOSTS = ("opencode", "cursor", "grok-bot", "antigravity")
# Bots that auto-load nothing start from the bot entry; independent bots use the grok-bot path.
BOT_ENTRY = "chaos-engine/references/bot-entry.md"
BOT_HOSTS = ("grok-bot",)
HOSTS = (*HOOK_HOSTS, *INSTRUCTION_ONLY_HOSTS)
HOST_FILES = {
    "claude": (".claude/settings.json", ".claude/skills/chaos-engine/SKILL.md", "CLAUDE.md"),
    "codex": (".codex/config.toml", ".codex/hooks.json", ".agents/skills/chaos-engine/SKILL.md"),
    "copilot": (".github/hooks/chaos-engine.json", ".github/skills/chaos-engine/SKILL.md"),
    "gemini": (".gemini/settings.json", ".gemini/skills/chaos-engine/SKILL.md", "GEMINI.md"),
    "grok": (".grok/hooks",),
    "opencode": ("AGENTS.md",),
    "cursor": ("AGENTS.md",),
    "grok-bot": (BOT_ENTRY,),
    "antigravity": ("AGENTS.md",),
}
SHARED_FILES = ("AGENTS.md", ".mcp.json", ".claude/agents", ".codex/agents")
# Paths CE may write that consumers commonly edit — keep strict provenance.
CONSUMER_EDITABLE = frozenset(
    {
        "AGENTS.md",
        "CLAUDE.md",
        "GEMINI.md",
        ".github/copilot-instructions.md",
        ".gitignore",
        ".gitattributes",
        ".mcp.json",
        ".agents/skills/README.md",
        ".memory/events.jsonl",
        "mempalace.yaml",
    }
)
AGENT_GLOBS = (
    (".claude/agents", "chaos-engine-*"),
    (".codex/agents", "chaos-engine-*"),
    (".cursor/agents", "chaos-engine-*"),
)
UPGRADE_BACKUP_DIR = ".chaos-engine-state/upgrade-backup"
SKIP_PARTS = {"__pycache__", ".pytest_cache"}
# Timing budget note (seconds) for callers / docs.
MATERIALIZE_BUDGET_S_PER_WORKTREE = 5.0
UPGRADE_WITH_PLUGINS_BUDGET_S = 15.0


class OverlayLocalEditError(RuntimeError):
    """Session file diverges from source and from the last materialized hash."""


def _git(cwd: Path, *arguments: str) -> str | None:
    git = shutil.which("git")
    if git is None:
        return None
    try:
        result = subprocess.run(  # nosec B603 - resolved git executable, fixed arguments.
            [git, *arguments], cwd=cwd, capture_output=True, text=True, timeout=15, check=False
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def checkout_root(cwd: Path) -> Path:
    """Top of the checkout that contains `cwd`; `cwd` itself outside git."""
    cwd = Path(cwd).resolve()
    top = _git(cwd, "rev-parse", "--show-toplevel")
    return Path(top).resolve() if top else cwd


def primary_checkout(path: Path) -> Path | None:
    """Primary checkout for a linked worktree, or None when `path` is primary or not git."""
    path = Path(path).resolve()
    output = _git(path, "rev-parse", "--path-format=absolute", "--git-dir", "--git-common-dir")
    if not output:
        return None
    lines = output.splitlines()
    if len(lines) != 2:
        return None
    git_dir, common = (Path(line).resolve() for line in lines)
    if git_dir == common:
        return None
    return common.parent if common.name == ".git" else common


def linked_worktree_reason(path: Path) -> str | None:
    """One-line refusal for installing into a linked worktree, or None."""
    primary = primary_checkout(path)
    if primary is None:
        return None
    return (
        f"linked git worktree: install ChaosEngine in the primary checkout {primary}, then run "
        f"`python3 {primary / TREE / 'worktree_overlay.py'} upgrade-session --primary {primary} "
        f"--session {Path(path).resolve()}`"
    )


def _host_reason(root: Path, host: str, primary: Path | None) -> str:
    if not (root / ROUTER).is_file():
        return "missing-overlay"
    if host in HOOK_HOSTS and not (root / GUARD).is_file():
        return "missing-guard"
    agents_md_host = host in INSTRUCTION_ONLY_HOSTS and host not in BOT_HOSTS
    if agents_md_host and not (root / "AGENTS.md").is_file():
        return "missing-agents-md"
    if primary is not None:
        for relative in HOST_FILES[host]:
            if (primary / relative).exists() and not (root / relative).exists():
                return f"missing-host-file:{relative}"
    return "ok"


def verify_ce_loads(cwd: Path) -> dict[str, str]:
    """Per host: "ok" or the first reason a session started at `cwd` would miss CE."""
    root = checkout_root(Path(cwd))
    primary = primary_checkout(root)
    return {host: _host_reason(root, host, primary) for host in HOSTS}


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(65536)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def _manifest_path(session: Path) -> Path:
    return session / MANIFEST_REL


def _load_manifest(session: Path) -> dict[str, str]:
    path = _manifest_path(session)
    if not path.is_file():
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    files = data.get("files") if isinstance(data, dict) else None
    if not isinstance(files, dict):
        return {}
    return {str(key): str(value) for key, value in files.items() if isinstance(value, str)}


def _save_manifest(session: Path, files: dict[str, str]) -> None:
    path = _manifest_path(session)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"schema": 1, "files": dict(sorted(files.items()))}
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _rel_key(session: Path, target: Path) -> str:
    session_r = session.resolve()
    abs_target = target if target.is_absolute() else session_r / target
    # Avoid resolve() on missing paths (which anchors at CWD); normpath under session.
    if abs_target.exists():
        return abs_target.resolve().relative_to(session_r).as_posix()
    normalized = Path(os.path.normpath(str(abs_target)))
    return normalized.relative_to(session_r).as_posix()


def _diff_hint(source: Path, destination: Path, *, limit: int = 12) -> str:
    """Short unified-diff (or hash) hint for a refused consumer edit."""
    try:
        src = source.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
        dst = destination.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
    except OSError:
        return f"(unreadable; primary={_file_sha256(source)[:12]} session={_file_sha256(destination)[:12]})"
    import difflib

    lines = list(
        difflib.unified_diff(
            dst, src, fromfile=f"session/{destination.name}", tofile=f"primary/{source.name}", n=2
        )
    )
    if not lines:
        return "(no textual diff)"
    body = "".join(lines[:limit])
    if len(lines) > limit:
        body += f"... ({len(lines) - limit} more diff lines)\n"
    return body.rstrip()


def _sync_file(
    source: Path,
    destination: Path,
    *,
    session: Path,
    manifest: dict[str, str],
    upgraded: list[str],
    copied: list[str],
    unchanged: list[str],
    dry_run: bool = False,
    deferred: list[tuple[Path, Path, str]] | None = None,
) -> None:
    if not source.is_file():
        return
    session_r = session.resolve()
    dest_abs = destination if destination.is_absolute() else session_r / destination
    key = _rel_key(session, dest_abs)
    src_hash = _file_sha256(source)
    if not dest_abs.exists():
        if dry_run:
            if deferred is not None:
                deferred.append((source, dest_abs, key))
            return
        dest_abs.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, dest_abs)
        manifest[key] = src_hash
        copied.append(dest_abs.as_posix())
        return
    dest_hash = _file_sha256(dest_abs)
    if dest_hash == src_hash:
        if not dry_run:
            manifest[key] = src_hash
            unchanged.append(dest_abs.as_posix())
        return
    recorded = manifest.get(key)
    if recorded is not None and dest_hash == recorded:
        if dry_run:
            if deferred is not None:
                deferred.append((source, dest_abs, key))
            return
        shutil.copy2(source, dest_abs)
        manifest[key] = src_hash
        upgraded.append(dest_abs.as_posix())
        return
    hint = _diff_hint(source, dest_abs)
    raise OverlayLocalEditError(
        f"local edit or unknown provenance at {dest_abs}: diverges from primary "
        f"and from the last materialized hash; move it aside or reconcile, then re-run "
        f"materialize / upgrade-session\n--- diff hint ---\n{hint}"
    )


def _iter_source_files(source: Path, *, skip_version: bool = False) -> list[Path]:
    if source.is_dir():
        found: list[Path] = []
        for item in sorted(source.rglob("*")):
            if item.is_dir() or SKIP_PARTS & set(item.parts) or item.suffix == ".pyc":
                continue
            if item.name == ".overlay-materialize.json":
                continue
            if skip_version and item.name == "VERSION" and TREE in item.parts:
                continue
            found.append(item)
        return found
    if source.is_file():
        if skip_version and source.name == "VERSION" and TREE in source.parts:
            return []
        return [source]
    return []


def _copy(source: Path, destination: Path, *, session: Path, manifest: dict[str, str],
          upgraded: list[str], copied: list[str], unchanged: list[str],
          dry_run: bool = False, deferred: list[tuple[Path, Path, str]] | None = None,
          skip_version: bool = False) -> None:
    if source.is_dir():
        for item in _iter_source_files(source, skip_version=skip_version):
            target = destination / item.relative_to(source)
            _sync_file(
                item, target, session=session, manifest=manifest,
                upgraded=upgraded, copied=copied, unchanged=unchanged,
                dry_run=dry_run, deferred=deferred,
            )
    elif source.is_file():
        if skip_version and source.name == "VERSION" and TREE in source.parts:
            return
        _sync_file(
            source, destination, session=session, manifest=manifest,
            upgraded=upgraded, copied=copied, unchanged=unchanged,
            dry_run=dry_run, deferred=deferred,
        )


def _clean_stale_leftovers(primary: Path, session: Path, manifest: dict[str, str], removed: list[str]) -> None:
    """Remove session overlay files we previously materialized that primary no longer has.

    Only touches paths recorded in the materialize manifest (self-created leftovers).
    Never deletes never-touch / protected content outside that set.
    """
    primary = primary.resolve()
    session = session.resolve()
    for key in list(manifest.keys()):
        if key == MANIFEST_REL or key.endswith("/.overlay-materialize.json"):
            continue
        dest = session / key
        src = primary / key
        if dest.exists() and not src.exists():
            if dest.is_file() and not dest.is_symlink():
                dest.unlink()
                removed.append(dest.as_posix())
            del manifest[key]


def _materialize_relatives(primary: Path) -> list[str]:
    relatives = [TREE, *SHARED_FILES]
    for files in HOST_FILES.values():
        relatives.extend(files)
    return list(dict.fromkeys(relatives))


def _is_version_relative(relative: str) -> bool:
    rel = relative.replace("\\", "/")
    return rel == VERSION_REL or rel.endswith("/VERSION") and TREE in rel.split("/")


def materialize(primary: Path, session: Path, *, transactional: bool = True) -> list[str]:
    """Copy/upgrade the overlay and host files from `primary` into `session`.

    Returns paths newly written (copied or upgraded). Idempotent: a second call
    with the same primary leaves current files untouched. Raises
    ``OverlayLocalEditError`` when a session file has local edits.

    When ``transactional`` (default): preflight every path before writing;
    on refusal write nothing; on mid-write failure roll back from a session-dir
    backup; ``VERSION`` is written last.
    """
    primary, session = Path(primary).resolve(), Path(session).resolve()
    if not (primary / ROUTER).is_file():
        raise FileNotFoundError(f"primary checkout has no ChaosEngine overlay: {primary}")
    relatives = _materialize_relatives(primary)
    manifest = _load_manifest(session)
    copied: list[str] = []
    upgraded: list[str] = []
    unchanged: list[str] = []
    removed: list[str] = []

    def _pass(*, dry_run: bool, skip_version: bool, deferred=None, man=None) -> None:
        target_manifest = man if man is not None else manifest
        for relative in relatives:
            _copy(
                primary / relative,
                session / relative,
                session=session,
                manifest=target_manifest,
                upgraded=upgraded,
                copied=copied,
                unchanged=unchanged,
                dry_run=dry_run,
                deferred=deferred,
                skip_version=skip_version,
            )

    if not transactional:
        _pass(dry_run=False, skip_version=False)
        _clean_stale_leftovers(primary, session, manifest, removed)
        _save_manifest(session, manifest)
        return copied + upgraded

    # --- transactional path ---
    preflight_deferred: list[tuple[Path, Path, str]] = []
    refusals: list[str] = []
    # Preflight every relative (skip VERSION first pass semantics via skip_version
    # on TREE walk); collect ALL refusals before writing anything.
    for relative in relatives:
        try:
            _copy(
                primary / relative,
                session / relative,
                session=session,
                manifest=dict(manifest),
                upgraded=[],
                copied=[],
                unchanged=[],
                dry_run=True,
                deferred=preflight_deferred,
                skip_version=False,
            )
        except OverlayLocalEditError as error:
            refusals.append(str(error))
    # Deduplicate deferred entries by key (VERSION may appear once).
    seen_keys: set[str] = set()
    unique_deferred: list[tuple[Path, Path, str]] = []
    for item in preflight_deferred:
        if item[2] in seen_keys:
            continue
        seen_keys.add(item[2])
        unique_deferred.append(item)
    preflight_deferred = unique_deferred
    if refusals:
        joined = "\n---\n".join(refusals)
        raise OverlayLocalEditError(
            "upgrade preflight refused (nothing written):\n" + joined
        )

    backup_root = _backup_session_targets(session, preflight_deferred)
    try:
        copied.clear()
        upgraded.clear()
        unchanged.clear()
        # Write everything except VERSION first.
        _pass(dry_run=False, skip_version=True)
        # VERSION last.
        version_src = primary / VERSION_REL
        if version_src.is_file():
            _sync_file(
                version_src,
                session / VERSION_REL,
                session=session,
                manifest=manifest,
                upgraded=upgraded,
                copied=copied,
                unchanged=unchanged,
            )
        _clean_stale_leftovers(primary, session, manifest, removed)
        _save_manifest(session, manifest)
    except Exception:
        if backup_root is not None:
            _restore_session_backup(session, backup_root)
        raise
    return list(dict.fromkeys(copied + upgraded))



def _backup_session_targets(
    session: Path, deferred: list[tuple[Path, Path, str]]
) -> Path | None:
    """Copy existing session files that will change into a session-dir backup."""
    existing = [(src, dest, key) for src, dest, key in deferred if dest.is_file()]
    if not existing:
        return None
    stamp = time.strftime("%Y%m%dT%H%M%S")
    root = session / UPGRADE_BACKUP_DIR / stamp
    root.mkdir(parents=True, exist_ok=True)
    for _src, dest, key in existing:
        target = root / key
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(dest, target)
    (root / "MANIFEST.json").write_text(
        json.dumps({"files": [key for _s, _d, key in existing]}, indent=2) + "\n",
        encoding="utf-8",
    )
    return root


def _restore_session_backup(session: Path, backup_root: Path) -> None:
    """Restore files from an upgrade backup after a mid-write failure."""
    manifest_path = backup_root / "MANIFEST.json"
    keys: list[str] = []
    if manifest_path.is_file():
        try:
            data = json.loads(manifest_path.read_text(encoding="utf-8"))
            keys = list(data.get("files") or [])
        except (OSError, ValueError):
            keys = []
    if not keys:
        keys = [
            p.relative_to(backup_root).as_posix()
            for p in backup_root.rglob("*")
            if p.is_file() and p.name != "MANIFEST.json"
        ]
    for key in keys:
        src = backup_root / key
        if not src.is_file():
            continue
        dest = session / key
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dest)


def _read_version(root: Path) -> str:
    path = root / VERSION_REL
    if not path.is_file():
        # tracked tree may keep VERSION at chaos-engine/VERSION in repo root layout
        alt = root / "chaos-engine" / "VERSION"
        path = alt if alt.is_file() else path
    if not path.is_file():
        return ""
    return path.read_text(encoding="utf-8").strip()


def _try_load_hosts(primary: Path):
    """Load hosts.py from the primary overlay (or source tree) for ownership lists."""
    candidates = [
        primary / TREE / "hosts.py",
        primary / "chaos-engine" / "hosts.py",
        Path(__file__).resolve().with_name("hosts.py"),
    ]
    for path in candidates:
        if not path.is_file():
            continue
        try:
            spec = importlib.util.spec_from_file_location("_ce_hosts_owned", path)
            if spec is None or spec.loader is None:
                continue
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            if hasattr(module, "receipt_owned_paths"):
                return module
        except Exception:  # nosec B110 - ownership helper must not break upgrade
            continue
    return None


def _normalize_owned_rel(rel: str) -> str:
    rel = rel.replace("\\", "/")
    while rel.startswith("./"):
        rel = rel[2:]
    return rel


def _add_owned_path(owned: list[str], seen: set[str], rel: str) -> None:
    rel = _normalize_owned_rel(rel)
    if rel in seen or rel in CONSUMER_EDITABLE:
        return
    seen.add(rel)
    owned.append(rel)


def _add_owned_tree_or_file(
    primary: Path, owned: list[str], seen: set[str], rel: str
) -> None:
    path = primary / rel
    if path.is_file():
        _add_owned_path(owned, seen, rel)
    elif path.is_dir():
        for item in _iter_source_files(path):
            _add_owned_path(owned, seen, item.relative_to(primary).as_posix())


def _collect_receipt_owned(
    primary: Path, owned: list[str], seen: set[str]
) -> None:
    hosts = _try_load_hosts(primary)
    if hosts is None:
        return
    live = set(getattr(hosts, "LIVE_PERSISTENT_PATHS", ()))
    try:
        for rel in hosts.receipt_owned_paths():
            if rel in live or rel in CONSUMER_EDITABLE:
                continue
            _add_owned_tree_or_file(primary, owned, seen, rel)
    except Exception:  # nosec B110 - ownership helper must not break upgrade
        return


def _collect_agent_glob_owned(
    primary: Path, owned: list[str], seen: set[str]
) -> None:
    for agents_rel, pattern in AGENT_GLOBS:
        agents = primary / agents_rel
        if not agents.is_dir():
            continue
        for item in sorted(agents.glob(pattern)):
            if item.is_file():
                _add_owned_path(owned, seen, item.relative_to(primary).as_posix())


def _collect_host_files_owned(
    primary: Path, owned: list[str], seen: set[str]
) -> None:
    for files in HOST_FILES.values():
        for rel in files:
            if rel in CONSUMER_EDITABLE:
                continue
            _add_owned_tree_or_file(primary, owned, seen, rel)


def _installer_owned_relatives(primary: Path) -> list[str]:
    """Every path CE writes for host adapters + overlay, from installer ownership.

    Derived from ``hosts.receipt_owned_paths()`` plus ``.chaos-engine/**`` and
    host-agent globs (any suffix). Not a hand-maintained file list. Consumer-
    editable docs stay off this set so they keep strict provenance.
    """
    primary = Path(primary).resolve()
    owned: list[str] = []
    seen: set[str] = set()
    tree = primary / TREE
    if tree.is_dir():
        for item in _iter_source_files(tree):
            _add_owned_path(owned, seen, item.relative_to(primary).as_posix())
    _collect_receipt_owned(primary, owned, seen)
    _collect_agent_glob_owned(primary, owned, seen)
    _collect_host_files_owned(primary, owned, seen)
    return owned


def upgrade_session(
    primary: Path, session: Path, *, repair_plugins: bool | None = None, recover: bool = False
) -> dict:
    """One command: bring ``session`` to primary CE version (materialize + host repair).

    Skips the slow plugin marketplace round-trip when VERSION already matches
    unless ``repair_plugins=True``. Returns a result dict with timings.

    ``recover=True`` force-adopts CE-owned paths first (half-applied worktrees).
    """
    primary, session = Path(primary).resolve(), Path(session).resolve()
    started = time.perf_counter()
    primary_ver = _read_version(primary)
    session_ver_before = _read_version(session)
    # Reconcile tracked host adapters the consumer never touched BEFORE
    # materialize, so provenance refuses do not fire on stale worktree SHAs.
    _maybe_checkout_tracked_ce(primary, session)
    # Gitignored overlay/agents (normal engine + consumer installs) are invisible
    # to git checkout. Adopt their current hashes into the materialize manifest
    # so stale CE-owned files upgrade instead of raising unknown provenance.
    _adopt_ce_owned(primary, session, force=recover)
    try:
        written = materialize(primary, session, transactional=True)
    except OverlayLocalEditError:
        # Self-heal once after a partial / raced materialize: re-adopt CE-owned
        # paths and retry. Consumer-edited non-CE files still refuse.
        _adopt_ce_owned(primary, session, force=True)
        written = materialize(primary, session, transactional=True)
    session_ver_after = _read_version(session)
    version_match = bool(primary_ver) and primary_ver == session_ver_after
    if repair_plugins is None:
        repair_plugins = not version_match or session_ver_before != primary_ver
    plugins_ran = False
    hosts_repaired = False
    if repair_plugins:
        plugins_ran = _try_repair_plugins(session)
    else:
        hosts_repaired = _try_repair_hosts_light(session)
    elapsed = time.perf_counter() - started
    return {
        "written": written,
        "primary_version": primary_ver,
        "session_version": session_ver_after,
        "plugins_repaired": plugins_ran,
        "hosts_light_repaired": hosts_repaired,
        "recovered": recover,
        "elapsed_s": round(elapsed, 3),
        "budget_s": MATERIALIZE_BUDGET_S_PER_WORKTREE if not plugins_ran else UPGRADE_WITH_PLUGINS_BUDGET_S,
    }


def recover_session(primary: Path, session: Path) -> dict:
    """Repair a half-applied worktree upgrade (VERSION bumped, hosts/files stuck)."""
    return upgrade_session(primary, session, repair_plugins=True, recover=True)


def session_is_half_applied(primary: Path, session: Path) -> bool:
    """True when VERSION matches primary but some CE-owned files still diverge."""
    primary, session = Path(primary).resolve(), Path(session).resolve()
    if not (primary / VERSION_REL).is_file() or not (session / VERSION_REL).is_file():
        return False
    if _read_version(primary) != _read_version(session):
        return False
    for relative in _installer_owned_relatives(primary):
        if relative == VERSION_REL or relative.endswith("/VERSION"):
            continue
        src = primary / relative
        dest = session / relative
        if not src.is_file():
            continue
        if not dest.is_file():
            return True
        if _file_sha256(src) != _file_sha256(dest):
            return True
    return False


def _managed_checkout_paths(primary: Path) -> list[str]:
    """Tracked CE overlay + host adapter paths safe to reset from primary HEAD."""
    paths = [TREE, *SHARED_FILES]
    for files in HOST_FILES.values():
        paths.extend(files)
    # Keep order stable and unique; only paths that exist on primary.
    ordered: list[str] = []
    seen: set[str] = set()
    for relative in paths:
        if relative in seen:
            continue
        seen.add(relative)
        if (primary / relative).exists():
            ordered.append(relative)
    return ordered


def _path_has_local_edit(session: Path, relative: str) -> bool:
    """True when the session worktree has an unstaged/staged edit on ``relative``."""
    diff = _git(session, "diff", "--name-only", "HEAD", "--", relative)
    cached = _git(session, "diff", "--name-only", "--cached", "--", relative)
    dirty = set()
    if diff:
        dirty.update(line for line in diff.splitlines() if line.strip())
    if cached:
        dirty.update(line for line in cached.splitlines() if line.strip())
    return bool(dirty)


def _maybe_checkout_tracked_ce(primary: Path, session: Path) -> list[str]:
    """
    Reset tracked CE/host adapters from primary HEAD when the consumer never edited them.

    Worktrees parked on an older SHA still carry tracked ``.chaos-engine/`` and
    host adapters (for example ``.claude/agents/``). ``materialize`` would refuse
    those as unknown provenance. Checking them out from primary HEAD first
    reconciles consumer-untouched adapters; paths with local edits are skipped.
    """
    restored: list[str] = []
    if not (primary / TREE).is_dir():
        return restored
    # Only when session is a linked worktree of the same primary.
    if primary_checkout(session) is None:
        return restored
    top = _git(session, "rev-parse", "--show-toplevel")
    if not top:
        return restored
    sha = _git(primary, "rev-parse", "HEAD")
    if not sha:
        return restored
    git = shutil.which("git")
    if git is None:
        return restored
    for relative in _managed_checkout_paths(primary):
        # Skip paths the consumer edited — except CE-owned overlay / agents,
        # which upgrade-session always reconciles (including staged prior-CE
        # overlays that would otherwise trip OverlayLocalEditError).
        if _path_has_local_edit(session, relative) and not _is_ce_owned_relative(relative):
            continue
        listed = _git(primary, "ls-files", relative)
        if not listed:
            continue
        try:
            result = subprocess.run(  # nosec B603
                [git, "checkout", "-q", sha, "--", relative],
                cwd=session, capture_output=True, text=True, timeout=60, check=False,
            )
        except (OSError, subprocess.TimeoutExpired):
            continue
        if result.returncode == 0:
            restored.append(relative)
    return restored


def _is_ce_owned_relative(relative: str) -> bool:
    """True for overlay paths and ChaosEngine host-adapter files upgrade may overwrite."""
    rel = relative.replace("\\", "/")
    while rel.startswith("./"):
        rel = rel[2:]
    if rel in CONSUMER_EDITABLE:
        return False
    if rel == TREE or rel.startswith(f"{TREE}/"):
        return True
    name = Path(rel).name
    if name.startswith("chaos-engine-"):
        parent = Path(rel).parent.as_posix()
        if parent in {".claude/agents", ".codex/agents", ".cursor/agents"}:
            return True
    # Skills / hooks / settings under host roots that CE owns.
    for prefix in (
        ".claude/skills/chaos-engine/",
        ".codex/",
        ".gemini/skills/chaos-engine/",
        ".github/skills/chaos-engine/",
        ".github/hooks/",
        ".agents/skills/chaos-engine/",
        ".grok/hooks/",
        "plugins/chaos-engine/",
    ):
        if rel == prefix.rstrip("/") or rel.startswith(prefix):
            return True
    if rel in {
        ".claude/settings.json",
        ".gemini/settings.json",
        ".codex/config.toml",
        ".codex/hooks.json",
    }:
        return True
    return False


def _ce_owned_upgrade_relatives(primary: Path) -> list[str]:
    """Backward-compatible alias — prefer ``_installer_owned_relatives``."""
    return _installer_owned_relatives(primary)


def _path_is_ignored(session: Path, relative: str) -> bool:
    """True when git reports ``relative`` ignored in ``session`` (or git unavailable)."""
    git = shutil.which("git")
    if git is None:
        return True
    try:
        result = subprocess.run(  # nosec B603
            [git, "check-ignore", "-q", "--", relative],
            cwd=session, capture_output=True, text=True, timeout=15, check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return True
    if result.returncode == 0:
        return True
    # Not ignored: still treat as eligible when the path is absent from the index.
    listed = _git(session, "ls-files", "--", relative)
    return not listed


def _adopt_ce_owned(primary: Path, session: Path, *, force: bool = False) -> list[str]:
    """Seed the materialize manifest for stale CE-owned session files.

    Ownership comes from the installer (``receipt_owned_paths`` + overlay +
    host-agent globs), not a hand list. CE-owned paths that diverge from
    primary are treated as stale CE output and adopted so materialize can
    overwrite. Consumer-editable paths stay on strict provenance.

    ``force=True`` (recover path) adopts even when a prior manifest hash
    already matched a different value — used for half-applied worktrees.
    """
    adopted: list[str] = []
    if not (primary / TREE).is_dir():
        return adopted
    # Allow adopt for linked worktrees; also allow recover on any session with overlay.
    if primary_checkout(session) is None and not force:
        return adopted
    manifest = _load_manifest(session)
    dirty = False
    for relative in _installer_owned_relatives(primary):
        source = primary / relative
        dest = session / relative
        if not source.is_file() or not dest.is_file():
            continue
        src_hash = _file_sha256(source)
        dest_hash = _file_sha256(dest)
        if dest_hash == src_hash:
            if manifest.get(relative) != src_hash:
                manifest[relative] = src_hash
                dirty = True
            continue
        # Diverge from primary: adopt current dest hash so stale-upgrade overwrites.
        if force or manifest.get(relative) != dest_hash:
            manifest[relative] = dest_hash
            dirty = True
            adopted.append(relative)
        elif relative not in adopted and manifest.get(relative) == dest_hash:
            # already adopted; materialize will upgrade
            adopted.append(relative)
    if dirty:
        _save_manifest(session, manifest)
    return adopted


def _try_repair_hosts_light(session: Path) -> bool:
    """Copy missing host pointer files only; no marketplace."""
    # materialize already synced HOST_FILES; nothing more required for light path.
    return (session / ROUTER).is_file()


def _try_repair_plugins(session: Path) -> bool:
    """Best-effort plugin repair via install.py --component plugins when available."""
    installer = session / TREE / "install.py"
    if not installer.is_file():
        installer = session / "chaos-engine" / "install.py"
    if not installer.is_file():
        return False
    py = sys.executable or "python3"
    try:
        result = subprocess.run(  # nosec B603
            [py, str(installer), "repair", "--project", str(session), "--component", "plugins"],
            cwd=session, capture_output=True, text=True, timeout=120, check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return False
    return result.returncode == 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    commands = parser.add_subparsers(dest="command", required=True)
    verify = commands.add_parser("verify", help="Report per-host CE load status for a directory.")
    verify.add_argument("--cwd", type=Path, default=Path.cwd())
    make = commands.add_parser("materialize", help="Copy/upgrade the overlay into a linked worktree.")
    make.add_argument("--primary", type=Path, required=True)
    make.add_argument("--session", type=Path, required=True)
    upgrade = commands.add_parser(
        "upgrade-session",
        help="Fast: materialize + host repair; skip plugins when VERSION matches.",
    )
    upgrade.add_argument("--primary", type=Path, required=True)
    upgrade.add_argument("--session", type=Path, required=True)
    upgrade.add_argument(
        "--force-plugins",
        action="store_true",
        help="Run plugin repair even when VERSION already matches (slow ~60s).",
    )
    upgrade.add_argument(
        "--recover",
        action="store_true",
        help="Force-adopt CE-owned paths first (half-applied worktrees).",
    )
    recover = commands.add_parser(
        "recover-session",
        help="Repair a half-applied upgrade (VERSION bumped, hosts/files stuck).",
    )
    recover.add_argument("--primary", type=Path, required=True)
    recover.add_argument("--session", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.command == "verify":
        report = verify_ce_loads(args.cwd)
        failing = {host: reason for host, reason in report.items() if reason != "ok"}
        print("ChaosEngine loads on every host" if not failing else json.dumps(failing, sort_keys=True))
        return 0 if not failing else 1
    if args.command in {"upgrade-session", "recover-session"}:
        recover_flag = args.command == "recover-session" or getattr(args, "recover", False)
        try:
            result = upgrade_session(
                args.primary,
                args.session,
                repair_plugins=True if getattr(args, "force_plugins", False) or recover_flag else None,
                recover=recover_flag,
            )
        except OverlayLocalEditError as error:
            print(f"{args.command} refused: {error}", file=sys.stderr)
            return 2
        print(json.dumps(result, sort_keys=True))
        return 0
    try:
        copied = materialize(args.primary, args.session)
    except OverlayLocalEditError as error:
        print(f"materialize refused: {error}", file=sys.stderr)
        return 2
    print(f"materialized {len(copied)} file(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
