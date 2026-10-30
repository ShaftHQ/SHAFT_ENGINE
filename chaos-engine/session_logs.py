#!/usr/bin/env python3
"""Session-scoped install/doctor log paths for self-delete.

Bots used to tee install/doctor output to workspace-root ``*.log`` files,
which the delete guard then refused (outside git, not on the allow-list).
Write those logs under ``.chaos-engine-state/logs/`` inside the session
project (or an explicit session worktree) and record them on the session
created-paths manifest so cleanup-at-close can self-delete them.
"""

from __future__ import annotations

import os
from pathlib import Path

STATE = ".chaos-engine-state"
LOG_DIR = f"{STATE}/logs"
SESSION_WORKTREE_ENV = "CHAOS_ENGINE_SESSION_WORKTREE"


def session_root(project: Path | None = None) -> Path:
    """Prefer the session worktree env, else the project / cwd."""
    hint = os.environ.get(SESSION_WORKTREE_ENV, "").strip()
    if hint:
        return Path(hint).expanduser().resolve()
    if project is not None:
        return Path(project).resolve()
    return Path.cwd().resolve()


def log_path(kind: str, project: Path | None = None) -> Path:
    """Return ``<session>/.chaos-engine-state/logs/<kind>.log`` and ensure the dir exists."""
    root = session_root(project)
    folder = root / LOG_DIR
    folder.mkdir(parents=True, exist_ok=True)
    safe = "".join(ch if ch.isalnum() or ch in "-_" else "-" for ch in (kind or "ce"))
    return folder / f"{safe}.log"


def record_log_path(path: Path, project: Path | None = None) -> None:
    """Best-effort: add ``path`` to the session created-paths manifest for self-delete."""
    try:
        from delete_guard import (  # type: ignore
            SESSION_CREATED_MANIFEST_ENV,
            record_session_created_paths,
            session_manifest_path,
        )
    except Exception:  # nosec B110
        try:
            import importlib.util

            here = Path(__file__).resolve().with_name("delete_guard.py")
            spec = importlib.util.spec_from_file_location("_ce_dg_session_logs", here)
            if spec is None or spec.loader is None:
                return
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
        except Exception:  # nosec B110
            return
        else:
            manifest = mod.session_manifest_path()
            if manifest is None:
                return
            root = session_root(project)
            try:
                mod.record_session_created_paths([path], manifest=manifest, roots=[root])
            except Exception:  # nosec B110
                return
            return
    manifest = session_manifest_path()
    if manifest is None and not os.environ.get(SESSION_CREATED_MANIFEST_ENV):
        return
    root = session_root(project)
    try:
        record_session_created_paths([path], manifest=manifest, roots=[root])
    except Exception:  # nosec B110
        return


def open_session_log(kind: str, project: Path | None = None):
    """Open a session log for append and record it on the manifest when possible."""
    path = log_path(kind, project)
    record_log_path(path, project)
    return path.open("a", encoding="utf-8")
