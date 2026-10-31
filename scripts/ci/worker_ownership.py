"""
Worker ownership manifest (CE 2026.10.31.6 / H-OWNERSHIP).

Workers record paths they create. Cleanup-at-close may delete only entries
listed here (delete-guard never-touch still wins).
"""
from __future__ import annotations

import json
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

SCHEMA = 1
ENV_MANIFEST = "CHAOS_WORKER_OWNERSHIP_MANIFEST"


def default_manifest_path(
    task: str,
    *,
    environ: dict[str, str] | None = None,
    checkpoints_dir: Path | str | None = None,
) -> Path:
    env = environ if environ is not None else os.environ
    override = str(env.get(ENV_MANIFEST, "")).strip()
    if override:
        return Path(override)
    if checkpoints_dir is not None:
        base = Path(checkpoints_dir)
    else:
        configured = str(env.get("CHAOS_CHECKPOINTS_DIR", "")).strip()
        base = Path(configured) if configured else Path.cwd() / "checkpoints"
    safe = "".join(ch if ch.isalnum() or ch in "-_" else "-" for ch in task.strip()) or "task"
    return base / f"{safe}-ownership.json"


def _empty(task: str) -> dict[str, Any]:
    return {
        "schema": SCHEMA,
        "task": task,
        "created_at": datetime.now(timezone.utc).isoformat(),
        "paths": [],
    }


def load(path: Path | str) -> dict[str, Any]:
    target = Path(path)
    if not target.is_file():
        return _empty("")
    data = json.loads(target.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise ValueError("ownership manifest must be an object")
    data.setdefault("paths", [])
    if not isinstance(data["paths"], list):
        raise ValueError("paths must be a list")
    return data


def record(
    task: str,
    path: Path | str,
    *,
    kind: str = "worktree",
    manifest_path: Path | str | None = None,
    environ: dict[str, str] | None = None,
    checkpoints_dir: Path | str | None = None,
) -> Path:
    """Append ``path`` to the task ownership manifest (deduped)."""
    target = (
        Path(manifest_path)
        if manifest_path is not None
        else default_manifest_path(task, environ=environ, checkpoints_dir=checkpoints_dir)
    )
    target.parent.mkdir(parents=True, exist_ok=True)
    data = load(target) if target.is_file() else _empty(task)
    data["task"] = task or data.get("task") or ""
    abs_path = str(Path(path).resolve())
    existing = {str(entry.get("path")) for entry in data["paths"] if isinstance(entry, dict)}
    if abs_path not in existing:
        data["paths"].append(
            {
                "path": abs_path,
                "kind": kind,
                "recorded_at": datetime.now(timezone.utc).isoformat(),
            }
        )
    target.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")
    return target


def listed_paths(manifest_path: Path | str) -> list[str]:
    data = load(manifest_path)
    return [str(entry.get("path")) for entry in data.get("paths", []) if isinstance(entry, dict)]


def allows_cleanup(manifest_path: Path | str, path: Path | str) -> bool:
    """True only when ``path`` is an exact recorded entry."""
    abs_path = str(Path(path).resolve())
    return abs_path in listed_paths(manifest_path)
