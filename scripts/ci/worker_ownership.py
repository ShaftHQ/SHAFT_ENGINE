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
# Temporary artifacts a task may register for cleanup-at-close (#7755).
ARTIFACT_KINDS = frozenset({
    "worktree",
    "clone",
    "venv",
    "build_output",
    "report_server",
    "browser",
    "scratch",
})


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
    kind_norm = (kind or "worktree").strip()
    if kind_norm not in ARTIFACT_KINDS:
        raise ValueError(f"unknown artifact kind: {kind!r}; expected one of {sorted(ARTIFACT_KINDS)}")
    kind = kind_norm
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

def mark_accounted(
    manifest_path: Path | str,
    path: Path | str,
    *,
    disposition: str = "removed",
) -> None:
    """Mark a recorded path accounted (removed / archived / retained-final)."""
    target = Path(manifest_path)
    data = load(target)
    abs_path = str(Path(path).resolve())
    updated = False
    for entry in data.get("paths", []):
        if not isinstance(entry, dict):
            continue
        if str(entry.get("path")) != abs_path:
            continue
        entry["accounted"] = True
        entry["disposition"] = disposition
        entry["accounted_at"] = datetime.now(timezone.utc).isoformat()
        updated = True
    if not updated:
        raise KeyError(f"path not in ownership manifest: {abs_path}")
    target.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")


def remaining_artifacts(manifest_path: Path | str) -> list[dict]:
    """Return recorded entries that still exist on disk and are not accounted."""
    data = load(manifest_path)
    leftover: list[dict] = []
    for entry in data.get("paths", []):
        if not isinstance(entry, dict):
            continue
        if entry.get("accounted"):
            continue
        raw = str(entry.get("path") or "").strip()
        if not raw:
            continue
        if Path(raw).exists():
            leftover.append(dict(entry))
    return leftover


def remaining_artifact_blockers(manifest_path: Path | str) -> list[str]:
    """Close-gate blockers for unaccounted registered artifacts that still exist."""
    leftover = remaining_artifacts(manifest_path)
    if not leftover:
        return []
    kinds = sorted({str(e.get("kind") or "worktree") for e in leftover})
    return [f"registered-artifacts-remain:{len(leftover)}:{','.join(kinds)}"]

