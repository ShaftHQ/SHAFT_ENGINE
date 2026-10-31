"""
File-claim lock: one writer per file path; one task of a kind (WoW §3).

Workers claim exclusive writable paths before edit. Overlapping active claims
refuse. Complements ``parallel_stream_conflict.py`` (stream ownership) with a
lightweight per-path claim file used by bots and sub-workers.
"""
from __future__ import annotations

import json
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

SCHEMA = 1
ENV_CLAIMS = "CHAOS_FILE_CLAIMS_PATH"
DEFAULT_NAME = "file-claims.json"


def default_claims_path(
    *,
    environ: dict[str, str] | None = None,
    checkpoints_dir: Path | str | None = None,
) -> Path:
    env = environ if environ is not None else os.environ
    override = str(env.get(ENV_CLAIMS, "")).strip()
    if override:
        return Path(override)
    if checkpoints_dir is not None:
        base = Path(checkpoints_dir)
    else:
        configured = str(env.get("CHAOS_CHECKPOINTS_DIR", "")).strip()
        base = Path(configured) if configured else Path("/workspace/checkpoints")
    return base / DEFAULT_NAME


def _now() -> str:
    return datetime.now(timezone.utc).isoformat()


def _empty() -> dict[str, Any]:
    return {"schema": SCHEMA, "updated_at": _now(), "claims": []}


def load(path: Path | str) -> dict[str, Any]:
    target = Path(path)
    if not target.is_file():
        return _empty()
    data = json.loads(target.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise ValueError("claims file must be an object")
    data.setdefault("claims", [])
    if not isinstance(data["claims"], list):
        raise ValueError("claims must be a list")
    return data


def save(path: Path | str, data: dict[str, Any]) -> None:
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    payload = dict(data)
    payload["schema"] = SCHEMA
    payload["updated_at"] = _now()
    target.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def _project_root(
    *,
    project_root: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> Path | None:
    if project_root is not None:
        return Path(project_root)
    env = environ if environ is not None else os.environ
    configured = str(env.get("CHAOS_PROJECT_ROOT", "")).strip()
    return Path(configured) if configured else None


def _norm(
    path: str | Path,
    *,
    project_root: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> str:
    """Normalize so rel and abs forms of the same file collide.

    Resolves against ``project_root`` / ``CHAOS_PROJECT_ROOT`` when relative;
    always ``normpath``; ``resolve`` when the target exists.
    """
    raw = str(path).strip()
    if not raw:
        return ""
    root = _project_root(project_root=project_root, environ=environ)
    candidate = Path(raw)
    if not candidate.is_absolute() and root is not None:
        candidate = root / candidate
    candidate = Path(os.path.normpath(str(candidate)))
    try:
        if candidate.exists():
            candidate = candidate.resolve()
    except (OSError, RuntimeError):
        # Path may be a not-yet-created claim target; keep normpath form.
        pass
    return str(candidate).replace(chr(92), "/").rstrip("/")


def paths_overlap(
    left: str,
    right: str,
    *,
    project_root: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> bool:
    a = _norm(left, project_root=project_root, environ=environ)
    b = _norm(right, project_root=project_root, environ=environ)
    if not a or not b:
        return False
    return a == b or a.startswith(b + "/") or b.startswith(a + "/")


def active_claims(data: dict[str, Any]) -> list[dict[str, Any]]:
    out: list[dict[str, Any]] = []
    for row in data.get("claims", []):
        if not isinstance(row, dict):
            continue
        status = str(row.get("status", "active")).casefold()
        if status in {"done", "released", "cancelled", "canceled"}:
            continue
        out.append(row)
    return out


def conflicting_claims(
    proposed_paths: list[str],
    data: dict[str, Any],
    *,
    exclude_claim_id: str | None = None,
    exclude_owner: str | None = None,
    project_root: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> list[dict[str, Any]]:
    """Return active claims that overlap *proposed_paths*."""
    conflicts: list[dict[str, Any]] = []
    exclude_id = (exclude_claim_id or "").strip()
    exclude_own = (exclude_owner or "").strip()
    for row in active_claims(data):
        claim_id = str(row.get("id") or row.get("claim_id") or "")
        owner = str(row.get("owner") or row.get("worker") or "")
        if exclude_id and claim_id == exclude_id:
            continue
        if exclude_own and owner and owner == exclude_own and claim_id == exclude_id:
            continue
        owned = row.get("paths") or row.get("owned_paths") or []
        if isinstance(owned, str):
            owned = [owned]
        if not isinstance(owned, list):
            continue
        for prop in proposed_paths:
            if any(paths_overlap(prop, str(peer), project_root=project_root, environ=environ) for peer in owned):
                conflicts.append(row)
                break
    return conflicts


def refuse_claim(
    proposed_paths: list[str],
    data: dict[str, Any],
    *,
    exclude_claim_id: str | None = None,
    project_root: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> bool:
    return bool(
        conflicting_claims(
            proposed_paths,
            data,
            exclude_claim_id=exclude_claim_id,
            project_root=project_root,
            environ=environ,
        )
    )


def kinds_in_progress(data: dict[str, Any]) -> dict[str, list[str]]:
    """Map kind → list of active claim ids (one task of a kind)."""
    by_kind: dict[str, list[str]] = {}
    for row in active_claims(data):
        kind = str(row.get("kind") or "").strip().casefold()
        if not kind:
            continue
        claim_id = str(row.get("id") or row.get("claim_id") or "?")
        by_kind.setdefault(kind, []).append(claim_id)
    return by_kind


def refuse_same_kind(
    kind: str,
    data: dict[str, Any],
    *,
    exclude_claim_id: str | None = None,
) -> bool:
    """True when another active claim already holds this kind (WoW §3a)."""
    needle = (kind or "").strip().casefold()
    if not needle:
        return False
    exclude = (exclude_claim_id or "").strip()
    for row in active_claims(data):
        claim_id = str(row.get("id") or row.get("claim_id") or "")
        if exclude and claim_id == exclude:
            continue
        if str(row.get("kind") or "").strip().casefold() == needle:
            return True
    return False


def claim(
    claim_id: str,
    owner: str,
    paths: list[str],
    *,
    kind: str = "",
    path: Path | str | None = None,
    environ: dict[str, str] | None = None,
    project_root: Path | str | None = None,
) -> dict[str, Any]:
    """Record an exclusive claim; raise ``PermissionError`` on overlap / kind WIP."""
    target = Path(path) if path is not None else default_claims_path(environ=environ)
    data = load(target)
    if refuse_claim(
        paths, data, exclude_claim_id=claim_id,
        project_root=project_root, environ=environ,
    ):
        peers = conflicting_claims(
            paths, data, exclude_claim_id=claim_id,
            project_root=project_root, environ=environ,
        )
        peer_ids = [str(p.get("id") or p.get("claim_id") or "?") for p in peers]
        raise PermissionError(
            f"file-claim overlap with active claim(s): {', '.join(peer_ids)}"
        )
    if kind and refuse_same_kind(kind, data, exclude_claim_id=claim_id):
        raise PermissionError(
            f"one-task-of-a-kind: kind={kind!r} already in progress"
        )
    # upsert
    claims = [row for row in data.get("claims", []) if isinstance(row, dict)
              and str(row.get("id") or row.get("claim_id")) != claim_id]
    claims.append(
        {
            "id": claim_id,
            "owner": owner,
            "paths": [
                _norm(p, project_root=project_root, environ=environ) for p in paths
            ],
            "kind": kind,
            "status": "active",
            "claimed_at": _now(),
        }
    )
    data["claims"] = claims
    save(target, data)
    return data


def release(
    claim_id: str,
    *,
    path: Path | str | None = None,
    environ: dict[str, str] | None = None,
) -> dict[str, Any]:
    target = Path(path) if path is not None else default_claims_path(environ=environ)
    data = load(target)
    for row in data.get("claims", []):
        if not isinstance(row, dict):
            continue
        if str(row.get("id") or row.get("claim_id")) == claim_id:
            row["status"] = "released"
            row["released_at"] = _now()
    save(target, data)
    return data
