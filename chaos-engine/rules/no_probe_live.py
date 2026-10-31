"""L75: never probe write tools on live user files — scratch only."""

from __future__ import annotations

from pathlib import Path
from typing import Any

_WRITE_ACTIONS = frozenset({
    "write", "update", "update_file_content", "delete", "overwrite", "upload",
    "patch", "create", "move", "rename",
})


def _resolve(path: str | Path) -> Path | None:
    """Resolve *path*; reject ``..`` escapes that leave the declared prefix tree."""
    text = str(path).replace("\\", "/")
    if not text:
        return None
    # Reject raw .. segments before resolve so relative escapes are obvious.
    parts = Path(text).parts
    if ".." in parts:
        # Still resolve for callers that need the absolute form, but mark unsafe
        # via returning a sentinel only used by is_scratch_path / probe_allowed.
        try:
            return Path(text).resolve()
        except (OSError, RuntimeError):
            return None
    try:
        return Path(text).resolve()
    except (OSError, RuntimeError):
        return None


def _under_root(resolved: Path, root: str | Path) -> bool:
    try:
        root_res = Path(root).resolve()
    except (OSError, RuntimeError):
        return False
    try:
        resolved.relative_to(root_res)
        return True
    except ValueError:
        return False


def is_scratch_path(path: str | Path, *, scratch_roots: list[str] | None = None) -> bool:
    # Path-prefix allowlist for probe targets; does not create temp files.
    roots = scratch_roots or ["/tmp/", "/workspace/"]  # nosec B108
    # Reject any path that still contains .. after string form (escape attempt).
    if ".." in Path(str(path)).parts:
        return False
    resolved = _resolve(path)
    if resolved is None:
        return False
    return any(_under_root(resolved, root) for root in roots)


def probe_allowed(
    action: str,
    path: str | Path,
    *,
    live_roots: list[str] | None = None,
    scratch_roots: list[str] | None = None,
    allow_live: bool = False,
) -> tuple[bool, str]:
    """Return (ok, reason). Write probes on live paths refuse unless allow_live."""
    act = (action or "").strip().casefold()
    if act not in _WRITE_ACTIONS:
        return True, "non-write"
    if allow_live:
        return True, "allow_live"
    if ".." in Path(str(path)).parts:
        return False, "probe-write-path-escape"
    resolved = _resolve(path)
    if resolved is None:
        return False, "probe-write-unresolvable"
    lives = live_roots or []
    if lives and any(_under_root(resolved, root) for root in lives):
        return False, "probe-write-on-live-path"
    if not is_scratch_path(path, scratch_roots=scratch_roots):
        # Outside known scratch → treat as live-ish fail-closed for write probes.
        return False, "probe-write-outside-scratch"
    return True, "scratch"


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    if not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"]}
    hits: list[str] = []
    action = str(obs.get("action") or obs.get("tool_action") or "")
    path = obs.get("path") or obs.get("target_path") or ""
    ok, reason = probe_allowed(
        action,
        path,
        live_roots=list(obs.get("live_roots") or []),
        scratch_roots=list(obs.get("scratch_roots") or []) or None,
        allow_live=obs.get("allow_live") is True,
    )
    if not ok:
        hits.append(reason)
    return {"ok": not hits, "hits": hits, "reason": reason}
