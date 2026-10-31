"""L75: never probe write tools on live user files — scratch only."""

from __future__ import annotations

from pathlib import Path
from typing import Any

_WRITE_ACTIONS = frozenset({
    "write", "update", "update_file_content", "delete", "overwrite", "upload",
    "patch", "create", "move", "rename",
})


def is_scratch_path(path: str | Path, *, scratch_roots: list[str] | None = None) -> bool:
    text = str(path).replace("\\", "/")
    roots = scratch_roots or ["/tmp/", "/workspace/"]
    return any(text.startswith(root.rstrip("/") + "/") or text == root.rstrip("/") for root in roots)


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
    text = str(path).replace("\\", "/")
    lives = live_roots or []
    if lives and any(text.startswith(root.rstrip("/") + "/") or text.startswith(root) for root in lives):
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
