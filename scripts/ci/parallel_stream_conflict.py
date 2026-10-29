#!/usr/bin/env python3
"""Refuse parallel-stream dispatch on ownership overlap or disk/worktree caps."""
from __future__ import annotations

from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path
from typing import Any

from scripts.ci.box_disk_guard import (  # noqa: E402
    disk_warn,
    new_worker_blockers,
    refuse_new_worker,
)


def _stem(glob: str) -> str:
    """Collapse a writable glob to a comparable path prefix."""
    text = glob.replace("\\", "/").strip()
    for suffix in ("/**/*", "/**", "/*", "/***", "**"):
        if text.endswith(suffix):
            text = text[: -len(suffix)]
            break
    return text.rstrip("/")


def writable_globs_overlap(left: Iterable[str], right: Iterable[str]) -> bool:
    """True when any left writable glob can cover the same paths as any right glob."""
    left_stems = [_stem(item) for item in left if str(item).strip()]
    right_stems = [_stem(item) for item in right if str(item).strip()]
    for candidate in left_stems:
        for peer in right_stems:
            if not candidate or not peer:
                continue
            if candidate == peer or candidate.startswith(peer + "/") or peer.startswith(
                candidate + "/"
            ):
                return True
    return False


def _peer_globs(peer: Mapping[str, Any]) -> list[str]:
    raw = peer.get("owned_path_globs")
    if raw is None:
        raw = peer.get("ownedPathGlobs")
    if raw is None:
        raw = peer.get("owned_globs")
    if isinstance(raw, str):
        return [raw]
    if isinstance(raw, Sequence):
        return [str(item) for item in raw]
    return []


def _peer_id(peer: Mapping[str, Any]) -> str:
    for key in ("stream_id", "id", "streamId", "name"):
        value = peer.get(key)
        if value:
            return str(value)
    return "?"


def _peer_active(peer: Mapping[str, Any]) -> bool:
    status = str(peer.get("status", "active")).strip().casefold()
    return status not in {"done", "merged", "cancelled", "canceled", "shipped"}


def conflicting_peer_stream_ids(
    proposed_owned_globs: Sequence[str],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
) -> list[str]:
    """Return active peer stream ids whose owned globs overlap the proposal.

    Reads the ownership manifest peer rows (not only open PRs / dirty worktrees).
    Empty list means dispatch may proceed on the ownership axis.
    """
    conflicts: list[str] = []
    exclude = (exclude_stream_id or "").strip()
    for peer in peer_streams:
        if not isinstance(peer, Mapping):
            continue
        peer_id = _peer_id(peer)
        if exclude and peer_id == exclude:
            continue
        if not _peer_active(peer):
            continue
        if writable_globs_overlap(proposed_owned_globs, _peer_globs(peer)):
            conflicts.append(peer_id)
    return list(dict.fromkeys(conflicts))


def refuse_dispatch_on_ownership_overlap(
    proposed_owned_globs: Sequence[str],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
) -> bool:
    """True when dispatch must be refused due to ownership-manifest overlap."""
    return bool(
        conflicting_peer_stream_ids(
            proposed_owned_globs,
            peer_streams,
            exclude_stream_id=exclude_stream_id,
        )
    )


def refuse_dispatch(
    proposed_owned_globs: Sequence[str],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
    workspace: Path | str | None = None,
    repo_root: Path | str | None = None,
    free: int | None = None,
    linked_count: int | None = None,
) -> list[str]:
    """Return every dispatch blocker: ownership overlap and disk/worktree caps.

    When ``workspace`` is omitted, only ownership is checked (unit tests /
    callers that gate disk separately). Pass ``workspace`` (and optionally
    ``repo_root``) before creating a new worktree or clone.
    """
    blockers: list[str] = []
    conflicts = conflicting_peer_stream_ids(
        proposed_owned_globs,
        peer_streams,
        exclude_stream_id=exclude_stream_id,
    )
    if conflicts:
        blockers.append("ownership-overlap:" + ",".join(conflicts))
    if workspace is not None:
        blockers.extend(
            new_worker_blockers(
                workspace,
                repo_root=repo_root,
                free=free,
                linked_count=linked_count,
            )
        )
    return blockers


__all__ = [
    "conflicting_peer_stream_ids",
    "disk_warn",
    "refuse_dispatch",
    "refuse_dispatch_on_ownership_overlap",
    "refuse_new_worker",
    "writable_globs_overlap",
]


if __name__ == "__main__":
    raise SystemExit("import parallel_stream_conflict; do not run as a script")
