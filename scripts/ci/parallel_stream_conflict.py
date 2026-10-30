#!/usr/bin/env python3
"""Refuse parallel-stream dispatch on ownership overlap, design-kind WIP, or disk caps."""
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


# Design kinds that share templates/gates so one defect multiplies across a batch.
DESIGN_KINDS = frozenset({"video", "podcast", "image", "website"})

_KIND_ALIASES = {
    "podcast chapter": "podcast",
    "podcast_chapter": "podcast",
    "website change": "website",
    "website_change": "website",
    "web": "website",
}


def _peer_kind(peer: Mapping[str, Any]) -> str | None:
    """Return normalized design kind from a manifest row, or None if absent/unknown."""
    raw = peer.get("kind")
    if raw is None:
        raw = peer.get("design_kind")
    if raw is None:
        raw = peer.get("designKind")
    if raw is None:
        return None
    kind = str(raw).strip().casefold()
    if not kind:
        return None
    kind = _KIND_ALIASES.get(kind, kind)
    return kind if kind in DESIGN_KINDS else kind


def _peer_first_pass_streak(peer: Mapping[str, Any]) -> int | None:
    """Consecutive first-pass sign-offs for this kind, or None if unset.

    Manifest may carry ``first_pass_signoff_streak`` (or camelCase). Explicit
    ``0`` (after a reject) must win over a peer's stale streak.
    """
    for key in (
        "first_pass_signoff_streak",
        "firstPassSignoffStreak",
        "kind_first_pass_streak",
        "kindFirstPassStreak",
    ):
        if key not in peer:
            continue
        value = peer.get(key)
        if value is None:
            return 0
        try:
            return max(0, int(value))
        except (TypeError, ValueError):
            return 0
    return None


def kind_wip_limit(
    kind: str | None,
    *,
    first_pass_signoff_streak: int = 0,
) -> int:
    """WIP cap for a design kind: 1 by default; 2 after two consecutive first-pass signs.

    Any reject drops the streak (orchestrator sets streak to 0); this helper only
    maps streak → limit.
    """
    if kind is None:
        return 1
    if first_pass_signoff_streak >= 2:
        return 2
    return 1


def active_peers_of_kind(
    peer_streams: Sequence[Mapping[str, Any]],
    kind: str,
    *,
    exclude_stream_id: str | None = None,
) -> list[str]:
    """Active peer stream ids whose ``kind`` matches (case-insensitive)."""
    wanted = _KIND_ALIASES.get(str(kind).strip().casefold(), str(kind).strip().casefold())
    exclude = (exclude_stream_id or "").strip()
    hits: list[str] = []
    for peer in peer_streams:
        if not isinstance(peer, Mapping):
            continue
        peer_id = _peer_id(peer)
        if exclude and peer_id == exclude:
            continue
        if not _peer_active(peer):
            continue
        peer_kind = _peer_kind(peer)
        if peer_kind is None:
            continue
        if peer_kind == wanted:
            hits.append(peer_id)
    return list(dict.fromkeys(hits))


def refuse_dispatch_on_same_kind_wip(
    proposed: Mapping[str, Any],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
) -> bool:
    """True when dispatch would exceed the design-kind WIP cap.

    - No ``kind`` on the proposal → not a design-kind conflict (path check only).
    - Count active peers of the same kind; refuse when count >= kind WIP limit.
    - Limit is 1, or 2 when ``first_pass_signoff_streak >= 2`` on the proposal
      (or any peer row carrying the kind's streak — proposal wins when set).
    """
    kind = _peer_kind(proposed)
    if kind is None:
        return False
    streak = _peer_first_pass_streak(proposed)
    if streak is None:
        streak = 0
        for peer in peer_streams:
            if not isinstance(peer, Mapping):
                continue
            if _peer_kind(peer) != kind:
                continue
            peer_streak = _peer_first_pass_streak(peer)
            if peer_streak is not None:
                streak = max(streak, peer_streak)
    limit = kind_wip_limit(kind, first_pass_signoff_streak=streak)
    exclude = exclude_stream_id or _peer_id(proposed)
    if exclude == "?":
        exclude = None
    in_flight = active_peers_of_kind(
        peer_streams, kind, exclude_stream_id=exclude
    )
    return len(in_flight) >= limit


def conflicting_same_kind_stream_ids(
    proposed: Mapping[str, Any],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
) -> list[str]:
    """Peer ids that block dispatch under the design-kind WIP rule.

    Empty when under the WIP cap (including different kinds / no kind).
    """
    if not refuse_dispatch_on_same_kind_wip(
        proposed, peer_streams, exclude_stream_id=exclude_stream_id
    ):
        return []
    kind = _peer_kind(proposed)
    if kind is None:
        return []
    exclude = exclude_stream_id or _peer_id(proposed)
    if exclude == "?":
        exclude = None
    return active_peers_of_kind(peer_streams, kind, exclude_stream_id=exclude)


def refuse_dispatch(
    proposed_owned_globs: Sequence[str],
    peer_streams: Sequence[Mapping[str, Any]],
    *,
    exclude_stream_id: str | None = None,
    workspace: Path | str | None = None,
    repo_root: Path | str | None = None,
    free: int | None = None,
    linked_count: int | None = None,
    proposed: Mapping[str, Any] | None = None,
) -> list[str]:
    """Return every dispatch blocker: ownership, design-kind WIP, disk/worktree.

    When ``workspace`` is omitted, disk/worktree caps are skipped. When
    ``proposed`` is omitted, design-kind WIP is skipped (path-only callers).
    Pass both before creating a new worktree or design stream.
    """
    blockers: list[str] = []
    conflicts = conflicting_peer_stream_ids(
        proposed_owned_globs,
        peer_streams,
        exclude_stream_id=exclude_stream_id,
    )
    if conflicts:
        blockers.append("ownership-overlap:" + ",".join(conflicts))
    if proposed is not None:
        kind_hits = conflicting_same_kind_stream_ids(
            proposed,
            peer_streams,
            exclude_stream_id=exclude_stream_id,
        )
        if kind_hits:
            blockers.append("same-kind-wip:" + ",".join(kind_hits))
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
    "DESIGN_KINDS",
    "active_peers_of_kind",
    "conflicting_peer_stream_ids",
    "conflicting_same_kind_stream_ids",
    "disk_warn",
    "kind_wip_limit",
    "refuse_dispatch",
    "refuse_dispatch_on_ownership_overlap",
    "refuse_dispatch_on_same_kind_wip",
    "refuse_new_worker",
    "writable_globs_overlap",
]


if __name__ == "__main__":
    raise SystemExit("import parallel_stream_conflict; do not run as a script")
