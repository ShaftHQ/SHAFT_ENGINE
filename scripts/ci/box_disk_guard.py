#!/usr/bin/env python3
"""Executable shared-box disk gate, worktree cap, cache caps, and scratch TTL.

Extends the parallel-streams protocol from advisory docs into callable
gates (cleanup-at-close). Delete-guard never-touch paths stay refused.
Cache reclaim uses only allowlisted tool-native prune commands (never raw
``rm``). Git worktree / branch cleanup stays in ``worktree_hygiene``
proofs; this module does not delete worktrees. It reports stale tip-on-origin
candidates and classifies intermediates vs finals for the close gate.
"""
from __future__ import annotations

import os
import shutil
import subprocess  # nosec B404 - allowlisted fixed argv only
import sys
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

_DELETE_GUARD_DIR = ROOT / "chaos-engine"
if _DELETE_GUARD_DIR.is_dir() and str(_DELETE_GUARD_DIR) not in sys.path:
    sys.path.insert(0, str(_DELETE_GUARD_DIR))
_DELETE_GUARD_NEVER_TOUCH_AVAILABLE = False
try:
    from delete_guard import never_touch as _imported_delete_guard_never_touch  # noqa: E402

    _delete_guard_never_touch = _imported_delete_guard_never_touch
    _DELETE_GUARD_NEVER_TOUCH_AVAILABLE = True
except ImportError:

    def _delete_guard_never_touch(path: Path) -> str:
        """Fail closed when delete-guard cannot be imported."""
        return "never-touch-check-unavailable"


GiB = 1024**3
# Tiered gate (2026-10-29): warn under 15 GiB; block new workers under 10 GiB.
# Replaces the #7615 doc-only "20 GiB" refuse line with executable thresholds.
FREE_WARN_BYTES = 15 * GiB
FREE_BLOCK_BYTES = 10 * GiB
# Linked worktrees per primary (excludes the main worktree). Crisis: 140 stale.
MAX_LINKED_WORKTREES = 24
DEFAULT_SCRATCH_TTL_HOURS = 24
OWN_SCRATCH_MARKER = ".chaos-engine-owned-scratch"
GIT_TIMEOUT_SECONDS = 30
# Cleanup-at-close: used-percent alert (alongside free-GiB tiers).
DISK_USED_PCT_BLOCKED_INFRA = 80
# Shared venvs live outside per-task trees (guidance constant).
SHARED_VENV_ROOT = "~/.cache/chaos-engine/venvs"
# Tip-on-origin linked worktrees older than this are cleanup candidates.
STALE_TIP_ON_ORIGIN_HOURS = 24
# Path classifiers for cleanup-at-close (name / suffix heuristics only).
INTERMEDIATE_NAME_MARKERS = (
    "cut.mp4",
    ".venv-",
    "_chrome_",
)
INTERMEDIATE_DIR_NAMES = frozenset({"work", "frames", "proof-frames", "proof_frames"})
INTERMEDIATE_SUFFIXES = frozenset({".wav", ".pcm", ".aup", ".aup3"})
FINAL_SUFFIXES = frozenset({".mp4", ".png", ".jpg", ".jpeg", ".webp"})
FINAL_NAME_MARKERS = (
    "signed",
    "final",
    "release",
)

# Tool-native prune only. Empty prune_argv => report-only over-cap (no rm).
@dataclass(frozen=True)
class CacheSpec:
    """One shared host cache with a size cap and optional native prune argv."""

    name: str
    relative_path: str
    max_bytes: int
    prune_argv: tuple[str, ...]


CACHE_SPECS = (
    CacheSpec("pip", ".cache/pip", 2 * GiB, ("pip", "cache", "purge")),
    CacheSpec("yarn", ".cache/yarn", 2 * GiB, ("yarn", "cache", "clean")),
    CacheSpec("npm", ".npm", 1 * GiB, ("npm", "cache", "clean", "--force")),
    CacheSpec("uv", ".cache/uv", 1 * GiB, ("uv", "cache", "prune")),
    CacheSpec("uv-share", ".local/share/uv", 1 * GiB, ("uv", "cache", "prune")),
    CacheSpec("gradle", ".gradle", 2 * GiB, ()),  # no safe tool-native size prune
    CacheSpec("m2", ".m2/repository", 2 * GiB, ()),
    CacheSpec("ce-cache", ".cache/chaos-engine", 512 * 1024**2, ()),
    CacheSpec("ce-share", ".local/share/chaos-engine", 512 * 1024**2, ()),
)


def free_bytes(path: Path | str) -> int:
    """Return free bytes on the filesystem that holds ``path``."""
    return int(shutil.disk_usage(Path(path)).free)


def disk_level(path: Path | str, *, free: int | None = None) -> str:
    """Return ``ok``, ``warn`` (<15 GiB), or ``block`` (<10 GiB)."""
    available = free_bytes(path) if free is None else int(free)
    if available < FREE_BLOCK_BYTES:
        return "block"
    if available < FREE_WARN_BYTES:
        return "warn"
    return "ok"


def never_touch_blockers(path: Path | str) -> list[str]:
    """Refuse protected paths; fail closed if delete-guard is unavailable."""
    resolved = Path(path).resolve()
    if not _DELETE_GUARD_NEVER_TOUCH_AVAILABLE:
        return ["never-touch-check-unavailable"]
    verdict = _delete_guard_never_touch(resolved)
    if verdict:
        return ["never-touch-path"]
    return []


def _git_executable() -> str | None:
    """Resolve git once for subprocess argv (bandit B607)."""
    return shutil.which("git")


def _git(cwd: Path, *arguments: str) -> str | None:
    git = _git_executable()
    if not git:
        return None
    try:
        completed = subprocess.run(  # nosec B603 - fixed git argv
            [git, *arguments],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=GIT_TIMEOUT_SECONDS,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    if completed.returncode != 0:
        return None
    return completed.stdout


def linked_worktree_count(repo_root: Path | str) -> int | None:
    """Count linked worktrees (exclude the primary). None if git cannot answer."""
    root = Path(repo_root).resolve()
    output = _git(root, "worktree", "list", "--porcelain")
    if output is None:
        return None
    paths = [
        line[len("worktree ") :]
        for line in output.splitlines()
        if line.startswith("worktree ")
    ]
    if not paths:
        return 0
    primary = str(root)
    linked = 0
    for item in paths:
        if Path(item).resolve() != Path(primary).resolve():
            linked += 1
    return linked


def new_worker_blockers(
    workspace: Path | str,
    *,
    repo_root: Path | str | None = None,
    free: int | None = None,
    linked_count: int | None = None,
    max_linked: int = MAX_LINKED_WORKTREES,
) -> list[str]:
    """Return reasons a new worktree/clone/worker must not start."""
    blockers: list[str] = []
    level = disk_level(workspace, free=free)
    if level == "block":
        blockers.append("disk-free-below-10giB")
    if repo_root is not None:
        count = (
            linked_count
            if linked_count is not None
            else linked_worktree_count(repo_root)
        )
        if count is None:
            blockers.append("worktree-count-unavailable")
        elif count >= max_linked:
            blockers.append("linked-worktree-cap")
    return blockers


def refuse_new_worker(
    workspace: Path | str,
    *,
    repo_root: Path | str | None = None,
    free: int | None = None,
    linked_count: int | None = None,
    max_linked: int = MAX_LINKED_WORKTREES,
) -> bool:
    """True when a new worker/worktree/clone must be refused."""
    return bool(
        new_worker_blockers(
            workspace,
            repo_root=repo_root,
            free=free,
            linked_count=linked_count,
            max_linked=max_linked,
        )
    )


def disk_warn(workspace: Path | str, *, free: int | None = None) -> bool:
    """True when free space is under the 15 GiB warn threshold (includes block)."""
    return disk_level(workspace, free=free) in {"warn", "block"}


def _dir_size_bytes(path: Path) -> int:
    if not path.exists():
        return 0
    total = 0
    if path.is_file():
        return path.stat().st_size
    for root, _dirs, files in os.walk(path, followlinks=False):
        for name in files:
            try:
                total += (Path(root) / name).stat().st_size
            except OSError:
                continue
    return total


def cache_path(spec: CacheSpec, home: Path | None = None) -> Path:
    """Resolve one cache path under ``home`` (default: ``Path.home()``)."""
    base = Path.home() if home is None else Path(home)
    return (base / spec.relative_path).expanduser()


def cache_inventory(home: Path | None = None) -> list[dict]:
    """Return size and over-cap status for each known shared cache."""
    rows: list[dict] = []
    for spec in CACHE_SPECS:
        path = cache_path(spec, home=home)
        size = _dir_size_bytes(path)
        rows.append(
            {
                "name": spec.name,
                "path": str(path),
                "bytes": size,
                "max_bytes": spec.max_bytes,
                "over_cap": size > spec.max_bytes,
                "prune_argv": list(spec.prune_argv),
                "prune_available": bool(spec.prune_argv),
            }
        )
    return rows


def caches_over_cap(home: Path | None = None) -> list[dict]:
    """Return inventory rows whose size exceeds the configured cap."""
    return [row for row in cache_inventory(home=home) if row["over_cap"]]


def prune_cache(
    name: str,
    *,
    home: Path | None = None,
    runner: Callable[..., subprocess.CompletedProcess] | None = None,
) -> dict:
    """Run the allowlisted tool-native prune for one cache, or refuse.

    Never uses ``rm``. Caches without a native prune command return
    ``prune-unavailable`` so an orchestrator can route regenerable reclaim
    through delete-check instead of inventing a delete here.
    """
    specs = {spec.name: spec for spec in CACHE_SPECS}
    if name not in specs:
        return {"name": name, "status": "unknown-cache", "blockers": ["unknown-cache"]}
    spec = specs[name]
    path = cache_path(spec, home=home)
    blockers = never_touch_blockers(path)
    if blockers:
        return {"name": name, "status": "refused", "blockers": blockers}
    if not spec.prune_argv:
        return {
            "name": name,
            "status": "prune-unavailable",
            "blockers": ["prune-unavailable"],
            "path": str(path),
        }
    run = runner or subprocess.run
    try:
        completed = run(  # nosec B603 - allowlisted argv from CACHE_SPECS only
            list(spec.prune_argv),
            capture_output=True,
            text=True,
            check=False,
            timeout=600,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        return {
            "name": name,
            "status": "prune-failed",
            "blockers": ["prune-failed"],
            "error": str(exc),
        }
    if completed.returncode != 0:
        return {
            "name": name,
            "status": "prune-failed",
            "blockers": ["prune-failed"],
            "returncode": completed.returncode,
        }
    return {"name": name, "status": "pruned", "blockers": [], "path": str(path)}


def own_scratch_ttl_candidates(
    roots: Sequence[Path | str],
    *,
    ttl_hours: float = DEFAULT_SCRATCH_TTL_HOURS,
    now_epoch: float | None = None,
    marker_name: str = OWN_SCRATCH_MARKER,
) -> list[dict]:
    """List own-created scratch dirs older than TTL (candidates only).

    A directory qualifies only when it contains ``marker_name`` (owner proof).
    Never-touch / delete-guard failures are excluded. This function never
    deletes; callers must use delete-check for filesystem reclaim.
    """
    import time

    now = time.time() if now_epoch is None else float(now_epoch)
    cutoff = now - (float(ttl_hours) * 3600)
    found: list[dict] = []
    for root in roots:
        base = Path(root)
        if not base.is_dir():
            continue
        try:
            children = list(base.iterdir())
        except OSError:
            continue
        for child in children:
            if not child.is_dir():
                continue
            marker = child / marker_name
            if not marker.is_file():
                continue
            blockers = scratch_ttl_blockers(
                child, ttl_hours=ttl_hours, now_epoch=now, marker_name=marker_name
            )
            if blockers:
                continue
            try:
                mtime = child.stat().st_mtime
            except OSError:
                continue
            if mtime > cutoff:
                continue
            found.append(
                {
                    "path": str(child.resolve()),
                    "mtime": mtime,
                    "ttl_hours": ttl_hours,
                }
            )
    return found


def scratch_ttl_blockers(
    path: Path | str,
    *,
    ttl_hours: float = DEFAULT_SCRATCH_TTL_HOURS,
    now_epoch: float | None = None,
    marker_name: str = OWN_SCRATCH_MARKER,
) -> list[str]:
    """Return blockers that forbid treating ``path`` as own TTL scratch."""
    import time

    target = Path(path)
    blockers: list[str] = []
    blockers.extend(never_touch_blockers(target))
    if not (target / marker_name).is_file():
        blockers.append("not-own-scratch")
    try:
        mtime = target.stat().st_mtime
    except OSError:
        blockers.append("stat-unavailable")
        return list(dict.fromkeys(blockers))
    now = time.time() if now_epoch is None else float(now_epoch)
    if mtime > now - (float(ttl_hours) * 3600):
        blockers.append("within-ttl")
    return list(dict.fromkeys(blockers))



def disk_used_percent(path: Path | str, *, used: int | None = None, total: int | None = None) -> float:
    """Return disk used percent for the filesystem that holds ``path``."""
    if used is None or total is None:
        usage = shutil.disk_usage(Path(path))
        used_bytes = int(usage.used)
        total_bytes = int(usage.total)
    else:
        used_bytes = int(used)
        total_bytes = int(total)
    if total_bytes <= 0:
        return 100.0
    return (used_bytes / total_bytes) * 100.0


def blocked_infra_reasons(
    path: Path | str,
    *,
    free: int | None = None,
    used_percent: float | None = None,
    used: int | None = None,
    total: int | None = None,
) -> list[str]:
    """Return BLOCKED-INFRA reasons (free-GiB warn/block and ≥80% used).

    Free-GiB tiers still refuse new workers via ``refuse_new_worker``. This
    helper is the board/alert surface: any non-empty result means set
    BLOCKED-INFRA and stop starting heavy work.
    """
    reasons: list[str] = []
    level = disk_level(path, free=free)
    if level == "block":
        reasons.append("disk-free-below-10giB")
    elif level == "warn":
        reasons.append("disk-free-below-15giB")
    pct = (
        float(used_percent)
        if used_percent is not None
        else disk_used_percent(path, used=used, total=total)
    )
    if pct >= DISK_USED_PCT_BLOCKED_INFRA:
        reasons.append("disk-used-pct-ge-80")
    return reasons


def classify_cleanup_path(path: Path | str) -> str:
    """Classify a path as ``final``, ``intermediate``, or ``other``.

    Finals (signed deliverables) must remain until upload verify.
    Intermediates are safe to delete after tip-on-origin / merge verify when
    ownership manifests list them. Never-touch refusal stays in delete-check.
    """
    target = Path(path)
    name = target.name
    lower = name.lower()
    if any(marker in lower for marker in FINAL_NAME_MARKERS) and (
        target.suffix.lower() in FINAL_SUFFIXES or target.suffix.lower() == ".json"
    ):
        return "final"
    if target.suffix.lower() in FINAL_SUFFIXES and "cut." not in lower:
        # Bare media: treat as final unless name markers say intermediate.
        if not any(marker in lower for marker in INTERMEDIATE_NAME_MARKERS):
            return "final"
    if any(marker in lower for marker in INTERMEDIATE_NAME_MARKERS):
        return "intermediate"
    if target.suffix.lower() in INTERMEDIATE_SUFFIXES:
        return "intermediate"
    if name in INTERMEDIATE_DIR_NAMES or lower in INTERMEDIATE_DIR_NAMES:
        return "intermediate"
    if lower.startswith(".venv"):
        return "intermediate"
    return "other"


def stale_tip_on_origin_candidates(
    entries: Sequence[Mapping],
    *,
    now_epoch: float | None = None,
    max_age_hours: float = STALE_TIP_ON_ORIGIN_HOURS,
) -> list[dict]:
    """Report linked worktrees whose tip is on origin and older than max age.

    ``entries`` are dicts with keys: path, tip_on_origin (bool), mtime (epoch),
    own (bool). Only ``own`` + ``tip_on_origin`` rows older than the TTL are
    returned. Report-only: never deletes.
    """
    import time

    now = time.time() if now_epoch is None else float(now_epoch)
    cutoff = now - (float(max_age_hours) * 3600)
    found: list[dict] = []
    for entry in entries:
        if not entry.get("own"):
            continue
        if not entry.get("tip_on_origin"):
            continue
        mtime = entry.get("mtime")
        if mtime is None:
            continue
        if float(mtime) > cutoff:
            continue
        path = entry.get("path")
        if not path:
            continue
        blockers = never_touch_blockers(path)
        if blockers:
            continue
        found.append(
            {
                "path": str(path),
                "mtime": float(mtime),
                "max_age_hours": float(max_age_hours),
                "action": "suggest-remove",
            }
        )
    return found



def youtube_replace_cleanup_allowed(
    *,
    replacement_watch_ok: bool,
    replacement_schedule_ok: bool,
    outdated_video_id: str,
) -> list[str]:
    """Return blockers that forbid deleting an outdated YouTube upload.

    Deletion is allowed only when the replacement is verified live (watch page
    and schedule both OK) and an outdated video id is named. Report-only
    helper — callers still go through platform APIs + delete-check policy.
    """
    blockers: list[str] = []
    if not replacement_watch_ok:
        blockers.append("replacement-watch-not-verified")
    if not replacement_schedule_ok:
        blockers.append("replacement-schedule-not-verified")
    vid = (outdated_video_id or "").strip()
    if not vid:
        blockers.append("missing-outdated-video-id")
    return blockers


def log_deleted_youtube_ids(
    video_ids: Sequence[str],
    *,
    log_path: Path | str,
    now_iso: str | None = None,
) -> dict:
    """Append deleted YouTube video IDs to an append-only receipt log.

    Never deletes videos itself. Creates parent dirs as needed. Returns the
    number of IDs logged.
    """
    from datetime import datetime, timezone

    stamp = now_iso or datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    target = Path(log_path)
    target.parent.mkdir(parents=True, exist_ok=True)
    cleaned = [vid.strip() for vid in video_ids if vid and str(vid).strip()]
    if not cleaned:
        return {"logged": 0, "path": str(target), "status": "empty"}
    sep = chr(9)
    lines = [f"{stamp}{sep}{vid}" + "\n" for vid in cleaned]
    with target.open("a", encoding="utf-8") as handle:
        handle.writelines(lines)
    return {"logged": len(cleaned), "path": str(target), "status": "appended", "ids": cleaned}



def verify_live_item_state_before_status(
    *,
    proposed_status: str,
    live_state: str,
) -> list[str]:
    """Block stale board/status writes that contradict live gh state.

    Call at write time with a freshly fetched ``live_state`` (e.g. from
    ``gh pr view --json state,statusCheckRollup,autoMergeRequest``). Refuses
    writing "merge queue" / "in queue" when the PR is already MERGED, and
    refuses "open" claims when live state is MERGED/CLOSED.
    """
    proposed = (proposed_status or "").strip().casefold()
    live = (live_state or "").strip().casefold()
    blockers: list[str] = []
    if not live:
        blockers.append("live-state-unverified")
        return blockers
    if live in {"merged", "closed"} and (
        "merge queue" in proposed or "mergequeue" in proposed.replace(" ", "") or proposed in {"queued", "in queue", "open", "draft"}
    ):
        blockers.append(f"stale-status:{proposed_status!r}-but-live-{live_state!r}")
    if "merged" in proposed and live not in {"merged"}:
        blockers.append(f"unverified-merged-claim:live-{live_state!r}")
    return blockers


def doctor_dirt_close_blockers(
    *,
    porcelain: str,
    doctor_touched_paths: Sequence[str] | None = None,
) -> list[str]:
    """Refuse item close while doctor/repair dirt remains uncommitted/unreverted.

    Workers must commit doctor/repair tracked edits through the normal PR path
    or revert them before close. Session logs under the shared workspace root must be removed
    or moved into a scoped temp dir (recorded in the session created-paths
    manifest) — not left at close.
    """
    blockers: list[str] = []
    lines = [line for line in (porcelain or "").splitlines() if line.strip()]
    if lines:
        blockers.append("git-status-dirty")
    touched = [str(p) for p in (doctor_touched_paths or []) if str(p).strip()]
    if touched and lines:
        blockers.append("doctor-repair-dirt-unresolved")
    return blockers


def item_close_blockers(
    *,
    git_status_clean: bool,
    created_files: Sequence[str] | None = None,
    created_files_accounted: bool = False,
    session_logs_cleared: bool = True,
) -> list[str]:
    """Cleanup-at-close gate: clean git status plus accounted created-files list.

    ``created_files`` is the ownership / session-manifest list of paths the
    worker created. ``created_files_accounted`` means each entry was deleted,
    archived, or explicitly retained as a final. Session logs under the shared workspace root
    must be cleared or moved to scoped temp.
    """
    blockers: list[str] = []
    if not git_status_clean:
        blockers.append("git-status-not-clean")
    files = [str(p) for p in (created_files or []) if str(p).strip()]
    if files and not created_files_accounted:
        blockers.append("created-files-unaccounted")
    if not session_logs_cleared:
        blockers.append("session-logs-remain")
    return blockers


def post_merge_dod_required() -> Mapping[str, str]:
    """Contract strings every bot DoD must satisfy after MERGED delivery."""
    return {
        "cleanup": "post-merge own cleanup",
        "hygiene": "scripts/ci/worktree_hygiene.py",
        "disk_guard": "scripts/ci/box_disk_guard.py",
        "order": (
            "MERGED+verified → finals hold until upload verified → "
            "own intermediates delete → own worktree/branch cleanup via hygiene "
            "proofs → disk warn/80pct/cache inventory → Learning Session → "
            "tool.py maintain"
        ),
        "blocked_infra": "blocked_infra_reasons (free GiB warn/block + used≥80%)",
        "shared_venv": SHARED_VENV_ROOT,
        "live_status": "verify_live_item_state_before_status (gh at write time)",
        "doctor_dirt": "commit through PR or revert before close; no dirty tree",
        "close_gate": "item_close_blockers (git clean + created files accounted)",
        "session_logs": "remove or move shared-workspace session logs to scoped temp at close",
    }


if __name__ == "__main__":
    raise SystemExit("import box_disk_guard; do not run as a script")
