"""HRSD J11 retro harness helpers (issues 7493-7499).

Pure helpers so mobile / backend executor loops can freeze evidence, classify
backend gates as Blocked early, time-cap a failing primary path, keep a tracked
NO_PUSH guard, pin evidence to build+branch, and key NamedBlock on gap ids.
"""

from __future__ import annotations

import shutil
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable

NO_PUSH_GUARD_REL = "scripts/ci/no_push_guard.py"


@dataclass(frozen=True)
class EvidencePin:
    """Immutable identity of the green evidence cohort (issue 7498)."""

    build_id: str
    branch: str


@dataclass(frozen=True)
class NamedBlock:
    """One block keyed by gap id, not scenario id (issue 7499)."""

    gap_id: str
    scenarios: tuple[str, ...]
    reason: str


def freeze_results_snapshot(results_dir: Path, snapshot_dir: Path, *, green: bool) -> Path | None:
    """Copy a green results directory into snapshot_dir (issue 7494).

    A later failing run must not overwrite an existing green snapshot.
    Returns the snapshot path when a freeze happened or an existing green
    snapshot was preserved; None when the run was not green and no prior
    snapshot exists.
    """
    results_dir = Path(results_dir)
    snapshot_dir = Path(snapshot_dir)
    if green:
        if snapshot_dir.exists():
            shutil.rmtree(snapshot_dir)
        snapshot_dir.parent.mkdir(parents=True, exist_ok=True)
        shutil.copytree(results_dir, snapshot_dir)
        return snapshot_dir
    if snapshot_dir.exists():
        return snapshot_dir
    return None


def classify_blocker(signature: str) -> str:
    """Return Blocked early for known backend-gated signatures (issue 7495).

    App rebuilds cannot clear a server-side gate (for example an API that
    ignores the app version code).
    """
    text = (signature or "").strip().casefold()
    backend_markers = (
        "versioncode ignored",
        "server-side gate",
        "backend gate",
        "api ignores app version",
        "upstream feature flag",
    )
    if any(marker in text for marker in backend_markers):
        return "Blocked"
    return "Failed"


def run_with_time_cap(
    primary: Callable[[], object],
    fallback: Callable[[], object],
    *,
    cap_s: float,
    clock: Callable[[], float] | None = None,
    sleeper: Callable[[float], None] | None = None,
) -> object:
    """Run primary under a wall-clock cap; on timeout or failure use fallback (issue 7496)."""
    if cap_s <= 0:
        raise ValueError("cap_s must be positive")
    now = clock or time.monotonic
    sleep = sleeper or time.sleep
    started = now()
    try:
        result = primary()
    except Exception:
        return fallback()
    elapsed = now() - started
    if elapsed > cap_s:
        sleep(0)
        return fallback()
    return result


def no_push_guard_tracked(root: Path, *, tracked_paths: Iterable[str] | None = None) -> list[str]:
    """Fail when the NO_PUSH guard is missing from the tracked set (issue 7497)."""
    rel = NO_PUSH_GUARD_REL
    if tracked_paths is None:
        import subprocess  # nosec B404

        proc = subprocess.run(  # nosec B603 B607
            ["git", "-C", str(root), "ls-files", "--", rel],
            check=False,
            capture_output=True,
            text=True,
        )
        tracked = {line.strip().replace("\\", "/") for line in proc.stdout.splitlines() if line.strip()}
    else:
        tracked = {p.replace("\\", "/").removeprefix("./") for p in tracked_paths}
    if rel not in tracked:
        return [f"NO_PUSH guard missing from tracked files: {rel}"]
    path = Path(root) / rel
    if not path.is_file():
        return [f"NO_PUSH guard tracked path missing on disk: {rel}"]
    return []


def evidence_matches_pin(pin: EvidencePin, *, build_id: str, branch: str) -> bool:
    """Accept evidence only when build id and branch match the green pin (issue 7498)."""
    return pin.build_id == build_id and pin.branch == branch


def named_blocks_by_gap(scenarios: Iterable[tuple[str, str, str]]) -> list[NamedBlock]:
    """Collapse scenarios that share a gap id into one NamedBlock (issue 7499)."""
    buckets: dict[str, list[tuple[str, str]]] = {}
    for scenario_id, gap_id, reason in scenarios:
        buckets.setdefault(gap_id, []).append((scenario_id, reason))
    blocks: list[NamedBlock] = []
    for gap_id, rows in buckets.items():
        scenarios_ids = tuple(scenario for scenario, _ in rows)
        reason = rows[0][1]
        blocks.append(NamedBlock(gap_id=gap_id, scenarios=scenarios_ids, reason=reason))
    return blocks
