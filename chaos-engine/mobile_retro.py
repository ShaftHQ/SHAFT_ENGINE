"""Mobile-automation retro harness helpers.

Pure helpers so executor loops can freeze evidence, classify backend gates as
Blocked early, time-cap a failing primary path (killing a hung primary), keep a
tracked NO_PUSH guard, pin evidence to build+branch, and key NamedBlock on gap
ids. See references/mobile-retro-harness.md.
"""

from __future__ import annotations

import multiprocessing
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable

NO_PUSH_GUARD_REL = "scripts/ci/no_push_guard.py"
HELPER_REL = "chaos-engine/mobile_retro.py"


@dataclass(frozen=True)
class EvidencePin:
    """Immutable identity of the green evidence cohort."""

    build_id: str
    branch: str


@dataclass(frozen=True)
class NamedBlock:
    """One block keyed by gap id, not scenario id."""

    gap_id: str
    scenarios: tuple[str, ...]
    reason: str


def freeze_results_snapshot(results_dir: Path, snapshot_dir: Path, *, green: bool) -> Path | None:
    """Copy a green results directory into snapshot_dir.

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
    """Return Blocked early for known backend-gated signatures.

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


def _primary_worker(queue: multiprocessing.Queue, primary: Callable[[], object]) -> None:
    try:
        queue.put(("ok", primary()))
    except BaseException as exc:  # noqa: BLE001 - surface any primary failure to parent
        queue.put(("err", exc))


def run_with_time_cap(
    primary: Callable[[], object],
    fallback: Callable[[], object],
    *,
    cap_s: float,
) -> object:
    """Run primary under a wall-clock cap; kill it and use fallback on hang or error.

    Uses a forked child process so a hung primary can be terminated. Requires a
    picklable-or-forkable callable (closures work under fork on Linux/macOS).
    """
    if cap_s <= 0:
        raise ValueError("cap_s must be positive")
    ctx = multiprocessing.get_context("fork")
    queue: multiprocessing.Queue = ctx.Queue(1)
    proc = ctx.Process(target=_primary_worker, args=(queue, primary), daemon=True)
    proc.start()
    proc.join(timeout=cap_s)
    if proc.is_alive():
        proc.terminate()
        proc.join(timeout=min(1.0, max(cap_s, 0.1)))
        if proc.is_alive():
            proc.kill()
            proc.join(timeout=1.0)
        return fallback()
    if queue.empty():
        return fallback()
    kind, payload = queue.get_nowait()
    if kind == "err":
        return fallback()
    return payload


def feature_present(root: Path) -> bool:
    """True when this checkout ships the mobile-retro helper (not a sparse fixture)."""
    return (Path(root) / HELPER_REL).is_file()


def no_push_guard_tracked(root: Path, *, tracked_paths: Iterable[str] | None = None) -> list[str]:
    """Fail when the NO_PUSH guard is missing from the tracked set."""
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
    """Accept evidence only when build id and branch match the green pin."""
    return pin.build_id == build_id and pin.branch == branch


def named_blocks_by_gap(scenarios: Iterable[tuple[str, str, str]]) -> list[NamedBlock]:
    """Collapse scenarios that share a gap id into one NamedBlock."""
    buckets: dict[str, list[tuple[str, str]]] = {}
    for scenario_id, gap_id, reason in scenarios:
        buckets.setdefault(gap_id, []).append((scenario_id, reason))
    blocks: list[NamedBlock] = []
    for gap_id, rows in buckets.items():
        scenarios_ids = tuple(scenario for scenario, _ in rows)
        reason = rows[0][1]
        blocks.append(NamedBlock(gap_id=gap_id, scenarios=scenarios_ids, reason=reason))
    return blocks
