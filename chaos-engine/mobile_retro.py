"""Mobile-automation retro harness helpers.

Pure helpers so executor loops can freeze evidence, classify backend gates as
Blocked early, time-cap a failing primary path (killing a hung primary), keep a
tracked NO_PUSH guard, pin evidence to build+branch, and key NamedBlock on gap
ids. Live orchestration entry points (post-run freeze, rebuild-loop classify,
device-fallback time-cap, pin persist/enforce, NamedBlock reporter) call these
helpers; see references/mobile-retro-harness.md and scripts/ci/mobile_retro_live.py.
"""

from __future__ import annotations

import json
import multiprocessing
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable

NO_PUSH_GUARD_REL = "scripts/ci/no_push_guard.py"
HELPER_REL = "chaos-engine/mobile_retro.py"
LIVE_CLI_REL = "scripts/ci/mobile_retro_live.py"


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


def _mp_context():
    """Prefer fork (closures work) on POSIX; spawn on Windows / when fork is gone."""
    if sys.platform == "win32":
        return multiprocessing.get_context("spawn")
    try:
        return multiprocessing.get_context("fork")
    except ValueError:
        return multiprocessing.get_context("spawn")


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

    Uses a child process so a hung primary can be terminated. On POSIX, fork keeps
    closures working. On Windows (spawn), primary/fallback must be picklable.
    """
    if cap_s <= 0:
        raise ValueError("cap_s must be positive")
    ctx = _mp_context()
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


# --- Live paths (executor call sites) -----------------------------------------


def post_run_freeze(
    results_dir: Path,
    snapshot_dir: Path,
    *,
    green: bool,
    pin_path: Path | None = None,
    build_id: str | None = None,
    branch: str | None = None,
) -> Path | None:
    """Post-run path: freeze green results; optionally persist an evidence pin."""
    kept = freeze_results_snapshot(results_dir, snapshot_dir, green=green)
    if green and pin_path is not None and build_id and branch:
        persist_evidence_pin(Path(pin_path), EvidencePin(build_id=build_id, branch=branch))
    return kept


def should_stop_rebuild(classification: str) -> bool:
    """True when rebuild loop must stop (backend Blocked)."""
    return classification == "Blocked"


def rebuild_loop_classify(signature: str, *, rebuild: Callable[[], object] | None = None) -> str:
    """Rebuild-loop path: classify first; skip rebuild when Blocked."""
    classification = classify_blocker(signature)
    if should_stop_rebuild(classification):
        return classification
    if rebuild is not None:
        rebuild()
    return classification


def device_fallback_run(
    primary: Callable[[], object],
    fallback: Callable[[], object],
    *,
    cap_s: float,
) -> object:
    """Device-fallback path: time-cap the primary, then authorized fallback."""
    return run_with_time_cap(primary, fallback, cap_s=cap_s)


def persist_evidence_pin(path: Path, pin: EvidencePin) -> Path:
    """Write build_id+branch pin JSON for later enforcement."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"build_id": pin.build_id, "branch": pin.branch}, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return path


def load_evidence_pin(path: Path) -> EvidencePin:
    """Load a previously persisted evidence pin."""
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    return EvidencePin(build_id=str(data["build_id"]), branch=str(data["branch"]))


def enforce_evidence_pin(pin_path: Path, *, build_id: str, branch: str) -> bool:
    """Live pin path: accept only evidence matching the persisted green pin."""
    return evidence_matches_pin(load_evidence_pin(pin_path), build_id=build_id, branch=branch)


def live_report_named_blocks(
    scenarios: Iterable[tuple[str, str, str]],
    *,
    report_path: Path | None = None,
) -> list[NamedBlock]:
    """Live reporter: NamedBlocks keyed by gap id; optional JSON report write."""
    blocks = named_blocks_by_gap(scenarios)
    if report_path is not None:
        path = Path(report_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        payload = [
            {"gap_id": b.gap_id, "scenarios": list(b.scenarios), "reason": b.reason}
            for b in blocks
        ]
        path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return blocks
