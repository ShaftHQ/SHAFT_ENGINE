#!/usr/bin/env python3
"""Plan, preflight, and collect immutable native Harbor ChaosGauge campaigns."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import re
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Callable


ARMS = ("control", "chaos-engine")
GIT_SHA = re.compile(r"[0-9a-f]{40}")
SHA256 = re.compile(r"sha256:[0-9a-f]{64}")
ROOT = Path(__file__).resolve().parent


def _object(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} is invalid")
    return value


def _campaign(manifest: object, campaign: str) -> tuple[dict[str, object], list[dict[str, object]]]:
    value = _object(manifest, "experiment manifest")
    campaigns = _object(value.get("campaigns"), "campaigns")
    selected = _object(campaigns.get(campaign), "campaign")
    visibility = selected.get("taskVisibility")
    if not isinstance(visibility, list) or selected.get("taskCount") not in (12, 16):
        raise ValueError("campaign selection is invalid")
    tasks = [
        _object(task, "task") for task in value.get("tasks", [])
        if isinstance(task, dict) and task.get("visibility") in visibility
    ]
    if len(tasks) != selected["taskCount"]:
        raise ValueError("campaign task matrix is invalid")
    return selected, tasks


def _schedule(manifest: object) -> dict[str, object]:
    value = json.loads((ROOT / "schedule.json").read_text(encoding="utf-8"))
    if value != {
        "schemaVersion": 1,
        "seed": _object(manifest, "experiment manifest").get("seed"),
        "algorithm": "sha256(seed:task:attempt)-low-bit-first-arm",
        "unit": "task-attempt-pair",
        "arms": list(ARMS),
        "attemptsPerTask": _object(manifest, "experiment manifest").get("attemptsPerTask"),
        "retryBudget": 2,
        "campaigns": {
            "publicCalibration": {"tasks": 12, "trials": 120},
            "fullPilot": {"tasks": 16, "trials": 160, "requiresPrivatePackageResolution": True},
        },
    }:
        raise ValueError("schedule contract is invalid")
    return value


def private_resolution(manifest: object) -> dict[str, object]:
    """Return public opaque metadata needed to resolve, never private task bytes."""
    value = _object(manifest, "experiment manifest")
    package = _object(value.get("privatePackage"), "private package")
    tasks = [
        {
            "name": task["name"],
            "stratum": task["stratum"],
            "sha256": task["sha256"],
        }
        for task in value.get("tasks", [])
        if isinstance(task, dict) and task.get("visibility") == "private-reference"
    ]
    return {**package, "tasks": tasks}


def plan(manifest: object, campaign: str) -> dict[str, object]:
    """Build ordered two-arm Harbor work without launching a Harbor job."""
    value = _object(manifest, "experiment manifest")
    selected, tasks = _campaign(value, campaign)
    schedule = _schedule(value)
    if selected["privateResolutionRequired"] and len(private_resolution(value)["tasks"]) != 4:
        raise ValueError("private task metadata is invalid")
    seed = value.get("seed")
    attempts = value.get("attemptsPerTask")
    revision = value.get("implementationRevision")
    if not isinstance(seed, int) or not isinstance(attempts, int) or not GIT_SHA.fullmatch(str(revision)):
        raise ValueError("implementation identity is invalid")
    pairs = []
    for task in tasks:
        for attempt in range(1, attempts + 1):
            name = str(task["name"])
            first = ARMS[hashlib.sha256(f"{seed}:{name}:{attempt}".encode()).digest()[0] & 1]
            pairs.append({
                "pairId": f"{name}__{attempt}",
                "task": name,
                "sha256": task["sha256"],
                "attempt": attempt,
                "arms": [first, ARMS[1 - ARMS.index(first)]],
                "retryBudget": schedule["retryBudget"],
            })
    if len(pairs) * len(ARMS) != selected["taskCount"] * attempts * len(ARMS):
        raise ValueError("campaign trial matrix is invalid")
    return {
        "schemaVersion": 1,
        "campaign": campaign,
        "implementationRevision": revision,
        "trials": len(pairs) * len(ARMS),
        "pairs": pairs,
    }


def resume(planned: object, completed: object) -> dict[str, object]:
    """Skip only already-complete exact pairs; mixed arm state is never resumable."""
    value = _object(planned, "planned campaign")
    state = _object(completed, "completed pairs")
    pairs = value.get("pairs")
    if not isinstance(pairs, list):
        raise ValueError("planned pair matrix is invalid")
    remaining = []
    known = set()
    for raw in pairs:
        pair = _object(raw, "planned pair")
        pair_id = str(pair.get("pairId"))
        known.add(pair_id)
        record = state.get(pair_id)
        if record is None:
            remaining.append(pair)
            continue
        attempts = _object(record, "resume pair")
        if set(attempts) != set(ARMS):
            raise ValueError("resume pair is incomplete")
        budget = pair.get("retryBudget")
        if not isinstance(budget, int) or any(
            isinstance(value, bool) or not isinstance(value, int) or value < 0 or value > budget
            for value in attempts.values()
        ):
            raise ValueError("retry evidence is invalid")
    if set(state) - known:
        raise ValueError("resume pair is unknown")
    return {**value, "pairs": remaining, "trials": len(remaining) * len(ARMS)}


def _timestamp(value: object) -> datetime:
    try:
        parsed = datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError("Harbor start timestamp is invalid") from error
    if parsed.tzinfo is None:
        raise ValueError("Harbor start timestamp is invalid")
    return parsed


def _trials(job: object, arm: str, pairs: dict[str, dict[str, object]]) -> dict[str, datetime]:
    value = _object(job, f"{arm} Harbor result")
    stats = _object(value.get("stats"), "Harbor retries")
    retries = stats.get("n_retries")
    if isinstance(retries, bool) or not isinstance(retries, int) or not 0 <= retries <= 2:
        raise ValueError("Harbor retry evidence is invalid")
    results = value.get("trial_results")
    if not isinstance(results, list) or len(results) != len(pairs):
        raise ValueError("Harbor trial matrix is incomplete")
    starts = {}
    for raw in results:
        trial = _object(raw, "Harbor trial")
        pair_id = str(trial.get("trial_name"))
        pair = pairs.get(pair_id)
        if pair is None or pair_id in starts:
            raise ValueError("Harbor pair identity is invalid")
        if trial.get("task_name") != pair["task"] or trial.get("task_checksum") != pair["sha256"]:
            raise ValueError("Harbor task identity is invalid")
        agent = _object(trial.get("agent_info"), "Harbor agent identity")
        model = _object(agent.get("model_info"), "Harbor model identity")
        if agent.get("name") != "codex" or agent.get("version") != "0.118.0" or model.get("name") != "gpt-5.6-terra":
            raise ValueError("Harbor agent identity is invalid")
        if trial.get("verifier_environment_mode") != "separate":
            raise ValueError("Harbor verifier isolation is invalid")
        starts[pair_id] = _timestamp(_object(trial.get("agent_execution"), "Harbor timing").get("started_at"))
    return starts


def collect(
    manifest: object,
    planned: object,
    control: object,
    candidate: object,
    *,
    private_resolution: object | None,
    execution_revision: str,
) -> dict[str, object]:
    """Validate exported Harbor results and make a secret-free post-run receipt."""
    value = _object(manifest, "experiment manifest")
    planned_value = _object(planned, "planned campaign")
    if not GIT_SHA.fullmatch(execution_revision) or execution_revision == value.get("implementationRevision"):
        raise ValueError("execution revision must be a post-run merged SHA")
    campaign = str(planned_value.get("campaign"))
    selected, _ = _campaign(value, campaign)
    if selected["privateResolutionRequired"] and private_resolution != globals()["private_resolution"](value):
        raise ValueError("full-pilot private package is unresolved")
    pairs_list = planned_value.get("pairs")
    expected_pairs = int(selected["taskCount"]) * int(value["attemptsPerTask"])
    if (
        not isinstance(pairs_list, list)
        or len(pairs_list) != expected_pairs
        or planned_value.get("trials") != len(pairs_list) * 2
    ):
        raise ValueError("planned campaign is invalid")
    pairs = {str(pair.get("pairId")): _object(pair, "planned pair") for pair in pairs_list if isinstance(pair, dict)}
    if len(pairs) != len(pairs_list):
        raise ValueError("planned pair identity is invalid")
    starts = {"control": _trials(control, "control", pairs), "chaos-engine": _trials(candidate, "chaos-engine", pairs)}
    observed = []
    for pair in pairs_list:
        pair = _object(pair, "planned pair")
        pair_id = str(pair["pairId"])
        planned_arms = pair.get("arms")
        if not isinstance(planned_arms, list) or set(planned_arms) != set(ARMS) or len(planned_arms) != 2:
            raise ValueError("planned arm order is invalid")
        first, second = planned_arms
        if starts[first][pair_id] >= starts[second][pair_id]:
            raise ValueError("observed Harbor start order is invalid")
        observed_first = min(ARMS, key=lambda arm: starts[arm][pair_id])
        observed.append({"pairId": pair_id, "plannedFirstArm": first, "observedFirstArm": observed_first})
    plan_digest = hashlib.sha256(json.dumps(planned_value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    result_digest = hashlib.sha256(json.dumps({"control": control, "chaos-engine": candidate}, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return {
        "schemaVersion": 1,
        "campaign": campaign,
        "implementationRevision": value["implementationRevision"],
        "executionRevision": execution_revision,
        "planSha256": plan_digest,
        "resultsSha256": result_digest,
        "pairAccounting": {"planned": len(pairs), "completed": len(observed)},
        "trialAccounting": {"planned": len(pairs) * 2, "observed": len(observed) * 2},
        "pairs": observed,
    }


def _run(command: list[str]) -> str:
    return subprocess.run(command, check=True, capture_output=True, text=True).stdout  # nosec B603 - fixed prerequisite probes.


def full_preflight(manifest: object, checkout: Path, run: Callable[[list[str]], str] = _run) -> None:
    """Validate capabilities and private source pins before any Harbor command is allowed."""
    value = _object(manifest, "experiment manifest")
    validator_spec = importlib.util.spec_from_file_location(
        "chaos_gauge_validator", ROOT / "validate_experiment.py"
    )
    if validator_spec is None or validator_spec.loader is None:
        raise ValueError("ChaosGauge validation is unavailable")
    validator = importlib.util.module_from_spec(validator_spec)
    validator_spec.loader.exec_module(validator)
    validator.validate_manifest(value)
    for campaign in ("calibration", "full-pilot"):
        validator.validate_job_contracts(
            value, validator.load_jobs(ROOT, campaign), campaign=campaign
        )
    package = _object(value.get("privatePackage"), "private package")
    if not checkout.is_dir() or checkout.is_symlink():
        raise ValueError("private checkout credentials are unavailable")
    if run(["git", "-C", str(checkout), "rev-parse", "HEAD"]).strip() != package["commit"]:
        raise ValueError("private checkout commit is invalid")
    if run(["git", "-C", str(checkout), "ls-remote", "origin", "HEAD"]).strip() == "":
        raise ValueError("private checkout credentials are unavailable")
    dataset = checkout / "dataset.toml"
    if not dataset.is_file() or f"sha256:{hashlib.sha256(dataset.read_bytes()).hexdigest()}" != package["contentSha256"]:
        raise ValueError("private package content digest is invalid")
    if not SHA256.fullmatch(str(package.get("ref"))):
        raise ValueError("private package digest is invalid")
    if not run(["docker", "version", "--format", "{{.Server.Version}}"]).strip():
        raise ValueError("Docker is unavailable")
    if run(["python3", "-c", "from importlib.metadata import version; print(version('harbor'))"]).strip() != "0.22.0":
        raise ValueError("Harbor version is invalid")
    if re.search(r"(?<![0-9.])0\\.118\\.0(?![0-9.])", run(["codex", "--version"])) is None:
        raise ValueError("Codex version is invalid")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("plan", "preflight", "collect"))
    parser.add_argument("--campaign", choices=("calibration", "full-pilot"), required=True)
    parser.add_argument("--manifest", type=Path, default=ROOT / "experiment.json")
    parser.add_argument("--private-checkout", type=Path)
    parser.add_argument("--control", type=Path)
    parser.add_argument("--chaos-engine", type=Path)
    parser.add_argument("--resolution", type=Path)
    parser.add_argument("--execution-revision")
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()
    manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
    planned = plan(manifest, args.campaign)
    if args.command == "plan":
        print(json.dumps(planned, sort_keys=True, indent=2))
        return 0
    if args.command == "preflight":
        if args.campaign != "full-pilot" or args.private_checkout is None:
            raise ValueError("full preflight requires a private checkout")
        full_preflight(manifest, args.private_checkout)
        return 0
    if None in (args.control, args.chaos_engine, args.resolution, args.execution_revision, args.out):
        raise ValueError("collection inputs are required")
    receipt = collect(
        manifest, planned, json.loads(args.control.read_text()), json.loads(args.chaos_engine.read_text()),
        private_resolution=json.loads(args.resolution.read_text()), execution_revision=args.execution_revision,
    )
    args.out.write_text(json.dumps(receipt, sort_keys=True, indent=2) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
