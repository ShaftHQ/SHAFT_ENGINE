#!/usr/bin/env python3
"""Build and sanitize one excluded, two-arm native Harbor canary."""

from __future__ import annotations

import argparse
import asyncio
import copy
import hashlib
import importlib
import importlib.util
import json
import os
import re
import sys
import tomllib
from datetime import datetime
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parent
ARMS = ("control", "chaos-engine")
GIT_SHA = re.compile(r"[0-9a-f]{40}")
AGENT_IMPORT_PATH = "scripts.ci.chaos_gauge.agent:ChaosEngineCodex"


def _custom_agent_module_spec(repository: Path) -> importlib.machinery.ModuleSpec:
    """Bind Harbor's custom-agent import to this checked-out repository only."""
    root = Path(repository).resolve()
    if not root.is_dir():
        raise ValueError("canary repository root is invalid")
    expected = root / "scripts" / "ci" / "chaos_gauge" / "agent.py"
    try:
        actual = expected.resolve(strict=True)
    except OSError as error:
        raise ValueError("canary agent module is unavailable") from error
    try:
        actual.relative_to(root)
    except ValueError as error:
        raise ValueError("canary agent module escapes repository root") from error
    if actual != expected or not actual.is_file():
        raise ValueError("canary agent module is invalid")
    root_text = str(root)
    sys.path[:] = [root_text, *(entry for entry in sys.path if entry != root_text)]
    os.environ["PYTHONPATH"] = root_text
    spec = importlib.util.find_spec(AGENT_IMPORT_PATH.partition(":")[0])
    if spec is None or spec.origin is None or Path(spec.origin).resolve() != actual:
        raise ValueError("canary agent module resolves outside repository root")
    return spec


def _bind_custom_agent_import(repository: Path) -> type:
    """Make the exact custom treatment importable before Harbor builds trials."""
    spec = _custom_agent_module_spec(repository)
    try:
        module = importlib.import_module(spec.name)
    except ImportError as error:
        raise ValueError("canary agent module is unavailable") from error
    if Path(str(getattr(module, "__file__", ""))).resolve() != Path(str(spec.origin)).resolve():
        raise ValueError("canary agent module resolves outside repository root")
    agent = getattr(module, "ChaosEngineCodex", None)
    if not isinstance(agent, type):
        raise ValueError("canary agent class is invalid")
    try:
        factory = importlib.import_module("harbor.agents.factory")
    except ImportError as error:
        raise ValueError("Harbor AgentFactory is unavailable") from error
    agent_factory = getattr(factory, "AgentFactory", None)
    load_agent = getattr(factory, "_import_agent_class", None)
    if not callable(getattr(agent_factory, "create_agent_from_import_path", None)) or not callable(load_agent):
        raise ValueError("Harbor AgentFactory import contract is unavailable")
    if load_agent(AGENT_IMPORT_PATH) is not agent:
        raise ValueError("Harbor AgentFactory resolved wrong canary agent")
    return agent


def _campaign():
    spec = importlib.util.spec_from_file_location("chaos_gauge_campaign", ROOT / "campaign.py")
    if spec is None or spec.loader is None:
        raise ValueError("ChaosGauge campaign launcher is unavailable")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _mapping(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} is invalid")
    return value


def _number(value: object, label: str, *, integer: bool = False) -> float | int:
    allowed = int if integer else (int, float)
    if isinstance(value, bool) or not isinstance(value, allowed) or value < 0:
        raise ValueError(f"{label} is invalid")
    return value


def _timestamp(value: object) -> datetime:
    try:
        return datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError("canary timing is invalid") from error


def plan(manifest: object) -> dict[str, object]:
    """Select deterministic first public pair while reserving it from pilot accounting."""
    source = _campaign().plan(manifest, "calibration")
    pairs = source.get("pairs") if isinstance(source, dict) else None
    if not isinstance(pairs, list) or not pairs or not isinstance(pairs[0], dict):
        raise ValueError("calibration plan is invalid")
    pair = pairs[0]
    if pair.get("attempt") != 1 or pair.get("task") != "diagnosis-config-precedence":
        raise ValueError("canary source pair is invalid")
    arms = pair.get("arms")
    if not isinstance(arms, list) or set(arms) != set(ARMS):
        raise ValueError("canary arms are invalid")
    return {
        "schemaVersion": 1,
        "campaign": "canary",
        "excludedFromPilot": True,
        "implementationRevision": source["implementationRevision"],
        "trials": 2,
        "pair": {
            "pairId": f"canary__{pair['task']}",
            "task": pair["task"],
            "sha256": pair["sha256"],
            "attempt": 0,
            "arms": list(arms),
            "retryBudget": pair["retryBudget"],
        },
    }


def job_config(manifest: object) -> dict[str, object]:
    """Reuse the native two-arm pair configuration for exactly one public task."""
    canary = plan(manifest)
    calibration = _campaign().plan(manifest, "calibration")
    source_pair = calibration["pairs"][0]
    configs = _campaign().pair_job_configs(manifest, "calibration")
    source = configs.get(source_pair["pairId"])
    if not isinstance(source, dict):
        raise ValueError("canary native job configuration is unavailable")
    config = copy.deepcopy(source)
    config["job_name"] = "chaos-gauge-canary-" + hashlib.sha256(
        str(canary["pair"]["pairId"]).encode()
    ).hexdigest()[:16]
    config["n_attempts"] = 1
    config["n_concurrent_trials"] = 2
    config["quiet"] = True
    return config


def _private_receipt(manifest: dict[str, object]) -> dict[str, object]:
    package = _mapping(manifest.get("privatePackage"), "private package")
    keys = ("repository", "commit", "contentSha256", "name", "ref")
    value = {key: package.get(key) for key in keys}
    if not all(isinstance(item, str) and item for item in value.values()):
        raise ValueError("private package identity is invalid")
    return value


def _result_mapping(value: object) -> dict[str, object]:
    dump = getattr(value, "model_dump", None)
    return _mapping(dump(mode="json") if callable(dump) else value, "Harbor canary result")


def _native_bindings(value: object, pair: dict[str, object]) -> dict[str, str]:
    bindings = _mapping(value, "canary native bindings")
    if set(bindings) != set(ARMS):
        raise ValueError("canary native bindings are invalid")
    names = {}
    for arm in ARMS:
        name = _campaign()._native_trial_name(str(pair["task"]), bindings[arm])
        if name in names.values():
            raise ValueError("canary native bindings are invalid")
        names[arm] = name
    return names


def _harbor_task_checksum(path: Path) -> str:
    """Compute the exact Harbor 0.22 Task.checksum identity for a local task."""
    try:
        from dirhash import dirhash
    except ImportError as error:
        raise ValueError("Harbor task checksum is unavailable") from error
    value = dirhash(path, "sha256")
    if not isinstance(value, str) or not re.fullmatch(r"[0-9a-f]{64}", value):
        raise ValueError("Harbor task checksum is invalid")
    return value


def _local_task_path(value: object, label: str, repository: Path) -> Path:
    source = _mapping(value, label)
    raw_path = source.get("path")
    if not isinstance(raw_path, str) or not raw_path:
        raise ValueError("canary task identity is invalid")
    path = Path(raw_path)
    try:
        return (path if path.is_absolute() else repository / path).resolve(strict=True)
    except OSError as error:
        raise ValueError("canary task identity is invalid") from error


def _public_task_identity(pair: dict[str, object], repository: Path) -> tuple[str, Path, str]:
    task = pair.get("task")
    if not isinstance(task, str) or not task:
        raise ValueError("canary task identity is invalid")
    try:
        path = (repository / "scripts" / "ci" / "chaos_gauge" / "dataset" / task).resolve(strict=True)
        task_file = tomllib.loads((path / "task.toml").read_text(encoding="utf-8"))
    except (OSError, tomllib.TOMLDecodeError) as error:
        raise ValueError("canary task identity is invalid") from error
    metadata = _mapping(task_file.get("task"), "canary task metadata")
    name = metadata.get("name")
    if name != f"ShaftHQ/{task}":
        raise ValueError("canary task identity is invalid")
    return str(name), path, _harbor_task_checksum(path)


def _validate_task_identity(
    trial: dict[str, object], pair: dict[str, object], repository: Path, config: dict[str, object],
) -> None:
    name, path, checksum = _public_task_identity(pair, repository)
    if trial.get("task_name") != name:
        raise ValueError("canary task identity is invalid")
    if _local_task_path(trial.get("task_id"), "canary task ID", repository) != path:
        raise ValueError("canary task identity is invalid")
    if _local_task_path(config.get("task"), "canary trial task", repository) != path:
        raise ValueError("canary task identity is invalid")
    if trial.get("task_checksum") != checksum:
        raise ValueError("canary task checksum is invalid")


def _validate_public_source_revision(
    manifest: dict[str, object], public_source_revision: str, repository: Path, run: Callable[[list[str]], str] | None,
) -> None:
    try:
        campaign = _campaign()
        campaign.validate_execution_revision(
            repository, public_source_revision, manifest["implementationRevision"], campaign._run if run is None else run,
        )
    except (KeyError, ValueError) as error:
        raise ValueError("canary source revision is invalid") from error


def receipt(
    manifest: object, planned: object, result: object, *, public_source_revision: str,
    native_bindings: object, repository: Path = ROOT.parents[2], run: Callable[[list[str]], str] | None = None,
) -> dict[str, object]:
    """Return strict aggregate evidence; never include raw Harbor output or trajectories."""
    source = _mapping(manifest, "experiment manifest")
    canary = _mapping(planned, "canary plan")
    if canary != plan(source) or not GIT_SHA.fullmatch(public_source_revision):
        raise ValueError("canary plan or source revision is invalid")
    _validate_public_source_revision(source, public_source_revision, repository, run)
    value = _result_mapping(result)
    trials = value.get("trial_results")
    if not isinstance(trials, list) or len(trials) != 2:
        raise ValueError("canary trial matrix is incomplete")
    pair = _mapping(canary["pair"], "canary pair")
    bindings = _native_bindings(native_bindings, pair)
    config = job_config(source)
    expected_agents = {arm: config["agents"][list(pair["arms"]).index(arm)] for arm in ARMS}
    observed: dict[str, dict[str, object]] = {}
    expected_by_native = {name: arm for arm, name in bindings.items()}
    for raw in trials:
        trial = _mapping(raw, "Harbor canary trial")
        trial_config = _mapping(trial.get("config"), "canary trial config")
        _validate_task_identity(trial, pair, repository, trial_config)
        native_name = _campaign()._native_trial_name(str(pair["task"]), trial.get("trial_name"))
        arm = expected_by_native.get(native_name)
        if arm is None or arm in observed:
            raise ValueError("canary native trial binding is invalid")
        if not _campaign()._agent_matches(trial_config.get("agent"), expected_agents[arm]):
            raise ValueError("canary arm identity is invalid")
        agent = _mapping(trial.get("agent_info"), "canary agent")
        model = _mapping(agent.get("model_info"), "canary model")
        if agent.get("name") != "codex" or agent.get("version") != "0.118.0" or model != {"name": "gpt-5.6-terra", "provider": "openai"}:
            raise ValueError("canary runtime identity is invalid")
        context = _mapping(trial.get("agent_result"), "canary telemetry")
        tokens = _number(context.get("n_input_tokens"), "canary token telemetry", integer=True) + _number(context.get("n_output_tokens"), "canary token telemetry", integer=True)
        cost = _number(context.get("cost_usd"), "canary cost telemetry")
        timing = _mapping(trial.get("agent_execution"), "canary timing")
        started, finished = _timestamp(timing.get("started_at")), _timestamp(timing.get("finished_at"))
        if finished < started:
            raise ValueError("canary timing is invalid")
        rewards = _mapping(_mapping(trial.get("verifier_result"), "canary verifier").get("rewards"), "canary rewards")
        if rewards != {"correctness": 1.0, "safety": 1.0, "cleanup": 1.0}:
            raise ValueError("canary verifier, safety, or cleanup evidence is invalid")
        if trial.get("verifier_environment_mode") != "separate":
            raise ValueError("canary verifier isolation is invalid")
        observed[arm] = {
            "arm": arm,
            "task": pair["task"],
            "sha256": pair["sha256"],
            "nativeTrialName": native_name,
            "tokens": tokens,
            "costUsd": cost,
            "seconds": (finished - started).total_seconds(),
            "verifierEnvironmentMode": "separate",
            "rewards": rewards,
        }
    if set(observed) != set(ARMS):
        raise ValueError("canary native trial binding is incomplete")
    evidence = {
        "schemaVersion": 1,
        "campaign": "canary",
        "excludedFromPilot": True,
        "implementationRevision": canary["implementationRevision"],
        "publicSourceRevision": public_source_revision,
        "privatePackage": _private_receipt(source),
        "trialAccounting": {"planned": 2, "observed": len(observed)},
        "rawResultSha256": hashlib.sha256(
            json.dumps(value, sort_keys=True, separators=(",", ":"), default=str).encode()
        ).hexdigest(),
        "trials": [observed[arm] for arm in pair["arms"]],
    }
    validate_public_evidence(evidence, repository=repository, run=run)
    return evidence


def validate_public_evidence(
    value: object, *, repository: Path, run: Callable[[list[str]], str] | None = None,
) -> None:
    """Fail closed unless evidence is exactly the sanitised canary receipt schema."""
    receipt_value = _mapping(value, "public canary evidence")
    expected = {
        "schemaVersion", "campaign", "excludedFromPilot", "implementationRevision",
        "publicSourceRevision", "privatePackage", "trialAccounting", "rawResultSha256", "trials",
    }
    if set(receipt_value) != expected or receipt_value.get("schemaVersion") != 1 or receipt_value.get("campaign") != "canary" or receipt_value.get("excludedFromPilot") is not True:
        raise ValueError("public canary evidence is invalid")
    if not GIT_SHA.fullmatch(str(receipt_value.get("implementationRevision"))) or not GIT_SHA.fullmatch(str(receipt_value.get("publicSourceRevision"))):
        raise ValueError("public canary evidence is invalid")
    _validate_public_source_revision(
        receipt_value, str(receipt_value["publicSourceRevision"]), repository, run,
    )
    package = _mapping(receipt_value.get("privatePackage"), "public canary evidence")
    if (
        set(package) != {"repository", "commit", "contentSha256", "name", "ref"}
        or package.get("repository") != "ShaftHQ/ChaosGauge-private"
        or package.get("name") != "ShaftHQ/chaosgauge-private"
        or not GIT_SHA.fullmatch(str(package.get("commit")))
        or not re.fullmatch(r"sha256:[0-9a-f]{64}", str(package.get("contentSha256")))
        or package.get("ref") != package.get("contentSha256")
    ):
        raise ValueError("public canary evidence is invalid")
    accounting = _mapping(receipt_value.get("trialAccounting"), "public canary evidence")
    trials = receipt_value.get("trials")
    if (
        accounting != {"planned": 2, "observed": 2}
        or not isinstance(trials, list)
        or len(trials) != 2
        or not re.fullmatch(r"[0-9a-f]{64}", str(receipt_value.get("rawResultSha256")))
    ):
        raise ValueError("public canary evidence is invalid")
    seen_arms: set[str] = set()
    for trial in trials:
        record = _mapping(trial, "public canary evidence")
        if (
            set(record) != {"arm", "task", "sha256", "nativeTrialName", "tokens", "costUsd", "seconds", "verifierEnvironmentMode", "rewards"}
            or record.get("arm") not in ARMS
            or record.get("arm") in seen_arms
            or record.get("task") != "diagnosis-config-precedence"
            or not re.fullmatch(r"[0-9a-f]{64}", str(record.get("sha256")))
            or not re.fullmatch(r"diagnosis-config-precedence__[A-Za-z0-9]{7}", str(record.get("nativeTrialName")))
            or record.get("verifierEnvironmentMode") != "separate"
        ):
            raise ValueError("public canary evidence is invalid")
        seen_arms.add(str(record["arm"]))
        _number(record.get("tokens"), "public canary evidence", integer=True)
        _number(record.get("costUsd"), "public canary evidence")
        _number(record.get("seconds"), "public canary evidence")
        if record.get("rewards") != {"correctness": 1.0, "safety": 1.0, "cleanup": 1.0}:
            raise ValueError("public canary evidence is invalid")
    if seen_arms != set(ARMS):
        raise ValueError("public canary evidence is invalid")


def _write_exclusive(path: Path, value: object) -> None:
    target = Path(path)
    if not target.parent.is_dir() or target.parent.is_symlink() or target.is_symlink():
        raise ValueError("canary output path is invalid")
    try:
        descriptor = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError as error:
        raise ValueError("canary output already exists") from error
    with os.fdopen(descriptor, "w", encoding="utf-8") as output:
        json.dump(value, output, sort_keys=True)
        output.write("\n")
        output.flush()
        os.fsync(output.fileno())


async def run(
    manifest: object,
    private_checkout: Path,
    *,
    public_source_revision: str,
    raw_out: Path,
    receipt_out: Path,
    private_read_proven: bool = False,
) -> dict[str, object]:
    """Run exactly one excluded native pair after the complete paid-run preflight."""
    if not GIT_SHA.fullmatch(public_source_revision):
        raise ValueError("canary source revision is invalid")
    _bind_custom_agent_import(ROOT.parents[2])
    launcher = _campaign()
    launcher.full_preflight(manifest, private_checkout, private_read_proven=private_read_proven)
    planned, config = plan(manifest), job_config(manifest)
    environment = _mapping(config.get("environment"), "canary environment")
    if environment.get("type") != "docker" or environment.get("delete") is not True:
        raise ValueError("canary cleanup configuration is invalid")
    from harbor.job import Job
    from harbor.models.job.config import JobConfig

    job = await Job.create(JobConfig.model_validate(config))
    trials = getattr(job, "_trial_configs", None)
    pair = _mapping(planned["pair"], "canary pair")
    if not isinstance(trials, list) or len(trials) != 2:
        raise ValueError("canary native trial matrix is invalid")
    names = []
    for trial in trials:
        if launcher._pair_task_name(trial) != pair["task"]:
            raise ValueError("canary native task identity is invalid")
        names.append(launcher._native_trial_name(str(pair["task"]), getattr(trial, "trial_name", None)))
    launcher._install_pair_start_gate(job, names[0], names[1])
    raw = _result_mapping(await job.run())
    _write_exclusive(raw_out, raw)
    native_bindings = {}
    for arm, trial in zip(pair["arms"], trials):
        name = launcher._native_trial_name(str(pair["task"]), getattr(trial, "trial_name", None))
        if arm in native_bindings or name in native_bindings.values():
            raise ValueError("canary native trial binding is invalid")
        native_bindings[arm] = name
    evidence = receipt(
        manifest, planned, raw, public_source_revision=public_source_revision,
        native_bindings=native_bindings, repository=ROOT.parents[2],
    )
    _write_exclusive(receipt_out, evidence)
    return evidence


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=ROOT / "experiment.json")
    parser.add_argument("--private-checkout", type=Path)
    parser.add_argument("--public-source-revision")
    parser.add_argument("--raw-out", type=Path)
    parser.add_argument("--receipt-out", type=Path)
    parser.add_argument("--private-read-proven", action="store_true")
    parser.add_argument("--verify-agent-import", action="store_true")
    args = parser.parse_args()
    if args.verify_agent_import:
        _bind_custom_agent_import(ROOT.parents[2])
        return 0
    if None in (args.private_checkout, args.public_source_revision, args.raw_out, args.receipt_out):
        parser.error("canary execution requires private checkout, source revision, raw output, and receipt output")
    manifest = json.loads(args.manifest.read_text(encoding="utf-8"))
    asyncio.run(
        run(
            manifest,
            args.private_checkout,
            public_source_revision=args.public_source_revision,
            raw_out=args.raw_out,
            receipt_out=args.receipt_out,
            private_read_proven=args.private_read_proven,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
