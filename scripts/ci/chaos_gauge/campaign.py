#!/usr/bin/env python3
"""Plan, preflight, and collect immutable native Harbor ChaosGauge campaigns."""

from __future__ import annotations

import argparse
import asyncio
import copy
import hashlib
import importlib.util
import json
import os
import re
import subprocess
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path
from typing import Callable
from uuid import UUID


ARMS = ("control", "chaos-engine")
GIT_SHA = re.compile(r"[0-9a-f]{40}")
SHA256 = re.compile(r"sha256:[0-9a-f]{64}")
ROOT = Path(__file__).resolve().parent
NATIVE_NAME = re.compile(r"[A-Za-z0-9]{7}")
CAPABILITY_MARKER = "CHAOSGAUGE_CAPABILITY_OK"


def _object(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} is invalid")
    return value


def _campaign(manifest: object, campaign: str) -> tuple[dict[str, object], list[dict[str, object]]]:
    value = _object(manifest, "experiment manifest")
    selected = _object(_object(value.get("campaigns"), "campaigns").get(campaign), "campaign")
    visibility = selected.get("taskVisibility")
    if not isinstance(visibility, list) or selected.get("taskCount") not in (12, 16):
        raise ValueError("campaign selection is invalid")
    tasks = [_object(task, "task") for task in value.get("tasks", []) if isinstance(task, dict) and task.get("visibility") in visibility]
    if len(tasks) != selected["taskCount"]:
        raise ValueError("campaign task matrix is invalid")
    return selected, tasks


def _schedule(manifest: object) -> dict[str, object]:
    value = json.loads((ROOT / "schedule.json").read_text(encoding="utf-8"))
    if value != {
        "schemaVersion": 1,
        "seed": _object(manifest, "experiment manifest").get("seed"),
        "algorithm": "sha256(seed:task)-balanced-2-or-3-control-first;sha256(seed:task:attempt)-rank",
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
    tasks = [{"name": task["name"], "stratum": task["stratum"], "sha256": task["sha256"]} for task in value.get("tasks", []) if isinstance(task, dict) and task.get("visibility") == "private-reference"]
    return {**package, "tasks": tasks}


def _rank(seed: int, *parts: object) -> bytes:
    return hashlib.sha256(":".join(map(str, (seed, *parts))).encode()).digest()


def _first_arms(tasks: list[dict[str, object]], seed: int, attempts: int) -> dict[tuple[str, int], str]:
    if attempts != 5 or len(tasks) % 2:
        raise ValueError("counterbalanced schedule is invalid")
    high_control = {str(task["name"]) for task in sorted(tasks, key=lambda task: _rank(seed, "task", task["name"]))[:len(tasks) // 2]}
    first: dict[tuple[str, int], str] = {}
    for task in tasks:
        name = str(task["name"])
        control_attempts = {
            attempt for attempt in sorted(range(1, attempts + 1), key=lambda attempt: _rank(seed, name, attempt))
            [:3 if name in high_control else 2]
        }
        for attempt in range(1, attempts + 1):
            first[name, attempt] = "control" if attempt in control_attempts else "chaos-engine"
    return first


def plan(manifest: object, campaign: str) -> dict[str, object]:
    """Build exact balanced two-arm work; Harbor retains native trial IDs."""
    value = _object(manifest, "experiment manifest")
    selected, tasks = _campaign(value, campaign)
    schedule = _schedule(value)
    if selected["privateResolutionRequired"] and len(private_resolution(value)["tasks"]) != 4:
        raise ValueError("private task metadata is invalid")
    seed, attempts, revision = value.get("seed"), value.get("attemptsPerTask"), value.get("implementationRevision")
    if not isinstance(seed, int) or not isinstance(attempts, int) or not GIT_SHA.fullmatch(str(revision)):
        raise ValueError("implementation identity is invalid")
    first_arms = _first_arms(tasks, seed, attempts)
    pairs = []
    for task in tasks:
        name = str(task["name"])
        for attempt in range(1, attempts + 1):
            first = first_arms[name, attempt]
            pairs.append({
                "pairId": f"{name}__{attempt}", "task": name, "sha256": task["sha256"], "attempt": attempt,
                "arms": [first, ARMS[1 - ARMS.index(first)]], "retryBudget": schedule["retryBudget"],
            })
    if len(pairs) * len(ARMS) != selected["taskCount"] * attempts * len(ARMS):
        raise ValueError("campaign trial matrix is invalid")
    if Counter(pair["arms"][0] for pair in pairs) != Counter({"control": len(pairs) // 2, "chaos-engine": len(pairs) // 2}):
        raise ValueError("counterbalanced schedule is invalid")
    return {"schemaVersion": 1, "campaign": campaign, "implementationRevision": revision, "trials": len(pairs) * 2, "pairs": pairs}


def _pair_job_name(campaign: str, pair_id: str) -> str:
    return f"chaos-gauge-{campaign}-{hashlib.sha256(pair_id.encode()).hexdigest()[:16]}"


def pair_job_configs(manifest: object, campaign: str) -> dict[str, dict[str, object]]:
    """Build one native two-arm Harbor job per planned pair without changing resources or budgets."""
    value = _object(manifest, "experiment manifest")
    planned = plan(value, campaign)
    validator = _load_validator()
    templates = validator.load_jobs(ROOT, campaign)
    tasks = {str(task["name"]): task for task in value["tasks"] if isinstance(task, dict)}
    configs = {}
    for pair in planned["pairs"]:
        task = _object(tasks.get(str(pair["task"])), "experiment task")
        first, second = pair["arms"]
        template = _object(templates[first], "Harbor job")
        config = copy.deepcopy(template)
        config["job_name"] = _pair_job_name(campaign, str(pair["pairId"]))
        config["n_attempts"] = 1
        config["n_concurrent_trials"] = 2
        config["agents"] = [copy.deepcopy(_object(templates[arm], "Harbor job")["agents"][0]) for arm in (first, second)]
        public = task.get("visibility") == "public"
        dataset = next((item for item in config["datasets"] if isinstance(item, dict) and ("path" in item) == public), None)
        if not isinstance(dataset, dict):
            raise ValueError("Harbor pair dataset is invalid")
        config["datasets"] = [{**dataset, "task_names": [pair["task"]]}]
        configs[str(pair["pairId"])] = config
    return configs


def _pair_task_name(trial: object) -> str:
    task = getattr(trial, "task", None)
    get_task_id = getattr(task, "get_task_id", None)
    task_id = get_task_id() if callable(get_task_id) else None
    get_name = getattr(task_id, "get_name", None)
    if not callable(get_name):
        raise ValueError("resolved Harbor trials are invalid")
    return str(get_name())


def bind_pair_jobs(manifest: object, planned: object, jobs: object, *, prepared_at: object) -> dict[str, object]:
    """Map 80 pre-run native pair jobs to their exact generated trial names."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    supplied = _object(jobs, "resolved Harbor pair jobs")
    if set(supplied) != {str(pair["pairId"]) for pair in canonical["pairs"]}:
        raise ValueError("resolved Harbor pair jobs are invalid")
    prepared = _timestamp(prepared_at)
    names: set[str] = set()
    bindings = []
    for pair in canonical["pairs"]:
        pair_id, job = str(pair["pairId"]), supplied[str(pair["pairId"])]
        if getattr(job, "_existing_job_result", None) is not None or getattr(job, "_job_result", None) is not None:
            raise ValueError("resolved Harbor job has already started")
        trials = getattr(job, "_trial_configs", None)
        config = getattr(job, "config", None)
        job_name = getattr(config, "job_name", None)
        if not isinstance(trials, list) or len(trials) != 2 or job_name != _pair_job_name(str(canonical["campaign"]), pair_id):
            raise ValueError("resolved Harbor pair job is invalid")
        arms = {}
        for arm, trial in zip(pair["arms"], trials):
            name = _native_trial_name(str(pair["task"]), getattr(trial, "trial_name", None))
            if _pair_task_name(trial) != pair["task"] or name in names:
                raise ValueError("resolved Harbor pair identity is invalid")
            names.add(name)
            arms[arm] = name
        bindings.append({
            "pairId": pair_id, "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"],
            "jobId": _job_id(getattr(job, "id", None)), "jobName": job_name, "arms": arms,
        })
    return {
        "schemaVersion": 2, "campaign": canonical["campaign"], "implementationRevision": canonical["implementationRevision"],
        "planSha256": _plan_digest(canonical), "preparedAt": prepared.isoformat(), "pairs": bindings,
    }


def _canonical_pair_bindings(manifest: dict[str, object], planned: dict[str, object], bindings: object) -> tuple[dict[str, dict[str, object]], datetime]:
    value = _object(bindings, "native pair bindings")
    expected = {"schemaVersion": 2, "campaign": planned["campaign"], "implementationRevision": planned["implementationRevision"], "planSha256": _plan_digest(planned)}
    if any(value.get(key) != item for key, item in expected.items()):
        raise ValueError("native pair bindings are invalid")
    prepared = _timestamp(value.get("preparedAt"))
    raw_pairs = value.get("pairs")
    if not isinstance(raw_pairs, list) or len(raw_pairs) != len(planned["pairs"]):
        raise ValueError("native pair binding matrix is invalid")
    expected_pairs = {str(pair["pairId"]): pair for pair in planned["pairs"]}
    bound = {}
    identities: set[str] = set()
    for raw in raw_pairs:
        pair = _object(raw, "native pair binding")
        pair_id = str(pair.get("pairId"))
        source = expected_pairs.get(pair_id)
        if source is None or any(pair.get(key) != source[key] for key in ("task", "sha256", "attempt")):
            raise ValueError("native pair binding identity is invalid")
        if pair.get("jobName") != _pair_job_name(str(planned["campaign"]), pair_id):
            raise ValueError("native pair job identity is invalid")
        job_id = _job_id(pair.get("jobId"))
        arms = _object(pair.get("arms"), "native pair binding")
        if set(arms) != set(ARMS):
            raise ValueError("native pair binding arms are invalid")
        for arm in ARMS:
            name = _native_trial_name(str(source["task"]), arms[arm])
            if name in identities:
                raise ValueError("native trial identity is reused")
            identities.add(name)
        if pair_id in bound:
            raise ValueError("native pair binding identity is invalid")
        bound[pair_id] = {"jobId": job_id, "arms": arms}
    if set(bound) != set(expected_pairs):
        raise ValueError("native pair binding matrix is invalid")
    return bound, prepared


def _native_trial_name(task: str, value: object) -> str:
    name = str(value)
    prefix = task.rsplit("/", 1)[-1][:32].rstrip("_-") + "__"
    if not name.startswith(prefix) or not NATIVE_NAME.fullmatch(name.removeprefix(prefix)):
        raise ValueError("Harbor native trial identity is invalid")
    return name


def _arm(manifest: dict[str, object], name: str) -> dict[str, object]:
    matches = [item for item in manifest["arms"] if isinstance(item, dict) and item.get("name") == name]
    if len(matches) != 1:
        raise ValueError("experiment arm identity is invalid")
    return matches[0]


def _agent_matches(value: object, expected: object) -> bool:
    actual, wanted = _object(value, "Harbor agent identity"), _object(expected, "configured agent identity")
    return (
        actual.get("name") == wanted.get("name")
        and actual.get("import_path") == wanted.get("import_path")
        and actual.get("model_name") == wanted.get("model_name")
        and actual.get("kwargs") == wanted.get("kwargs")
    )


def _task_counts(pairs: list[dict[str, object]]) -> Counter[tuple[str, str]]:
    return Counter((str(pair["task"]), str(pair["sha256"])) for pair in pairs)


def _validate_lock(lock: object, expected_agent: object, pairs: list[dict[str, object]]) -> None:
    value = _object(lock, "Harbor job lock")
    harbor, retry = _object(value.get("harbor"), "Harbor lock"), _object(value.get("retry"), "Harbor retry lock")
    if value.get("schema_version") != 3 or harbor.get("version") != "0.22.0" or value.get("n_concurrent_trials") != 2:
        raise ValueError("Harbor lock identity is invalid")
    if retry.get("max_retries") != 2 or set(retry.get("include_exceptions", [])) != {"EnvironmentStartError", "EnvironmentBuildError"}:
        raise ValueError("Harbor lock retry identity is invalid")
    trials = value.get("trials")
    if not isinstance(trials, list) or len(trials) != len(pairs):
        raise ValueError("Harbor lock trial matrix is invalid")
    observed: Counter[tuple[str, str]] = Counter()
    for raw in trials:
        trial = _object(raw, "Harbor lock trial")
        task = _object(trial.get("task"), "Harbor lock task")
        if not _agent_matches(trial.get("agent"), expected_agent):
            raise ValueError("Harbor lock arm identity is invalid")
        observed[str(task.get("name")), str(task.get("digest", "")).removeprefix("sha256:")] += 1
    if observed != _task_counts(pairs):
        raise ValueError("Harbor lock task identity is invalid")


def _trials(job: object, arm: str, expected_agent: object, expected_job_id: str, lock: object, pairs: list[dict[str, object]]) -> tuple[dict[str, dict[str, object]], int]:
    value = _object(job, f"{arm} Harbor result")
    if _job_id(value.get("id")) != expected_job_id:
        raise ValueError("Harbor job identity is invalid")
    stats = _object(value.get("stats"), "Harbor retries")
    retries = stats.get("n_retries")
    if isinstance(retries, bool) or not isinstance(retries, int) or retries < 0:
        raise ValueError("Harbor job retry evidence is invalid")
    _validate_lock(lock, expected_agent, pairs)
    results = value.get("trial_results")
    if not isinstance(results, list) or len(results) != len(pairs):
        raise ValueError("Harbor trial matrix is incomplete")
    expected = _task_counts(pairs)
    grouped: dict[str, list[dict[str, object]]] = defaultdict(list)
    by_name: dict[str, dict[str, object]] = {}
    names: set[str] = set()
    for raw in results:
        trial = _object(raw, "Harbor trial")
        task, checksum = str(trial.get("task_name")), str(trial.get("task_checksum"))
        if (task, checksum) not in expected:
            raise ValueError("Harbor task identity is invalid")
        native_name = _native_trial_name(task, trial.get("trial_name"))
        if native_name in names:
            raise ValueError("Harbor native trial identity is invalid")
        names.add(native_name)
        config = _object(trial.get("config"), "Harbor trial config")
        if not _agent_matches(config.get("agent"), expected_agent):
            raise ValueError("Harbor arm identity is invalid")
        agent, model = _object(trial.get("agent_info"), "Harbor agent identity"), _object(_object(trial.get("agent_info"), "Harbor agent identity").get("model_info"), "Harbor model identity")
        if agent.get("name") != "codex" or agent.get("version") != "0.118.0" or model != {"name": "gpt-5.6-terra", "provider": "openai"}:
            raise ValueError("Harbor agent identity is invalid")
        if trial.get("verifier_environment_mode") != "separate":
            raise ValueError("Harbor verifier isolation is invalid")
        started = _timestamp(_object(trial.get("agent_execution"), "Harbor timing").get("started_at"))
        item = {"trialName": native_name, "started": started, "task": task, "sha256": checksum}
        grouped[task].append(item)
        by_name[native_name] = item
    if Counter((task, item["sha256"]) for task, items in grouped.items() for item in items) != expected:
        raise ValueError("Harbor task identity is invalid")
    return by_name, retries


def _timestamp(value: object) -> datetime:
    try:
        parsed = datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError("Harbor start timestamp is invalid") from error
    if parsed.tzinfo is None:
        raise ValueError("Harbor start timestamp is invalid")
    return parsed


def _job_id(value: object) -> str:
    try:
        return str(UUID(str(value)))
    except (AttributeError, ValueError) as error:
        raise ValueError("Harbor job identity is invalid") from error


def _canonical_plan(manifest: dict[str, object], planned: object) -> dict[str, object]:
    value = _object(planned, "planned campaign")
    campaign = value.get("campaign")
    if not isinstance(campaign, str) or value != plan(manifest, campaign):
        raise ValueError("planned campaign is invalid")
    return value


def _plan_digest(planned: dict[str, object]) -> str:
    return hashlib.sha256(json.dumps(planned, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def _bind_native_trials(manifest: object, planned: object, native_trials: object, *, job_ids: object, prepared_at: object) -> dict[str, object]:
    """Bind Harbor-created native names to planned attempts before either job starts."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    prepared = _timestamp(prepared_at)
    supplied = _object(native_trials, "native trial bindings")
    if set(supplied) != set(ARMS):
        raise ValueError("native trial binding arms are invalid")
    supplied_job_ids = _object(job_ids, "Harbor job identities")
    if set(supplied_job_ids) != set(ARMS):
        raise ValueError("Harbor job identity is invalid")
    grouped: dict[str, dict[str, list[str]]] = {arm: defaultdict(list) for arm in ARMS}
    names: set[str] = set()
    for arm in ARMS:
        trials = supplied[arm]
        if not isinstance(trials, list) or len(trials) != len(canonical["pairs"]):
            raise ValueError("native trial binding matrix is invalid")
        for raw in trials:
            trial = _object(raw, "native trial binding")
            task = str(trial.get("task"))
            name = _native_trial_name(task, trial.get("nativeTrialName"))
            if name in names:
                raise ValueError("native trial identity is reused")
            names.add(name)
            grouped[arm][task].append(name)
    bindings = []
    for task in sorted({str(pair["task"]) for pair in canonical["pairs"]}):
        task_pairs = [pair for pair in canonical["pairs"] if pair["task"] == task]
        if any(len(grouped[arm][task]) != len(task_pairs) for arm in ARMS):
            raise ValueError("native trial binding task matrix is invalid")
        for index, pair in enumerate(task_pairs):
            bindings.append({
                "pairId": pair["pairId"], "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"],
                "arms": {arm: grouped[arm][task][index] for arm in ARMS},
            })
    return {
        "schemaVersion": 1, "campaign": canonical["campaign"], "implementationRevision": canonical["implementationRevision"],
        "planSha256": _plan_digest(canonical), "preparedAt": prepared.isoformat(),
        "jobIds": {arm: _job_id(supplied_job_ids[arm]) for arm in ARMS}, "pairs": bindings,
    }


def bind_resolved_jobs(manifest: object, planned: object, jobs: object, *, prepared_at: object) -> dict[str, object]:
    """Bind exact resolved Harbor Job instances before their scheduler launches them.

    The caller retains these same instances and calls ``run`` only after writing
    this artifact; a started or resumed job cannot establish a new binding.
    """
    supplied = _object(jobs, "resolved Harbor jobs")
    if set(supplied) != set(ARMS):
        raise ValueError("resolved Harbor jobs are invalid")
    native_trials = {}
    job_ids = {}
    for arm in ARMS:
        job = supplied[arm]
        if getattr(job, "_existing_job_result", None) is not None or getattr(job, "_job_result", None) is not None:
            raise ValueError("resolved Harbor job has already started")
        trials = getattr(job, "_trial_configs", None)
        if not isinstance(trials, list):
            raise ValueError("resolved Harbor trials are invalid")
        native_trials[arm] = []
        for trial in trials:
            task = getattr(trial, "task", None)
            get_task_id = getattr(task, "get_task_id", None)
            task_id = get_task_id() if callable(get_task_id) else None
            get_name = getattr(task_id, "get_name", None)
            if not callable(get_name):
                raise ValueError("resolved Harbor trials are invalid")
            native_trials[arm].append({"task": get_name(), "nativeTrialName": getattr(trial, "trial_name", None)})
        job_ids[arm] = getattr(job, "id", None)
    return _bind_native_trials(manifest, planned, native_trials, job_ids=job_ids, prepared_at=prepared_at)


def _bindings_digest(bindings: object) -> str:
    return hashlib.sha256(json.dumps(bindings, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def write_native_bindings(path: Path, bindings: object) -> None:
    """Create one immutable, durable launch map before a resolved Job starts."""
    target = Path(path)
    if not target.parent.is_dir() or target.parent.is_symlink() or target.is_symlink():
        raise ValueError("native trial binding output is invalid")
    payload = json.dumps(bindings, sort_keys=True, indent=2).encode() + b"\n"
    try:
        descriptor = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError as error:
        raise ValueError("native trial bindings already exist") from error
    with os.fdopen(descriptor, "wb") as output:
        output.write(payload)
        output.flush()
        os.fsync(output.fileno())
    directory = os.open(target.parent, os.O_RDONLY)
    try:
        os.fsync(directory)
    finally:
        os.close(directory)


def bind_and_persist_resolved_jobs(manifest: object, planned: object, jobs: object, path: Path, *, prepared_at: object) -> dict[str, object]:
    """Persist launch map from same pre-run Jobs; scheduler calls their ``run`` only after return."""
    bindings = bind_resolved_jobs(manifest, planned, jobs, prepared_at=prepared_at)
    write_native_bindings(path, bindings)
    return bindings


def _install_pair_start_gate(job: object, first_name: str, second_name: str) -> None:
    """Prevent second planned arm entering agent phase before first arm starts."""
    from harbor.trial.hooks import TrialEvent

    first_started, first_failed = asyncio.Event(), asyncio.Event()

    async def agent_started(event) -> None:
        if event.trial_name == first_name:
            first_started.set()
            return
        if event.trial_name != second_name:
            raise ValueError("Harbor pair start identity is invalid")
        waiting = [asyncio.create_task(first_started.wait()), asyncio.create_task(first_failed.wait())]
        done, pending = await asyncio.wait(waiting, return_when=asyncio.FIRST_COMPLETED)
        for task in pending:
            task.cancel()
        await asyncio.gather(*pending, return_exceptions=True)
        if waiting[1] in done and not first_started.is_set():
            raise ValueError("planned first Harbor arm did not start")

    async def terminal(event) -> None:
        if event.trial_name == first_name and not first_started.is_set():
            first_failed.set()

    job.add_hook(TrialEvent.AGENT_START, agent_started)
    job.add_hook(TrialEvent.END, terminal)
    job.add_hook(TrialEvent.CANCEL, terminal)


async def launch_pair_jobs(manifest: object, campaign: str, launch_map: Path, *, prepared_at: object, create_job=None, install_gate: Callable[[object, str, str], None] = _install_pair_start_gate) -> dict[str, object]:
    """Create all 80 pair jobs, durably bind native IDs, then run at most two pairs at once."""
    value = _object(manifest, "experiment manifest")
    planned = plan(value, campaign)
    configs = pair_job_configs(value, campaign)
    if create_job is None:
        from harbor.job import Job
        from harbor.models.job.config import JobConfig

        async def create_job(config):
            return await Job.create(JobConfig.model_validate(config))
    jobs = {pair_id: await create_job(config) for pair_id, config in configs.items()}
    bindings = bind_pair_jobs(value, planned, jobs, prepared_at=prepared_at)
    write_native_bindings(launch_map, bindings)
    bound, _ = _canonical_pair_bindings(value, planned, bindings)
    semaphore = asyncio.Semaphore(2)

    async def run_pair(pair: dict[str, object]) -> tuple[str, object]:
        pair_id = str(pair["pairId"])
        first, second = pair["arms"]
        install_gate(jobs[pair_id], bound[pair_id]["arms"][first], bound[pair_id]["arms"][second])
        async with semaphore:
            return pair_id, await jobs[pair_id].run()

    pairs = [_object(pair, "planned pair") for pair in planned["pairs"]]
    tasks = {}
    async with asyncio.TaskGroup() as group:
        for pair in pairs:
            tasks[str(pair["pairId"])] = group.create_task(run_pair(pair))
    completed = {pair_id: task.result()[1] for pair_id, task in tasks.items()}
    return {"bindings": bindings, "results": completed}


def _validate_pair_lock(lock: object, pair: dict[str, object], jobs: dict[str, object]) -> None:
    value = _object(lock, "Harbor pair job lock")
    harbor, retry = _object(value.get("harbor"), "Harbor lock"), _object(value.get("retry"), "Harbor retry lock")
    if value.get("schema_version") != 3 or harbor.get("version") != "0.22.0" or value.get("n_concurrent_trials") != 2:
        raise ValueError("Harbor pair lock identity is invalid")
    if retry.get("max_retries") != 2 or set(retry.get("include_exceptions", [])) != {"EnvironmentStartError", "EnvironmentBuildError"}:
        raise ValueError("Harbor pair lock retry identity is invalid")
    trials = value.get("trials")
    if not isinstance(trials, list) or len(trials) != len(ARMS):
        raise ValueError("Harbor pair lock trial matrix is invalid")
    for arm, raw in zip(pair["arms"], trials):
        trial, task = _object(raw, "Harbor pair lock trial"), _object(_object(raw, "Harbor pair lock trial").get("task"), "Harbor lock task")
        if task.get("name") != pair["task"] or task.get("digest") != f"sha256:{pair['sha256']}" or not _agent_matches(trial.get("agent"), _object(jobs[arm], "Harbor job")["agents"][0]):
            raise ValueError("Harbor pair lock identity is invalid")


def _pair_trials(result: object, pair: dict[str, object], binding: dict[str, object], jobs: dict[str, object], lock: object, prepared: datetime) -> tuple[dict[str, dict[str, object]], int]:
    value = _object(result, "Harbor pair result")
    if _job_id(value.get("id")) != binding["jobId"]:
        raise ValueError("Harbor pair job identity is invalid")
    stats = _object(value.get("stats"), "Harbor pair retries")
    retries = stats.get("n_retries")
    if isinstance(retries, bool) or not isinstance(retries, int) or retries < 0:
        raise ValueError("Harbor pair retry evidence is invalid")
    _validate_pair_lock(lock, pair, jobs)
    raw_trials = value.get("trial_results")
    if not isinstance(raw_trials, list) or len(raw_trials) != len(ARMS):
        raise ValueError("Harbor pair trial matrix is incomplete")
    expected_names = _object(binding["arms"], "native pair binding")
    by_arm: dict[str, dict[str, object]] = {}
    for raw in raw_trials:
        trial = _object(raw, "Harbor pair trial")
        task, checksum = str(trial.get("task_name")), str(trial.get("task_checksum"))
        native_name = _native_trial_name(task, trial.get("trial_name"))
        arm = next((candidate for candidate in ARMS if expected_names.get(candidate) == native_name), None)
        if arm is None or arm in by_arm or task != pair["task"] or checksum != pair["sha256"]:
            raise ValueError("Harbor native pair identity is invalid")
        config = _object(trial.get("config"), "Harbor pair config")
        agent, model = _object(trial.get("agent_info"), "Harbor agent identity"), _object(_object(trial.get("agent_info"), "Harbor agent identity").get("model_info"), "Harbor model identity")
        if not _agent_matches(config.get("agent"), _object(jobs[arm], "Harbor job")["agents"][0]) or agent.get("name") != "codex" or agent.get("version") != "0.118.0" or model != {"name": "gpt-5.6-terra", "provider": "openai"}:
            raise ValueError("Harbor pair arm identity is invalid")
        if trial.get("verifier_environment_mode") != "separate":
            raise ValueError("Harbor verifier isolation is invalid")
        started = _timestamp(_object(trial.get("agent_execution"), "Harbor timing").get("started_at"))
        if started <= prepared:
            raise ValueError("native pair bindings were not prepared before execution")
        by_arm[arm] = {"trialName": native_name, "started": started}
    if set(by_arm) != set(ARMS):
        raise ValueError("Harbor native pair binding is incomplete")
    first, second = pair["arms"]
    if by_arm[first]["started"] >= by_arm[second]["started"]:
        raise ValueError("observed Harbor start order is invalid")
    return by_arm, retries


def collect_pair_jobs(manifest: object, planned: object, pair_results: object, *, native_bindings: object, private_resolution: object | None, execution_revision: str, repository: Path, run: Callable[[list[str]], str]) -> dict[str, object]:
    """Collect only the 80 pre-bound pair Jobs; no timestamp-based pairing is permitted."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    campaign = str(canonical["campaign"])
    selected, _ = _campaign(value, campaign)
    if selected["privateResolutionRequired"] and private_resolution != globals()["private_resolution"](value):
        raise ValueError("full-pilot private package is unresolved")
    validate_execution_revision(repository, execution_revision, value["implementationRevision"], run)
    validator = _load_validator()
    jobs = validator.load_jobs(ROOT, campaign)
    validator.validate_job_contracts(value, jobs, campaign=campaign, root=ROOT)
    bound, prepared = _canonical_pair_bindings(value, canonical, native_bindings)
    supplied = _object(pair_results, "Harbor pair results")
    if set(supplied) != set(bound):
        raise ValueError("Harbor pair results are incomplete")
    observed, completed, result_material = [], {}, {}
    retries = 0
    for pair in canonical["pairs"]:
        pair_id = str(pair["pairId"])
        evidence = _object(supplied[pair_id], "Harbor pair result")
        if set(evidence) != {"result", "lock"}:
            raise ValueError("Harbor pair result is invalid")
        records, pair_retries = _pair_trials(evidence["result"], pair, bound[pair_id], jobs, evidence["lock"], prepared)
        retries += pair_retries
        observed.append({"pairId": pair_id, "plannedFirstArm": pair["arms"][0], "observedFirstArm": pair["arms"][0], "nativeTrialNames": bound[pair_id]["arms"], "jobId": bound[pair_id]["jobId"]})
        completed[pair_id] = {
            "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"], "implementationRevision": canonical["implementationRevision"],
            "arms": {arm: {"treatmentSha256": _object(_arm(value, arm)["treatmentSha256"], "treatment identity")[campaign], "nativeTrialName": bound[pair_id]["arms"][arm]} for arm in ARMS},
        }
        result_material[pair_id] = evidence
    return {
        "schemaVersion": 2, "campaign": campaign, "implementationRevision": value["implementationRevision"], "executionRevision": execution_revision,
        "planSha256": _plan_digest(canonical), "nativeBindingsSha256": _bindings_digest(native_bindings),
        "resultsSha256": hashlib.sha256(json.dumps(result_material, sort_keys=True, separators=(",", ":"), default=str).encode()).hexdigest(),
        "jobRetryAccounting": {"pairJobs": retries}, "pairAccounting": {"planned": len(canonical["pairs"]), "completed": len(observed)},
        "trialAccounting": {"planned": len(canonical["pairs"]) * 2, "observed": len(observed) * 2}, "pairs": observed, "completedPairs": completed,
    }


def resume_pair_jobs(manifest: object, planned: object, bindings: object, jobs: object, completed: object) -> dict[str, object]:
    """Resume only incomplete, already-created pair Jobs, restoring their bound native names."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    bound, _ = _canonical_pair_bindings(value, canonical, bindings)
    supplied = _object(jobs, "resolved Harbor pair jobs")
    state = _object(completed, "completed pairs")
    remaining = resume(value, canonical, state)
    for pair in canonical["pairs"]:
        pair_id = str(pair["pairId"])
        if pair_id not in state:
            continue
        evidence = _object(_object(state[pair_id], "resume pair").get("arms"), "resume pair")
        if any(_object(evidence.get(arm), "resume arm").get("nativeTrialName") != bound[pair_id]["arms"][arm] for arm in ARMS):
            raise ValueError("resume native pair binding is invalid")
    remaining_ids = {str(pair["pairId"]) for pair in remaining["pairs"]}
    if set(supplied) != remaining_ids:
        raise ValueError("resolved Harbor pair jobs are invalid")
    for pair in remaining["pairs"]:
        pair_id, job = str(pair["pairId"]), supplied[str(pair["pairId"])]
        if _job_id(getattr(job, "id", None)) != bound[pair_id]["jobId"] or getattr(getattr(job, "config", None), "job_name", None) != _pair_job_name(str(canonical["campaign"]), pair_id):
            raise ValueError("Harbor pair job identity is invalid")
        trials = getattr(job, "_remaining_trial_configs", None)
        if not isinstance(trials, list) or len(trials) != len(ARMS):
            raise ValueError("resolved Harbor pair trial matrix is invalid")
        for arm, trial in zip(pair["arms"], trials):
            if _pair_task_name(trial) != pair["task"]:
                raise ValueError("resolved Harbor pair identity is invalid")
            trial.trial_name = bound[pair_id]["arms"][arm]
    return remaining


def _canonical_native_bindings(manifest: dict[str, object], planned: dict[str, object], bindings: object) -> tuple[dict[str, dict[str, str]], datetime, dict[str, str]]:
    value = _object(bindings, "native trial bindings")
    expected = {
        "schemaVersion": 1, "campaign": planned["campaign"], "implementationRevision": planned["implementationRevision"], "planSha256": _plan_digest(planned),
    }
    if any(value.get(key) != item for key, item in expected.items()):
        raise ValueError("native trial bindings are invalid")
    prepared = _timestamp(value.get("preparedAt"))
    job_ids = _object(value.get("jobIds"), "Harbor job identities")
    if set(job_ids) != set(ARMS):
        raise ValueError("Harbor job identity is invalid")
    pairs = value.get("pairs")
    if not isinstance(pairs, list) or len(pairs) != len(planned["pairs"]):
        raise ValueError("native trial binding matrix is invalid")
    expected_pairs = {str(pair["pairId"]): pair for pair in planned["pairs"]}
    bound: dict[str, dict[str, str]] = {}
    names: set[str] = set()
    for raw in pairs:
        pair = _object(raw, "native trial binding")
        pair_id = str(pair.get("pairId"))
        expected_pair = expected_pairs.get(pair_id)
        if expected_pair is None or any(pair.get(key) != expected_pair[key] for key in ("task", "sha256", "attempt")):
            raise ValueError("native trial binding identity is invalid")
        arms = _object(pair.get("arms"), "native trial binding")
        if set(arms) != set(ARMS):
            raise ValueError("native trial binding arms are invalid")
        mapped = {}
        for arm in ARMS:
            name = _native_trial_name(str(expected_pair["task"]), arms[arm])
            if name in names:
                raise ValueError("native trial identity is reused")
            names.add(name)
            mapped[arm] = name
        if pair_id in bound:
            raise ValueError("native trial binding identity is invalid")
        bound[pair_id] = mapped
    if set(bound) != set(expected_pairs):
        raise ValueError("native trial binding matrix is invalid")
    return bound, prepared, {arm: _job_id(job_ids[arm]) for arm in ARMS}


def resume_resolved_jobs(manifest: object, planned: object, bindings: object, jobs: object, completed: object) -> dict[str, object]:
    """Restore bound native names only on same Harbor jobs after a persisted interruption."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    bound, _, job_ids = _canonical_native_bindings(value, canonical, bindings)
    supplied = _object(jobs, "resolved Harbor jobs")
    state = _object(completed, "completed pairs")
    remaining = resume(value, canonical, state)
    for pair_id, record in state.items():
        pair = next((item for item in canonical["pairs"] if item["pairId"] == pair_id), None)
        if pair is None:
            continue
        _completed_pair(value, canonical, _object(pair, "planned pair"), record)
        arms = _object(_object(record, "resume pair").get("arms"), "resume pair")
        if any(arms[arm].get("nativeTrialName") != bound[pair_id][arm] for arm in ARMS):
            raise ValueError("resume native trial binding is invalid")
    for arm in ARMS:
        job = supplied.get(arm)
        if _job_id(getattr(job, "id", None)) != job_ids[arm]:
            raise ValueError("Harbor job identity is invalid")
        trials = getattr(job, "_remaining_trial_configs", None)
        if not isinstance(trials, list) or len(trials) != len(remaining["pairs"]):
            raise ValueError("resolved Harbor trial matrix is invalid")
        per_task: dict[str, list[dict[str, object]]] = defaultdict(list)
        for pair in remaining["pairs"]:
            per_task[str(pair["task"])].append(pair)
        cursors: Counter[str] = Counter()
        for trial in trials:
            task = getattr(trial, "task", None)
            get_task_id = getattr(task, "get_task_id", None)
            task_id = get_task_id() if callable(get_task_id) else None
            get_name = getattr(task_id, "get_name", None)
            name = get_name() if callable(get_name) else None
            if name not in per_task or cursors[name] >= len(per_task[name]):
                raise ValueError("resolved Harbor trial identity is invalid")
            pair = per_task[name][cursors[name]]
            cursors[name] += 1
            trial.trial_name = bound[str(pair["pairId"])][arm]
        if any(cursors[task] != len(pairs) for task, pairs in per_task.items()):
            raise ValueError("resolved Harbor trial matrix is invalid")
    return remaining


def _completed_pair(manifest: dict[str, object], planned: dict[str, object], pair: dict[str, object], record: object) -> set[str]:
    value = _object(record, "resume pair")
    expected = {"task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"], "implementationRevision": planned["implementationRevision"]}
    if any(value.get(key) != item for key, item in expected.items()):
        raise ValueError("resume pair identity is invalid")
    arms = _object(value.get("arms"), "resume pair")
    if set(arms) != set(ARMS):
        raise ValueError("resume pair is incomplete")
    names = set()
    for arm in ARMS:
        evidence = _object(arms[arm], "resume arm")
        treatment = _object(_arm(manifest, arm).get("treatmentSha256"), "treatment identity").get(planned["campaign"])
        name = _native_trial_name(str(pair["task"]), evidence.get("nativeTrialName"))
        if evidence.get("treatmentSha256") != treatment:
            raise ValueError("resume pair identity is invalid")
        if name in names:
            raise ValueError("native trial identity is reused")
        names.add(name)
    return names


def resume(manifest: object, planned: object, completed: object) -> dict[str, object]:
    """Skip only completed native pairs that retain task, treatment, and revision proof."""
    value = _object(manifest, "experiment manifest")
    canonical = _canonical_plan(value, planned)
    state = _object(completed, "completed pairs")
    remaining = []
    known = set()
    native_names: set[str] = set()
    for raw in canonical["pairs"]:
        pair = _object(raw, "planned pair")
        pair_id = str(pair["pairId"])
        known.add(pair_id)
        if pair_id not in state:
            remaining.append(pair)
            continue
        for identity in _completed_pair(value, canonical, pair, state[pair_id]):
            if identity in native_names:
                raise ValueError("native trial identity is reused")
            native_names.add(identity)
    if set(state) - known:
        raise ValueError("resume pair is unknown")
    return {**canonical, "pairs": remaining, "trials": len(remaining) * len(ARMS)}


def _load_validator():
    spec = importlib.util.spec_from_file_location("chaos_gauge_validator", ROOT / "validate_experiment.py")
    if spec is None or spec.loader is None:
        raise ValueError("ChaosGauge validation is unavailable")
    validator = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(validator)
    return validator


def _validate_live_campaign(validator, manifest: dict[str, object], root: Path) -> None:
    validator.validate_manifest(manifest, root=root)
    for campaign in ("calibration", "full-pilot"):
        validator.validate_job_contracts(manifest, validator.load_jobs(root, campaign), campaign=campaign, root=root)


def validate_execution_revision(repository: Path, execution_revision: str, implementation_revision: object, run: Callable[[list[str]], str]) -> None:
    if not GIT_SHA.fullmatch(execution_revision) or execution_revision == implementation_revision:
        raise ValueError("execution revision must be a post-run merged SHA")
    prefix = ["git", "-C", str(repository)]
    try:
        if run([*prefix, "rev-parse", "--verify", f"{execution_revision}^{{commit}}"]).strip() != execution_revision:
            raise ValueError("execution revision is not an existing commit")
        if len(run([*prefix, "rev-list", "--parents", "-n", "1", execution_revision]).split()) < 3:
            raise ValueError("execution revision is not a merge commit")
        run([*prefix, "merge-base", "--is-ancestor", str(implementation_revision), execution_revision])
        run([*prefix, "merge-base", "--is-ancestor", execution_revision, "origin/main"])
    except subprocess.CalledProcessError as error:
        raise ValueError("execution revision proof is unavailable") from error


def collect(manifest: object, planned: object, control: object, candidate: object, *, control_lock: object, candidate_lock: object, native_bindings: object, private_resolution: object | None, execution_revision: str, repository: Path, run: Callable[[list[str]], str]) -> dict[str, object]:
    """Bind native Harbor results, locks, exact plan, and merged execution evidence."""
    value = _object(manifest, "experiment manifest")
    planned_value = _canonical_plan(value, planned)
    campaign = str(planned_value["campaign"])
    selected, _ = _campaign(value, campaign)
    if selected["privateResolutionRequired"] and private_resolution != globals()["private_resolution"](value):
        raise ValueError("full-pilot private package is unresolved")
    validate_execution_revision(repository, execution_revision, value["implementationRevision"], run)
    validator = _load_validator()
    jobs = validator.load_jobs(ROOT, campaign)
    validator.validate_job_contracts(value, jobs, campaign=campaign, root=ROOT)
    pairs = [_object(pair, "planned pair") for pair in planned_value["pairs"]]
    bindings, prepared, job_ids = _canonical_native_bindings(value, planned_value, native_bindings)
    records = {}
    retries = {}
    for arm, job, lock in (("control", control, control_lock), ("chaos-engine", candidate, candidate_lock)):
        records[arm], retries[arm] = _trials(job, arm, _object(jobs[arm], "Harbor job")["agents"][0], job_ids[arm], lock, pairs)
    observed = []
    completed = {}
    for pair in pairs:
        names = bindings[str(pair["pairId"])]
        first, second = pair["arms"]
        try:
            first_result, second_result = records[first][names[first]], records[second][names[second]]
        except KeyError as error:
            raise ValueError("Harbor native trial binding is incomplete") from error
        if any(item["task"] != pair["task"] or item["sha256"] != pair["sha256"] for item in (first_result, second_result)):
            raise ValueError("Harbor native trial binding is invalid")
        if prepared >= first_result["started"] or prepared >= second_result["started"]:
            raise ValueError("native trial bindings were not prepared before execution")
        if first_result["started"] >= second_result["started"]:
            raise ValueError("observed Harbor start order is invalid")
        observed.append({"pairId": pair["pairId"], "plannedFirstArm": first, "observedFirstArm": first, "nativeTrialNames": names})
        completed[pair["pairId"]] = {
            "task": pair["task"], "sha256": pair["sha256"], "attempt": pair["attempt"], "implementationRevision": planned_value["implementationRevision"],
            "arms": {arm: {"treatmentSha256": _object(_arm(value, arm)["treatmentSha256"], "treatment identity")[campaign], "nativeTrialName": names[arm]} for arm in ARMS},
        }
    plan_digest = _plan_digest(planned_value)
    result_digest = hashlib.sha256(json.dumps({"control": control, "chaos-engine": candidate, "controlLock": control_lock, "chaosEngineLock": candidate_lock}, sort_keys=True, separators=(",", ":"), default=str).encode()).hexdigest()
    return {
        "schemaVersion": 1, "campaign": campaign, "implementationRevision": value["implementationRevision"], "executionRevision": execution_revision,
        "planSha256": plan_digest, "nativeBindingsSha256": _bindings_digest(native_bindings), "resultsSha256": result_digest, "jobRetryAccounting": retries,
        "pairAccounting": {"planned": len(pairs), "completed": len(observed)}, "trialAccounting": {"planned": len(pairs) * 2, "observed": len(observed) * 2},
        "pairs": observed, "completedPairs": completed,
    }


def _run(command: list[str]) -> str:
    return subprocess.run(command, check=True, capture_output=True, text=True).stdout  # nosec B603 - fixed prerequisite probes.


def codex_version_is_pinned(output: str) -> bool:
    return output.strip() == "codex-cli 0.118.0"


def provider_capability_is_available(run: Callable[[list[str]], str]) -> bool:
    try:
        output = run(["codex", "exec", "--ephemeral", "--skip-git-repo-check", "--sandbox", "read-only", "--model", "gpt-5.6-terra", "-c", 'model_reasoning_effort="medium"', f"Reply with exactly {CAPABILITY_MARKER}."])
    except (OSError, subprocess.CalledProcessError):
        return False
    return output.strip() == CAPABILITY_MARKER


def full_preflight(manifest: object, checkout: Path, run: Callable[[list[str]], str] = _run) -> None:
    """Validate live content, private package, runtime, and paid-model capability before a run."""
    value = _object(manifest, "experiment manifest")
    package = _object(value.get("privatePackage"), "private package")
    if not checkout.is_dir() or checkout.is_symlink():
        raise ValueError("private checkout credentials are unavailable")
    _validate_live_campaign(_load_validator(), value, ROOT)
    repository = ROOT.parents[2]
    try:
        run(["git", "-C", str(repository), "cat-file", "-e", f"{value['implementationRevision']}:scripts/ci/chaos_gauge/campaign.py"])
    except subprocess.CalledProcessError as error:
        raise ValueError("implementation revision does not contain campaign source") from error
    if run(["git", "-C", str(checkout), "rev-parse", "HEAD"]).strip() != package["commit"]:
        raise ValueError("private checkout commit is invalid")
    if not run(["git", "-C", str(checkout), "ls-remote", "origin", "HEAD"]).strip():
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
    if not codex_version_is_pinned(run(["codex", "--version"])):
        raise ValueError("Codex version is invalid")
    if not provider_capability_is_available(run):
        raise ValueError("Codex provider credentials or gpt-5.6-terra capability is unavailable")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("plan", "preflight", "launch", "collect", "collect-pairs"))
    parser.add_argument("--campaign", choices=("calibration", "full-pilot"), required=True)
    parser.add_argument("--manifest", type=Path, default=ROOT / "experiment.json")
    parser.add_argument("--private-checkout", type=Path)
    parser.add_argument("--control", type=Path)
    parser.add_argument("--chaos-engine", type=Path)
    parser.add_argument("--control-lock", type=Path)
    parser.add_argument("--chaos-engine-lock", type=Path)
    parser.add_argument("--native-bindings", type=Path)
    parser.add_argument("--launch-map", type=Path)
    parser.add_argument("--pair-results", type=Path)
    parser.add_argument("--prepared-at")
    parser.add_argument("--resolution", type=Path)
    parser.add_argument("--execution-revision")
    parser.add_argument("--repository", type=Path, default=ROOT.parents[2])
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
    if args.command == "launch":
        if args.launch_map is None:
            raise ValueError("launch map is required")
        if args.campaign == "full-pilot":
            if args.private_checkout is None:
                raise ValueError("full launch requires a private checkout")
            full_preflight(manifest, args.private_checkout)
        else:
            _validate_live_campaign(_load_validator(), manifest, ROOT)
        prepared_at = args.prepared_at or datetime.now().astimezone().isoformat()
        launched = asyncio.run(launch_pair_jobs(manifest, args.campaign, args.launch_map, prepared_at=prepared_at))
        print(json.dumps({"launchMap": str(args.launch_map), "pairs": len(launched["results"])}, sort_keys=True))
        return 0
    if args.command == "collect-pairs":
        inputs = (args.pair_results, args.native_bindings, args.resolution, args.execution_revision, args.out)
        if any(item is None for item in inputs):
            raise ValueError("pair collection inputs are required")
        receipt = collect_pair_jobs(manifest, planned, json.loads(args.pair_results.read_text()), native_bindings=json.loads(args.native_bindings.read_text()), private_resolution=json.loads(args.resolution.read_text()), execution_revision=args.execution_revision, repository=args.repository, run=_run)
        args.out.write_text(json.dumps(receipt, sort_keys=True, indent=2) + "\n", encoding="utf-8")
        return 0
    inputs = (args.control, args.chaos_engine, args.control_lock, args.chaos_engine_lock, args.native_bindings, args.resolution, args.execution_revision, args.out)
    if any(item is None for item in inputs):
        raise ValueError("collection inputs are required")
    receipt = collect(manifest, planned, json.loads(args.control.read_text()), json.loads(args.chaos_engine.read_text()), control_lock=json.loads(args.control_lock.read_text()), candidate_lock=json.loads(args.chaos_engine_lock.read_text()), native_bindings=json.loads(args.native_bindings.read_text()), private_resolution=json.loads(args.resolution.read_text()), execution_revision=args.execution_revision, repository=args.repository, run=_run)
    args.out.write_text(json.dumps(receipt, sort_keys=True, indent=2) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
