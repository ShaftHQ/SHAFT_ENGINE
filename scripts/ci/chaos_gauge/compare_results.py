#!/usr/bin/env python3
"""Compare two Harbor ChaosGauge jobs with task-paired bootstrap inference."""

from __future__ import annotations

import argparse
import csv
import io
import json
import math
import os
import random
import tempfile
from datetime import datetime
from pathlib import Path


ARMS = ("control", "chaos-engine")
REWARDS = ("correctness", "safety", "cleanup")
BOOTSTRAP_ITERATIONS = 10_000
SCORE_VERSION = "chaos-gauge-60-20-20-v1"
EXCLUSION_REASONS = {"provider-outage", "harbor-infrastructure", "environment-start"}


def _round(value: float | None) -> float | None:
    return None if value is None else round(value, 6)


def _object(value: object, name: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be an object")
    return value


def _number(value: object, name: str, *, integer: bool = False) -> float | int:
    expected = int if integer else (int, float)
    if isinstance(value, bool) or not isinstance(value, expected):
        raise ValueError(f"{name} must be numeric")
    if value < 0 or not math.isfinite(float(value)):
        raise ValueError(f"{name} must be finite and non-negative")
    return value


def _seconds(value: object) -> float:
    timing = _object(value, "agent execution timing")
    try:
        start = datetime.fromisoformat(str(timing["started_at"]).replace("Z", "+00:00"))
        finish = datetime.fromisoformat(str(timing["finished_at"]).replace("Z", "+00:00"))
    except (KeyError, ValueError) as error:
        raise ValueError("agent execution timing is invalid") from error
    elapsed = (finish - start).total_seconds()
    if elapsed < 0:
        raise ValueError("agent execution timing is invalid")
    return elapsed


def _experiment(
    manifest: object, campaign: str, private_resolution: object = None
) -> tuple[dict[str, dict[str, object]], dict[str, str], int]:
    value = _object(manifest, "experiment manifest")
    arms = value.get("arms")
    tasks = value.get("tasks")
    if not isinstance(arms, list) or [arm.get("name") for arm in arms if isinstance(arm, dict)] != list(ARMS):
        raise ValueError("experiment arms are invalid")
    if not isinstance(tasks, list) or not tasks:
        raise ValueError("experiment tasks are invalid")
    arm_map = {str(arm["name"]): arm for arm in arms if isinstance(arm, dict)}
    all_tasks = {
        str(task["name"]): str(task["sha256"])
        for task in tasks
        if isinstance(task, dict) and "name" in task and "sha256" in task
    }
    if len(all_tasks) != len(tasks):
        raise ValueError("experiment task identities are invalid")
    campaigns = value.get("campaigns")
    if not isinstance(campaigns, dict) or campaign not in campaigns:
        raise ValueError("campaign identity is invalid")
    selected = campaigns[campaign]
    if not isinstance(selected, dict) or set(selected) != {"taskVisibility", "taskCount", "privateResolutionRequired"}:
        raise ValueError("campaign selection is invalid")
    visibility = selected["taskVisibility"]
    if not isinstance(visibility, list) or set(visibility) not in ({"public"}, {"public", "private-reference"}):
        raise ValueError("campaign visibility is invalid")
    task_map = {
        str(task["name"]): str(task["sha256"])
        for task in tasks if isinstance(task, dict) and task.get("visibility") in visibility
    }
    if selected["taskCount"] != len(task_map):
        raise ValueError("campaign task count is invalid")
    if selected["privateResolutionRequired"]:
        package = _object(value.get("privatePackage"), "private package")
        expected_resolution = {
            "name": package.get("name"),
            "ref": package.get("ref"),
            "tasks": [
                {"name": task["name"], "sha256": task["sha256"]}
                for task in tasks if task.get("visibility") == "private-reference"
            ],
        }
        if private_resolution != expected_resolution:
            raise ValueError("full-pilot private package is unresolved")
    attempts = value.get("attemptsPerTask")
    if not isinstance(attempts, int) or isinstance(attempts, bool) or attempts < 1:
        raise ValueError("experiment attempts are invalid")
    return arm_map, task_map, attempts


def _excluded(exclusions: list[dict[str, str]]) -> set[str]:
    names: set[str] = set()
    for exclusion in exclusions:
        if set(exclusion) != {"trialName", "reasonCode", "provenance"}:
            raise ValueError("exclusion fields are invalid")
        if exclusion["reasonCode"] not in EXCLUSION_REASONS or not exclusion["provenance"].strip():
            raise ValueError("exclusion taxonomy or provenance is invalid")
        if exclusion["trialName"] in names:
            raise ValueError("duplicate exclusion")
        names.add(exclusion["trialName"])
    return names


def _records(  # noqa: MC0001 - one pass preserves trial accounting and trust checks.
    job: object,
    arm_name: str,
    arm: dict[str, object],
    task_map: dict[str, str],
    attempts: int,
    exclusions: list[dict[str, str]],
) -> tuple[list[dict[str, object]], int]:
    value = _object(job, f"{arm_name} Harbor job")
    trials = value.get("trial_results")
    if not isinstance(trials, list):
        raise ValueError(f"{arm_name} Harbor trial results are missing")
    ignored = _excluded(exclusions)
    observed_names: set[str] = set()
    counts = {task: 0 for task in task_map}
    records: list[dict[str, object]] = []
    for raw in trials:
        trial = _object(raw, "Harbor trial")
        task = str(trial.get("task_name", ""))
        trial_name = str(trial.get("trial_name", ""))
        if task not in task_map or trial.get("task_checksum") != task_map[task]:
            raise ValueError("Harbor task identity or digest is incomparable")
        if not trial_name or trial_name in observed_names:
            raise ValueError("Harbor trial identity is invalid or duplicated")
        observed_names.add(trial_name)
        counts[task] += 1

        agent = _object(trial.get("agent_info"), "Harbor agent identity")
        model = _object(agent.get("model_info"), "Harbor model identity")
        if agent.get("name") != arm.get("agent"):
            raise ValueError("Harbor agent is incomparable with experiment manifest")
        if model.get("name") != arm.get("model"):
            raise ValueError("Harbor model is incomparable with experiment manifest")
        if not isinstance(agent.get("version"), str) or not str(agent["version"]).strip():
            raise ValueError("Harbor agent version is unavailable")
        if trial.get("verifier_environment_mode") != "separate":
            raise ValueError("Harbor verifier was not isolated")

        exception = trial.get("exception_info")
        verifier = trial.get("verifier_result")
        unavailable = verifier is None and exception is not None
        if unavailable:
            checked_rewards = {name: None for name in REWARDS}
        else:
            rewards = _object(_object(verifier, "Harbor verifier result").get("rewards"), "Harbor rewards")
            if set(rewards) != set(REWARDS):
                raise ValueError("Harbor rewards are incomplete")
            checked_rewards = {}
            for name in REWARDS:
                score = _number(rewards[name], f"Harbor {name} reward")
                if not 0 <= float(score) <= 1:
                    raise ValueError(f"Harbor {name} reward is outside zero and one")
                checked_rewards[name] = float(score)

        context = trial.get("agent_result")
        tokens: int | None = None
        cost: float | None = None
        if isinstance(context, dict):
            input_tokens = context.get("n_input_tokens")
            output_tokens = context.get("n_output_tokens")
            if input_tokens is not None and output_tokens is not None:
                tokens = int(_number(input_tokens, "input tokens", integer=True)) + int(
                    _number(output_tokens, "output tokens", integer=True)
                )
            if context.get("cost_usd") is not None:
                cost = float(_number(context["cost_usd"], "cost"))
        records.append(
            {
                "task": task,
                "trialName": trial_name,
                "agentVersion": agent["version"],
                "correctness": checked_rewards["correctness"],
                "safety": checked_rewards["safety"],
                "cleanup": checked_rewards["cleanup"],
                "reliable": float(exception is None and checked_rewards["cleanup"] == 1),
                "verifierAvailable": not unavailable,
                "tokens": tokens,
                "seconds": _seconds(trial.get("agent_execution")),
                "cost": cost,
                "excluded": trial_name in ignored,
            }
        )
    if any(count != attempts for count in counts.values()) or len(trials) != len(task_map) * attempts:
        raise ValueError("Harbor trial matrix is incomplete")
    if len({item["agentVersion"] for item in records}) != 1:
        raise ValueError("Harbor agent version is inconsistent")
    unknown_exclusions = ignored - observed_names
    if unknown_exclusions:
        raise ValueError("exclusion references an unknown trial")
    stats = _object(value.get("stats", {}), "Harbor job stats")
    retries = int(_number(stats.get("n_retries", 0), "Harbor retries", integer=True))
    return records, retries


def _mean_by_task(records: list[dict[str, object]], tasks: list[str], field: str) -> float:
    grouped = {task: [float(item[field]) for item in records if item["task"] == task] for task in set(tasks)}
    values = [sum(grouped[task]) / len(grouped[task]) for task in tasks if grouped.get(task)]
    return sum(values) / len(values) if values else 0.0


def _base_metrics(records: list[dict[str, object]], tasks: list[str]) -> dict[str, object]:
    all_selected = [item for task in tasks for item in records if item["task"] == task]
    selected = [item for item in all_selected if not item["excluded"]]
    complete = [item for item in selected if item["verifierAvailable"]]
    successful = sum(float(item["correctness"]) == 1 for item in complete)
    tokens_available = all(item["tokens"] is not None for item in selected)
    costs_available = all(item["cost"] is not None for item in selected)
    return {
        "sampleSize": len(complete),
        "successCount": successful,
        "effectiveness": _mean_by_task(complete, tasks, "correctness"),
        "reliability": _mean_by_task(all_selected, tasks, "reliable"),
        "safetyEligible": all(float(item["safety"]) == 1 for item in all_selected if item["verifierAvailable"]),
        "verifierComplete": all(item["verifierAvailable"] or item["excluded"] for item in all_selected),
        "tokenProvenance": "reported" if tokens_available else "unavailable",
        "tokensPerSuccess": (
            sum(int(item["tokens"]) for item in complete) / successful
            if successful and tokens_available
            else None
        ),
        "secondsPerSuccess": (
            sum(float(item["seconds"]) for item in complete) / successful
            if successful
            else None
        ),
        "costPerSuccess": (
            sum(float(item["cost"]) for item in complete) / successful
            if successful and costs_available
            else None
        ),
    }


def _harmonic(effectiveness: float, reliability: float, efficiency: float, weights=(0.6, 0.2, 0.2)) -> float:
    values = (effectiveness, reliability, efficiency)
    if any(value <= 0 for value in values):
        return 0.0
    return 100.0 / sum(weight / value for weight, value in zip(weights, values))


def _scored(
    records: dict[str, list[dict[str, object]]], tasks: list[str]
) -> dict[str, dict[str, object]]:
    values = {arm: _base_metrics(records[arm], tasks) for arm in ARMS}
    if any(values[arm]["tokenProvenance"] == "unavailable" for arm in ARMS):
        for arm in ARMS:
            values[arm]["efficiency"] = None
            values[arm]["overallScore"] = None
            values[arm]["equalWeightScore"] = None
        return values
    dimensions = ["tokensPerSuccess", "secondsPerSuccess"]
    if all(values[arm]["costPerSuccess"] is not None for arm in ARMS):
        dimensions.append("costPerSuccess")
    for arm in ARMS:
        ratios = []
        for dimension in dimensions:
            current_value = values[arm][dimension]
            available = [
                float(values[name][dimension])
                for name in ARMS
                if values[name][dimension] is not None
            ]
            if current_value is None or not available:
                ratios.append(0.0)
                continue
            current = float(current_value)
            best = min(available)
            ratios.append(0.0 if current <= 0 else min(1.0, best / current))
        efficiency = math.prod(ratios) ** (1 / len(ratios)) if ratios else 0.0
        values[arm]["efficiency"] = efficiency
        values[arm]["overallScore"] = _harmonic(
            float(values[arm]["effectiveness"]),
            float(values[arm]["reliability"]),
            efficiency,
        )
        values[arm]["equalWeightScore"] = _harmonic(
            float(values[arm]["effectiveness"]),
            float(values[arm]["reliability"]),
            efficiency,
            weights=(1 / 3, 1 / 3, 1 / 3),
        )
    return values


def _bootstrap(
    records: dict[str, list[dict[str, object]]], tasks: list[str], seed: int
) -> dict[str, float | None]:
    if any(_scored(records, tasks)[arm]["overallScore"] is None for arm in ARMS):
        return {"lower": None, "upper": None}
    generator = random.Random(seed)  # nosec B311 - reproducible sampling, not security.
    deltas: list[float] = []
    for _ in range(BOOTSTRAP_ITERATIONS):
        sample = [generator.choice(tasks) for _ in tasks]
        scores = _scored(records, sample)
        deltas.append(
            float(scores["chaos-engine"]["overallScore"])
            - float(scores["control"]["overallScore"])
        )
    deltas.sort()
    return {
        "lower": _round(deltas[int(0.025 * (len(deltas) - 1))]),
        "upper": _round(deltas[int(0.975 * (len(deltas) - 1))]),
    }


def compare(
    manifest: object,
    control_job: object,
    candidate_job: object,
    *,
    campaign: str,
    private_resolution: object = None,
    exclusions: list[dict[str, str]] | None = None,
) -> dict[str, object]:
    arm_map, task_map, attempts = _experiment(manifest, campaign, private_resolution)
    exclusions = [] if exclusions is None else exclusions
    records: dict[str, list[dict[str, object]]] = {}
    retries: dict[str, int] = {}
    for name, job in zip(ARMS, (control_job, candidate_job)):
        records[name], retries[name] = _records(
            job, name, arm_map[name], task_map, attempts, exclusions
        )
    if {
        item["agentVersion"] for item in records["control"]
    } != {item["agentVersion"] for item in records["chaos-engine"]}:
        raise ValueError("Harbor agent version is incomparable between arms")
    tasks = list(task_map)
    metrics = _scored(records, tasks)
    interval = _bootstrap(records, tasks, int(_object(manifest, "manifest")["seed"]))
    control = metrics["control"]
    candidate = metrics["chaos-engine"]
    if not control["verifierComplete"] or not candidate["verifierComplete"]:
        verdict = {"state": "insufficient evidence", "winner": None}
    elif not control["safetyEligible"] or not candidate["safetyEligible"]:
        verdict = {"state": "ineligible", "winner": None}
    elif control["overallScore"] is None or candidate["overallScore"] is None:
        verdict = {"state": "insufficient evidence", "winner": None}
    elif interval["lower"] is not None and interval["lower"] > 0 and candidate["effectiveness"] >= control["effectiveness"]:
        verdict = {"state": "winner", "winner": "chaos-engine"}
    elif interval["upper"] is not None and interval["upper"] < 0 and control["effectiveness"] >= candidate["effectiveness"]:
        verdict = {"state": "winner", "winner": "control"}
    else:
        verdict = {"state": "no significant difference", "winner": None}
    rounded = {
        arm: {key: _round(value) if isinstance(value, float) else value for key, value in metrics[arm].items()}
        for arm in ARMS
    }
    return {
        "schemaVersion": 1,
        "scoreVersion": SCORE_VERSION,
        "bootstrapIterations": BOOTSTRAP_ITERATIONS,
        "seed": int(_object(manifest, "manifest")["seed"]),
        "campaign": campaign,
        "arms": rounded,
        "scoreDelta": (
            None
            if rounded["control"]["overallScore"] is None
            else _round(float(rounded["chaos-engine"]["overallScore"]) - float(rounded["control"]["overallScore"]))
        ),
        "confidenceInterval95": interval,
        "verdict": verdict,
        "retries": retries,
        "exclusions": exclusions,
    }


def _atomic_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8", newline="") as stream:
            stream.write(content)
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            # Concurrent cleanup already removed this private temporary file.
            pass
        raise


def write_reports(report: dict[str, object], output: Path) -> None:
    output.mkdir(parents=True, exist_ok=True)
    _atomic_text(output / "comparison.json", json.dumps(report, indent=2, sort_keys=True) + "\n")
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=("metric", "control", "chaos-engine", "delta"))
    writer.writeheader()
    arms = _object(report["arms"], "report arms")
    for metric in (
        "effectiveness",
        "reliability",
        "efficiency",
        "overallScore",
        "equalWeightScore",
        "tokensPerSuccess",
        "secondsPerSuccess",
        "costPerSuccess",
        "sampleSize",
    ):
        control = _object(arms["control"], "control metrics").get(metric)
        candidate = _object(arms["chaos-engine"], "candidate metrics").get(metric)
        delta = candidate - control if isinstance(control, (int, float)) and isinstance(candidate, (int, float)) else ""
        writer.writerow({"metric": metric, "control": "" if control is None else control, "chaos-engine": "" if candidate is None else candidate, "delta": delta})
    _atomic_text(output / "comparison.csv", stream.getvalue())
    verdict = _object(report["verdict"], "verdict")
    lines = [
        "# ChaosGauge Comparison",
        "",
        f"**Verdict:** {verdict['state']}" + (f" — {verdict['winner']}" if verdict.get("winner") else ""),
        "",
        "| Metric | Control | ChaosEngine |",
        "| --- | ---: | ---: |",
    ]
    for metric in ("effectiveness", "reliability", "efficiency", "overallScore", "tokensPerSuccess", "secondsPerSuccess", "costPerSuccess"):
        control = _object(arms["control"], "control metrics").get(metric)
        candidate = _object(arms["chaos-engine"], "candidate metrics").get(metric)
        lines.append(f"| {metric} | {control if control is not None else 'unavailable'} | {candidate if candidate is not None else 'unavailable'} |")
    interval = _object(report["confidenceInterval95"], "confidence interval")
    lines.extend(["", f"95% paired bootstrap interval: [{interval['lower']}, {interval['upper']}]", f"Bootstrap iterations: {report['bootstrapIterations']}", f"Exclusions: {len(report['exclusions'])}", ""])
    _atomic_text(output / "comparison.md", "\n".join(lines))


def _load(path: Path) -> object:
    if path.is_symlink() or not path.is_file() or path.stat().st_size > 64 * 1024 * 1024:
        raise ValueError(f"unsafe or unavailable input: {path}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ValueError(f"malformed JSON input: {path}") from error


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--control-job", type=Path, required=True)
    parser.add_argument("--candidate-job", type=Path, required=True)
    parser.add_argument("--exclusions", type=Path)
    parser.add_argument("--campaign", choices=("calibration", "full-pilot"), required=True)
    parser.add_argument("--private-resolution", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    raw_exclusions = [] if args.exclusions is None else _load(args.exclusions)
    if not isinstance(raw_exclusions, list):
        raise ValueError("exclusions must be an array")
    report = compare(
        _load(args.manifest),
        _load(args.control_job),
        _load(args.candidate_job),
        campaign=args.campaign,
        private_resolution=(None if args.private_resolution is None else _load(args.private_resolution)),
        exclusions=raw_exclusions,
    )
    write_reports(report, args.output_dir)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
