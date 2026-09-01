#!/usr/bin/env python3
"""
Map ChaosGauge public calibration outputs to redacted decision-quality evidence.

Reuses the immutable #5450 calibration campaign (12 public tasks, two arms, five
attempts, seed 5450) without changing task identities. Missing telemetry is the
literal string UNAVAILABLE and is never coerced to zero. Blocked runs record
exact missing runtime inputs instead of inventing trial results.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess  # nosec B404 - fixed local harbor/docker probe commands only.
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parent
UNAVAILABLE = "UNAVAILABLE"
ARMS = ("control", "chaos-engine")
CALIBRATION_METRICS = (
    "correctness",
    "tokens",
    "latency_seconds",
    "external_run_minutes",
    "actions",
    "retries",
    "cost_usd",
    "variance",
)
PUBLIC_TASK_COUNT = 12
ATTEMPTS_PER_TASK = 5
TRIAL_COUNT = 120
SEED = 5450
HARBOR_VERSION = "0.22.0"
AUTHORIZATION_ENV = "CHAOS_GAUGE_PUBLIC_CALIBRATION_AUTHORIZED"
FORBIDDEN_PRIVACY = (
    re.compile(r"model_id\s*:", re.I),
    re.compile(r"provider_route\s*:", re.I),
    re.compile(r"endpoint\s*:", re.I),
    re.compile(r"anthropic\.com/", re.I),
    re.compile(r"openai\.com/", re.I),
    re.compile(r"prompt content", re.I),
    re.compile(r"session transcript", re.I),
    re.compile(r"~/\."),
)


def _object(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} is invalid")
    return value


def _run(command: list[str]) -> str:
    return subprocess.run(  # nosec B603 B607 - fixed local probe argv, never shell=True.
        command, check=True, capture_output=True, text=True
    ).stdout


def load_manifest(path: Path | None = None) -> dict[str, object]:
    target = ROOT / "experiment.json" if path is None else Path(path)
    return _object(json.loads(target.read_text(encoding="utf-8")), "experiment manifest")


def public_tasks(manifest: object) -> list[dict[str, object]]:
    value = _object(manifest, "experiment manifest")
    tasks = [
        _object(task, "task")
        for task in value.get("tasks", [])
        if isinstance(task, dict) and task.get("visibility") == "public"
    ]
    return tasks


def calibration_identity(manifest: object) -> dict[str, object]:
    """Freeze the public calibration identity surface from the live manifest."""
    value = _object(manifest, "experiment manifest")
    selected = _object(_object(value.get("campaigns"), "campaigns").get("calibration"), "calibration")
    tasks = public_tasks(value)
    if (
        value.get("seed") != SEED
        or value.get("attemptsPerTask") != ATTEMPTS_PER_TASK
        or selected.get("taskCount") != PUBLIC_TASK_COUNT
        or selected.get("taskVisibility") != ["public"]
        or selected.get("privateResolutionRequired") is not False
        or len(tasks) != PUBLIC_TASK_COUNT
        or len(tasks) * 2 * ATTEMPTS_PER_TASK != TRIAL_COUNT
    ):
        raise ValueError("calibration identity drifted from #5450 public contracts")
    arms = [_object(arm, "arm").get("name") for arm in value.get("arms", []) if isinstance(arm, dict)]
    if arms != list(ARMS):
        raise ValueError("calibration arm identity is invalid")
    return {
        "seed": SEED,
        "campaign": "calibration",
        "taskCount": PUBLIC_TASK_COUNT,
        "attemptsPerTask": ATTEMPTS_PER_TASK,
        "trialCount": TRIAL_COUNT,
        "arms": list(ARMS),
        "taskVisibility": ["public"],
        "implementationRevision": value.get("implementationRevision"),
        "tasks": [{"name": task["name"], "sha256": task["sha256"]} for task in tasks],
    }


def metric_or_unavailable(value: object) -> object:
    """Return UNAVAILABLE for missing telemetry; never coerce absence to zero."""
    if value is None:
        return UNAVAILABLE
    if isinstance(value, bool):
        raise ValueError("boolean telemetry is invalid")
    if isinstance(value, (int, float)):
        if value != value or value in (float("inf"), float("-inf")):  # noqa: PLR0124
            return UNAVAILABLE
        return value
    if value == UNAVAILABLE:
        return UNAVAILABLE
    raise ValueError("metric value is invalid")


def unavailable_arm_metrics(*, retries: object = UNAVAILABLE) -> dict[str, object]:
    values = {name: UNAVAILABLE for name in CALIBRATION_METRICS}
    values["retries"] = metric_or_unavailable(retries)
    return values


def probe_runtime(run: Callable[[list[str]], str] = _run) -> dict[str, object]:
    """Return exact missing inputs required before paid public calibration may start."""
    missing: list[str] = []
    details: dict[str, object] = {}

    try:
        harbor = run(
            ["python3", "-c", "from importlib.metadata import version; print(version('harbor'))"]
        ).strip()
        details["harborVersion"] = harbor
        if harbor != HARBOR_VERSION:
            missing.append(f"harbor=={HARBOR_VERSION} (found {harbor or 'none'})")
    except (OSError, subprocess.CalledProcessError):
        details["harborVersion"] = UNAVAILABLE
        missing.append(f"harbor=={HARBOR_VERSION}")

    try:
        docker = run(["docker", "version", "--format", "{{.Server.Version}}"]).strip()
        details["dockerServerVersion"] = docker or UNAVAILABLE
        if not docker:
            missing.append("docker-engine-access")
    except (OSError, subprocess.CalledProcessError):
        details["dockerServerVersion"] = UNAVAILABLE
        missing.append("docker-engine-access")

    provider = os.environ.get("OPENAI_API_KEY", "").strip()
    details["providerCredential"] = "present" if provider else UNAVAILABLE
    if not provider:
        missing.append("OPENAI_API_KEY")

    authorized = os.environ.get(AUTHORIZATION_ENV, "").strip() == "1"
    details["campaignAuthorization"] = "granted" if authorized else UNAVAILABLE
    if not authorized:
        missing.append(
            f"{AUTHORIZATION_ENV}=1 (owner-authorized 120-trial public calibration budget)"
        )

    return {
        "ready": not missing,
        "missingInputs": missing,
        "details": details,
        "plannedTrials": TRIAL_COUNT,
    }


def _arm_metrics(comparison_arm: object, retries: object) -> dict[str, object]:
    arm = _object(comparison_arm, "comparison arm")
    provenance = arm.get("tokenProvenance")
    tokens = arm.get("tokensPerSuccess") if provenance == "reported" else None
    return {
        "correctness": metric_or_unavailable(arm.get("effectiveness")),
        "tokens": metric_or_unavailable(tokens),
        "latency_seconds": metric_or_unavailable(arm.get("secondsPerSuccess")),
        "external_run_minutes": UNAVAILABLE,
        "actions": UNAVAILABLE,
        "retries": metric_or_unavailable(retries),
        "cost_usd": metric_or_unavailable(arm.get("costPerSuccess")),
        "variance": UNAVAILABLE,
    }


def _comparison_variance(interval: object) -> object:
    bounds = _object(interval, "confidence interval")
    lower = metric_or_unavailable(bounds.get("lower"))
    upper = metric_or_unavailable(bounds.get("upper"))
    if lower is UNAVAILABLE or upper is UNAVAILABLE:
        return UNAVAILABLE
    if not isinstance(lower, (int, float)) or not isinstance(upper, (int, float)):
        return UNAVAILABLE
    return upper - lower


def build_redacted_aggregate(
    manifest: object,
    comparison: object,
    *,
    collect_receipt: object | None = None,
) -> dict[str, object]:
    """Build secret-scanned redacted aggregate evidence from ChaosGauge compare output."""
    identity = calibration_identity(manifest)
    report = _object(comparison, "comparison report")
    if report.get("campaign") != "calibration":
        raise ValueError("comparison campaign is not calibration")
    arms = _object(report.get("arms"), "comparison arms")
    if set(arms) != set(ARMS):
        raise ValueError("comparison arms are invalid")
    retries = _object(report.get("retries"), "comparison retries")
    interval = _object(report.get("confidenceInterval95"), "confidence interval")
    variance = _comparison_variance(interval)
    metrics = {}
    for arm in ARMS:
        values = _arm_metrics(arms[arm], retries.get(arm))
        values["variance"] = variance
        metrics[arm] = values
    # Never invent observed=120. Without a collect receipt, observed stays 0.
    observed = 0
    if collect_receipt is not None:
        receipt = _object(collect_receipt, "collect receipt")
        accounting = _object(receipt.get("trialAccounting"), "trial accounting")
        observed_value = accounting.get("observed")
        if isinstance(observed_value, bool) or not isinstance(observed_value, int) or observed_value < 0:
            raise ValueError("collect receipt trial accounting is invalid")
        observed = observed_value
    status = "complete" if observed == TRIAL_COUNT else "incomplete"
    score_delta = report.get("scoreDelta")
    evidence = {
        "schemaVersion": 1,
        "campaign": "calibration",
        "status": status,
        "identity": identity,
        "trialAccounting": {"planned": TRIAL_COUNT, "observed": observed},
        "missingInputs": [],
        "metrics": metrics,
        "comparison": {
            "verdict": _object(report.get("verdict"), "verdict"),
            "scoreDelta": metric_or_unavailable(score_delta),
            "confidenceInterval95": {
                "lower": metric_or_unavailable(interval.get("lower")),
                "upper": metric_or_unavailable(interval.get("upper")),
            },
            "scoreVersion": report.get("scoreVersion"),
            "bootstrapIterations": report.get("bootstrapIterations"),
        },
        "privacy": {
            "prompts": False,
            "transcripts": False,
            "secrets": False,
            "privatePaths": False,
            "providerRoutes": False,
            "modelIds": False,
        },
    }
    validate_redacted_aggregate(evidence, manifest)
    return evidence


def blocked_evidence(manifest: object, missing_inputs: list[str]) -> dict[str, object]:
    """Emit honest blocked evidence with planned trials and UNAVAILABLE metrics only."""
    if not missing_inputs:
        raise ValueError("blocked evidence requires exact missing inputs")
    identity = calibration_identity(manifest)
    evidence = {
        "schemaVersion": 1,
        "campaign": "calibration",
        "status": "blocked",
        "identity": identity,
        "trialAccounting": {"planned": TRIAL_COUNT, "observed": 0},
        "missingInputs": list(missing_inputs),
        "metrics": {arm: unavailable_arm_metrics() for arm in ARMS},
        "comparison": {
            "verdict": {"state": "insufficient evidence", "winner": None},
            "scoreDelta": UNAVAILABLE,
            "confidenceInterval95": {"lower": UNAVAILABLE, "upper": UNAVAILABLE},
            "scoreVersion": UNAVAILABLE,
            "bootstrapIterations": UNAVAILABLE,
        },
        "privacy": {
            "prompts": False,
            "transcripts": False,
            "secrets": False,
            "privatePaths": False,
            "providerRoutes": False,
            "modelIds": False,
        },
    }
    validate_redacted_aggregate(evidence, manifest)
    return evidence


def _walk_strings(value: object) -> list[str]:
    if isinstance(value, str):
        return [value]
    if isinstance(value, dict):
        found: list[str] = []
        for key, item in value.items():
            found.extend(_walk_strings(str(key)))
            found.extend(_walk_strings(item))
        return found
    if isinstance(value, list):
        found = []
        for item in value:
            found.extend(_walk_strings(item))
        return found
    return []


def _validate_trial_accounting(evidence: dict[str, object]) -> None:
    accounting = _object(evidence.get("trialAccounting"), "trial accounting")
    if accounting.get("planned") != TRIAL_COUNT:
        raise ValueError("redacted aggregate planned trial count is invalid")
    observed = accounting.get("observed")
    if isinstance(observed, bool) or not isinstance(observed, int) or observed < 0 or observed > TRIAL_COUNT:
        raise ValueError("redacted aggregate observed trial count is invalid")
    status = evidence.get("status")
    if status not in {"blocked", "incomplete", "complete"}:
        raise ValueError("redacted aggregate status is invalid")
    if status == "blocked" and observed != 0:
        raise ValueError("blocked evidence must not claim observed trials")
    if status == "blocked" and not evidence.get("missingInputs"):
        raise ValueError("blocked evidence must list exact missing inputs")


def _validate_arm_metrics(evidence: dict[str, object]) -> None:
    metrics = _object(evidence.get("metrics"), "metrics")
    if set(metrics) != set(ARMS):
        raise ValueError("redacted aggregate arms are invalid")
    for arm in ARMS:
        arm_metrics = _object(metrics[arm], f"{arm} metrics")
        if set(arm_metrics) != set(CALIBRATION_METRICS):
            raise ValueError(f"{arm} metric set is invalid")
        for name, item in arm_metrics.items():
            if item is None:
                raise ValueError(f"{arm}.{name} must use UNAVAILABLE rather than null")
            if item != UNAVAILABLE and not isinstance(item, (int, float)):
                raise ValueError(f"{arm}.{name} is invalid")


def _validate_privacy(evidence: dict[str, object]) -> None:
    for text in _walk_strings(evidence):
        for pattern in FORBIDDEN_PRIVACY:
            if pattern.search(text):
                raise ValueError("redacted aggregate failed privacy scan")


def validate_redacted_aggregate(value: object, manifest: object) -> None:
    """Fail closed on identity drift, zero-filled gaps, or privacy leaks."""
    evidence = _object(value, "redacted aggregate")
    identity = calibration_identity(manifest)
    if evidence.get("schemaVersion") != 1 or evidence.get("campaign") != "calibration":
        raise ValueError("redacted aggregate schema is invalid")
    if evidence.get("identity") != identity:
        raise ValueError("redacted aggregate identity drifted from #5450 contracts")
    _validate_trial_accounting(evidence)
    _validate_arm_metrics(evidence)
    _validate_privacy(evidence)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("probe", "blocked", "build", "validate"))
    parser.add_argument("--manifest", type=Path, default=ROOT / "experiment.json")
    parser.add_argument("--comparison", type=Path)
    parser.add_argument("--collect-receipt", type=Path)
    parser.add_argument("--evidence", type=Path)
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()
    manifest = load_manifest(args.manifest)
    if args.command == "probe":
        print(json.dumps(probe_runtime(), sort_keys=True, indent=2))
        return 0
    if args.command == "blocked":
        probe = probe_runtime()
        evidence = blocked_evidence(manifest, list(probe["missingInputs"]))
        payload = json.dumps(evidence, sort_keys=True, indent=2) + "\n"
        if args.out is None:
            print(payload, end="")
        else:
            args.out.write_text(payload, encoding="utf-8")
        return 0
    if args.command == "build":
        if args.comparison is None:
            raise ValueError("comparison JSON is required")
        comparison = json.loads(args.comparison.read_text(encoding="utf-8"))
        collect = (
            None
            if args.collect_receipt is None
            else json.loads(args.collect_receipt.read_text(encoding="utf-8"))
        )
        evidence = build_redacted_aggregate(manifest, comparison, collect_receipt=collect)
        payload = json.dumps(evidence, sort_keys=True, indent=2) + "\n"
        if args.out is None:
            print(payload, end="")
        else:
            args.out.write_text(payload, encoding="utf-8")
        return 0
    if args.evidence is None:
        raise ValueError("evidence JSON is required")
    validate_redacted_aggregate(json.loads(args.evidence.read_text(encoding="utf-8")), manifest)
    print("ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
