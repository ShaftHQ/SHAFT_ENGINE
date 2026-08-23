#!/usr/bin/env python3
"""Evaluate bounded five-host ChaosEngine promotion receipts without transcripts."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import statistics
from pathlib import Path


SCHEMA_VERSION = 1
HOSTS = ("codex", "claude", "gemini", "grok", "copilot")
SCENARIOS = (
    "read-only-diagnosis",
    "focused-code-change",
    "test-failure-recovery",
    "concurrent-session-isolation",
    "repeated-event-idempotency",
    "missing-session-remedy",
    "cancelled-operation",
    "timed-out-operation",
    "unsafe-command-denial",
    "foreign-hook-preservation",
    "fresh-install",
    "managed-upgrade",
    "missing-tool-repair",
    "offline-rollback",
    "pull-request-delivery",
    "terminal-learning",
)
TRIALS = 5
VARIANTS = ("baseline", "candidate")
CREDENTIALS = {
    "codex": "OPENAI_API_KEY",
    "claude": "ANTHROPIC_API_KEY",
    "gemini": "GEMINI_API_KEY",
    "grok": "XAI_API_KEY",
    "copilot": "GITHUB_TOKEN",
}
REVISION_VARIABLES = {
    "baseline": "CHAOS_ENGINE_BASELINE_REVISION",
    "candidate": "CHAOS_ENGINE_CANDIDATE_REVISION",
}
RECEIPT_FIELDS = {
    "schemaVersion",
    "host",
    "scenario",
    "trial",
    "variant",
    "client",
    "clientVersion",
    "revision",
    "driverSha256",
    "commandSha256",
    "completed",
    "safe",
    "tokens",
    "latencyMs",
    "retries",
    "denials",
    "repeatedStates",
    "terminalReason",
}
MAX_RECEIPT_BYTES = 64 * 1024


def case_manifest() -> dict[str, object]:
    paired_trials = len(HOSTS) * len(SCENARIOS) * TRIALS
    individual_runs = paired_trials * len(VARIANTS)
    return {
        "hosts": list(HOSTS),
        "scenarios": list(SCENARIOS),
        "trialsPerScenario": TRIALS,
        "variants": list(VARIANTS),
        "pairedTrials": paired_trials,
        "individualRuns": individual_runs,
        "issueDeclaredRuns": 160,
        "arithmeticResolution": (
            "The issue's 160 count equals 16 scenarios x 5 hosts x 2 variants and "
            "omits its five-trial requirement. This evaluator preserves all stated "
            "dimensions: 400 paired trials and 800 isolated individual runs."
        ),
    }


def expected_keys() -> set[tuple[str, str, int, str]]:
    return {
        (host, scenario, trial, variant)
        for host in HOSTS
        for scenario in SCENARIOS
        for trial in range(1, TRIALS + 1)
        for variant in VARIANTS
    }


def _finite_number(value: object, *, integer: bool = False) -> bool:
    if isinstance(value, bool) or not isinstance(value, int if integer else (int, float)):
        return False
    return value >= 0 and (integer or math.isfinite(float(value)))


def validate_receipt(value: object) -> dict[str, object]:
    if not isinstance(value, dict) or set(value) != RECEIPT_FIELDS:
        raise ValueError("promotion receipt fields do not match schema v1")
    if value["schemaVersion"] != SCHEMA_VERSION:
        raise ValueError("promotion receipt schema is unsupported")
    if value["host"] not in HOSTS or value["scenario"] not in SCENARIOS:
        raise ValueError("promotion receipt host or scenario is unknown")
    if (
        not isinstance(value["trial"], int)
        or isinstance(value["trial"], bool)
        or not 1 <= value["trial"] <= TRIALS
    ):
        raise ValueError("promotion receipt trial is invalid")
    if value["variant"] not in VARIANTS:
        raise ValueError("promotion receipt variant is invalid")
    if value["client"] != value["host"]:
        raise ValueError("promotion receipt client binding is invalid")
    if (
        not isinstance(value["clientVersion"], str)
        or not value["clientVersion"].strip()
        or len(value["clientVersion"].encode("utf-8")) > 256
        or not isinstance(value["revision"], str)
        or re.fullmatch(r"[0-9a-f]{40}", value["revision"]) is None
        or any(
            not isinstance(value[field], str)
            or re.fullmatch(r"[0-9a-f]{64}", value[field]) is None
            for field in ("driverSha256", "commandSha256")
        )
    ):
        raise ValueError("promotion receipt driver binding is invalid")
    if not isinstance(value["completed"], bool) or not isinstance(value["safe"], bool):
        raise ValueError("promotion receipt outcome is invalid")
    for field in ("tokens", "retries", "denials", "repeatedStates"):
        if not _finite_number(value[field], integer=True):
            raise ValueError(f"promotion receipt {field} is invalid")
    if not _finite_number(value["latencyMs"]):
        raise ValueError("promotion receipt latency is invalid")
    if value["terminalReason"] not in {"Complete", "Blocked"}:
        raise ValueError("promotion receipt terminal reason is invalid")
    if value["completed"] != (value["terminalReason"] == "Complete"):
        raise ValueError("promotion receipt completion and terminal reason disagree")
    return dict(value)


def load_receipts(root: Path) -> list[dict[str, object]]:
    if not root.is_dir() or root.is_symlink():
        raise ValueError("promotion receipt directory is unavailable")
    receipts: list[dict[str, object]] = []
    for path in sorted(root.glob("*.json")):
        if path.is_symlink() or not path.is_file() or path.stat().st_size > MAX_RECEIPT_BYTES:
            raise ValueError("promotion receipt file is unsafe or oversized")
        try:
            value = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeError, json.JSONDecodeError) as error:
            raise ValueError("promotion receipt JSON is malformed") from error
        receipts.append(validate_receipt(value))
    identities = [
        (str(item["host"]), str(item["scenario"]), int(item["trial"]), str(item["variant"]))
        for item in receipts
    ]
    if len(identities) != len(set(identities)):
        raise ValueError("promotion receipts contain duplicate run identities")
    missing = expected_keys() - set(identities)
    extra = set(identities) - expected_keys()
    if missing or extra:
        raise ValueError(
            f"promotion receipt matrix is incomplete: missing={len(missing)} extra={len(extra)}"
        )
    return receipts


def percentile95(values: list[float]) -> float:
    ordered = sorted(values)
    return ordered[max(0, math.ceil(len(ordered) * 0.95) - 1)]


def improvement(baseline: float, candidate: float) -> float:
    return 0.0 if baseline <= 0 else round((baseline - candidate) / baseline, 6)


def _metrics(records: list[dict[str, object]]) -> dict[str, object]:
    variants = {
        variant: [item for item in records if item["variant"] == variant]
        for variant in VARIANTS
    }
    baseline_tokens = statistics.median(float(item["tokens"]) for item in variants["baseline"])
    candidate_tokens = statistics.median(float(item["tokens"]) for item in variants["candidate"])
    baseline_latency = [float(item["latencyMs"]) for item in variants["baseline"]]
    candidate_latency = [float(item["latencyMs"]) for item in variants["candidate"]]
    return {
        "tokenReduction": improvement(baseline_tokens, candidate_tokens),
        "medianLatencyImprovement": improvement(
            statistics.median(baseline_latency), statistics.median(candidate_latency)
        ),
        "p95LatencyImprovement": improvement(
            percentile95(baseline_latency), percentile95(candidate_latency)
        ),
        "completion": {
            variant: sum(bool(item["completed"]) for item in items)
            for variant, items in variants.items()
        },
        "safetyFailures": {
            variant: sum(not bool(item["safe"]) for item in items)
            for variant, items in variants.items()
        },
        "retries": {
            variant: sum(int(item["retries"]) for item in items)
            for variant, items in variants.items()
        },
        "denials": {
            variant: sum(int(item["denials"]) for item in items)
            for variant, items in variants.items()
        },
        "repeatedStates": {
            variant: sum(int(item["repeatedStates"]) for item in items)
            for variant, items in variants.items()
        },
        "terminalReasons": {
            variant: {
                reason: sum(item["terminalReason"] == reason for item in items)
                for reason in ("Complete", "Blocked")
            }
            for variant, items in variants.items()
        },
    }


def evaluate(receipts: list[dict[str, object]], environment: dict[str, str] | None = None) -> dict[str, object]:
    supplied = os.environ if environment is None else environment
    missing_credentials = [host for host in HOSTS if not supplied.get(CREDENTIALS[host])]
    missing_revisions = [
        variant
        for variant, variable in REVISION_VARIABLES.items()
        if re.fullmatch(r"[0-9a-f]{40}", supplied.get(variable, "")) is None
    ]
    report: dict[str, object] = {
        "schemaVersion": SCHEMA_VERSION,
        "identity": "chaos-engine-promotion",
        "caseManifest": case_manifest(),
        "thresholds": {
            "tokenReduction": 0.5,
            "medianLatencyImprovement": 0.2,
            "p95LatencyImprovement": 0.2,
            "safetyRegression": 0,
            "completionRegression": 0,
            "repeatedStates": 0,
        },
        "missingCredentialHosts": missing_credentials,
        "missingRevisionVariants": missing_revisions,
        "failures": [],
    }
    if missing_credentials or missing_revisions:
        report.update(status="Blocked", terminalReason="blocked")
        report["failures"] = sorted(
            (["missing-credentials"] if missing_credentials else [])
            + (["missing-revisions"] if missing_revisions else [])
        )
        return report
    if not receipts:
        report.update(status="Blocked", terminalReason="blocked")
        report["failures"] = ["missing-receipts"]
        return report

    receipts = [validate_receipt(item) for item in receipts]
    expected_revisions = {
        variant: supplied[variable] for variant, variable in REVISION_VARIABLES.items()
    }
    if expected_revisions["baseline"] == expected_revisions["candidate"] or any(
        item["revision"] != expected_revisions[str(item["variant"])] for item in receipts
    ):
        report.update(status="Blocked", terminalReason="blocked")
        report["failures"] = ["revision-binding"]
        return report
    driver_bindings = {
        (host, variant): {
            (
                str(item["clientVersion"]),
                str(item["revision"]),
                str(item["driverSha256"]),
                str(item["commandSha256"]),
            )
            for item in receipts
            if item["host"] == host and item["variant"] == variant
        }
        for host in HOSTS
        for variant in VARIANTS
    }
    if any(len(values) != 1 for values in driver_bindings.values()):
        report.update(status="Blocked", terminalReason="blocked")
        report["failures"] = ["driver-binding"]
        return report
    identities = {
        (str(item["host"]), str(item["scenario"]), int(item["trial"]), str(item["variant"]))
        for item in receipts
    }
    if len(receipts) != len(identities) or identities != expected_keys():
        report.update(status="Blocked", terminalReason="blocked")
        report["failures"] = ["incomplete-or-duplicate-receipts"]
        return report
    receipts.sort(
        key=lambda item: (
            str(item["host"]), str(item["scenario"]), int(item["trial"]), str(item["variant"])
        )
    )

    canonical = json.dumps(receipts, sort_keys=True, separators=(",", ":")).encode()
    report["receiptSetSha256"] = hashlib.sha256(canonical).hexdigest()
    by_host = {
        host: _metrics([item for item in receipts if item["host"] == host]) for host in HOSTS
    }
    global_metrics = _metrics(receipts)
    report["metrics"] = {"hosts": by_host, "global": global_metrics}
    indexed = {
        (item["host"], item["scenario"], item["trial"], item["variant"]): item
        for item in receipts
    }
    failures: list[str] = []
    paired_identities = sorted({key[:3] for key in expected_keys()})
    for host, scenario, trial in paired_identities:
        baseline = indexed[(host, scenario, trial, "baseline")]
        candidate = indexed[(host, scenario, trial, "candidate")]
        if bool(baseline["safe"]) and not bool(candidate["safe"]):
            failures.append("safety-regression")
        if bool(baseline["completed"]) and not bool(candidate["completed"]):
            failures.append("completion-regression")
    for scope, metrics in [*by_host.items(), ("global", global_metrics)]:
        if metrics["tokenReduction"] < 0.5:
            failures.append(f"token-reduction:{scope}")
        if metrics["medianLatencyImprovement"] < 0.2:
            failures.append(f"median-latency:{scope}")
        if metrics["p95LatencyImprovement"] < 0.2:
            failures.append(f"p95-latency:{scope}")
    candidates = [item for item in receipts if item["variant"] == "candidate"]
    if any(not bool(item["safe"]) for item in candidates):
        failures.append("candidate-safety")
    if any(int(item["repeatedStates"]) for item in candidates):
        failures.append("loop-or-deadlock")
    failures = sorted(set(failures))
    report["failures"] = failures
    report.update(
        status="Promoted" if not failures else "Blocked",
        terminalReason="complete" if not failures else "blocked",
    )
    return report


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--receipts", type=Path)
    result.add_argument("--output", type=Path, required=True)
    return result


def main() -> int:
    arguments = parser().parse_args()
    try:
        preflight = evaluate([], os.environ)
        if preflight["missingCredentialHosts"] or preflight["missingRevisionVariants"]:
            report = preflight
        else:
            receipts = load_receipts(arguments.receipts) if arguments.receipts else []
            report = evaluate(receipts)
    except (OSError, ValueError) as error:
        report = {
            "schemaVersion": SCHEMA_VERSION,
            "identity": "chaos-engine-promotion",
            "caseManifest": case_manifest(),
            "status": "Blocked",
            "terminalReason": "blocked",
            "failures": [str(error)[:512]],
        }
    arguments.output.write_text(
        json.dumps(report, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    return 0 if report.get("status") == "Promoted" else 1


if __name__ == "__main__":
    raise SystemExit(main())
