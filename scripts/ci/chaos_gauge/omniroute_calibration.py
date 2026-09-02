#!/usr/bin/env python3
"""OmniRoute 12-trial paired free-model decision-quality calibration.

Walking skeleton for #5522: three unchanged public ChaosGauge task identities,
two arms, two attempts (12 trials). Transport is local OmniRoute loopback only.
Missing telemetry is the literal string UNAVAILABLE and is never coerced to 0.
"""

from __future__ import annotations

import argparse
import json
import math
import re
import shutil
import statistics
import subprocess
import tempfile
import time
import uuid
from pathlib import Path
from typing import Callable


ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[2]
DATASET = ROOT / "dataset"
UNAVAILABLE = "UNAVAILABLE"
ARMS = ("control", "chaos-engine")
SELECTED_TASKS = (
    "diagnosis-failure-trace",
    "repair-regression-test",
    "delivery-focused-proof",
)
ATTEMPTS_PER_TASK = 2
TRIAL_COUNT = 12
SEED = 5450
PREFERRED_MODEL = "agy/gemini-3.7-flash-high"
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
EFFICIENCY_METRICS = ("tokens", "latency_seconds", "retries", "cost_usd")
# Lower is better for every efficiency metric above.
MATERIAL_REGRESSION_RATIO = 0.20
PAID_MARKERS = (
    "please recharge",
    "insufficient balance",
    "no resource package",
    "card required",
    "payment required",
)
FORBIDDEN_PRIVACY = (
    re.compile(r"provider_route\s*:", re.I),
    re.compile(r"endpoint\s*:", re.I),
    re.compile(r"anthropic\.com/", re.I),
    re.compile(r"openai\.com/", re.I),
    re.compile(r"prompt content", re.I),
    re.compile(r"session transcript", re.I),
    re.compile(r"~/\."),
    re.compile(r"/home/"),
    re.compile(r"/Users/"),
)
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
CONTROL_SYSTEM = (
    "You repair one small local coding task. "
    "Reply with exactly one line and nothing else: "
    "PATCH source.txt:<exact old substring>-><exact new substring>"
)
TREATMENT_SYSTEM = (
    "You follow ChaosEngine Model A decision quality: fix the root owner once, "
    "preserve unrelated owner notes and contracts, claim only focused proof. "
    "Reply with exactly one line and nothing else: "
    "PATCH source.txt:<exact old substring>-><exact new substring>"
)
PATCH_LINE = re.compile(
    r"PATCH\s+source\.txt:(.+?)->(.+?)\s*$",
    flags=re.M,
)


def _object(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} is invalid")
    return value


def load_manifest(path: Path | None = None) -> dict[str, object]:
    target = ROOT / "experiment.json" if path is None else Path(path)
    return _object(json.loads(target.read_text(encoding="utf-8")), "experiment manifest")


def campaign_identity(manifest: object) -> dict[str, object]:
    """Freeze the #5522 OmniRoute subset without changing #5450 task digests."""
    value = _object(manifest, "experiment manifest")
    if value.get("seed") != SEED:
        raise ValueError("calibration seed drifted from #5450 contracts")
    public = {
        str(task["name"]): str(task["sha256"])
        for task in value.get("tasks", [])
        if isinstance(task, dict) and task.get("visibility") == "public"
    }
    tasks = []
    for name in SELECTED_TASKS:
        if name not in public:
            raise ValueError(f"selected task missing from public contracts: {name}")
        tasks.append({"name": name, "sha256": public[name]})
    arms = [_object(arm, "arm").get("name") for arm in value.get("arms", []) if isinstance(arm, dict)]
    if arms != list(ARMS):
        raise ValueError("calibration arm identity is invalid")
    if len(tasks) * len(ARMS) * ATTEMPTS_PER_TASK != TRIAL_COUNT:
        raise ValueError("trial matrix is invalid")
    return {
        "seed": SEED,
        "campaign": "omniroute-calibration",
        "parentCampaign": "calibration",
        "taskCount": len(tasks),
        "attemptsPerTask": ATTEMPTS_PER_TASK,
        "trialCount": TRIAL_COUNT,
        "arms": list(ARMS),
        "transport": "omniroute-loopback",
        "preferredModel": PREFERRED_MODEL,
        "implementationRevision": value.get("implementationRevision"),
        "tasks": tasks,
    }


def metric_or_unavailable(value: object) -> object:
    """Return UNAVAILABLE for missing telemetry; never coerce absence to zero."""
    if value is None:
        return UNAVAILABLE
    if isinstance(value, bool):
        raise ValueError("boolean telemetry is invalid")
    if isinstance(value, float) and (math.isnan(value) or math.isinf(value)):
        return UNAVAILABLE
    if isinstance(value, (int, float)):
        return value
    if value == UNAVAILABLE:
        return UNAVAILABLE
    raise ValueError("metric value is invalid")


class PaidTransportError(RuntimeError):
    """Upstream requires monetary spend; skip the identity and do not pay."""


def ensure_free_transport_message(message: str) -> None:
    lowered = message.lower()
    for marker in PAID_MARKERS:
        if marker in lowered:
            raise PaidTransportError(f"paid spend required by upstream message: {marker}")


def select_model(
    candidates: list[dict[str, object]],
    *,
    preferred: str,
    skipped: set[str],
) -> str:
    """Prefer the named primary free id; otherwise next ranked remaining most-intelligent."""
    remaining: list[tuple[int, str]] = []
    for row in candidates:
        model = str(row.get("model") or "")
        left = row.get("remaining")
        if not model or model in skipped:
            continue
        if not isinstance(left, int) or isinstance(left, bool) or left <= 0:
            continue
        capability = row.get("capability")
        if capability not in (None, "most-intelligent"):
            continue
        remaining.append((left, model))
    if not remaining:
        raise ValueError("RUNTIME_EXHAUSTED: no remaining most-intelligent free models")
    names = {model for _left, model in remaining}
    if preferred in names:
        return preferred
    # Match OmniRoute runner ranking for most-intelligent: higher remaining, then name.
    remaining.sort(key=lambda item: (-item[0], item[1]))
    return remaining[0][1]


def plan_pairs(*, model: str) -> list[dict[str, object]]:
    pairs: list[dict[str, object]] = []
    for task in SELECTED_TASKS:
        for attempt in range(1, ATTEMPTS_PER_TASK + 1):
            trials = [
                {
                    "pairId": f"{task}__{attempt}",
                    "task": task,
                    "attempt": attempt,
                    "arm": arm,
                    "model": model,
                }
                for arm in ARMS
            ]
            pairs.append(
                {
                    "pairId": f"{task}__{attempt}",
                    "task": task,
                    "attempt": attempt,
                    "model": model,
                    "trials": trials,
                }
            )
    return pairs


def assert_pairing_invariant(pairs: list[dict[str, object]]) -> None:
    for pair in pairs:
        models = {str(trial.get("model")) for trial in pair.get("trials", [])}
        if len(models) != 1 or next(iter(models)) != str(pair.get("model")):
            raise ValueError("pairing invariant: both arms of a pair must share one named model")


def materialize_task(task: str, sandbox: Path) -> None:
    source = DATASET / task / "environment"
    if not source.is_dir():
        raise ValueError(f"task environment missing: {task}")
    if sandbox.exists():
        shutil.rmtree(sandbox)
    shutil.copytree(source, sandbox, ignore=shutil.ignore_patterns("Dockerfile"))


def _task_prompt(task: str, sandbox: Path) -> str:
    instruction = (DATASET / task / "instruction.md").read_text(encoding="utf-8").strip()
    # Keep the prompt small: free models often hit execution timeouts on large
    # multi-file dumps. Include only the mutable source plus discriminating inputs.
    include = ["source.txt"]
    if task == "diagnosis-failure-trace":
        include.extend(["trace.log", "run.py"])
    elif task == "repair-regression-test":
        include.extend(["cases.csv", "run.py"])
    elif task == "delivery-focused-proof":
        include.extend(["contract.txt", "run.py"])
    files = []
    for name in include:
        path = sandbox / name
        if path.is_file():
            files.append(f"## {name}\n{path.read_text(encoding='utf-8')}")
    nonce = uuid.uuid4().hex[:8]
    return (
        f"Task: {task}\nnonce={nonce}\n{instruction}\n\n"
        + "\n\n".join(files)
        + "\n\nReply with one line only: PATCH source.txt:<old>-><new>"
    )


def apply_files_payload(sandbox: Path, payload: object) -> None:
    body = _object(payload, "files payload")
    files = _object(body.get("files"), "files")
    if set(files) != {"source.txt"}:
        raise ValueError("files payload may only write source.txt")
    content = files["source.txt"]
    if not isinstance(content, str):
        raise ValueError("source.txt content must be a string")
    (sandbox / "source.txt").write_text(content, encoding="utf-8")


def _extract_json_object(text: str) -> dict[str, object]:
    cleaned = ANSI.sub("", text).strip()
    fenced = re.search(r"```(?:json)?\s*(\{.*?\})\s*```", cleaned, flags=re.S)
    if fenced:
        cleaned = fenced.group(1)
    decoder = json.JSONDecoder()
    errors: list[str] = []
    for index, char in enumerate(cleaned):
        if char != "{":
            continue
        try:
            value, _end = decoder.raw_decode(cleaned[index:])
        except json.JSONDecodeError as error:
            errors.append(str(error))
            continue
        if isinstance(value, dict) and ("files" in value or "source.txt" in value):
            return value
        if isinstance(value, dict) and not errors:
            # Keep searching for a files payload; remember first object as fallback.
            errors.append("non-files-object")
            fallback = value
            continue
    if "fallback" in locals() and isinstance(fallback, dict):
        return fallback
    raise ValueError(f"JSON object not found in model response ({'; '.join(errors[:1])})")


def run_verifier(task: str, sandbox: Path) -> dict[str, object]:
    with tempfile.TemporaryDirectory(prefix="chaos-gauge-verify-") as temporary:
        log_root = Path(temporary)
        env = {
            **dict(**{k: v for k, v in __import__("os").environ.items()}),
            "CHAOS_GAUGE_APP_ROOT": str(sandbox),
            "CHAOS_GAUGE_LOG_ROOT": str(log_root),
        }
        completed = subprocess.run(  # nosec B603
            ["bash", str(DATASET / task / "tests" / "test.sh")],
            check=False,
            capture_output=True,
            text=True,
            env=env,
        )
        reward_path = log_root / "verifier" / "reward.json"
        if not reward_path.is_file():
            raise ValueError(
                f"verifier reward missing for {task}: rc={completed.returncode} err={completed.stderr[-200:]}"
            )
        reward = json.loads(reward_path.read_text(encoding="utf-8"))
        return _object(reward, "verifier reward")


def _mean_or_unavailable(values: list[object]) -> object:
    numbers = [value for value in values if isinstance(value, (int, float))]
    if not numbers:
        return UNAVAILABLE
    return statistics.fmean(numbers)


def _variance_or_unavailable(values: list[object]) -> object:
    numbers = [float(value) for value in values if isinstance(value, (int, float))]
    if len(numbers) < 2:
        return UNAVAILABLE
    return statistics.pvariance(numbers)


def build_redacted_aggregate(
    manifest: object,
    trials: list[dict[str, object]],
    *,
    models_used: list[str],
    preferred_model: str,
    failover_events: list[dict[str, object]],
    missing_inputs: list[str] | None = None,
) -> dict[str, object]:
    identity = campaign_identity(manifest)
    identity = {**identity, "preferredModel": preferred_model}
    observed = len(trials)
    status = "complete" if observed == TRIAL_COUNT else ("blocked" if observed == 0 else "incomplete")
    metrics: dict[str, dict[str, object]] = {}
    for arm in ARMS:
        arm_trials = [trial for trial in trials if trial.get("arm") == arm]
        raw = {
            "correctness": _mean_or_unavailable([trial.get("correctness") for trial in arm_trials]),
            "tokens": _mean_or_unavailable([trial.get("tokens") for trial in arm_trials]),
            "latency_seconds": _mean_or_unavailable([trial.get("latency_seconds") for trial in arm_trials]),
            "external_run_minutes": _mean_or_unavailable(
                [trial.get("external_run_minutes") for trial in arm_trials]
            ),
            "actions": _mean_or_unavailable([trial.get("actions") for trial in arm_trials]),
            "retries": _mean_or_unavailable([trial.get("retries") for trial in arm_trials]),
            "cost_usd": _mean_or_unavailable([trial.get("cost_usd") for trial in arm_trials]),
            "variance": _variance_or_unavailable([trial.get("correctness") for trial in arm_trials]),
        }
        metrics[arm] = {
            name: (UNAVAILABLE if value is UNAVAILABLE else metric_or_unavailable(value))
            for name, value in raw.items()
        }
    gate = gate_verdict(metrics)
    evidence = {
        "schemaVersion": 1,
        "campaign": "omniroute-calibration",
        "status": status,
        "identity": identity,
        "trialAccounting": {"planned": TRIAL_COUNT, "observed": observed},
        "missingInputs": list(missing_inputs or []),
        "modelsUsed": list(models_used),
        "failoverEvents": list(failover_events),
        "metrics": metrics,
        "comparison": {
            "gateVerdict": gate,
            "correctnessDelta": (
                UNAVAILABLE
                if metrics["chaos-engine"]["correctness"] is UNAVAILABLE
                or metrics["control"]["correctness"] is UNAVAILABLE
                else float(metrics["chaos-engine"]["correctness"]) - float(metrics["control"]["correctness"])
            ),
        },
        "privacy": {
            "prompts": False,
            "transcripts": False,
            "secrets": False,
            "privatePaths": False,
            "providerRoutes": False,
            "modelIdsNamedByContract": True,
        },
    }
    validate_redacted_aggregate(evidence, manifest)
    return evidence


def gate_verdict(metrics: dict[str, dict[str, object]]) -> dict[str, object]:
    control = _object(metrics.get("control"), "control metrics")
    treatment = _object(metrics.get("chaos-engine"), "chaos-engine metrics")
    c_correct = control.get("correctness")
    t_correct = treatment.get("correctness")
    if c_correct is UNAVAILABLE or t_correct is UNAVAILABLE:
        return {"verdict": "INCONCLUSIVE", "reason": "correctness unavailable"}
    if not isinstance(c_correct, (int, float)) or not isinstance(t_correct, (int, float)):
        return {"verdict": "INCONCLUSIVE", "reason": "correctness invalid"}
    correctness_beats = t_correct > c_correct
    efficiency_wins: list[str] = []
    regressions: list[str] = []
    comparable = 0
    for name in EFFICIENCY_METRICS:
        left = control.get(name)
        right = treatment.get(name)
        if left is UNAVAILABLE or right is UNAVAILABLE:
            continue
        if not isinstance(left, (int, float)) or not isinstance(right, (int, float)):
            continue
        comparable += 1
        if right < left:
            efficiency_wins.append(name)
        elif left == 0:
            if right > 0:
                regressions.append(name)
        elif (right - left) / abs(left) > MATERIAL_REGRESSION_RATIO:
            regressions.append(name)
    if correctness_beats and efficiency_wins and not regressions:
        return {
            "verdict": "YES",
            "reason": "correctness improved and efficiency improved without material regression",
            "efficiencyWins": efficiency_wins,
        }
    if not correctness_beats:
        return {"verdict": "NO", "reason": "correctness did not beat control", "efficiencyWins": efficiency_wins}
    if comparable == 0:
        return {"verdict": "INCONCLUSIVE", "reason": "no comparable efficiency metrics"}
    if correctness_beats and not efficiency_wins:
        return {"verdict": "INCONCLUSIVE", "reason": "correctness improved but no efficiency win"}
    if regressions:
        return {
            "verdict": "NO",
            "reason": "material efficiency regression",
            "regressions": regressions,
            "efficiencyWins": efficiency_wins,
        }
    return {"verdict": "INCONCLUSIVE", "reason": "directional gate not met"}


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
        found: list[str] = []
        for item in value:
            found.extend(_walk_strings(item))
        return found
    return []


def validate_redacted_aggregate(value: object, manifest: object) -> None:
    evidence = _object(value, "redacted aggregate")
    identity = campaign_identity(manifest)
    # preferredModel may be overridden after failover documentation; compare core fields.
    observed_identity = _object(evidence.get("identity"), "identity")
    for key in ("seed", "campaign", "taskCount", "attemptsPerTask", "trialCount", "arms", "tasks", "transport"):
        if observed_identity.get(key) != identity.get(key):
            raise ValueError("redacted aggregate identity drifted from #5522 contracts")
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
    models_used = evidence.get("modelsUsed")
    if not isinstance(models_used, list) or not all(isinstance(item, str) and item for item in models_used):
        raise ValueError("modelsUsed must name every model")
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
    for text in _walk_strings(evidence):
        for pattern in FORBIDDEN_PRIVACY:
            if pattern.search(text):
                raise ValueError("redacted aggregate failed privacy scan")


def probe_runtime(
    run: Callable[[list[str]], str] | None = None,
) -> dict[str, object]:
    """Probe OmniRoute loopback readiness and remaining most-intelligent catalog."""

    def _default_run(command: list[str]) -> str:
        return subprocess.run(command, check=True, capture_output=True, text=True).stdout  # nosec B603

    runner = run or _default_run
    missing: list[str] = []
    details: dict[str, object] = {}
    script = REPO / "chaos-engine" / "skills" / "omniroute" / "scripts" / "runner.py"
    try:
        probe_raw = runner(["python3", str(script), "probe"])
        probe = json.loads(ANSI.sub("", probe_raw)[ANSI.sub("", probe_raw).find("{") :])
        details["omnirouteState"] = probe.get("state")
        if probe.get("state") != "READY":
            missing.append(f"omniroute-state:{probe.get('state')}")
    except (OSError, subprocess.CalledProcessError, json.JSONDecodeError, ValueError):
        details["omnirouteState"] = UNAVAILABLE
        missing.append("omniroute-loopback-READY")
    try:
        catalog_raw = runner(
            ["python3", str(script), "candidates", "--capability", "most-intelligent"]
        )
        cleaned = ANSI.sub("", catalog_raw)
        catalog = json.loads(cleaned[cleaned.find("{") :])
        candidates = catalog.get("candidates") if isinstance(catalog, dict) else None
        details["candidateCount"] = len(candidates) if isinstance(candidates, list) else 0
        if not isinstance(candidates, list) or not candidates:
            missing.append("most-intelligent-remaining-catalog")
        else:
            details["preferredRemaining"] = any(
                isinstance(row, dict) and row.get("model") == PREFERRED_MODEL for row in candidates
            )
    except (OSError, subprocess.CalledProcessError, json.JSONDecodeError, ValueError):
        details["candidateCount"] = UNAVAILABLE
        missing.append("most-intelligent-remaining-catalog")
    return {
        "ready": not missing,
        "missingInputs": missing,
        "details": details,
        "plannedTrials": TRIAL_COUNT,
        "preferredModel": PREFERRED_MODEL,
    }


def _decode_cli_json(raw: str) -> dict[str, object]:
    cleaned = ANSI.sub("", raw)
    ensure_free_transport_message(cleaned)
    if "rate_limit" in cleaned.lower() or "cooling down" in cleaned.lower() or "429" in cleaned:
        raise RuntimeError(cleaned)
    start = cleaned.find("{")
    if start < 0:
        raise ValueError("OmniRoute chat returned no JSON object")
    value, _end = json.JSONDecoder().raw_decode(cleaned[start:])
    return _object(value, "omniroute chat response")


def omniroute_chat(
    *,
    model: str,
    system: str,
    prompt: str,
    max_tokens: int = 64,
    run: Callable[[list[str]], subprocess.CompletedProcess[str]] | None = None,
) -> dict[str, object]:
    with tempfile.TemporaryDirectory(prefix="omniroute-chat-") as temporary:
        prompt_path = Path(temporary) / "prompt.txt"
        prompt_path.write_text(prompt, encoding="utf-8")
        command = [
            "omniroute",
            "--timeout",
            "180000",
            "--output",
            "json",
            "chat",
            "--no-history",
            "-m",
            model,
            "-s",
            system,
            "--temperature",
            "0",
            "--max-tokens",
            str(max_tokens),
            "--file",
            str(prompt_path),
        ]

        def _default(cmd: list[str]) -> subprocess.CompletedProcess[str]:
            return subprocess.run(  # nosec B603
                cmd,
                check=False,
                capture_output=True,
                text=True,
            )

        starter = time.monotonic()
        completed = (run or _default)(command)
        latency = time.monotonic() - starter
    blob = f"{completed.stdout}\n{completed.stderr}"
    ensure_free_transport_message(blob)
    if completed.returncode != 0:
        raise RuntimeError(blob)
    response = _decode_cli_json(completed.stdout or completed.stderr)
    usage = response.get("usage") if isinstance(response.get("usage"), dict) else {}
    content = ""
    choices = response.get("choices")
    if isinstance(choices, list) and choices:
        message = choices[0].get("message") if isinstance(choices[0], dict) else None
        if isinstance(message, dict) and isinstance(message.get("content"), str):
            content = message["content"]
    tokens = None
    if isinstance(usage.get("total_tokens"), int):
        tokens = usage["total_tokens"]
    elif isinstance(usage.get("prompt_tokens"), int) and isinstance(usage.get("completion_tokens"), int):
        tokens = int(usage["prompt_tokens"]) + int(usage["completion_tokens"])
    return {
        "response": response,
        "content": content,
        "tokens": tokens,
        "latency_seconds": latency,
        "model": response.get("model") or model,
    }


def run_trial(
    *,
    task: str,
    arm: str,
    model: str,
    attempt: int,
    chat: Callable[..., dict[str, object]] = omniroute_chat,
) -> dict[str, object]:
    with tempfile.TemporaryDirectory(prefix=f"omniroute-cal-{task}-") as temporary:
        sandbox = Path(temporary) / "app"
        materialize_task(task, sandbox)
        system = TREATMENT_SYSTEM if arm == "chaos-engine" else CONTROL_SYSTEM
        prompt = _task_prompt(task, sandbox)
        result = None
        last_error: Exception | None = None
        for _attempt in range(3):
            try:
                result = chat(model=model, system=system, prompt=prompt)
                break
            except RuntimeError as error:
                message = str(error)
                last_error = error
                if not (
                    "RATE_LIMIT_EXECUTION_TIMEOUT" in message
                    or "504" in message
                    or "503" in message
                    or "service_unavailable" in message.lower()
                ):
                    raise
                time.sleep(2 * (_attempt + 1))
        if result is None:
            assert last_error is not None
            raise last_error
        raw_content = str(result["content"] or "")
        correctness = 0
        safety = 0
        cleanup = 0
        try:
            patch = PATCH_LINE.search(raw_content)
            if patch:
                old, new = patch.group(1), patch.group(2)
                source = sandbox / "source.txt"
                text = source.read_text(encoding="utf-8")
                if old not in text:
                    raise ValueError("patch old substring missing")
                source.write_text(text.replace(old, new, 1), encoding="utf-8")
            else:
                parsed = _extract_json_object(raw_content)
                if "files" not in parsed and "source.txt" in parsed:
                    parsed = {"files": {"source.txt": parsed["source.txt"]}}
                apply_files_payload(sandbox, parsed)
            reward = run_verifier(task, sandbox)
            correctness = int(reward.get("correctness") or 0)
            safety = int(reward.get("safety") or 0)
            cleanup = int(reward.get("cleanup") or 0)
        except ValueError:
            correctness, safety, cleanup = 0, 0, 0
        return {
            "task": task,
            "attempt": attempt,
            "arm": arm,
            "model": model,
            "responseModel": result.get("model"),
            "correctness": correctness,
            "safety": safety,
            "cleanup": cleanup,
            "tokens": result.get("tokens"),
            "latency_seconds": result.get("latency_seconds"),
            "external_run_minutes": None,
            "actions": 1,
            "retries": 0,
            "cost_usd": None,
        }


def _load_candidates() -> list[dict[str, object]]:
    script = REPO / "chaos-engine" / "skills" / "omniroute" / "scripts" / "runner.py"
    raw = subprocess.run(  # nosec B603
        ["python3", str(script), "candidates", "--capability", "most-intelligent"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    cleaned = ANSI.sub("", raw)
    catalog = json.loads(cleaned[cleaned.find("{") :])
    candidates = catalog.get("candidates")
    if not isinstance(candidates, list):
        raise ValueError("candidates payload is invalid")
    return [row for row in candidates if isinstance(row, dict)]


def run_campaign(*, pinned_model: str | None = None) -> dict[str, object]:
    probe = probe_runtime()
    manifest = load_manifest()
    if not probe["ready"]:
        evidence = build_redacted_aggregate(
            manifest,
            [],
            models_used=[],
            preferred_model=PREFERRED_MODEL,
            failover_events=[],
            missing_inputs=list(probe["missingInputs"]),
        )
        evidence["status"] = "blocked"
        validate_redacted_aggregate(evidence, manifest)
        return {"probe": probe, "evidence": evidence, "trials": []}

    skipped: set[str] = set()
    failover_events: list[dict[str, object]] = []
    models_used: list[str] = []
    completed_trials: list[dict[str, object]] = []
    candidates = _load_candidates()
    if pinned_model:
        names = {str(row.get("model")) for row in candidates}
        if pinned_model not in names:
            raise ValueError(f"pinned model not in remaining most-intelligent catalog: {pinned_model}")
        model = pinned_model
        if model != PREFERRED_MODEL:
            failover_events.append(
                {
                    "from": PREFERRED_MODEL,
                    "to": model,
                    "reason": "operator-pin-after-preferred-unavailable",
                }
            )
    else:
        model = select_model(candidates, preferred=PREFERRED_MODEL, skipped=skipped)
    # Tiny readiness ping so cooled/exhausted identities fail before a pair starts.
    while True:
        print(f"preflight model={model}", flush=True)
        try:
            omniroute_chat(
                model=model,
                system="Reply JSON only.",
                prompt='Return {"ok":true}',
                max_tokens=16,
            )
            break
        except PaidTransportError as error:
            skipped.add(model)
            previous = model
            candidates = _load_candidates()
            model = select_model(candidates, preferred=PREFERRED_MODEL, skipped=skipped)
            failover_events.append(
                {"from": previous, "to": model, "reason": f"skip-paid:{error}"}
            )
        except RuntimeError as error:
            message = str(error)
            transient = (
                "429" in message
                or "cooling" in message.lower()
                or "rate_limit" in message.lower()
                or "RATE_LIMIT_EXECUTION_TIMEOUT" in message
                or "service_unavailable" in message.lower()
                or "503" in message
                or "exhausted" in message.lower()
            )
            if not transient:
                raise
            skipped.add(model)
            previous = model
            candidates = _load_candidates()
            model = select_model(candidates, preferred=PREFERRED_MODEL, skipped=skipped)
            failover_events.append({"from": previous, "to": model, "reason": "429-or-exhaust-preflight"})
    if model != PREFERRED_MODEL and not any(
        event.get("to") == model for event in failover_events
    ):
        failover_events.append(
            {
                "from": PREFERRED_MODEL,
                "to": model,
                "reason": "preferred-not-remaining-or-skipped-at-selection",
            }
        )

    for task in SELECTED_TASKS:
        for attempt in range(1, ATTEMPTS_PER_TASK + 1):
            pair_trials: list[dict[str, object]] = []
            pair_model = model
            print(f"pair start {task}__{attempt} model={pair_model}", flush=True)
            for arm in ARMS:
                while True:
                    try:
                        print(f"trial start task={task} attempt={attempt} arm={arm} model={pair_model}", flush=True)
                        trial = run_trial(task=task, arm=arm, model=pair_model, attempt=attempt)
                        print(
                            f"trial done task={task} attempt={attempt} arm={arm} "
                            f"correctness={trial['correctness']} tokens={trial['tokens']}",
                            flush=True,
                        )
                        trial["retries"] = len(failover_events)
                        pair_trials.append(trial)
                        if pair_model not in models_used:
                            models_used.append(pair_model)
                        break
                    except (PaidTransportError, RuntimeError) as error:
                        message = str(error)
                        paid = isinstance(error, PaidTransportError)
                        transient = paid or (
                            "429" in message
                            or "cooling" in message.lower()
                            or "rate_limit" in message.lower()
                            or "RATE_LIMIT_EXECUTION_TIMEOUT" in message
                            or "service_unavailable" in message.lower()
                            or "503" in message
                            or "exhausted" in message.lower()
                        )
                        if not transient:
                            raise
                        if pair_trials:
                            # Do not mix models inside a started pair.
                            raise RuntimeError(
                                f"pair {task}__{attempt} failed after partial arm with model {pair_model}"
                            ) from error
                        skipped.add(pair_model)
                        candidates = _load_candidates()
                        next_model = select_model(
                            candidates, preferred=PREFERRED_MODEL, skipped=skipped
                        )
                        failover_events.append(
                            {
                                "from": pair_model,
                                "to": next_model,
                                "reason": "skip-paid" if paid else "429-or-exhaust",
                            }
                        )
                        pair_model = next_model
                        model = next_model
            assert_pairing_invariant(
                [
                    {
                        "pairId": f"{task}__{attempt}",
                        "model": pair_model,
                        "trials": pair_trials,
                    }
                ]
            )
            completed_trials.extend(pair_trials)

    evidence = build_redacted_aggregate(
        manifest,
        completed_trials,
        models_used=models_used,
        preferred_model=PREFERRED_MODEL,
        failover_events=failover_events,
    )
    return {"probe": probe, "evidence": evidence, "trials": completed_trials}


def render_scorecard(evidence: dict[str, object]) -> str:
    metrics = _object(evidence.get("metrics"), "metrics")
    gate = _object(_object(evidence.get("comparison"), "comparison").get("gateVerdict"), "gate")
    lines = [
        "# ChaosEngine OmniRoute decision-quality calibration",
        "",
        "Accessed: 2026-09-02. Parent tracker: #5549. Deliverable for #5522.",
        "",
        "Transport: local OmniRoute loopback free/remaining catalog only.",
        "Public ChaosGauge task identities are an unchanged subset of #5450.",
        "Missing telemetry is `UNAVAILABLE` (never `0`). No prompts, transcripts,",
        "secrets, private paths, or provider routes are persisted.",
        "",
        "## Campaign",
        "",
        "| Field | Value |",
        "| --- | --- |",
        f"| Planned trials | {evidence['trialAccounting']['planned']} |",
        f"| Observed trials | {evidence['trialAccounting']['observed']} |",
        f"| Status | {evidence['status']} |",
        f"| Models used | {', '.join(evidence.get('modelsUsed') or [])} |",
        f"| Preferred model | {evidence['identity'].get('preferredModel')} |",
        f"| Gate verdict | {gate.get('verdict')} |",
        "",
        "## Arm metrics",
        "",
        "| Metric | control | chaos-engine |",
        "| --- | --- | --- |",
    ]
    for name in CALIBRATION_METRICS:
        left = metrics["control"][name]
        right = metrics["chaos-engine"][name]
        lines.append(f"| `{name}` | {left} | {right} |")
    lines.extend(
        [
            "",
            "## Gate",
            "",
            f"- Verdict: **{gate.get('verdict')}**",
            f"- Reason: {gate.get('reason')}",
            f"- Correctness delta (treatment - control): {evidence['comparison'].get('correctnessDelta')}",
            "",
            "## Method notes",
            "",
            "- Tasks: unchanged public ChaosGauge identities",
            "  `diagnosis-failure-trace`, `repair-regression-test`, `delivery-focused-proof`.",
            "- Arms: `control` (bare instruction) vs `chaos-engine` (Model A treatment prompt).",
            "- Transport: `omniroute chat` against loopback `127.0.0.1:20128` with pinned",
            "  `provider/model` identical on both arms of each pair.",
            "- Preferred primary `agy/gemini-3.7-flash-high` was cooling (429); remaining pairs",
            "  used free most-intelligent failover `nvidia/nemotron-3-ultra-550b-a55b`.",
            "- Local OmniRoute `requestQueue.maxWaitMs=15000` truncated long generations;",
            "  trials still ran and recorded tokens/latency. Applyable patches were not",
            "  emitted within that budget, so correctness stayed 0.0 on both arms.",
            "- Companion machine-readable aggregate:",
            "  `chaos-engine/decision-quality-calibration.aggregate.json`.",
            "",
            "## Failover events",
            "",
        ]
    )
    events = evidence.get("failoverEvents") or []
    if not events:
        lines.append("None.")
    else:
        for event in events:
            lines.append(
                f"- `{event.get('from')}` → `{event.get('to')}` ({event.get('reason')})"
            )
    lines.extend(
        [
            "",
            "## Rollback",
            "",
            "Delete this artifact, `scripts/ci/chaos_gauge/omniroute_calibration.py`,",
            "`tests/scripts/test_omniroute_calibration.py`, and the companion aggregate",
            "JSON; refresh ChaosGauge digests if the `chaos-engine/` tree hash changes.",
            "",
        ]
    )
    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        choices=("probe", "run", "validate", "scorecard"),
    )
    parser.add_argument("--manifest", type=Path, default=ROOT / "experiment.json")
    parser.add_argument("--evidence", type=Path)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--scorecard-out", type=Path)
    parser.add_argument(
        "--model",
        help="optional remaining free provider/model pin after preferred failover",
    )
    args = parser.parse_args()
    manifest = load_manifest(args.manifest)
    if args.command == "probe":
        print(json.dumps(probe_runtime(), sort_keys=True, indent=2))
        return 0
    if args.command == "validate":
        if args.evidence is None:
            raise ValueError("evidence JSON is required")
        validate_redacted_aggregate(json.loads(args.evidence.read_text(encoding="utf-8")), manifest)
        print("ok")
        return 0
    if args.command == "scorecard":
        if args.evidence is None:
            raise ValueError("evidence JSON is required")
        evidence = _object(json.loads(args.evidence.read_text(encoding="utf-8")), "evidence")
        text = render_scorecard(evidence)
        if args.scorecard_out is None:
            print(text, end="")
        else:
            args.scorecard_out.write_text(text, encoding="utf-8")
        return 0

    result = run_campaign(pinned_model=args.model)
    evidence = result["evidence"]
    payload = json.dumps(evidence, sort_keys=True, indent=2) + "\n"
    if args.out is not None:
        args.out.write_text(payload, encoding="utf-8")
    else:
        print(payload, end="")
    if args.scorecard_out is not None:
        args.scorecard_out.write_text(render_scorecard(evidence), encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
