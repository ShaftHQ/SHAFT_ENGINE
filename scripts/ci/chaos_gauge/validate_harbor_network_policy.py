#!/usr/bin/env python3
"""Exercise Harbor 0.22's phase-scoped allowlist merge before a canary starts."""

from __future__ import annotations

import tomllib
from pathlib import Path

import yaml
from harbor.models.task.config import TaskConfig, VerifierEnvironmentMode
from harbor.models.task.verifier_mode import resolve_task_verifier_mode
from harbor.models.trial.config import AgentConfig as TrialAgentConfig
from harbor.models.trial.config import EnvironmentConfig as TrialEnvironmentConfig
from harbor.trial.network_policy import resolve_trial_network_plan


ROOT = Path(__file__).resolve().parents[3]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
MODEL_HOST = "chroma-onnx-models.s3.amazonaws.com"
JOB_NAMES = ("control", "chaos-engine", "full-pilot-control", "full-pilot-chaos-engine")


def _plan(task: TaskConfig):
    return resolve_trial_network_plan(
        task,
        TrialAgentConfig(),
        TrialEnvironmentConfig(extra_allowed_hosts=[MODEL_HOST]),
        None,
        verifier_mode=resolve_task_verifier_mode(task),
    )


def validate() -> None:
    for name in JOB_NAMES:
        job = yaml.safe_load((GAUGE / f"job-configs/{name}.yaml").read_text(encoding="utf-8"))
        if job["environment"].get("extra_allowed_hosts") != [MODEL_HOST]:
            raise ValueError("Harbor environment model-host overlay is invalid")
        if "extra_allowed_hosts" in job["agents"][0]:
            raise ValueError("Harbor model-host overlay must not reach the agent phase")

    for path in (GAUGE / "dataset").glob("*/task.toml"):
        plan = _plan(TaskConfig.model_validate(tomllib.loads(path.read_text(encoding="utf-8"))))
        if MODEL_HOST not in plan.agent_env_baseline.allowed_hosts:
            raise ValueError("public environment baseline omitted the model host")
        if MODEL_HOST in plan.agent_phase.allowed_hosts:
            raise ValueError("public agent phase inherited the model host")
        if plan.verifier_phase.network_mode.value != "no-network":
            raise ValueError("public verifier phase network isolation drifted")

    private_baseline = TaskConfig.model_validate({
        "environment": {"network_mode": "no-network"},
        "agent": {"network_mode": "no-network"},
        "verifier": {
            "environment_mode": "separate",
            "environment": {"network_mode": "no-network"},
        },
    })
    plan = _plan(private_baseline)
    if (
        plan.agent_env_baseline.network_mode.value != "allowlist"
        or plan.agent_env_baseline.allowed_hosts != [MODEL_HOST]
        or plan.agent_phase.network_mode.value != "no-network"
        or plan.verifier_phase.network_mode.value != "no-network"
    ):
        raise ValueError("private phase-scoped model-host isolation drifted")


if __name__ == "__main__":
    validate()
