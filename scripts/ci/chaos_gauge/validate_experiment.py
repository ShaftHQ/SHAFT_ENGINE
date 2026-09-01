#!/usr/bin/env python3
"""Validate an immutable ChaosGauge two-arm experiment before Harbor runs."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import tomllib
from pathlib import Path

import yaml


SCHEMA_VERSION = 1
ARM_FIELDS = {
    "name",
    "agent",
    "model",
    "effort",
    "repositoryRevision",
    "harness",
    "harnessSha256",
    "treatmentSha256",
    "imageDigest",
    "timeoutSeconds",
    "resources",
}
TREATMENT_FIELDS = {"name", "harness", "harnessSha256", "treatmentSha256"}
SHA256 = re.compile(r"[0-9a-f]{64}")
GIT_SHA = re.compile(r"[0-9a-f]{40}")
IMAGE_DIGEST = re.compile(r"[^\s@]+@sha256:[0-9a-f]{64}")


def _sha256(value: object) -> str:
    payload = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _tree_sha256(root: Path, *, task_package: bool = False) -> str:
    digest = hashlib.sha256()
    files = [path for path in root.rglob("*") if path.is_file()]
    if task_package:
        files = [path for path in files if path.name not in {"README.md", "trajectory.json"}]
    for path in sorted(files, key=lambda item: item.relative_to(root).as_posix()):
        if "__pycache__" in path.parts or path.suffix == ".pyc":
            continue
        digest.update(
            f"{path.relative_to(root).as_posix()}\0{_file_sha256(path)}\n".encode()
        )
    return digest.hexdigest()


def _gauge_root(root: Path) -> Path:
    direct = root / "experiment.json"
    return root if direct.is_file() else root / "scripts/ci/chaos_gauge"


def validate_live_evidence(
    manifest: dict[str, object], jobs: dict[str, object], root: Path, campaign: str
) -> dict[str, str]:
    """Bind every runnable local input into one digest per experiment arm."""
    gauge = _gauge_root(root.resolve())
    repository = root.resolve() if (root.resolve() / "chaos-engine").is_dir() else gauge.parents[2]
    tasks = manifest["tasks"]
    for task in tasks if isinstance(tasks, list) else []:
        if task.get("visibility") != "public":
            continue
        task_root = gauge / "dataset" / str(task["name"])
        if _tree_sha256(task_root, task_package=True) != task["sha256"]:
            raise ValueError(f"live task digest mismatch: {task['name']}")
    dataset = tomllib.loads((gauge / "dataset/dataset.toml").read_text(encoding="utf-8"))
    metric_digest = _file_sha256(gauge / "dataset/metric.py")
    task_digests = sorted(item["digest"].removeprefix("sha256:") for item in dataset["tasks"])
    dataset_digest = hashlib.sha256(
        (",".join(task_digests) + f";metric.py:{metric_digest}").encode()
    ).hexdigest()
    if dataset_digest != manifest["dataset"]["sha256"]:
        raise ValueError("live dataset digest mismatch")
    harness = _tree_sha256(repository / "chaos-engine")
    adapter = _file_sha256(gauge / "agent.py")
    lock = _file_sha256(gauge / "requirements.lock")
    candidate_kwargs = jobs["chaos-engine"]["agents"][0]["kwargs"]
    if candidate_kwargs.get("harness_sha256") != harness:
        raise ValueError("live harness tree digest mismatch")
    if manifest["arms"][1].get("harnessSha256") != harness:
        raise ValueError("manifest harness source digest mismatch")
    if candidate_kwargs.get("adapter_sha256") != adapter:
        raise ValueError("live adapter digest mismatch")
    identities = {
        name: _sha256({
            "repositoryRevision": manifest["arms"][index]["repositoryRevision"],
            "taskDataset": manifest["dataset"]["sha256"],
            "harnessTree": "none" if name == "control" else harness,
            "adapter": "none" if name == "control" else adapter,
            "dependencyLock": lock,
            "campaign": campaign,
            "job": jobs[name],
        })
        for index, name in enumerate(("control", "chaos-engine"))
    }
    if lock != manifest.get("dependencyLockSha256"):
        raise ValueError("live dependency lock digest mismatch")
    for index, name in enumerate(("control", "chaos-engine")):
        treatment = _mapping(manifest["arms"][index].get("treatmentSha256"), "treatment identity")
        if identities[name] != treatment.get(campaign):
            raise ValueError(f"live {name} treatment digest mismatch")
    return identities


def _mapping(value: object, name: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be an object")
    return value


def validate_manifest(  # noqa: MC0001 - immutable schema validation stays fail-closed.
    value: object, *, root: Path | None = None
) -> str:
    manifest = _mapping(value, "experiment manifest")
    expected = {
        "schemaVersion",
        "identity",
        "implementationRevision",
        "harbor",
        "dataset",
        "privatePackage",
        "attemptsPerTask",
        "dependencyLockSha256",
        "seed",
        "campaigns",
        "arms",
        "tasks",
    }
    if set(manifest) != expected or manifest["schemaVersion"] != SCHEMA_VERSION:
        raise ValueError("experiment schema is unsupported")
    if manifest["identity"] != "ShaftHQ/chaos-engine-effectiveness":
        raise ValueError("experiment identity is invalid")
    if not GIT_SHA.fullmatch(str(manifest["implementationRevision"])):
        raise ValueError("implementation identity is invalid")
    if manifest["attemptsPerTask"] != 5 or not isinstance(manifest["seed"], int):
        raise ValueError("experiment attempts or seed is invalid")
    if not SHA256.fullmatch(str(manifest["dependencyLockSha256"])):
        raise ValueError("dependency lock digest is invalid")
    if manifest["campaigns"] != {
        "calibration": {"taskVisibility": ["public"], "taskCount": 12, "privateResolutionRequired": False},
        "full-pilot": {"taskVisibility": ["public", "private-reference"], "taskCount": 16, "privateResolutionRequired": True},
    }:
        raise ValueError("campaign selection contract is invalid")

    harbor = _mapping(manifest["harbor"], "Harbor")
    if harbor != {
        "version": "0.22.0",
        "source": "harbor-framework/harbor@v0.22.0",
        "commit": "4407eb5227a2ff4f0d3f16b2eb48849382fdf276",
    }:
        raise ValueError("Harbor source is not the approved immutable v0.22.0 release")

    dataset = _mapping(manifest["dataset"], "dataset")
    if dataset.get("schemaVersion") != "1.4" or dataset.get("version") != "1.0.0":
        raise ValueError("dataset identity is invalid")
    if not SHA256.fullmatch(str(dataset.get("sha256", ""))):
        raise ValueError("dataset digest is invalid")

    private_package = _mapping(manifest["privatePackage"], "private package")
    if private_package != {
        "repository": "ShaftHQ/ChaosGauge-private",
        "commit": "5c5c00896139c767946747ba38029d88fe750472",
        "contentSha256": "sha256:7db9c6399f126edbaa60226e9eda09b5742b7302e7badea663c365ec7b2dce10",
        "name": "ShaftHQ/chaosgauge-private",
        "ref": "sha256:7db9c6399f126edbaa60226e9eda09b5742b7302e7badea663c365ec7b2dce10",
        "status": "requires-credentials",
    }:
        raise ValueError("private package plan is invalid")

    arms = manifest["arms"]
    if not isinstance(arms, list) or len(arms) != 2:
        raise ValueError("experiment must contain exactly two arms")
    checked: list[dict[str, object]] = []
    for arm in arms:
        candidate = _mapping(arm, "arm")
        if set(candidate) != ARM_FIELDS:
            raise ValueError("arm fields do not match schema v1")
        if not GIT_SHA.fullmatch(str(candidate["repositoryRevision"])):
            raise ValueError("arm repository identity is invalid")
        if not SHA256.fullmatch(str(candidate["harnessSha256"])):
            raise ValueError("harness digest is invalid")
        treatment = _mapping(candidate["treatmentSha256"], "treatment identity")
        if set(treatment) != {"calibration", "full-pilot"} or not all(
            SHA256.fullmatch(str(digest)) for digest in treatment.values()
        ):
            raise ValueError("treatment digest is invalid")
        if not IMAGE_DIGEST.fullmatch(str(candidate["imageDigest"])):
            raise ValueError("image digest is immutable")
        resources = _mapping(candidate["resources"], "arm resources")
        if resources != {"cpus": 2, "memoryMb": 4096, "storageMb": 10240}:
            raise ValueError("arm resources are invalid")
        if candidate["timeoutSeconds"] != 1800:
            raise ValueError("arm timeout is invalid")
        checked.append(candidate)
    control, candidate = checked
    if control["repositoryRevision"] != manifest["implementationRevision"]:
        raise ValueError("implementation revision is not bound to both arms")
    if control["name"] != "control" or control["harness"] != "none":
        raise ValueError("control treatment is invalid")
    if candidate["name"] != "chaos-engine" or candidate["harness"] != "chaos-engine":
        raise ValueError("candidate treatment is invalid")
    for field in sorted(ARM_FIELDS - TREATMENT_FIELDS):
        if control[field] != candidate[field]:
            label = {"imageDigest": "image", "repositoryRevision": "repository"}.get(
                field, field
            )
            raise ValueError(f"arm {label} drift is not allowed")
    if control["harnessSha256"] == candidate["harnessSha256"]:
        raise ValueError("harness treatment digests must differ")

    tasks = manifest["tasks"]
    if not isinstance(tasks, list):
        raise ValueError("tasks must be an array")
    raw_names = [item.get("name") for item in tasks if isinstance(item, dict)]
    if len(raw_names) != len(set(raw_names)):
        raise ValueError("duplicate task identity")
    if len(tasks) != 16:
        raise ValueError("task corpus must contain exactly 16 tasks")
    names: set[str] = set()
    public = private = 0
    for task in tasks:
        item = _mapping(task, "task")
        expected_fields = {"name", "visibility", "sha256"}
        if item.get("visibility") == "private-reference":
            expected_fields.add("stratum")
        if set(item) != expected_fields:
            raise ValueError("task fields do not match schema v1")
        name = str(item["name"])
        names.add(name)
        if not SHA256.fullmatch(str(item["sha256"])):
            raise ValueError("task digest is invalid")
        if item["visibility"] == "public":
            public += 1
        elif item["visibility"] == "private-reference":
            if item.get("stratum") not in {
                "diagnosis", "focused-repair", "cross-file-recovery", "safety-delivery"
            }:
                raise ValueError("private task stratum is invalid")
            private += 1
        else:
            raise ValueError("task visibility is invalid")
    if (public, private) != (12, 4):
        raise ValueError("task visibility split must be 12 public and 4 private references")

    return _sha256(manifest)


def validate_private_package(manifest: object, resolution_path: Path) -> None:
    value = _mapping(manifest, "experiment manifest")
    package = _mapping(value.get("privatePackage"), "private package")
    if resolution_path.is_symlink() or not resolution_path.is_file():
        raise ValueError("private Harbor package is unresolved")
    try:
        resolution = json.loads(resolution_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ValueError("private Harbor package is unresolved") from error
    private_tasks = [task for task in value.get("tasks", []) if task.get("visibility") == "private-reference"]
    expected = {
        **package,
        "tasks": [
            {"name": task["name"], "stratum": task["stratum"], "sha256": task["sha256"]}
            for task in private_tasks
        ],
    }
    if resolution != expected or len(private_tasks) != 4:
        raise ValueError("private Harbor package resolution does not match the 4-task holdout")


def validate_job_contracts(  # noqa: MC0001 - cross-arm equality is one invariant.
    manifest: object, jobs: object, *, campaign: str = "calibration", root: Path | None = None
) -> dict[str, str] | None:
    value = _mapping(manifest, "experiment manifest")
    job_map = _mapping(jobs, "Harbor jobs")
    if set(job_map) != {"control", "chaos-engine"}:
        raise ValueError("Harbor job arms are incomplete")
    arms = value.get("arms")
    if not isinstance(arms, list):
        raise ValueError("experiment arms are invalid")
    arm_map = {
        str(arm["name"]): arm for arm in arms if isinstance(arm, dict) and "name" in arm
    }
    if campaign not in {"calibration", "full-pilot"}:
        raise ValueError("campaign identity is invalid")
    package = _mapping(value.get("privatePackage"), "private package")
    datasets = [{"path": "scripts/ci/chaos_gauge/dataset"}]
    if campaign == "full-pilot":
        datasets.append({"name": package["name"], "ref": package["ref"]})
    for name in ("control", "chaos-engine"):
        job = _mapping(job_map[name], f"{name} Harbor job")
        suffix = name if campaign == "calibration" else f"full-pilot-{name}"
        if job.get("job_name") != f"chaos-gauge-{suffix}" or job.get("jobs_dir") != "scripts/ci/chaos_gauge/jobs":
            raise ValueError("Harbor job identity is invalid")
        if job.get("n_concurrent_trials") != 2 or job.get("timeout_multiplier") != 1.0:
            raise ValueError("Harbor execution budget drift is not allowed")
        if job.get("retry") != {
            "max_retries": 2,
            "include_exceptions": ["EnvironmentStartError", "EnvironmentBuildError"],
        }:
            raise ValueError("Harbor retry budget drift is not allowed")
        expected_environment = {
            "type": "docker", "delete": True,
            "cpu_enforcement_policy": "limit", "memory_enforcement_policy": "limit",
            "extra_allowed_hosts": ["chroma-onnx-models.s3.amazonaws.com"],
        }
        if job.get("environment") != expected_environment:
            raise ValueError("Harbor environment budget drift is not allowed")
        if campaign == "full-pilot" and job.get("quiet") is not False:
            raise ValueError("full-pilot job visibility is invalid")
        if campaign == "calibration" and "quiet" in job:
            raise ValueError("calibration job visibility is invalid")
        agents = job.get("agents")
        job_datasets = job.get("datasets")
        if not isinstance(agents, list) or len(agents) != 1 or not isinstance(agents[0], dict):
            raise ValueError("job agent contract is invalid")
        if not isinstance(job_datasets, list) or job_datasets != datasets:
            raise ValueError("job dataset contract is invalid")
        agent = agents[0]
        arm = _mapping(arm_map.get(name), "experiment arm")
        if name == "control":
            if agent.get("name") != "codex" or "import_path" in agent:
                raise ValueError("job agent drift is not allowed")
        elif (
            agent.get("import_path")
            != "scripts.ci.chaos_gauge.agent:ChaosEngineCodex"
            or "name" in agent
        ):
            raise ValueError("job harness treatment is invalid")
        if agent.get("model_name") != arm.get("model"):
            raise ValueError("job model drift is not allowed")
        kwargs = _mapping(agent.get("kwargs"), "job agent arguments")
        if kwargs.get("reasoning_effort") != arm.get("effort"):
            raise ValueError("job effort drift is not allowed")
        if "skills" in agent:
            raise ValueError("job harness treatment is invalid")
        if agent.get("override_setup_timeout_sec") != 900:
            raise ValueError("job setup timeout drift is not allowed")
        if name == "chaos-engine":
            expected = {
                "version": "0.152.0",
                "reasoning_effort": arm.get("effort"),
                "harness_source": "chaos-engine",
                "harness_commit": arm.get("repositoryRevision"),
                "harness_sha256": "ff693b8f4f728fcdb73d74c204ae2ce0190a0e4009108bf3d9baabe0aafe98a4",
                "adapter_sha256": "3d081c632519b2fb9d6df271b198e4e1404cfd26bc68072e3104131c352db3bd",
            }
            if kwargs != expected:
                raise ValueError("job harness treatment is invalid")
        if job.get("n_attempts") != value.get("attemptsPerTask"):
            raise ValueError("job attempt drift is not allowed")
    control = json.loads(json.dumps(job_map["control"]))
    candidate = json.loads(json.dumps(job_map["chaos-engine"]))
    for job in (control, candidate):
        job.pop("job_name", None)
        selected = _mapping(job["agents"][0], "job agent")
        selected.pop("name", None)
        selected.pop("import_path", None)
        selected_kwargs = _mapping(selected["kwargs"], "job agent arguments")
        for field in ("harness_source", "harness_commit", "harness_sha256", "adapter_sha256"):
            selected_kwargs.pop(field, None)
    if control != candidate:
        raise ValueError("Harbor jobs differ outside the harness treatment")
    return None if root is None else validate_live_evidence(value, job_map, root, campaign)


def load_jobs(root: Path, campaign: str = "calibration") -> dict[str, object]:
    jobs: dict[str, object] = {}
    if campaign not in {"calibration", "full-pilot"}:
        raise ValueError("campaign identity is invalid")
    for name in ("control", "chaos-engine"):
        prefix = "" if campaign == "calibration" else "full-pilot-"
        path = root / "job-configs" / f"{prefix}{name}.yaml"
        if path.is_symlink() or not path.is_file() or path.stat().st_size > 64 * 1024:
            raise ValueError("Harbor job configuration path is unsafe")
        try:
            jobs[name] = yaml.safe_load(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeError, yaml.YAMLError) as error:
            raise ValueError("Harbor job configuration is malformed") from error
    return jobs


def load_manifest(path: Path) -> dict[str, object]:
    if path.is_symlink() or not path.is_file() or path.stat().st_size > 256 * 1024:
        raise ValueError("experiment manifest path is unsafe")
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ValueError("experiment manifest JSON is malformed") from error
    validate_manifest(value, root=path.parent)
    return value


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path)
    args = parser.parse_args()
    value = load_manifest(args.manifest)
    for selected_campaign in ("calibration", "full-pilot"):
        validate_job_contracts(
            value, load_jobs(args.manifest.parent, selected_campaign), campaign=selected_campaign, root=args.manifest.parent
        )
    print(validate_manifest(value, root=args.manifest.parent))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
