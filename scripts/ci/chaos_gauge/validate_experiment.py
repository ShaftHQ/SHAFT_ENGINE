#!/usr/bin/env python3
"""Validate an immutable ChaosGauge two-arm experiment before Harbor runs."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path


SCHEMA_VERSION = 1
ARM_FIELDS = {
    "name",
    "agent",
    "model",
    "effort",
    "repositoryRevision",
    "harness",
    "harnessSha256",
    "imageDigest",
    "timeoutSeconds",
    "resources",
}
TREATMENT_FIELDS = {"name", "harness", "harnessSha256"}
SHA256 = re.compile(r"[0-9a-f]{64}")
GIT_SHA = re.compile(r"[0-9a-f]{40}")
IMAGE_DIGEST = re.compile(r"[^\s@]+@sha256:[0-9a-f]{64}")


def _sha256(value: object) -> str:
    payload = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def _mapping(value: object, name: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be an object")
    return value


def validate_manifest(value: object, *, root: Path | None = None) -> str:
    manifest = _mapping(value, "experiment manifest")
    expected = {
        "schemaVersion",
        "identity",
        "harbor",
        "dataset",
        "attemptsPerTask",
        "seed",
        "arms",
        "tasks",
    }
    if set(manifest) != expected or manifest["schemaVersion"] != SCHEMA_VERSION:
        raise ValueError("experiment schema is unsupported")
    if manifest["identity"] != "ShaftHQ/chaos-engine-effectiveness":
        raise ValueError("experiment identity is invalid")
    if manifest["attemptsPerTask"] != 5 or not isinstance(manifest["seed"], int):
        raise ValueError("experiment attempts or seed is invalid")

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
        if not IMAGE_DIGEST.fullmatch(str(candidate["imageDigest"])):
            raise ValueError("image digest is immutable")
        resources = _mapping(candidate["resources"], "arm resources")
        if resources != {"cpus": 2, "memoryMb": 4096, "storageMb": 10240}:
            raise ValueError("arm resources are invalid")
        if candidate["timeoutSeconds"] != 1800:
            raise ValueError("arm timeout is invalid")
        checked.append(candidate)
    control, candidate = checked
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
        if set(item) != {"name", "visibility", "sha256"}:
            raise ValueError("task fields do not match schema v1")
        name = str(item["name"])
        names.add(name)
        if not SHA256.fullmatch(str(item["sha256"])):
            raise ValueError("task digest is invalid")
        if item["visibility"] == "public":
            public += 1
        elif item["visibility"] == "private-reference":
            private += 1
        else:
            raise ValueError("task visibility is invalid")
    if (public, private) != (12, 4):
        raise ValueError("task visibility split must be 12 public and 4 private references")

    return _sha256(manifest)


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
    print(validate_manifest(value, root=args.manifest.parent))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
