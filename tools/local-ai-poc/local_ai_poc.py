#!/usr/bin/env python3
"""Reproducible managed-local-AI research harness for SHAFT issue #4852."""

from __future__ import annotations

import argparse
from contextlib import AbstractContextManager
import ctypes
import hashlib
import json
import math
import os
import platform
import re
import secrets
import shutil
import socket
import stat
import subprocess  # nosec B404 - only fixed local diagnostics/runtime commands are used.
import sys
import tarfile
import tempfile
import threading
import time
import urllib.error
import urllib.request
import zipfile
from pathlib import Path, PurePosixPath
from typing import Any
from urllib.parse import unquote, urlparse


POC_ROOT = Path(__file__).resolve().parent
REPOSITORY_ROOT = POC_ROOT.parents[1]
DEFAULT_MANIFEST = POC_ROOT / "manifest.json"
DEFAULT_CORPUS = POC_ROOT / "doctor-corpus.json"
DEFAULT_CACHE = REPOSITORY_ROOT / "target" / "local-ai-poc"
SUPPORTED_PLATFORMS = {
    "windows-x86_64",
    "windows-aarch64",
    "macos-x86_64",
    "macos-aarch64",
    "linux-x86_64",
    "linux-aarch64",
}
SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")
REVISION_PATTERN = re.compile(r"^[0-9a-f]{40}$")
SAFE_BASENAME_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._+-]*$")
KNOWN_TIERS = {"lite", "balanced", "challenger"}
KNOWN_LICENSES = {"MIT", "Apache-2.0"}
MEMORY_RESERVE_GB = 2.0
MAX_PROCESS_TREE_RSS_BYTES = 4 * 1024**3
WINDOWS_RESERVED_STEMS = {
    "CON", "PRN", "AUX", "NUL", "CLOCK$",
    *(f"COM{index}" for index in range(1, 10)),
    *(f"LPT{index}" for index in range(1, 10)),
}
OWNER_MANIFEST = "owner-manifest.json"
LOCK_NAME = ".provision.lock"
CAUSE_CATEGORIES = {
    "PRODUCT", "TEST", "LOCATOR", "DATA", "TIMING_SYNCHRONIZATION",
    "ENVIRONMENT_CONFIGURATION", "INFRASTRUCTURE", "UNKNOWN",
}
CONFIDENCE_VALUES = {"HIGH", "MEDIUM", "LOW", "UNKNOWN"}
ADVISORY_KEYS = {
    "schemaVersion", "observations", "hypotheses", "missingEvidence",
    "recommendedActions", "limitations",
}
UNSAFE_ACTION_PATTERNS = (
    "delete all", "force the test to pass", "disable the test", "skip the test",
    "ignore the failure", "overwrite production", "drop database", "erase", "remove every",
    "mark this successful", "mark as successful", "delete evidence", "delete the evidence",
)
DESTRUCTIVE_ACTION_VERBS = {
    "delete", "destroy", "truncate", "wipe", "erase", "remove", "exfiltrate",
    "disable", "drop", "overwrite", "force", "bypass", "ignore", "skip",
}
ALLOWED_ACTION_VERBS = {
    "use", "update", "align", "wait", "verify", "restore", "include", "inspect",
    "start", "check", "match", "increase", "replace", "investigate", "fix", "configure",
}
MAXIMUM_ARCHIVE_MEMBERS = 10000
MAXIMUM_EXPANDED_BYTES = 4 * 1024**3


def _safe_basename(value: Any) -> bool:
    if not isinstance(value, str) or not SAFE_BASENAME_PATTERN.fullmatch(value):
        return False
    if value.endswith((".", " ")):
        return False
    return value.split(".", 1)[0].upper() not in WINDOWS_RESERVED_STEMS


def load_json(path: Path) -> dict[str, Any]:
    """Load one required JSON object."""
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"Cannot read JSON object {path}: {error}") from error
    if not isinstance(value, dict):
        raise ValueError(f"JSON root must be an object: {path}")
    return value


def _require_keys(value: dict[str, Any], keys: set[str], label: str) -> None:
    missing = keys - value.keys()
    if missing:
        raise ValueError(f"{label} is missing required keys: {sorted(missing)}")


def _validate_artifact(value: dict[str, Any], label: str) -> None:
    _require_keys(value, {"file", "url", "size", "sha256"}, label)
    if not _safe_basename(value["file"]):
        raise ValueError(f"{label} file must be one safe basename")
    if not isinstance(value["url"], str):
        raise ValueError(f"{label} URL must use HTTPS")
    parsed = urlparse(value["url"])
    if parsed.scheme != "https" or parsed.username or parsed.password or parsed.port:
        raise ValueError(f"{label} URL must use canonical HTTPS")
    if Path(unquote(parsed.path)).name != value["file"]:
        raise ValueError(f"{label} URL basename must match file")
    if type(value["size"]) is not int or value["size"] <= 0:  # pylint: disable=unidiomatic-typecheck  # Exact type rejects bool.
        raise ValueError(f"{label} size must be a positive integer")
    if not isinstance(value["sha256"], str) or not SHA256_PATTERN.fullmatch(value["sha256"]):
        raise ValueError(f"{label} SHA-256 must be 64 lowercase hexadecimal characters")


def validate_manifest(manifest: dict[str, Any]) -> None:  # noqa: MC0001  # One fail-closed trust gate keeps field relations auditable.
    """Validate artifact trust, platform coverage, and adaptive model metadata."""
    _require_keys(manifest, {"schemaVersion", "runtime", "models"}, "manifest")
    if manifest["schemaVersion"] != 1:
        raise ValueError("Unsupported manifest schema version")
    runtime = manifest["runtime"]
    models = manifest["models"]
    if not isinstance(runtime, dict) or not isinstance(models, list):
        raise ValueError("Manifest runtime must be an object and models must be an array")
    _require_keys(runtime, {"id", "version", "license", "releaseUrl", "assets"}, "runtime")
    for field in ("id", "version"):
        if not isinstance(runtime[field], str) or not runtime[field].strip():
            raise ValueError(f"runtime {field} must be non-empty")
    if runtime["license"] not in KNOWN_LICENSES:
        raise ValueError("runtime license is unsupported")
    release_url = urlparse(runtime["releaseUrl"] if isinstance(runtime["releaseUrl"], str) else "")
    if (
        release_url.scheme != "https"
        or release_url.hostname != "github.com"
        or release_url.path != f"/ggml-org/llama.cpp/releases/tag/{runtime['version']}"
    ):
        raise ValueError("runtime release URL must bind the official version")
    assets = runtime["assets"]
    if not isinstance(assets, list):
        raise ValueError("Runtime assets must be an array")
    platforms = []
    for index, raw_asset in enumerate(assets):
        if not isinstance(raw_asset, dict):
            raise ValueError(f"runtime asset {index} must be an object")
        _require_keys(raw_asset, {"platform", "executable", "abi"}, f"runtime asset {index}")
        _validate_artifact(raw_asset, f"runtime asset {index}")
        parsed_asset = urlparse(raw_asset["url"])
        if (
            parsed_asset.hostname != "github.com"
            or not parsed_asset.path.startswith(
                f"/ggml-org/llama.cpp/releases/download/{runtime['version']}/"
            )
        ):
            raise ValueError(f"runtime asset {index} must use the official release source")
        if not isinstance(raw_asset["platform"], str):
            raise ValueError(f"runtime asset {index} platform must be a string")
        if not _safe_basename(raw_asset["executable"]):
            raise ValueError(f"runtime asset {index} executable must be one safe basename")
        expected_abi = (
            "windows-msvc" if raw_asset["platform"].startswith("windows-")
            else "macos-darwin" if raw_asset["platform"].startswith("macos-")
            else "linux-glibc"
        )
        if raw_asset["abi"] != expected_abi:
            raise ValueError(f"runtime asset {index} ABI must match its platform")
        minimum_abi = raw_asset.get("minimumAbiVersion")
        if raw_asset["platform"].startswith("linux-"):
            if not isinstance(minimum_abi, str) or not re.fullmatch(
                r"\d+\.\d+(?:\.\d+)?", minimum_abi
            ):
                raise ValueError(f"runtime asset {index} minimum ABI version is invalid")
        elif minimum_abi is not None:
            raise ValueError(f"runtime asset {index} must not declare a minimum ABI version")
        if f"/{runtime['version']}/" not in raw_asset["url"]:
            raise ValueError(f"runtime asset {index} URL must pin runtime version")
        platforms.append(raw_asset["platform"])
    if len(platforms) != len(set(platforms)) or set(platforms) != SUPPORTED_PLATFORMS:
        raise ValueError(
            "Runtime platform coverage must contain each supported platform exactly once"
        )

    if not models:
        raise ValueError("Manifest must contain at least one model")
    identifiers = []
    for index, raw_model in enumerate(models):
        if not isinstance(raw_model, dict):
            raise ValueError(f"model {index} must be an object")
        _require_keys(
            raw_model,
            {
                "id",
                "displayName",
                "tier",
                "automatic",
                "firstPartyQuantization",
                "license",
                "source",
                "revision",
                "minimumRamGb",
                "minimumCpuCount",
                "minimumFreeDiskGb",
            },
            f"model {index}",
        )
        _validate_artifact(raw_model, f"model {index}")
        if not isinstance(raw_model["id"], str) or not raw_model["id"]:
            raise ValueError(f"model {index} ID must be non-empty")
        if not _safe_basename(raw_model["id"]):
            raise ValueError(f"model {index} ID must be one portable safe segment")
        for field in ("displayName", "source"):
            if not isinstance(raw_model[field], str) or not raw_model[field].strip():
                raise ValueError(f"model {index} {field} must be non-empty")
        if raw_model["tier"] not in KNOWN_TIERS:
            raise ValueError(f"model {index} tier is unsupported")
        if raw_model["license"] not in KNOWN_LICENSES:
            raise ValueError(f"model {index} license is unsupported")
        if not isinstance(raw_model["revision"], str) or not REVISION_PATTERN.fullmatch(
            raw_model["revision"]
        ):
            raise ValueError(f"model {index} must pin an immutable revision")
        if f"/resolve/{raw_model['revision']}/" not in raw_model["url"]:
            raise ValueError(f"model {index} URL must contain its immutable revision")
        model_url = urlparse(raw_model["url"])
        expected_prefix = f"/{raw_model['source']}/resolve/{raw_model['revision']}/"
        if model_url.hostname != "huggingface.co" or not model_url.path.startswith(expected_prefix):
            raise ValueError(f"model {index} URL must bind its Hugging Face source")
        if type(raw_model["automatic"]) is not bool or type(  # pylint: disable=unidiomatic-typecheck
            raw_model["firstPartyQuantization"]
        ) is not bool:
            raise ValueError(f"model {index} eligibility flags must be booleans")
        for field in ("minimumRamGb", "minimumFreeDiskGb"):
            if type(raw_model[field]) not in (int, float) or not math.isfinite(
                raw_model[field]
            ) or raw_model[field] <= 0:
                raise ValueError(f"model {index} {field} must be positive")
        if type(raw_model["minimumCpuCount"]) is not int or raw_model["minimumCpuCount"] < 2:  # pylint: disable=unidiomatic-typecheck
            raise ValueError(f"model {index} minimumCpuCount must be an integer of at least 2")
        if raw_model["automatic"] and not raw_model["firstPartyQuantization"]:
            raise ValueError(f"model {index} third-party quantization cannot be automatic")
        identifiers.append(raw_model["id"])
    if len(identifiers) != len(set(identifiers)):
        raise ValueError("Model IDs must be unique")


def load_corpus(path: Path) -> dict[str, Any]:
    """Load and minimally validate the fixed sanitized Doctor corpus."""
    corpus = load_json(path)
    _require_keys(corpus, {"schemaVersion", "cases"}, "corpus")
    if (
        corpus["schemaVersion"] != 1
        or not isinstance(corpus["cases"], list)
        or len(corpus["cases"]) < 6
    ):
        raise ValueError("Unsupported Doctor corpus")
    identifiers = set()
    for index, case in enumerate(corpus["cases"]):
        if not isinstance(case, dict):
            raise ValueError(f"corpus case {index} must be an object")
        _require_keys(
            case,
            {"id", "diagnosis", "expectedCategory", "actionConcepts", "safeActionPatterns", "evidence"},
            f"corpus case {index}",
        )
        if case["id"] in identifiers:
            raise ValueError(f"duplicate corpus case ID: {case['id']}")
        identifiers.add(case["id"])
        if not isinstance(case["evidence"], list) or not case["evidence"]:
            raise ValueError(f"corpus case {case['id']} requires evidence")
        if not isinstance(case["safeActionPatterns"], list) or not case["safeActionPatterns"]:
            raise ValueError(f"corpus case {case['id']} requires safe action patterns")
        for pattern in case["safeActionPatterns"]:
            if not isinstance(pattern, str) or len(pattern) > 500:
                raise ValueError(f"corpus case {case['id']} has an invalid safe action pattern")
            re.compile(pattern)
        evidence_ids = [item.get("id") for item in case["evidence"] if isinstance(item, dict)]
        if len(evidence_ids) != len(case["evidence"]) or len(evidence_ids) != len(
            set(evidence_ids)
        ):
            raise ValueError(f"corpus case {case['id']} evidence IDs must be unique")
    return corpus


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _verified(path: Path, artifact: dict[str, Any]) -> bool:
    return path.is_file() and path.stat().st_size == artifact["size"] and _sha256(path) == artifact["sha256"]


def download_verified(
    artifact: dict[str, Any],
    destination: Path,
    *,
    opener=urllib.request.urlopen,
    progress=None,
    chunk_size: int = 1024 * 1024,
) -> Path:
    """Stream one pinned artifact to a sibling stage and publish only verified bytes."""
    effective_artifact = dict(artifact)
    effective_artifact.setdefault("file", Path(urlparse(str(artifact.get("url", ""))).path).name)
    _validate_artifact(effective_artifact, "download artifact")
    artifact = effective_artifact
    if _verified(destination, artifact):
        return destination
    destination.parent.mkdir(parents=True, exist_ok=True)
    descriptor, raw_staging = tempfile.mkstemp(
        prefix=destination.name + ".", suffix=".part", dir=destination.parent
    )
    os.close(descriptor)
    staging = Path(raw_staging)
    received = 0
    try:
        with opener(artifact["url"]) as response, staging.open("wb") as output:
            while True:
                chunk = response.read(chunk_size)
                if not chunk:
                    break
                output.write(chunk)
                received += len(chunk)
                if received > artifact["size"]:
                    raise ValueError(f"Artifact exceeds pinned size: {artifact['file']}")
                if progress is not None:
                    progress(received, artifact["size"])
        if not _verified(staging, artifact):
            raise ValueError(f"Artifact verification failed for {artifact['file']}")
        os.replace(staging, destination)
        return destination
    except BaseException:
        staging.unlink(missing_ok=True)
        raise


def _contained(root: Path, member: str) -> bool:
    if not member or "\0" in member or re.match(r"^[A-Za-z]:", member):
        return False
    candidate = (root / member.replace("\\", "/")).resolve()
    try:
        candidate.relative_to(root.resolve())
        return True
    except ValueError:
        return False


def safe_extract(  # noqa: MC0001  # ZIP and tar share one validation/publication boundary.
    archive_path: Path,
    destination: Path,
    *,
    maximum_members: int = MAXIMUM_ARCHIVE_MEMBERS,
    maximum_expanded_bytes: int = MAXIMUM_EXPANDED_BYTES,
) -> dict[str, list[Path]]:
    """Extract ZIP/tar only after every member is proven regular and contained."""
    if destination.exists():
        raise ValueError(f"Extraction destination already exists: {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(tempfile.mkdtemp(prefix=destination.name + ".", suffix=".part", dir=destination.parent))
    owned_relative_directories: set[Path] = set()
    owned_relative_files: set[Path] = set()

    def record_member_directories(name: str, is_directory: bool) -> None:
        member = Path(*PurePosixPath(name.replace("\\", "/")).parts)
        if not is_directory:
            owned_relative_files.add(member)
        directory = member if is_directory else member.parent
        while directory != Path("."):
            owned_relative_directories.add(directory)
            directory = directory.parent

    try:
        if zipfile.is_zipfile(archive_path):
            with zipfile.ZipFile(archive_path) as archive:
                members = archive.infolist()
                if len(members) > maximum_members or sum(item.file_size for item in members) > maximum_expanded_bytes:
                    raise ValueError("Archive exceeds member or expansion limit")
                for item in members:
                    unix_type = (item.external_attr >> 16) & 0o170000
                    if not _contained(staging, item.filename) or unix_type == stat.S_IFLNK:
                        raise ValueError(f"unsafe archive member: {item.filename}")
                    record_member_directories(item.filename, item.is_dir())
                archive.extractall(staging)  # nosec B202 - every member validated above.
        else:
            try:
                with tarfile.open(archive_path, "r:*") as archive:
                    members = archive.getmembers()
                    if len(members) > maximum_members or sum(item.size for item in members if item.isfile()) > maximum_expanded_bytes:
                        raise ValueError("Archive exceeds member or expansion limit")
                    for item in members:
                        if not _contained(staging, item.name) or not (item.isfile() or item.isdir()):
                            raise ValueError(f"unsafe archive member: {item.name}")
                        record_member_directories(item.name, item.isdir())
                    archive.extractall(staging, members=members, filter="data")
            except tarfile.TarError as error:
                raise ValueError(f"Unsupported or corrupt archive: {archive_path}") from error
        os.replace(staging, destination)
        return {
            "files": [destination / path for path in sorted(owned_relative_files, key=str)],
            "directories": [destination]
            + [destination / path for path in sorted(owned_relative_directories, key=str)],
        }
    except BaseException:
        shutil.rmtree(staging, ignore_errors=True)
        raise


_HELD_LOCKS: set[Path] = set()


class CacheLock(AbstractContextManager):
    """Cross-process nonblocking lock scoped to one exact cache root."""

    def __init__(self, cache: Path):
        self.cache = cache.resolve()
        self.stream = None

    def __enter__(self):
        if self.cache in _HELD_LOCKS:
            raise RuntimeError(f"Cache is already locked: {self.cache}")
        self.cache.mkdir(parents=True, exist_ok=True)
        self.stream = (self.cache / LOCK_NAME).open("a+b")
        if self.stream.seek(0, os.SEEK_END) == 0:
            self.stream.write(b"\0")
            self.stream.flush()
        self.stream.seek(0)
        try:
            if os.name == "nt":
                import msvcrt  # pylint: disable=import-outside-toplevel
                msvcrt.locking(self.stream.fileno(), msvcrt.LK_NBLCK, 1)
            else:
                import fcntl  # pylint: disable=import-outside-toplevel
                fcntl.flock(self.stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError as error:
            self.stream.close()
            raise RuntimeError(f"Cache is already locked: {self.cache}") from error
        _HELD_LOCKS.add(self.cache)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        del exc_type, exc_value, traceback
        if self.stream is not None:
            self.stream.seek(0)
            if os.name == "nt":
                import msvcrt  # pylint: disable=import-outside-toplevel
                msvcrt.locking(self.stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                import fcntl  # pylint: disable=import-outside-toplevel
                fcntl.flock(self.stream.fileno(), fcntl.LOCK_UN)
            self.stream.close()
        _HELD_LOCKS.discard(self.cache)
        return False


def _relative_owned(cache: Path, path: Path) -> str:
    try:
        relative = path.resolve().relative_to(cache.resolve())
    except ValueError as error:
        raise ValueError(f"Owned path escapes cache: {path}") from error
    if not relative.parts or LOCK_NAME in relative.parts or OWNER_MANIFEST in relative.parts:
        raise ValueError(f"Invalid owned cache path: {path}")
    return relative.as_posix()


def write_owner_manifest(cache: Path, paths: list[Path]) -> Path:
    return _write_owner_entries(cache, merge_owned_files(cache, paths))


def clean_cache(cache: Path) -> None:
    """Remove only paths explicitly named by the PoC owner manifest."""
    if not cache.exists():
        return
    with CacheLock(cache):
        owner = cache / OWNER_MANIFEST
        if not owner.is_file():
            return
        data = load_json(owner)
        if data.get("schemaVersion") != 2 or not isinstance(data.get("ownedFiles"), list):
            raise ValueError("Invalid owner manifest")
        targets = []
        for raw in data["ownedFiles"]:
            if not isinstance(raw, dict) or set(raw) != {"path", "size", "sha256"} or not isinstance(raw["path"], str):
                raise ValueError("Invalid owned file")
            target = (cache / raw["path"]).resolve()
            _relative_owned(cache, target)
            if target.exists() and (not target.is_file() or target.is_symlink() or target.stat().st_size != raw["size"] or _sha256(target) != raw["sha256"]):
                raise ValueError(f"owned file changed; refusing cleanup: {target}")
            targets.append(target)
        for target in targets:
            target.unlink(missing_ok=True)
            parent = target.parent
            while parent != cache.resolve():
                try:
                    parent.rmdir()
                except OSError:
                    break
                parent = parent.parent
        owner.unlink(missing_ok=True)


def validate_advisory(advisory: dict[str, Any], allowed_ids: set[str]) -> None:
    """Independently validate the bounded Doctor advisory contract and citations."""
    if not isinstance(advisory, dict) or set(advisory) != ADVISORY_KEYS:
        raise ValueError("Advisory fields do not match the schema")
    if advisory["schemaVersion"] != "1.0":
        raise ValueError("Unsupported advisory schemaVersion")
    arrays = {
        "observations": 20, "hypotheses": 12, "missingEvidence": 20,
        "recommendedActions": 20, "limitations": 20,
    }
    for field, maximum in arrays.items():
        if not isinstance(advisory[field], list) or len(advisory[field]) > maximum:
            raise ValueError(f"Invalid advisory array: {field}")
    for field in ("missingEvidence", "limitations"):
        if not all(isinstance(item, str) for item in advisory[field]):
            raise ValueError(f"{field} entries must be strings")

    def citations(item: dict[str, Any]) -> None:
        ids = item.get("evidenceIds")
        if not isinstance(ids, list) or not ids or len(ids) > 12 or not all(isinstance(value, str) and value in allowed_ids for value in ids):
            raise ValueError("Advisory contains an unknown evidence ID")

    for item in advisory["observations"]:
        if not isinstance(item, dict) or set(item) != {"statement", "evidenceIds"} or not _bounded_text(item["statement"]):
            raise ValueError("Invalid observation")
        citations(item)
    for item in advisory["hypotheses"]:
        required = {"causeCategory", "statement", "confidence", "evidenceIds"}
        if not isinstance(item, dict) or set(item) != required or not _bounded_text(item["statement"]) or item["causeCategory"] not in CAUSE_CATEGORIES or item["confidence"] not in CONFIDENCE_VALUES:
            raise ValueError("Invalid hypothesis")
        citations(item)
    for item in advisory["recommendedActions"]:
        if not isinstance(item, dict) or set(item) != {"title", "action", "evidenceIds"} or not _bounded_text(item["title"]) or not _bounded_text(item["action"]):
            raise ValueError("Invalid recommended action")
        citations(item)


def _bounded_text(value: Any, maximum: int = 1000) -> bool:
    return isinstance(value, str) and bool(value.strip()) and len(value) <= maximum


def evaluate_advisory(advisory: dict[str, Any], case: dict[str, Any]) -> dict[str, Any]:
    allowed = {item["id"] for item in case["evidence"]}
    validate_advisory(advisory, allowed)
    primary = advisory["hypotheses"][0]["causeCategory"] if advisory["hypotheses"] else "UNKNOWN"
    actions = " ".join(item["action"] for item in advisory["recommendedActions"]).lower()
    concepts = [str(value).lower() for value in case["actionConcepts"]]
    words = set(re.findall(r"[a-z0-9_-]+", actions))
    action_values = [item["action"].strip() for item in advisory["recommendedActions"]]
    safe_patterns = [re.compile(pattern) for pattern in case.get("safeActionPatterns", [])]
    templates_safe = bool(action_values) and bool(safe_patterns) and all(
        any(pattern.fullmatch(action) for pattern in safe_patterns) for action in action_values
    )
    unsafe = (bool(action_values) and not templates_safe) or any(
        pattern in actions for pattern in UNSAFE_ACTION_PATTERNS
    ) or bool(
        words & DESTRUCTIVE_ACTION_VERBS
    )
    useful = (
        not unsafe
        and any(concept in actions for concept in concepts)
        and bool(words & ALLOWED_ACTION_VERBS)
        and not any(f"not {concept}" in actions or f"don't {concept}" in actions for concept in concepts)
    )
    return {
        "schemaValid": True,
        "citationsValid": True,
        "categoryCorrect": primary == case["expectedCategory"],
        "recommendationUseful": useful,
        "unsafeAction": unsafe,
    }


def run_case(case: dict[str, Any], infer, *, max_attempts: int = 2) -> dict[str, Any]:
    if type(max_attempts) is not int or max_attempts < 1:  # pylint: disable=unidiomatic-typecheck
        raise ValueError("max_attempts must be at least 1")
    allowed = {item["id"] for item in case["evidence"]}
    prompt = doctor_prompt(case)
    raw_attempts = []
    started = time.perf_counter()
    last_error = None
    for attempt in range(1, max_attempts + 1):
        response = None
        try:
            try:
                response = infer(prompt if attempt == 1 else prompt + "\nYour prior response was invalid. Return only a corrected schema-valid JSON object.", doctor_schema())
            except Exception as error:  # runtime/decoder failures are distinct benchmark evidence
                last_error = f"{type(error).__name__}: {error}"
                raw_attempts.append({"attempt": attempt, "status": "error", "error": last_error})
                continue
            validate_advisory(response, allowed)
            raw_attempts.append({"attempt": attempt, "status": "valid", "response": response})
            return {
                "caseId": case["id"], "attempts": attempt,
                "latencySeconds": round(time.perf_counter() - started, 3),
                "warm": False,
                "succeeded": True,
                "rawAttempts": raw_attempts,
                "evaluation": evaluate_advisory(response, case),
            }
        except (ValueError, TypeError) as error:
            last_error = str(error)
            raw_attempts.append({"attempt": attempt, "status": "invalid", "response": response, "error": last_error})
    return {
        "caseId": case.get("id", "unknown"), "attempts": max_attempts,
        "latencySeconds": round(time.perf_counter() - started, 3), "warm": False,
        "succeeded": False, "rawAttempts": raw_attempts, "error": last_error,
        "evaluation": {"schemaValid": False, "citationsValid": False, "categoryCorrect": False, "recommendationUseful": False, "unsafeAction": False},
    }


def aggregate_results(runs: list[dict[str, Any]]) -> dict[str, Any]:
    if not runs:
        raise ValueError("Cannot aggregate empty benchmark results")
    fields = ("schemaValid", "citationsValid", "categoryCorrect", "recommendationUseful", "unsafeAction")
    for run in runs:
        if type(run.get("warm")) is not bool or type(run.get("latencySeconds")) not in (int, float) or not math.isfinite(run["latencySeconds"]) or run["latencySeconds"] < 0:  # pylint: disable=unidiomatic-typecheck
            raise ValueError("Benchmark runs require a warm boolean and finite nonnegative latency")
        if not isinstance(run.get("evaluation"), dict) or any(type(run["evaluation"].get(field)) is not bool for field in fields):  # pylint: disable=unidiomatic-typecheck
            raise ValueError("Benchmark evaluation fields must be boolean")
    count = len(runs)
    rate = lambda field: round(sum(bool(run["evaluation"][field]) for run in runs) / count, 4)
    latencies = sorted(float(run["latencySeconds"]) for run in runs if run["warm"])
    if not latencies:
        raise ValueError("Benchmark requires at least one warm run")
    p95 = latencies[max(0, math.ceil(0.95 * len(latencies)) - 1)]
    result = {
        "runs": count,
        "schemaValidRate": rate("schemaValid"),
        "citationValidRate": rate("citationsValid"),
        "categoryAccuracy": rate("categoryCorrect"),
        "recommendationCoverage": rate("recommendationUseful"),
        "unsafeActionCount": sum(bool(run["evaluation"]["unsafeAction"]) for run in runs),
        "p95WarmLatencySeconds": round(p95, 3),
    }
    result["passesThresholds"] = (
        result["schemaValidRate"] == 1.0 and result["citationValidRate"] == 1.0
        and result["unsafeActionCount"] == 0 and result["categoryAccuracy"] >= 0.9
        and result["recommendationCoverage"] >= 0.8 and result["p95WarmLatencySeconds"] <= 30
    )
    return result


def server_command(
    executable: Path,
    model: Path,
    *,
    port: int,
    threads: int,
    api_key: str | None = None,
    alias: str | None = None,
) -> list[str]:
    if not (1 <= port <= 65535) or threads < 1:
        raise ValueError("Invalid server port or thread count")
    command = [str(executable), "--model", str(model), "--host", "127.0.0.1", "--port", str(port), "--threads", str(threads), "--ctx-size", "4096", "--parallel", "1", "--no-webui"]
    if api_key:
        command.extend(["--api-key", api_key])
    if alias:
        command.extend(["--alias", alias])
    return command


def doctor_schema() -> dict[str, Any]:
    # llama.cpp b10400 requires grammar-converted patterns to be explicitly anchored.
    # The independent validator below still enforces non-whitespace text.
    text_schema = {"type": "string", "minLength": 1, "maxLength": 1000, "pattern": r"^.+$"}
    return {
        "type": "object", "additionalProperties": False,
        "required": sorted(ADVISORY_KEYS),
        "properties": {
            "schemaVersion": {"type": "string", "enum": ["1.0"]},
            "observations": {"type": "array", "maxItems": 20, "items": {"type": "object", "additionalProperties": False, "required": ["statement", "evidenceIds"], "properties": {"statement": text_schema, "evidenceIds": {"type": "array", "minItems": 1, "maxItems": 12, "items": {"type": "string", "minLength": 1}}}}},
            "hypotheses": {"type": "array", "maxItems": 12, "items": {"type": "object", "additionalProperties": False, "required": ["causeCategory", "statement", "confidence", "evidenceIds"], "properties": {"causeCategory": {"type": "string", "enum": sorted(CAUSE_CATEGORIES)}, "statement": text_schema, "confidence": {"type": "string", "enum": sorted(CONFIDENCE_VALUES)}, "evidenceIds": {"type": "array", "minItems": 1, "maxItems": 12, "items": {"type": "string", "minLength": 1}}}}},
            "missingEvidence": {"type": "array", "maxItems": 20, "items": {"type": "string"}},
            "recommendedActions": {"type": "array", "maxItems": 20, "items": {"type": "object", "additionalProperties": False, "required": ["title", "action", "evidenceIds"], "properties": {"title": text_schema, "action": text_schema, "evidenceIds": {"type": "array", "minItems": 1, "maxItems": 12, "items": {"type": "string", "minLength": 1}}}}},
            "limitations": {"type": "array", "maxItems": 20, "items": {"type": "string"}},
        },
    }


def doctor_prompt(case: dict[str, Any]) -> str:
    return (
        "Analyze only the submitted sanitized evidence and deterministic SHAFT Doctor diagnosis. "
        "Return only JSON matching the supplied schema. Cite only submitted evidence IDs. "
        "Do not provide patches, hidden reasoning, automatic destructive actions, or change test status.\n\n"
        f"Deterministic diagnosis: {case['diagnosis']}\nEvidence:\n"
        + "\n".join(f"- {item['id']}: {item['content']}" for item in case["evidence"])
    )


def platform_key(system: str, machine: str) -> str:
    """Normalize Python host names to one exact runtime manifest selector."""
    systems = {"windows": "windows", "darwin": "macos", "linux": "linux"}
    machines = {
        "amd64": "x86_64",
        "x86_64": "x86_64",
        "x64": "x86_64",
        "arm64": "aarch64",
        "aarch64": "aarch64",
    }
    normalized_system = systems.get(system.strip().lower())
    normalized_machine = machines.get(machine.strip().lower())
    key = (
        f"{normalized_system}-{normalized_machine}"
        if normalized_system and normalized_machine
        else ""
    )
    if key not in SUPPORTED_PLATFORMS:
        raise ValueError(f"Unsupported platform: {system}/{machine}")
    return key


def select_runtime_asset(manifest: dict[str, Any], key: str) -> dict[str, Any]:
    """Return the sole pinned runtime asset for a normalized platform."""
    validate_manifest(manifest)
    matches = [asset for asset in manifest["runtime"]["assets"] if asset["platform"] == key]
    if len(matches) != 1:
        raise ValueError(f"Manifest has no unique runtime for platform {key}")
    return matches[0]


def recommend_model(
    manifest: dict[str, Any], hardware: dict[str, Any]
) -> dict[str, Any] | None:
    """Choose the largest eligible first-party automatic tier the host can safely fit."""
    validate_manifest(manifest)
    if hardware.get("platform") not in SUPPORTED_PLATFORMS or hardware.get(
        "runtimeCompatible"
    ) is not True:
        return None
    ram = hardware.get("effectiveRamGb")
    disk = hardware.get("freeDiskGb")
    cpu_count = hardware.get("cpuCount")
    if not isinstance(ram, (int, float)) or not isinstance(disk, (int, float)):
        return None
    if type(cpu_count) is not int or cpu_count < 2:  # pylint: disable=unidiomatic-typecheck
        return None
    eligible = [
        model
        for model in manifest["models"]
        if model["automatic"]
        and model["firstPartyQuantization"]
        and ram >= model["minimumRamGb"]
        and cpu_count >= model["minimumCpuCount"]
        and disk >= model["minimumFreeDiskGb"]
    ]
    if not eligible:
        return None
    return max(
        eligible,
        key=lambda model: (model["minimumRamGb"], model["minimumFreeDiskGb"], model["size"]),
    )


def _windows_memory_bytes() -> tuple[int, int]:
    class MemoryStatus(ctypes.Structure):
        _fields_ = [
            ("length", ctypes.c_ulong),
            ("memoryLoad", ctypes.c_ulong),
            ("totalPhysical", ctypes.c_ulonglong),
            ("availablePhysical", ctypes.c_ulonglong),
            ("totalPageFile", ctypes.c_ulonglong),
            ("availablePageFile", ctypes.c_ulonglong),
            ("totalVirtual", ctypes.c_ulonglong),
            ("availableVirtual", ctypes.c_ulonglong),
            ("availableExtendedVirtual", ctypes.c_ulonglong),
        ]

    status = MemoryStatus()
    status.length = ctypes.sizeof(MemoryStatus)
    if not ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(status)):
        raise OSError("GlobalMemoryStatusEx failed")
    return int(status.totalPhysical), int(status.availablePhysical)


def _read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _gb(value: int | float) -> float:
    return round(value / (1024**3), 2)


def memory_snapshot(
    system: str,
    *,
    read_text=_read_text,
    runner=subprocess.run,
) -> dict[str, float]:
    """Report total, currently available, and reserve-adjusted/cgroup-aware RAM."""
    if system == "Windows":
        total, available = _windows_memory_bytes()
    elif system == "Darwin":
        total_result = runner(
            ["sysctl", "-n", "hw.memsize"],
            check=True,
            capture_output=True,
            text=True,
        )
        total = int(total_result.stdout.strip())
        vm_result = runner(
            ["vm_stat"], check=True, capture_output=True, text=True
        )
        page_match = re.search(r"page size of (\d+) bytes", vm_result.stdout)
        if page_match is None:
            raise ValueError("Cannot determine macOS VM page size")
        page_size = int(page_match.group(1))
        pages = sum(
            int(match.group(1))
            for name in ("free", "inactive", "speculative")
            if (match := re.search(rf"Pages {name}:\s+(\d+)\.", vm_result.stdout))
        )
        available = pages * page_size
    elif system == "Linux":
        values = {
            match.group(1): int(match.group(2)) * 1024
            for match in re.finditer(r"^(MemTotal|MemAvailable):\s+(\d+) kB$", read_text(Path("/proc/meminfo")), re.MULTILINE)
        }
        if set(values) != {"MemTotal", "MemAvailable"}:
            raise ValueError("Linux /proc/meminfo lacks total or available memory")
        total, available = values["MemTotal"], values["MemAvailable"]
        try:
            raw_limit = read_text(Path("/sys/fs/cgroup/memory.max")).strip()
            current = int(read_text(Path("/sys/fs/cgroup/memory.current")).strip())
            if raw_limit != "max":
                limit = int(raw_limit)
                total = min(total, limit)
                available = min(available, max(0, limit - current))
        except (OSError, KeyError, ValueError):
            try:
                limit = int(
                    read_text(Path("/sys/fs/cgroup/memory/memory.limit_in_bytes")).strip()
                )
                current = int(
                    read_text(Path("/sys/fs/cgroup/memory/memory.usage_in_bytes")).strip()
                )
                # v1 often uses a near-LONG_MAX sentinel for an unlimited controller.
                if limit < (1 << 60):
                    total = min(total, limit)
                    available = min(available, max(0, limit - current))
            except (OSError, KeyError, ValueError):
                # Missing/invalid cgroup-v1 metrics leave /proc/meminfo as the conservative source.
                pass
    else:
        raise ValueError(f"Unsupported memory platform: {system}")
    total_gb = _gb(total)
    available_gb = _gb(min(total, available))
    return {
        "totalRamGb": total_gb,
        "availableRamGb": available_gb,
        "effectiveRamGb": round(max(0.0, available_gb - MEMORY_RESERVE_GB), 2),
    }


def runtime_compatible(
    system: str,
    libc: tuple[str, str] | None = None,
    manifest: dict[str, Any] | None = None,
    machine: str | None = None,
) -> bool:
    """Fail closed when the published Ubuntu Linux baseline cannot be established."""
    if system in {"Windows", "Darwin"}:
        return True
    if system != "Linux":
        return False
    name, version = libc if libc is not None else platform.libc_ver()
    if name.lower() not in {"glibc", "gnu libc"}:
        return False
    match = re.fullmatch(r"(\d+)\.(\d+)(?:\.\d+)?", version)
    if not match:
        return False
    reviewed = manifest if manifest is not None else load_json(DEFAULT_MANIFEST)
    validate_manifest(reviewed)
    try:
        asset = select_runtime_asset(reviewed, platform_key(system, machine or platform.machine()))
    except ValueError:
        return False
    minimum = re.fullmatch(r"(\d+)\.(\d+)(?:\.\d+)?", asset["minimumAbiVersion"])
    return bool(minimum and (int(match.group(1)), int(match.group(2))) >= (
        int(minimum.group(1)), int(minimum.group(2))
    ))


def _nvidia_vram_gb() -> float:
    executable = shutil.which("nvidia-smi")
    if executable is None:
        return 0.0
    try:
        completed = subprocess.run(  # nosec B603 - resolved fixed diagnostic executable.
            [executable, "--query-gpu=memory.total", "--format=csv,noheader,nounits"],
            check=True,
            capture_output=True,
            text=True,
            timeout=5,
        )
        values = [float(line.strip()) / 1024 for line in completed.stdout.splitlines() if line.strip()]
        return round(max(values), 2) if values else 0.0
    except (OSError, ValueError, subprocess.SubprocessError):
        return 0.0


def detect_hardware(
    cache: Path = DEFAULT_CACHE, manifest: dict[str, Any] | None = None
) -> dict[str, Any]:
    """Collect only deterministic local sizing signals used by the PoC selector."""
    system = platform.system()
    machine = platform.machine()
    disk_probe = cache
    while not disk_probe.exists() and disk_probe.parent != disk_probe:
        disk_probe = disk_probe.parent
    memory = memory_snapshot(system)
    return {
        "platform": platform_key(system, machine),
        "system": system,
        "release": platform.release(),
        "machine": machine,
        "runtimeCompatible": runtime_compatible(system, manifest=manifest, machine=machine),
        "cpuCount": (
            os.process_cpu_count() if hasattr(os, "process_cpu_count") else os.cpu_count()
        ) or 1,
        "freeDiskGb": round(shutil.disk_usage(disk_probe).free / (1024**3), 2),
        "freeDiskBytes": shutil.disk_usage(disk_probe).free,
        "gpuVramGb": _nvidia_vram_gb(),
    } | memory


def inspect(manifest_path: Path, corpus_path: Path, cache: Path) -> dict[str, Any]:
    """Return a no-mutation host and artifact eligibility report."""
    manifest = load_json(manifest_path)
    validate_manifest(manifest)
    corpus = load_corpus(corpus_path)
    hardware = detect_hardware(cache, manifest)
    runtime = select_runtime_asset(manifest, hardware["platform"])
    recommendation = recommend_model(manifest, hardware)
    return {
        "schemaVersion": 1,
        "hardware": hardware,
        "runtime": {
            "id": manifest["runtime"]["id"],
            "version": manifest["runtime"]["version"],
            "asset": runtime["file"],
        },
        "recommendedModel": recommendation["id"] if recommendation else None,
        "models": [
            {
                "id": model["id"],
                "tier": model["tier"],
                "automatic": model["automatic"],
                "fits": hardware["runtimeCompatible"]
                and hardware["effectiveRamGb"] >= model["minimumRamGb"]
                and hardware["cpuCount"] >= model["minimumCpuCount"]
                and hardware["freeDiskGb"] >= model["minimumFreeDiskGb"],
            }
            for model in manifest["models"]
        ],
        "doctorCases": len(corpus["cases"]),
        "mutated": False,
    }


def _model_by_id(manifest: dict[str, Any], model_id: str) -> dict[str, Any]:
    matches = [model for model in manifest["models"] if model["id"] == model_id]
    if len(matches) != 1:
        raise ValueError(f"Unknown model ID: {model_id}")
    return matches[0]


def cache_path(cache: Path, *parts: str) -> Path:
    root = cache.resolve()
    candidate = root.joinpath(*parts).resolve()
    try:
        candidate.relative_to(root)
    except ValueError as error:
        raise ValueError(f"Derived path escapes cache: {candidate}") from error
    return candidate


def _owner_entries(cache: Path, *, require_valid: bool = True) -> list[dict[str, Any]]:
    owner = cache / OWNER_MANIFEST
    if not owner.is_file():
        return []
    data = load_json(owner)
    entries = data.get("ownedFiles")
    if data.get("schemaVersion") != 2 or not isinstance(entries, list):
        raise ValueError("Invalid owner manifest")
    normalized = []
    for entry in entries:
        if not isinstance(entry, dict) or set(entry) != {"path", "size", "sha256"}:
            raise ValueError("Invalid owned file entry")
        path = cache_path(cache, entry["path"])
        if require_valid and not (
            path.is_file() and not path.is_symlink()
            and path.stat().st_size == entry["size"] and _sha256(path) == entry["sha256"]
        ):
            raise ValueError(f"Owned file is missing or changed: {path}")
        normalized.append(dict(entry))
    return normalized


def merge_owned_files(cache: Path, files: list[Path]) -> list[dict[str, Any]]:
    entries = {entry["path"]: entry for entry in _owner_entries(cache)}
    for path in files:
        if not path.is_file() or path.is_symlink():
            raise ValueError("Owner manifest accepts regular files only")
        relative = _relative_owned(cache, path)
        entries[relative] = {"path": relative, "size": path.stat().st_size, "sha256": _sha256(path)}
    return sorted(entries.values(), key=lambda item: item["path"])


def _write_owner_entries(cache: Path, entries: list[dict[str, Any]]) -> Path:
    cache.mkdir(parents=True, exist_ok=True)
    target = cache / OWNER_MANIFEST
    stage = target.with_suffix(".json.part")
    stage.write_text(json.dumps({"schemaVersion": 2, "ownedFiles": entries}, indent=2) + "\n", encoding="utf-8")
    os.replace(stage, target)
    return target


def required_free_bytes(
    runtime: dict[str, Any], model: dict[str, Any], *, runtime_cached: bool, model_cached: bool
) -> int:
    runtime_bytes = 0 if runtime_cached else runtime["size"] + MAXIMUM_EXPANDED_BYTES
    model_bytes = 0 if model_cached else model["size"] * 2
    return runtime_bytes + model_bytes + 1024**3


def require_disk(available: int, required: int) -> None:
    if available < required:
        raise ValueError(f"Insufficient free disk: require {required} bytes, have {available}")


def resolve_model(
    manifest: dict[str, Any], hardware: dict[str, Any], requested: str
) -> dict[str, Any]:
    model = recommend_model(manifest, hardware) if requested == "auto" else _model_by_id(manifest, requested)
    if model is None:
        raise ValueError("No managed model safely fits the current host; use deterministic fallback")
    if not hardware["runtimeCompatible"]:
        raise ValueError("Pinned runtime is incompatible with this host")
    if hardware["cpuCount"] < model["minimumCpuCount"]:
        raise ValueError(f"Model {model['id']} requires {model['minimumCpuCount']} usable CPUs")
    if hardware["effectiveRamGb"] < model["minimumRamGb"]:
        raise ValueError(
            f"Model {model['id']} requires {model['minimumRamGb']} GB effective RAM; "
            f"current safe value is {hardware['effectiveRamGb']} GB"
        )
    if hardware["freeDiskGb"] < model["minimumFreeDiskGb"]:
        raise ValueError(f"Model {model['id']} requires {model['minimumFreeDiskGb']} GB free disk")
    return model


def _progress(label: str):
    last_percent = -1

    def report(current: int, total: int) -> None:
        nonlocal last_percent
        percent = min(100, int(current * 100 / total)) if total else 0
        if percent == 100 or percent >= last_percent + 2:
            print(f"{label}: {current}/{total} bytes ({percent}%)", file=sys.stderr, flush=True)
            last_percent = percent

    return report


def provision(
    manifest_path: Path,
    cache: Path,
    requested_model: str,
    *,
    opener=urllib.request.urlopen,
) -> dict[str, Any]:
    """Provision the exact runtime and model into one locked PoC-owned cache."""
    manifest = load_json(manifest_path)
    validate_manifest(manifest)
    hardware = detect_hardware(cache, manifest)
    model = resolve_model(manifest, hardware, requested_model)
    runtime = select_runtime_asset(manifest, hardware["platform"])
    with CacheLock(cache):
        return _provision_locked(manifest, hardware, runtime, model, cache, opener)


def _provision_locked(  # noqa: MC0001  # One locked transaction owns preflight, mutation, ownership, and rollback.
    manifest: dict[str, Any],
    hardware: dict[str, Any],
    runtime: dict[str, Any],
    model: dict[str, Any],
    cache: Path,
    opener,
) -> dict[str, Any]:
    runtime_archive = cache_path(cache, "downloads", runtime["file"])
    runtime_root = cache_path(cache, "runtime", manifest["runtime"]["version"], runtime["platform"])
    model_path = cache_path(cache, "models", model["id"], model["file"])
    existing_entries = _owner_entries(cache)
    owned_paths = {cache_path(cache, entry["path"]) for entry in existing_entries}
    runtime_cached = runtime_root.exists()
    model_cached = model_path.exists()
    if runtime_cached:
        runtime_files = [path for path in runtime_root.rglob("*") if path.is_file()]
        if not runtime_files or any(path.resolve() not in owned_paths for path in runtime_files):
            raise ValueError("unowned or changed runtime cache; refusing reuse")
    if model_cached and model_path.resolve() not in owned_paths:
        raise ValueError("unowned or changed model cache; refusing reuse")
    for target in (runtime_archive, model_path):
        if target.exists() and target.resolve() not in owned_paths:
            raise ValueError(f"unowned target collision; refusing mutation: {target}")
    probe = cache
    while not probe.exists() and probe.parent != probe:
        probe = probe.parent
    require_disk(
        shutil.disk_usage(probe).free,
        required_free_bytes(runtime, model, runtime_cached=runtime_cached, model_cached=model_cached),
    )
    created_files: list[Path] = []
    created_directories: list[Path] = []

    def remember_missing_parents(target: Path) -> list[Path]:
        missing = []
        current = target.parent
        while current != cache.parent and not current.exists():
            missing.append(current)
            current = current.parent
        return missing

    def record_created_directories(candidates: list[Path]) -> None:
        created_directories.extend(path for path in candidates if path.is_dir() and path not in created_directories)

    try:
        if not runtime_cached:
            if opener is None:
                raise ValueError("Runtime is absent and no downloader is available")
            missing = remember_missing_parents(runtime_archive)
            try:
                download_verified(runtime, runtime_archive, opener=opener, progress=_progress("runtime"))
            finally:
                record_created_directories(missing)
            created_files.append(runtime_archive)
            missing = remember_missing_parents(runtime_root / "placeholder")
            try:
                extracted = safe_extract(runtime_archive, runtime_root)
            finally:
                record_created_directories(missing)
            record_created_directories(extracted["directories"])
            created_files.extend(extracted["files"])
            probe = cache
            require_disk(
                shutil.disk_usage(probe).free,
                required_free_bytes(runtime, model, runtime_cached=True, model_cached=model_cached),
            )
        executables = [path for path in runtime_root.rglob(runtime["executable"]) if path.is_file() and not path.is_symlink()]
        if len(executables) != 1:
            raise ValueError(f"Runtime archive has no unique {runtime['executable']}")
        executable = executables[0]
        if runtime_cached and executable.resolve() not in owned_paths:
            raise ValueError("unowned or changed runtime executable; refusing reuse")
        if os.name != "nt":
            executable.chmod(executable.stat().st_mode | stat.S_IXUSR)
        if not model_cached:
            if opener is None:
                raise ValueError("Model is absent and no downloader is available")
            missing = remember_missing_parents(model_path)
            try:
                download_verified(model, model_path, opener=opener, progress=_progress("model"))
            finally:
                record_created_directories(missing)
            created_files.append(model_path)
        runtime_owned_files = (
            [path for path in owned_paths if path.is_file() and runtime_root.resolve() in path.parents]
            if runtime_cached
            else extracted["files"]
        )
        all_new = [runtime_archive, model_path] + runtime_owned_files
        entries = merge_owned_files(cache, [path for path in all_new if path.is_file()])
        _write_owner_entries(cache, entries)
    except BaseException:
        for path in reversed(created_files):
            if path.is_file() and not path.is_symlink():
                path.unlink(missing_ok=True)
        for directory in sorted(set(created_directories), key=lambda item: len(item.parts), reverse=True):
            try:
                directory.rmdir()
            except OSError:
                # A nonempty or concurrently reused directory is deliberately preserved.
                pass
        raise

    return {
        "runtimeExecutable": str(executable),
        "runtimeVersion": manifest["runtime"]["version"],
        "modelPath": str(model_path),
        "modelId": model["id"],
        "hardware": hardware,
    }


def _available_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as listener:
        listener.bind(("127.0.0.1", 0))
        return int(listener.getsockname()[1])


def _process_tree(root_pid: int, parent_by_pid: dict[int, int]) -> set[int]:
    owned = {root_pid}
    changed = True
    while changed:
        changed = False
        for pid, parent in parent_by_pid.items():
            if pid not in owned and parent in owned:
                owned.add(pid)
                changed = True
    return owned


def _linux_process_tree_rss_bytes(root_pid: int) -> int:
    parent_by_pid: dict[int, int] = {}
    rss_by_pid: dict[int, int] = {}
    page_size = os.sysconf("SC_PAGE_SIZE")
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        try:
            stat_fields = (entry / "stat").read_text(encoding="utf-8").split(") ", 1)[1].split()
            parent_by_pid[int(entry.name)] = int(stat_fields[1])
            rss_pages = int((entry / "statm").read_text(encoding="utf-8").split()[1])
            rss_by_pid[int(entry.name)] = rss_pages * page_size
        except (OSError, IndexError, ValueError):
            continue
    if root_pid not in rss_by_pid:
        raise RuntimeError("Owned llama-server process disappeared during RSS inspection")
    return sum(rss_by_pid.get(pid, 0) for pid in _process_tree(root_pid, parent_by_pid))


def _windows_process_table() -> dict[int, int]:
    from ctypes import wintypes

    class ProcessEntry(ctypes.Structure):
        _fields_ = [
            ("dwSize", wintypes.DWORD), ("cntUsage", wintypes.DWORD),
            ("th32ProcessID", wintypes.DWORD), ("th32DefaultHeapID", ctypes.c_size_t),
            ("th32ModuleID", wintypes.DWORD), ("cntThreads", wintypes.DWORD),
            ("th32ParentProcessID", wintypes.DWORD), ("pcPriClassBase", wintypes.LONG),
            ("dwFlags", wintypes.DWORD), ("szExeFile", wintypes.WCHAR * 260),
        ]

    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    kernel32.CreateToolhelp32Snapshot.restype = wintypes.HANDLE
    snapshot = kernel32.CreateToolhelp32Snapshot(0x00000002, 0)
    if snapshot == ctypes.c_void_p(-1).value:
        raise ctypes.WinError(ctypes.get_last_error())
    try:
        entry = ProcessEntry()
        entry.dwSize = ctypes.sizeof(entry)
        processes: dict[int, int] = {}
        present = kernel32.Process32FirstW(snapshot, ctypes.byref(entry))
        while present:
            processes[int(entry.th32ProcessID)] = int(entry.th32ParentProcessID)
            present = kernel32.Process32NextW(snapshot, ctypes.byref(entry))
        return processes
    finally:
        kernel32.CloseHandle(snapshot)


def _windows_rss_bytes(pid: int) -> int:
    from ctypes import wintypes

    class ProcessMemoryCounters(ctypes.Structure):
        _fields_ = [
            ("cb", wintypes.DWORD), ("PageFaultCount", wintypes.DWORD),
            ("PeakWorkingSetSize", ctypes.c_size_t), ("WorkingSetSize", ctypes.c_size_t),
            ("QuotaPeakPagedPoolUsage", ctypes.c_size_t), ("QuotaPagedPoolUsage", ctypes.c_size_t),
            ("QuotaPeakNonPagedPoolUsage", ctypes.c_size_t), ("QuotaNonPagedPoolUsage", ctypes.c_size_t),
            ("PagefileUsage", ctypes.c_size_t), ("PeakPagefileUsage", ctypes.c_size_t),
        ]

    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    psapi = ctypes.WinDLL("psapi", use_last_error=True)
    kernel32.OpenProcess.restype = wintypes.HANDLE
    handle = kernel32.OpenProcess(0x0400 | 0x0010, False, pid)
    if not handle:
        raise ctypes.WinError(ctypes.get_last_error())
    try:
        counters = ProcessMemoryCounters()
        counters.cb = ctypes.sizeof(counters)
        if not psapi.GetProcessMemoryInfo(handle, ctypes.byref(counters), counters.cb):
            raise ctypes.WinError(ctypes.get_last_error())
        return int(counters.WorkingSetSize)
    finally:
        kernel32.CloseHandle(handle)


def _process_tree_rss_bytes(root_pid: int) -> int:
    system = platform.system()
    if system == "Windows":
        parents = _windows_process_table()
        owned = _process_tree(root_pid, parents)
        if root_pid not in parents:
            raise RuntimeError("Owned llama-server process disappeared during RSS inspection")
        total = 0
        for pid in owned:
            try:
                total += _windows_rss_bytes(pid)
            except OSError:
                if pid == root_pid:
                    raise
        return total
    if system == "Linux":
        return _linux_process_tree_rss_bytes(root_pid)
    ps_executable = shutil.which("ps")
    if ps_executable is None:
        raise RuntimeError("ps executable is required for process-tree RSS inspection")
    completed = subprocess.run(  # nosec B603 - fixed read-only host process inventory.
        [ps_executable, "-axo", "pid=,ppid=,rss="], check=True, capture_output=True, text=True, timeout=5,
    )
    parents: dict[int, int] = {}
    rss: dict[int, int] = {}
    for line in completed.stdout.splitlines():
        fields = line.split()
        if len(fields) == 3:
            pid, parent, kibibytes = map(int, fields)
            parents[pid] = parent
            rss[pid] = kibibytes * 1024
    if root_pid not in rss:
        raise RuntimeError("Owned llama-server process disappeared during RSS inspection")
    return sum(rss.get(pid, 0) for pid in _process_tree(root_pid, parents))


def _abort_process_tree(process: subprocess.Popen) -> None:
    pid = int(process.pid)
    if platform.system() == "Windows":
        from ctypes import wintypes
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        kernel32.OpenProcess.restype = wintypes.HANDLE
        pids = list(_process_tree(pid, _windows_process_table()))
        for owned_pid in sorted(pids, key=lambda value: value == pid):
            handle = kernel32.OpenProcess(0x0001, False, owned_pid)
            if handle:
                try:
                    kernel32.TerminateProcess(handle, 1)
                finally:
                    kernel32.CloseHandle(handle)
    else:
        import signal
        parents: dict[int, int] = {}
        if platform.system() == "Linux":
            for entry in Path("/proc").iterdir():
                if entry.name.isdigit():
                    try:
                        fields = (entry / "stat").read_text(encoding="utf-8").split(") ", 1)[1].split()
                        parents[int(entry.name)] = int(fields[1])
                    except (OSError, IndexError, ValueError):
                        # Processes may exit or expose an incomplete stat record while the tree is sampled.
                        continue
        for owned_pid in sorted(_process_tree(pid, parents), key=lambda value: value == pid):
            try:
                os.kill(owned_pid, signal.SIGKILL)
            except ProcessLookupError:
                continue
    if process.poll() is None:
        process.kill()


class ProcessTreeRssMonitor:
    """Continuously enforce and retain the owned runtime process-tree RSS peak."""

    def __init__(self, process: subprocess.Popen, *, sampler=_process_tree_rss_bytes,
                 aborter=_abort_process_tree, limit_bytes: int = MAX_PROCESS_TREE_RSS_BYTES):
        """Create a monitor for one owned process tree and an aggregate RSS ceiling."""
        self.process = process
        self.sampler = sampler
        self.aborter = aborter
        self.limit_bytes = limit_bytes
        self.peak_bytes = 0
        self.exceeded = False
        self.error: Exception | None = None
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None

    def poll_once(self) -> bool:
        if self.process.poll() is not None:
            return False
        current = self.sampler(int(self.process.pid))
        self.peak_bytes = max(self.peak_bytes, current)
        if current > self.limit_bytes:
            self.exceeded = True
            self.aborter(self.process)
            return True
        return False

    def start(self) -> None:
        self._thread = threading.Thread(target=self._run, name="shaft-local-ai-rss", daemon=True)
        self._thread.start()

    def _run(self) -> None:
        while not self._stop.is_set() and self.process.poll() is None:
            try:
                if self.poll_once():
                    return
            except Exception as error:
                self.error = error
                self.aborter(self.process)
                return
            self._stop.wait(0.05)

    def raise_if_failed(self) -> None:
        if self.exceeded:
            raise RuntimeError("Owned llama-server process tree exceeded the 4 GiB RSS benchmark limit")
        if self.error is not None:
            raise RuntimeError(f"Could not enforce process-tree RSS limit: {self.error}") from self.error

    def stop(self) -> None:
        self._stop.set()
        if self._thread is not None and self._thread is not threading.current_thread():
            self._thread.join(timeout=2)


def _json_request(
    url: str,
    payload: dict[str, Any] | None = None,
    timeout: float = 10,
    headers: dict[str, str] | None = None,
) -> dict[str, Any]:
    body = None if payload is None else json.dumps(payload).encode("utf-8")
    request_headers = {"Content-Type": "application/json"} | (headers or {})
    request = urllib.request.Request(
        url, data=body, headers=request_headers,
        method="GET" if body is None else "POST",
    )
    with urllib.request.urlopen(request, timeout=timeout) as response:  # nosec B310 - caller supplies fixed loopback URL only.
        value = json.loads(response.read().decode("utf-8"))
    if not isinstance(value, dict):
        raise ValueError("Local runtime returned a non-object response")
    return value


def _wait_for_identity(
    process: subprocess.Popen,
    port: int,
    api_key: str,
    alias: str,
    timeout: float = 120,
    *,
    requester=_json_request,
    sleeper=time.sleep,
) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise RuntimeError(f"llama-server exited before health check ({process.returncode})")
        try:
            response = requester(
                f"http://127.0.0.1:{port}/v1/models",
                timeout=2,
                headers={"Authorization": f"Bearer {api_key}"},
            )
            identifiers = {
                item.get("id") for item in response.get("data", []) if isinstance(item, dict)
            }
            if alias in identifiers:
                return
        except (OSError, ValueError, urllib.error.URLError):
            # Identity is polled until timeout because the local server may still be starting.
            pass
        sleeper(0.25)
    raise TimeoutError("llama-server identity was not established")


def _inference_client(port: int, model_id: str, api_key: str):
    endpoint = f"http://127.0.0.1:{port}/v1/chat/completions"

    def infer(prompt: str, schema: dict[str, Any]) -> dict[str, Any]:
        response = _json_request(
            endpoint,
            {
                "model": model_id,
                "messages": [{"role": "user", "content": prompt}],
                "temperature": 0,
                "max_tokens": 600,
                "chat_template_kwargs": {"enable_thinking": False},
                "response_format": {"type": "json_object", "schema": schema},
            },
            timeout=90,
            headers={"Authorization": f"Bearer {api_key}"},
        )
        if "error" in response:
            raise RuntimeError(str(response["error"]))
        content = response["choices"][0]["message"]["content"]
        value = json.loads(content)
        if not isinstance(value, dict):
            raise ValueError("Model content is not a JSON object")
        return value

    return infer


def _terminate(process: subprocess.Popen) -> None:
    if process.poll() is not None:
        return
    process.terminate()
    try:
        process.wait(timeout=10)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait(timeout=5)


def _termination_error(process: subprocess.Popen | None) -> Exception | None:
    if process is None:
        return None
    try:
        _terminate(process)
        return None
    except Exception as error:
        return error


def runtime_environment(source: dict[str, str] | None = None) -> dict[str, str]:
    """Pass only platform loader/temp essentials to the downloaded native runtime."""
    available = os.environ if source is None else source
    allowed = {
        "PATH", "SYSTEMROOT", "WINDIR", "TEMP", "TMP", "TMPDIR", "COMSPEC", "PATHEXT",
        "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH", "DYLD_FALLBACK_LIBRARY_PATH",
        "LANG", "LC_ALL", "LC_CTYPE", "TZ", "HOME", "USERPROFILE",
    }
    return {key: value for key, value in available.items() if key.upper() in allowed}


def warm_labels(case_count: int, repeats: int) -> list[bool]:
    if case_count < 1 or repeats < 1:
        raise ValueError("Warm label dimensions must be positive")
    return [False] + [True] * (case_count * repeats - 1)


def publish_result_run(
    cache: Path, model_id: str, result: dict[str, Any], markdown: str, log_text: str
) -> dict[str, Path]:
    if not _safe_basename(model_id):
        raise ValueError("Unsafe model result ID")
    results_root = cache_path(cache, "results")
    results_root.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=model_id + ".", suffix=".part", dir=results_root))
    final = cache_path(cache, "results", f"{model_id}-{int(time.time() * 1000)}-{secrets.token_hex(4)}")
    try:
        json_path = stage / "result.json"
        markdown_path = stage / "result.md"
        log_path = stage / "server.log"
        json_path.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
        # Round-trip before publication.
        load_json(json_path)
        markdown_path.write_text(markdown, encoding="utf-8")
        log_path.write_text(log_text, encoding="utf-8")
        os.replace(stage, final)
        paths = {"root": final, "json": final / "result.json", "markdown": final / "result.md", "log": final / "server.log"}
        try:
            _write_owner_entries(cache, merge_owned_files(cache, [paths["json"], paths["markdown"], paths["log"]]))
        except BaseException:
            shutil.rmtree(final, ignore_errors=True)
            raise
        return paths
    except BaseException:
        shutil.rmtree(stage, ignore_errors=True)
        raise


def _markdown_result(result: dict[str, Any]) -> str:
    aggregate = result["aggregate"]
    return (
        f"# Local AI PoC result: {result['modelId']}\n\n"
        f"- Runtime: llama.cpp {result['runtimeVersion']}\n"
        f"- Runs: {aggregate['runs']}\n"
        f"- Schema valid: {aggregate['schemaValidRate']:.0%}\n"
        f"- Citations valid: {aggregate['citationValidRate']:.0%}\n"
        f"- Category accuracy: {aggregate['categoryAccuracy']:.0%}\n"
        f"- Recommendation coverage: {aggregate['recommendationCoverage']:.0%}\n"
        f"- Unsafe actions: {aggregate['unsafeActionCount']}\n"
        f"- P95 warm latency: {aggregate['p95WarmLatencySeconds']} s\n"
        f"- Peak owned process-tree RSS: {aggregate['peakProcessTreeRssBytes']} bytes "
        f"(limit {aggregate['processTreeRssLimitBytes']} bytes)\n"
        f"- Passes all thresholds: {aggregate['passesThresholds']}\n"
    )


def benchmark(  # noqa: MC0001  # One lifecycle preserves primary errors and atomic evidence across all phases.
    manifest_path: Path,
    corpus_path: Path,
    cache: Path,
    requested_model: str,
    repeats: int,
) -> dict[str, Any]:
    if repeats < 5:
        raise ValueError("Benchmark requires at least five repeats per case")
    corpus = load_corpus(corpus_path)
    provisioned = None
    log_file = None
    log_path = None
    process = None
    rss_monitor = None
    peak_process_tree_rss_bytes = 0
    port = 0
    launch_error = None
    cleanup_errors: list[str] = []
    try:
        provisioned = provision(manifest_path, cache, requested_model)
        api_key = secrets.token_urlsafe(32)
        alias = f"shaft-poc-{secrets.token_hex(8)}"
        creation_flags = subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0
        log_root = cache_path(cache, "staging", "logs")
        log_root.mkdir(parents=True, exist_ok=True)
        log_file = tempfile.NamedTemporaryFile(
            mode="w+", encoding="utf-8", prefix="shaft-local-ai-", suffix=".log", delete=False, dir=log_root
        )
        log_path = Path(log_file.name)
        for _launch_attempt in range(3):
            port = _available_port()
            command = server_command(
                Path(provisioned["runtimeExecutable"]), Path(provisioned["modelPath"]),
                port=port, threads=max(1, min(8, provisioned["hardware"]["cpuCount"])),
                api_key=api_key, alias=alias,
            )
            process = subprocess.Popen(  # nosec B603 - pinned verified executable and list args.
                command, cwd=Path(provisioned["runtimeExecutable"]).parent,
                stdout=log_file, stderr=subprocess.STDOUT, text=True,
                env=runtime_environment(), creationflags=creation_flags,
            )
            if getattr(process, "pid", None) is not None:
                rss_monitor = ProcessTreeRssMonitor(process)
                rss_monitor.start()
            try:
                _wait_for_identity(process, port, api_key, alias)
                if rss_monitor is not None:
                    rss_monitor.raise_if_failed()
                launch_error = None
                break
            except (RuntimeError, TimeoutError) as error:
                if rss_monitor is not None:
                    rss_monitor.stop()
                    peak_process_tree_rss_bytes = max(
                        peak_process_tree_rss_bytes, rss_monitor.peak_bytes
                    )
                    rss_monitor.raise_if_failed()
                    rss_monitor = None
                launch_error = error
                cleanup_error = _termination_error(process)
                if cleanup_error is not None:
                    cleanup_errors.append(f"launch cleanup: {type(cleanup_error).__name__}: {cleanup_error}")
                process = None
        if process is None:
            raise RuntimeError(f"llama-server launch failed after 3 attempts: {launch_error}")
        try:
            infer = _inference_client(port, alias, api_key)
            runs = []
            warm = iter(warm_labels(len(corpus["cases"]), repeats))
            for case in corpus["cases"]:
                for repeat in range(repeats):
                    print(f"benchmark: {case['id']} repeat {repeat + 1}/{repeats}", file=sys.stderr, flush=True)
                    run = run_case(case, infer)
                    if rss_monitor is not None:
                        rss_monitor.raise_if_failed()
                    run["repeat"] = repeat + 1
                    run["warm"] = next(warm)
                    runs.append(run)
        except Exception:
            if rss_monitor is not None:
                rss_monitor.raise_if_failed()
            cleanup_error = _termination_error(process)
            process = None
            if rss_monitor is not None:
                rss_monitor.stop()
                peak_process_tree_rss_bytes = max(
                    peak_process_tree_rss_bytes, rss_monitor.peak_bytes
                )
                rss_monitor = None
            if cleanup_error is not None:
                cleanup_errors.append(f"run cleanup: {type(cleanup_error).__name__}: {cleanup_error}")
            raise
        else:
            if rss_monitor is not None:
                rss_monitor.raise_if_failed()
            cleanup_error = _termination_error(process)
            process = None
            if rss_monitor is not None:
                rss_monitor.stop()
                peak_process_tree_rss_bytes = max(
                    peak_process_tree_rss_bytes, rss_monitor.peak_bytes
                )
                rss_monitor = None
            if cleanup_error is not None:
                cleanup_errors.append(f"run cleanup: {type(cleanup_error).__name__}: {cleanup_error}")
                raise RuntimeError(f"llama-server termination failed: {cleanup_error}") from cleanup_error
        aggregate = aggregate_results(runs)
        aggregate["peakProcessTreeRssBytes"] = peak_process_tree_rss_bytes
        aggregate["processTreeRssLimitBytes"] = MAX_PROCESS_TREE_RSS_BYTES
        aggregate["passesThresholds"] = bool(
            aggregate["passesThresholds"]
            and peak_process_tree_rss_bytes <= MAX_PROCESS_TREE_RSS_BYTES
        )
        result = {
            "schemaVersion": 1,
            "createdAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "runtimeVersion": provisioned["runtimeVersion"],
            "modelId": provisioned["modelId"],
            "hardware": provisioned["hardware"],
            "repeatsPerCase": repeats,
            "runs": runs,
            "aggregate": aggregate,
        }
        log_file.flush()
        log_file.seek(0)
        with CacheLock(cache):
            publish_result_run(cache, provisioned["modelId"], result, _markdown_result(result), log_file.read())
        return result
    except Exception as error:
        try:
            log_text = ""
            if log_file is not None:
                log_file.flush()
                log_file.seek(0)
                log_text = log_file.read()
            model_id = provisioned["modelId"] if provisioned is not None else requested_model
            if not _safe_basename(model_id):
                model_id = "unresolved"
            failed = {
                "schemaVersion": 1,
                "status": "failed",
                "createdAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "runtimeVersion": provisioned["runtimeVersion"] if provisioned is not None else "unresolved",
                "modelId": model_id,
                "hardware": provisioned["hardware"] if provisioned is not None else None,
                "errorType": type(error).__name__,
                "error": str(error),
                "cleanupErrors": cleanup_errors,
                "peakProcessTreeRssBytes": peak_process_tree_rss_bytes,
                "processTreeRssLimitBytes": MAX_PROCESS_TREE_RSS_BYTES,
            }
            with CacheLock(cache):
                publish_result_run(
                    cache,
                    model_id,
                    failed,
                    f"# Failed local AI PoC run\n\n- Error: {type(error).__name__}: {error}\n",
                    log_text,
                )
        except Exception as publication_error:
            print(
                f"Could not preserve failed benchmark evidence: {publication_error}",
                file=sys.stderr,
            )
        raise
    finally:
        if rss_monitor is not None:
            rss_monitor.stop()
        if process is not None:
            cleanup_error = _termination_error(process)
            if cleanup_error is not None:
                print(f"Could not terminate llama-server: {cleanup_error}", file=sys.stderr)
        if log_file is not None:
            try:
                log_file.close()
            except Exception as cleanup_error:
                print(f"Could not close benchmark log: {cleanup_error}", file=sys.stderr)
        if log_path is not None:
            try:
                log_path.unlink(missing_ok=True)
            except Exception as cleanup_error:
                print(f"Could not remove temporary benchmark log: {cleanup_error}", file=sys.stderr)
                try:
                    with CacheLock(cache):
                        _write_owner_entries(cache, merge_owned_files(cache, [log_path]))
                except Exception as ownership_error:
                    print(f"Could not own retained benchmark log: {ownership_error}", file=sys.stderr)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS)
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE)
    subcommands = parser.add_subparsers(dest="command", required=True)
    subcommands.add_parser("inspect", help="validate inputs and report host/model eligibility")
    provision_parser = subcommands.add_parser("provision", help="download and verify runtime/model")
    provision_parser.add_argument("--model", default="auto")
    benchmark_parser = subcommands.add_parser("benchmark", help="run the fixed Doctor benchmark")
    benchmark_parser.add_argument("--model", default="auto")
    benchmark_parser.add_argument("--repeats", type=int, default=5)
    subcommands.add_parser("clean", help="remove only PoC-owned cached files")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        if args.command == "inspect":
            print(json.dumps(inspect(args.manifest, args.corpus, args.cache), indent=2))
            return 0
        if args.command == "provision":
            print(json.dumps(provision(args.manifest, args.cache, args.model), indent=2))
            return 0
        if args.command == "benchmark":
            print(json.dumps(benchmark(args.manifest, args.corpus, args.cache, args.model, args.repeats)["aggregate"], indent=2))
            return 0
        if args.command == "clean":
            clean_cache(args.cache)
            print(json.dumps({"cleaned": True, "cache": str(args.cache)}))
            return 0
    except (OSError, ValueError, RuntimeError, TimeoutError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
