#!/usr/bin/env python3
"""Fail-closed optional local OmniRoute transport primitives."""

from __future__ import annotations

import argparse
import contextlib
import hashlib
import json
import os
import re
import signal
import shutil
import stat
import subprocess  # nosec B404 - dispatches a fixed local launcher with argv.
import sys
import tempfile
import threading
import time
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable
from urllib.error import HTTPError, URLError
from urllib.request import HTTPRedirectHandler, ProxyHandler, Request, build_opener


DEFAULT_ENDPOINT = "http://127.0.0.1:20128/"
HEALTH_PATH = "api/health"
MAX_RESPONSE_BYTES = 64 * 1024
MAX_DIAGNOSTIC_BYTES = 16 * 1024
MAX_CONTINUITY_WRITERS = 4
MAX_DELEGATE_ARGUMENTS = 64
MAX_DELEGATE_ARGUMENT_BYTES = 16 * 1024
HTTP_TIMEOUT_SECONDS = 2
SCHEMA_VERSION = 1
READINESS = frozenset({
    "ABSENT", "UNHEALTHY", "UNAUTHENTICATED", "ROUTE_UNQUALIFIED", "READY", "RUNTIME_EXHAUSTED",
})
RUN_STATUSES = frozenset({"planned", "running", "stalled", "blocked", "review", "completed", "cancelled", "quarantined"})
_CAPABILITY_RANK = {"mechanical": 0, "default": 1, "most-intelligent": 2}
_RUN_ID = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,127}\Z")
_TARGET = re.compile(r"[a-z][a-z0-9-]{0,63}\Z")
_HEX = re.compile(r"[0-9a-f]{64}\Z")
_SAFE_RUNTIME_ENVIRONMENT = (
    ("HOME", "TEMP", "TMP") if os.name == "posix"
    else ("USERPROFILE", "SystemRoot", "TEMP", "TMP")
)
_GIT_EXECUTABLE = shutil.which("git")
_LOCAL_MONITORS: dict[int, Any] = {}
if _GIT_EXECUTABLE is not None:
    _GIT_EXECUTABLE = str(Path(_GIT_EXECUTABLE).resolve())


class OmniRootError(RuntimeError):
    """A required transport invariant did not hold."""


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, message, headers, newurl):  # noqa: ANN001
        raise OmniRootError("gateway redirect is forbidden")


def default_config_path() -> Path:
    return Path.home() / ".config" / "chaos-engine" / "omniroot.json"


def default_state_path() -> Path:
    """Keep runtime state outside repositories by default."""
    base = os.environ.get("XDG_STATE_HOME")
    root = Path(base) if base and Path(base).is_absolute() else Path.home() / ".local" / "state"
    return root / "chaos-engine" / "omniroot"


def _platform_preflight() -> None:
    if sys.platform != "linux" or os.name != "posix" or not Path(os.sep, "proc", "self", "stat").is_file():
        raise OmniRootError("durable process identity and tree termination are unsupported; use native fallback")


def _open(request: Request, *, timeout: int):
    # Fixed loopback health must never inherit ambient proxies or redirects.
    return build_opener(ProxyHandler({}), _NoRedirect()).open(request, timeout=timeout)


def _utc_now() -> datetime:
    return datetime.now(UTC)


def _parse_time(value: object) -> datetime | None:
    if not isinstance(value, str):
        return None
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None
    return parsed if parsed.tzinfo is not None else None


def _read_config(path: Path) -> dict[str, Any] | None:
    try:
        flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0) | getattr(os, "O_NOFOLLOW", 0)
        descriptor = os.open(path, flags)
        metadata = os.fstat(descriptor)
        if not stat.S_ISREG(metadata.st_mode) or metadata.st_size > MAX_RESPONSE_BYTES:
            os.close(descriptor)
            return None
        if os.name == "posix" and (metadata.st_uid != os.getuid() or stat.S_IMODE(metadata.st_mode) != 0o600):
            os.close(descriptor)
            return None
        with os.fdopen(descriptor, "r", encoding="utf-8") as handle:
            value = json.load(handle)
    except (OSError, UnicodeError, json.JSONDecodeError):
        return None
    return value if isinstance(value, dict) else None


def _launcher(config: dict[str, Any]) -> tuple[list[str], str, str] | None:
    """Return operator-owned launcher argv and credential mode without exposing it."""
    launcher = config.get("launcher")
    if launcher == "omniroute":
        return ["omniroute", "run"], "environment", "gateway"
    if not isinstance(launcher, dict):
        return None
    argv, mode = launcher.get("argv"), launcher.get("credentialMode")
    invocation_mode = launcher.get("invocationMode", "gateway")
    if not isinstance(argv, list) or not argv or not all(isinstance(item, str) and item and "\x00" not in item for item in argv):
        return None
    if mode not in {"environment", "launcher"} or invocation_mode not in {"gateway", "direct"}:
        return None
    return list(argv), mode, invocation_mode


def _resolved_executable(argv: list[str]) -> tuple[list[str], tuple[int, int, int, int, int, int, str]] | None:
    executable = Path(argv[0]) if os.path.sep in argv[0] else Path(shutil.which(argv[0]) or "")
    try:
        resolved = executable.resolve(strict=True)
        metadata = resolved.stat()
    except OSError:
        return None
    mode = metadata.st_mode
    if not resolved.is_file() or not bool(mode & stat.S_IXUSR) or bool(mode & (stat.S_IWGRP | stat.S_IWOTH)):
        return None
    if os.name == "posix" and metadata.st_uid != os.getuid():
        return None
    try:
        digest = hashlib.sha256(resolved.read_bytes()).hexdigest()
    except OSError:
        return None
    return [str(resolved), *argv[1:]], (
        metadata.st_dev, metadata.st_ino, metadata.st_uid, mode,
        metadata.st_size, metadata.st_mtime_ns, digest,
    )


def _same_executable(argv: list[str], identity: tuple[int, int, int, int, int, int, str]) -> bool:
    qualified = _resolved_executable(argv)
    return qualified is not None and qualified[0][0] == argv[0] and qualified[1] == identity


def _seal_launcher(argv: list[str], identity: tuple[int, int, int, int, int, int, str], state_dir: Path) -> tuple[list[str], tuple[int, int, int, int, int, int, str]]:
    """Copy a verified launcher into private immutable run state before exec."""
    source_flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0) | getattr(os, "O_NOFOLLOW", 0)
    source = os.open(argv[0], source_flags)
    try:
        metadata = os.fstat(source)
        data = b""
        while True:
            chunk = os.read(source, 64 * 1024)
            if not chunk:
                break
            data += chunk
    finally:
        os.close(source)
    observed = (metadata.st_dev, metadata.st_ino, metadata.st_uid, metadata.st_mode,
                metadata.st_size, metadata.st_mtime_ns, hashlib.sha256(data).hexdigest())
    if observed != identity:
        raise OmniRootError("qualified launcher changed before sealing")
    directory = _private_directory(state_dir / "launchers")
    sealed = directory / identity[-1]
    if not sealed.exists():
        descriptor, temporary = tempfile.mkstemp(prefix=".launcher.", dir=directory)
        try:
            os.write(descriptor, data)
            os.fchmod(descriptor, 0o500)
            os.fsync(descriptor)
            os.close(descriptor)
            descriptor = -1
            with contextlib.suppress(FileExistsError):
                os.link(temporary, sealed, follow_symlinks=False)
        finally:
            if descriptor >= 0:
                os.close(descriptor)
            with contextlib.suppress(OSError):
                os.unlink(temporary)
    qualified = _resolved_executable([str(sealed), *argv[1:]])
    if qualified is None or qualified[1][-1] != identity[-1]:
        raise OmniRootError("sealed launcher is invalid")
    return qualified


def _attestation_valid(config: dict[str, Any], build: object, now: datetime) -> bool:
    if config.get("schemaVersion") != SCHEMA_VERSION:
        return False
    if not isinstance(config.get("routeId"), str) or not config["routeId"].strip():
        return False
    launcher = _launcher(config)
    if launcher is None or _resolved_executable(launcher[0]) is None:
        return False
    attestation = config.get("attestation")
    if not isinstance(attestation, dict) or attestation.get("schemaVersion") != SCHEMA_VERSION:
        return False
    if attestation.get("serverBuild") != build:
        return False
    if not all(
        isinstance(attestation.get(key), str) and _HEX.fullmatch(attestation[key])
        for key in ("routePolicySha256", "endpointKeyIdentitySha256", "deniedProbeTargetSha256")
    ):
        return False
    verified = _parse_time(attestation.get("verifiedAt"))
    expires = _parse_time(attestation.get("expiresAt"))
    if verified is None or expires is None or verified > now or expires <= now:
        return False
    return all(attestation.get(key) is True for key in (
        "noCostConfirmed", "noPaidFallbackConfirmed", "privacyConfirmed", "termsConfirmed",
        "deniedProbeConfirmed", "deniedProbeTargetKnownExistingConfirmed",
    ))


def _health(opener: Callable[..., Any]) -> tuple[dict[str, Any] | None, str | None]:
    request = Request(DEFAULT_ENDPOINT + HEALTH_PATH, headers={"Accept": "application/json"})
    try:
        with opener(request, timeout=HTTP_TIMEOUT_SECONDS) as response:
            if getattr(response, "status", 200) != 200:
                return None, "unhealthy"
            raw = response.read(MAX_RESPONSE_BYTES + 1)
    except HTTPError as error:
        if error.code in {401, 403}:
            return None, "unauthenticated"
        if error.code in {402, 429}:
            return None, "exhausted"
        return None, "unhealthy"
    except (OSError, URLError):
        return None, "absent"
    except OmniRootError:
        return None, "unhealthy"
    if not isinstance(raw, bytes) or len(raw) > MAX_RESPONSE_BYTES:
        return None, "unhealthy"
    try:
        payload = json.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError):
        return None, "unhealthy"
    if not isinstance(payload, dict) or payload.get("status") not in {"ok", "healthy"}:
        return None, "unhealthy"
    build = payload.get("build") or payload.get("version")
    payload["build"] = build if isinstance(build, str) and build else "health-unreported"
    return payload, None


def probe(
    *,
    config_path: Path | None = None,
    opener: Callable[..., Any] = _open,
    environ: dict[str, str] | None = None,
    now: Callable[[], datetime] = _utc_now,
    config: dict[str, Any] | None = None,
) -> dict[str, str]:
    """Return a secret-free readiness result for the fixed loopback gateway."""
    payload, error = _health(opener)
    result = {"endpoint": DEFAULT_ENDPOINT, "state": "UNHEALTHY"}
    if error == "absent":
        return {**result, "state": "ABSENT"}
    if error == "unauthenticated":
        return {**result, "state": "UNAUTHENTICATED"}
    if error == "exhausted":
        return {**result, "state": "RUNTIME_EXHAUSTED"}
    if error is not None or payload is None:
        return result
    config = config if config is not None else _read_config(config_path or default_config_path())
    if config is None or not _attestation_valid(config, payload["build"], now()):
        return {**result, "state": "ROUTE_UNQUALIFIED"}
    launcher = _launcher(config)
    if launcher is None:
        return {**result, "state": "ROUTE_UNQUALIFIED"}
    resolved = _resolved_executable(launcher[0])
    if resolved is None:
        return {**result, "state": "ROUTE_UNQUALIFIED"}
    environment = os.environ if environ is None else environ
    if launcher[1] == "environment" and not environment.get("OMNIROUTE_API_KEY"):
        return {**result, "state": "UNAUTHENTICATED"}
    fingerprint = hashlib.sha256(json.dumps({
        "config": config, "serverBuild": payload["build"], "launcher": resolved[0],
    }, sort_keys=True).encode("utf-8")).hexdigest()
    return {**result, "state": "READY", "serverBuild": payload["build"], "qualificationFingerprint": fingerprint}


class QualificationCache:
    """Compatibility wrapper; volatile readiness is always freshly probed."""

    def __init__(self):
        """Create an empty volatile qualification cache."""
        self._fingerprint: str | None = None
        self._result: dict[str, str] | None = None

    def probe(self, **kwargs: Any) -> dict[str, str]:
        result = probe(**kwargs)
        self._result = dict(result)
        return result


def _reject_symlink_components(path: Path) -> None:
    current = Path(path.anchor) if path.anchor else Path()
    for part in path.parts[1:] if path.anchor else path.parts:
        current /= part
        try:
            metadata = current.lstat()
        except FileNotFoundError:
            continue
        if stat.S_ISLNK(metadata.st_mode):
            raise OmniRootError("state path must not contain symlinks")


def _private_directory(path: Path) -> Path:
    _reject_symlink_components(path)
    missing: list[Path] = []
    cursor = path
    while not cursor.exists():
        missing.append(cursor)
        cursor = cursor.parent
    for component in reversed(missing):
        component.mkdir(mode=0o700)
    _reject_symlink_components(path)
    for cursor in (path.parent, path):
        metadata = cursor.lstat()
        if stat.S_ISLNK(metadata.st_mode):
            raise OmniRootError("state path must not contain symlinks")
        if os.name == "posix" and (metadata.st_uid != os.getuid() or stat.S_IMODE(metadata.st_mode) & 0o077):
            raise OmniRootError("state directory must be owner-owned and private")
    return path


def _write_json(path: Path, value: dict[str, Any]) -> None:
    """Atomically write a private state file without following a symlink target."""
    if path.exists() and path.is_symlink():
        raise OmniRootError("state target must not be a symlink")
    _private_directory(path.parent)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            os.fchmod(handle.fileno(), 0o600)
            json.dump(value, handle, sort_keys=True, separators=(",", ":"))
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, path)
        path.chmod(0o600)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def _create_immutable_json(path: Path, value: dict[str, Any]) -> None:
    """Create one receipt once; never replace an existing terminal claim."""
    _private_directory(path.parent)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            os.fchmod(handle.fileno(), 0o400)
            json.dump(value, handle, sort_keys=True, separators=(",", ":"))
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        try:
            os.link(temporary, path, follow_symlinks=False)
        except FileExistsError as error:
            raise OmniRootError("terminal receipt already exists") from error
        directory = os.open(path.parent, os.O_RDONLY | getattr(os, "O_DIRECTORY", 0))
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    except Exception:
        raise
    finally:
        with contextlib.suppress(OSError):
            os.unlink(temporary)


def _load_json(path: Path) -> dict[str, Any]:
    try:
        _reject_symlink_components(path)
        metadata = path.stat()
        parents = (path.parent, path.parent.parent)
        if not path.is_file() or path.is_symlink() or metadata.st_size > MAX_RESPONSE_BYTES:
            raise OmniRootError("run state is missing or unsafe")
        if os.name == "posix" and (
            metadata.st_uid != os.getuid() or stat.S_IMODE(metadata.st_mode) not in {0o400, 0o600}
            or any(parent.stat().st_uid != os.getuid() or stat.S_IMODE(parent.stat().st_mode) & 0o077 for parent in parents)
        ):
            raise OmniRootError("run state is missing or unsafe")
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise OmniRootError("run state is invalid") from error
    if not isinstance(value, dict):
        raise OmniRootError("run state is invalid")
    return value


def _run_path(state_dir: Path, run_id: str) -> Path:
    if not _RUN_ID.fullmatch(run_id):
        raise OmniRootError("run id is invalid")
    return state_dir / "runs" / f"{run_id}.json"


def _receipt_path(state_dir: Path, run_id: str) -> Path:
    if not _RUN_ID.fullmatch(run_id):
        raise OmniRootError("run id is invalid")
    return state_dir / "receipts" / f"{run_id}.json"


def _relative_paths(paths: list[str]) -> list[str]:
    forbidden = {".git", ".env", "private", "secrets"}
    if not all(
        isinstance(path, str) and path and not Path(path).is_absolute()
        and ".." not in Path(path).parts and not (set(Path(path).parts) & forbidden)
        for path in paths
    ):
        raise OmniRootError("changed paths must be repository-relative")
    return sorted(set(paths))


def process_identity(pid: int) -> str | None:
    """Return a Linux process-start identity; unsupported hosts never kill."""
    try:
        return _linux_start_time(Path(os.sep, "proc", str(pid), "stat").read_text(encoding="utf-8"))
    except (OSError, UnicodeError):
        return None


def _linux_start_time(value: str) -> str | None:
    end = value.rfind(")")
    if end < 0:
        return None
    fields = value[end + 1:].split()
    return fields[19] if len(fields) > 19 else None


def _dispatch_environment(environ: dict[str, str], credential_mode: str) -> dict[str, str]:
    result = {"PATH": environ.get("PATH", os.defpath)}
    for name in _SAFE_RUNTIME_ENVIRONMENT:
        if name in environ:
            result[name] = environ[name]
    if credential_mode == "environment":
        key = environ.get("OMNIROUTE_API_KEY")
        if not key:
            raise OmniRootError("endpoint key is unavailable")
        result["OMNIROUTE_API_KEY"] = key
    return result


def _sha256(value: object) -> str:
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")).hexdigest()


def _valid_private_candidate(candidate: object) -> bool:
    return bool(
        isinstance(candidate, dict)
        and set(candidate) == {"identity", "sessionId", "capability", "target", "arguments", "resumption"}
        and candidate.get("capability") in _CAPABILITY_RANK
        and _TARGET.fullmatch(candidate.get("target", ""))
        and isinstance(candidate.get("arguments"), list)
        and len(candidate["arguments"]) <= MAX_DELEGATE_ARGUMENTS
        and all(isinstance(item, str) and "\x00" not in item for item in candidate["arguments"])
        and sum(len(item.encode("utf-8")) for item in candidate["arguments"])
        <= MAX_DELEGATE_ARGUMENT_BYTES
        and all(isinstance(candidate.get(name), str) and candidate[name]
                for name in ("identity", "sessionId"))
    )


def _resumption_hashes(value: object) -> dict[str, Any] | None:
    required = {"task", "authority", "checkpoint", "completedActions", "trackerUrl", "pullRequestUrl"}
    if not isinstance(value, dict) or set(value) != required:
        return None
    completed = value.get("completedActions")
    if (not all(isinstance(value.get(name), str) and value[name]
                for name in required - {"completedActions"})
            or not isinstance(completed, list) or len(set(completed)) != len(completed)
            or not all(isinstance(item, str) and item for item in completed)):
        return None
    return {
        "taskSha256": _sha256(value["task"]),
        "authoritySha256": _sha256(value["authority"]),
        "checkpointSha256": _sha256(value["checkpoint"]),
        "completedActionSha256s": sorted(_sha256(item) for item in completed),
        "trackerUrlSha256": _sha256(value["trackerUrl"]),
        "pullRequestUrlSha256": _sha256(value["pullRequestUrl"]),
    }


def _verified_resumption(candidate: dict[str, Any], continuity: dict[str, Any]) -> dict[str, Any]:
    hashes = _resumption_hashes(candidate.get("resumption"))
    expected = {name: continuity.get(name) for name in (
        "taskSha256", "authoritySha256", "checkpointSha256", "completedActionSha256s",
        "trackerUrlSha256", "pullRequestUrlSha256",
    )}
    if hashes != expected:
        raise OmniRootError("continuity resumption does not match frozen evidence")
    return candidate["resumption"]


def _resumption_secrets(raw: str) -> list[str]:
    if not raw:
        return []
    try:
        value = json.loads(raw)
    except json.JSONDecodeError:
        return [raw]
    secrets = [raw]
    if isinstance(value, dict):
        secrets.extend(item for item in value.values() if isinstance(item, str))
        secrets.extend(item for items in value.values() if isinstance(items, list)
                       for item in items if isinstance(item, str))
    return secrets


def _continuity_contract(value: dict[str, Any] | None) -> dict[str, Any] | None:
    """Validate opt-in failover state; persist identities and links as hashes only."""
    if value is None:
        return None
    required = {
        "requiredCapability", "maxAttempts", "retryableExitCodes", "backoffSeconds",
        "authoritySha256", "checkpointSha256", "completedActionSha256s",
        "trackerUrlSha256", "pullRequestUrlSha256", "alternates",
    }
    if not isinstance(value, dict) or set(value) != required:
        raise OmniRootError("continuity contract fields are invalid")
    capability = value.get("requiredCapability")
    attempts = value.get("maxAttempts")
    exits = value.get("retryableExitCodes")
    backoff = value.get("backoffSeconds")
    hashes = [value.get(name) for name in (
        "authoritySha256", "checkpointSha256", "trackerUrlSha256", "pullRequestUrlSha256",
    )]
    completed = value.get("completedActionSha256s")
    alternates = value.get("alternates")
    if (capability not in _CAPABILITY_RANK or not isinstance(attempts, int)
            or not 2 <= attempts <= MAX_CONTINUITY_WRITERS):
        raise OmniRootError("continuity bounds are invalid")
    if (not isinstance(exits, list) or not exits or len(set(exits)) != len(exits)
            or not all(isinstance(code, int) and (1 <= code <= 255 or -code in signal.valid_signals())
                       for code in exits)):
        raise OmniRootError("continuity retry exits are invalid")
    if not isinstance(backoff, int) or not 0 <= backoff <= 300:
        raise OmniRootError("continuity backoff is invalid")
    if not all(isinstance(item, str) and _HEX.fullmatch(item) for item in hashes):
        raise OmniRootError("continuity evidence hashes are invalid")
    if (not isinstance(completed, list) or len(set(completed)) != len(completed)
            or not all(isinstance(item, str) and _HEX.fullmatch(item) for item in completed)):
        raise OmniRootError("continuity completed actions are invalid")
    if (not isinstance(alternates, list) or not alternates
            or len(alternates) >= MAX_CONTINUITY_WRITERS):
        raise OmniRootError("continuity alternates are invalid")
    safe_alternates = []
    seen = set()
    task_sha256 = None
    for candidate in alternates:
        if not _valid_private_candidate(candidate):
            raise OmniRootError("continuity alternate is invalid")
        resumption_hashes = _resumption_hashes(candidate["resumption"])
        expected_hashes = {
            name: value[name] for name in (
                "authoritySha256", "checkpointSha256", "completedActionSha256s",
                "trackerUrlSha256", "pullRequestUrlSha256",
            )
        }
        if (resumption_hashes is None
                or {name: resumption_hashes[name] for name in expected_hashes} != expected_hashes
                or task_sha256 not in {None, resumption_hashes["taskSha256"]}):
            raise OmniRootError("continuity resumption does not match frozen evidence")
        task_sha256 = resumption_hashes["taskSha256"]
        identity = (_sha256(candidate["identity"]), _sha256(candidate["sessionId"]))
        if identity in seen:
            raise OmniRootError("continuity alternates must be unique")
        seen.add(identity)
        safe_alternates.append({
            "identitySha256": identity[0], "sessionSha256": identity[1],
            "capability": candidate["capability"],
        })
    return {
        "requiredCapability": capability, "maxAttempts": attempts,
        "retryableExitCodes": sorted(exits), "backoffSeconds": backoff,
        "taskSha256": task_sha256,
        "authoritySha256": value["authoritySha256"],
        "checkpointSha256": value["checkpointSha256"],
        "completedActionSha256s": sorted(completed),
        "trackerUrlSha256": value["trackerUrlSha256"],
        "pullRequestUrlSha256": value["pullRequestUrlSha256"],
        "alternates": safe_alternates, "attempt": 1, "state": "running",
        "participants": [],
    }


def _advance_continuity(
    manifest: dict[str, Any], *, exit_code: int, group_dead: bool,
    candidates: list[dict[str, str]], register: Callable[[str], None],
    launch: Callable[[dict[str, str]], dict[str, Any]],
    compensate: Callable[[str], None] | None = None,
    sleep: Callable[[float], None] = time.sleep,
    now: Callable[[], datetime] = _utc_now,
) -> dict[str, Any]:
    """Advance one in-memory failover attempt without persisting launch inputs."""
    continuity = manifest.get("continuity")
    if not isinstance(continuity, dict) or continuity.get("state") == "replacement_running":
        return manifest
    deadline = _parse_time(manifest.get("deadline"))
    if deadline is None or deadline <= now():
        manifest["status"] = "blocked"
        continuity["state"] = "blocked"
        continuity["reason"] = "overall deadline expired"
        return manifest
    if not group_dead:
        manifest["status"] = "quarantined"
        continuity["state"] = "quarantined"
        continuity["reason"] = "prior process group death cannot be proven"
        return manifest
    if exit_code not in continuity["retryableExitCodes"]:
        manifest["status"] = "blocked"
        continuity["state"] = "blocked"
        continuity["reason"] = "non-retryable exit"
        return manifest
    if continuity["attempt"] >= continuity["maxAttempts"]:
        manifest["status"] = "blocked"
        continuity["state"] = "breaker_open"
        continuity["reason"] = "continuity attempts exhausted"
        return manifest
    used = {item["sessionSha256"] for item in continuity["participants"]}
    required = _CAPABILITY_RANK[continuity["requiredCapability"]]
    candidate = next((item for item in candidates
                      if item.get("capability") in _CAPABILITY_RANK
                      and _CAPABILITY_RANK[item["capability"]] >= required
                      and _sha256(item.get("sessionId")) not in used), None)
    if candidate is None:
        manifest["status"] = "blocked"
        continuity["state"] = "blocked"
        continuity["reason"] = "no compatible continuity alternate"
        return manifest
    try:
        register(candidate["sessionId"])
    except Exception:
        manifest["status"] = "blocked"
        continuity["state"] = "blocked"
        continuity["reason"] = "replacement learning registration failed"
        return manifest
    participant = {
        "identitySha256": _sha256(candidate["identity"]),
        "sessionSha256": _sha256(candidate["sessionId"]),
        "capability": candidate["capability"],
    }
    continuity["participants"].append(participant)
    continuity["state"] = "failover_pending"
    sleep(continuity["backoffSeconds"])
    if deadline <= now():
        if compensate is not None:
            with contextlib.suppress(Exception):
                compensate(candidate["sessionId"])
        manifest["status"] = "blocked"
        continuity["state"] = "blocked"
        continuity["reason"] = "overall deadline expired"
        return manifest
    continuity["state"] = "half_open"
    try:
        process = launch(candidate)
    except Exception:
        if compensate is not None:
            with contextlib.suppress(Exception):
                compensate(candidate["sessionId"])
        continuity["attempt"] += 1
        continuity["state"] = "failover_pending"
        continuity["reason"] = "replacement launch failed"
        manifest["status"] = "stalled"
        return manifest
    if (not isinstance(process, dict) or not isinstance(process.get("pid"), int)
            or not isinstance(process.get("pgid"), int)
            or not isinstance(process.get("processIdentity"), str)):
        manifest["status"] = "quarantined"
        continuity["state"] = "quarantined"
        continuity["reason"] = "replacement process identity cannot be proven"
        return manifest
    continuity["attempt"] += 1
    continuity["state"] = "replacement_running"
    continuity.pop("reason", None)
    manifest["status"] = "running"
    manifest["delegate"] = {**manifest.get("delegate", {}), **participant}
    manifest["monitor"] = dict(process)
    return manifest


_SECRET_TEXT = re.compile(
    r'''(?ix)
    (authorization\s*:\s*(?:bearer|basic)\s+)([^\s]+)
    |("(?:api[_-]?key|token|secret|password)"\s*:\s*")([^"]+)
    |((?:--)?(?:api[_-]?key|token|secret|password)(?:\s*[=:]\s*|\s+))([^\s]+)
    '''
)


def _redact_diagnostic(value: bytes, *, secrets: list[str] | None = None) -> tuple[str, bool]:
    """Decode, redact, and cap one diagnostic stream."""
    text = value.decode("utf-8", errors="replace")
    text = _SECRET_TEXT.sub(
        lambda match: (match.group(1) or match.group(3) or match.group(5)) + "[REDACTED]",
        text,
    )
    for secret in sorted((item for item in (secrets or []) if item), key=len, reverse=True):
        text = text.replace(secret, "[REDACTED]")
    encoded = text.encode("utf-8")
    truncated = len(encoded) > MAX_DIAGNOSTIC_BYTES
    if truncated:
        text = encoded[:MAX_DIAGNOSTIC_BYTES].decode("utf-8", errors="ignore")
    return text, truncated


def _read_bounded(stream: Any) -> tuple[bytes, bool]:
    kept = bytearray()
    truncated = False
    while True:
        chunk = stream.read(4096)
        if not chunk:
            break
        if isinstance(chunk, str):
            chunk = chunk.encode("utf-8", errors="replace")
        remaining = MAX_DIAGNOSTIC_BYTES * 4 - len(kept)
        if remaining > 0:
            kept.extend(chunk[:remaining])
        if len(chunk) > remaining:
            truncated = True
    return bytes(kept), truncated


def _group_alive(pid: int) -> bool:
    try:
        os.killpg(pid, 0)
    except ProcessLookupError:
        return False
    except OSError:
        return True
    return True


def _terminate_group(process: Any) -> int:
    if os.name != "posix" or not hasattr(os, "killpg"):
        raise OmniRootError("durable process identity and tree termination are unsupported; use native fallback")
    with contextlib.suppress(ProcessLookupError):
        os.killpg(process.pid, signal.SIGTERM)
    try:
        code = process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        with contextlib.suppress(ProcessLookupError):
            os.killpg(process.pid, signal.SIGKILL)
        try:
            code = process.wait(timeout=5)
        except subprocess.TimeoutExpired as error:
            raise OmniRootError("process group survived SIGKILL; state must remain quarantined") from error
    if _group_alive(process.pid):
        with contextlib.suppress(ProcessLookupError):
            os.killpg(process.pid, signal.SIGKILL)
        for _ in range(50):
            if not _group_alive(process.pid):
                break
            time.sleep(0.1)
        else:
            raise OmniRootError("process group death could not be proven; state must remain quarantined")
    return int(code)


def _collect_diagnostics(process: Any, path: Path, *, timeout_seconds: float, secrets: list[str] | None = None) -> None:
    """Drain both pipes, enforce runtime bound, then persist only redacted caps."""
    streams: dict[str, tuple[bytes, bool]] = {}

    def drain(name: str) -> None:
        streams[name] = _read_bounded(getattr(process, name))

    readers = [threading.Thread(target=drain, args=(name,), daemon=True) for name in ("stdout", "stderr")]
    for reader in readers:
        reader.start()
    timed_out = False
    try:
        exit_code = process.wait(timeout=timeout_seconds)
    except subprocess.TimeoutExpired:
        timed_out = True
        exit_code = _terminate_group(process)
    for reader in readers:
        reader.join(timeout=5)
    output: dict[str, Any] = {"schemaVersion": SCHEMA_VERSION, "exitCode": exit_code, "timedOut": timed_out}
    for name in ("stdout", "stderr"):
        value, read_truncated = streams.get(name, (b"", True))
        redacted, redact_truncated = _redact_diagnostic(value, secrets=secrets)
        output[name] = redacted
        output[f"{name}Truncated"] = read_truncated or redact_truncated
    _write_json(path, output)


def _capture_command(arguments: list[str]) -> int:
    """Run one qualified launcher in a durable monitor subprocess."""
    if len(arguments) < 12 or arguments[10] != "--":
        raise OmniRootError("capture arguments are invalid")
    try:
        timeout_seconds = int(arguments[2])
        expected_identity = (*tuple(int(value) for value in arguments[3:9]), arguments[9])
    except ValueError as error:
        raise OmniRootError("capture timeout is invalid") from error
    if not 1 <= timeout_seconds <= 86400:
        raise OmniRootError("capture timeout is invalid")
    command = arguments[11:]
    if not command or not all(value and "\x00" not in value for value in command):
        raise OmniRootError("capture command is invalid")
    if os.name != "posix":
        raise OmniRootError("bounded process-tree timeout is unsupported on this host")
    qualified = _resolved_executable(command)
    if qualified is None:
        raise OmniRootError("qualified launcher changed before execution")
    command, identity = qualified
    if identity != expected_identity or not _same_executable(command, identity):
        raise OmniRootError("qualified launcher changed before execution")
    try:
        process = subprocess.Popen(  # nosec B603 - argv was qualified before monitor launch.
            command, shell=False, close_fds=True, start_new_session=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
    except OSError as error:
        raise OmniRootError("qualified launcher could not start") from error
    identity = process_identity(process.pid)
    if identity is None:
        _terminate_group(process)
        raise OmniRootError("delegate process identity cannot be proven")
    _write_json(Path(arguments[1]), {
        "schemaVersion": SCHEMA_VERSION, "pid": process.pid, "pgid": os.getpgid(process.pid),
        "processIdentity": identity,
    })
    def forward(signum: int, _frame: Any) -> None:
        with contextlib.suppress(OSError):
            os.killpg(process.pid, signum)

    if os.name == "posix":
        signal.signal(signal.SIGTERM, forward)
        signal.signal(signal.SIGINT, forward)
    _collect_diagnostics(
        process, Path(arguments[0]), timeout_seconds=timeout_seconds,
        secrets=[os.environ.get("OMNIROUTE_API_KEY", "")],
    )
    process.stdout.close()
    process.stderr.close()
    return int(process.returncode or 0)


def _supervisor_arguments(arguments: list[str]) -> tuple[Path, Path, Path, int, tuple[Any, ...], list[str]]:
    if len(arguments) < 13 or arguments[11] != "--":
        raise OmniRootError("supervisor arguments are invalid")
    diagnostic_path, process_path, manifest_path = map(Path, arguments[:3])
    try:
        timeout_seconds = int(arguments[3])
        expected_identity = (*tuple(int(value) for value in arguments[4:10]), arguments[10])
    except ValueError as error:
        raise OmniRootError("supervisor timeout is invalid") from error
    command = arguments[12:]
    if not 1 <= timeout_seconds <= 86400 or not command:
        raise OmniRootError("supervisor arguments are invalid")
    return diagnostic_path, process_path, manifest_path, timeout_seconds, expected_identity, command


def _supervisor_environment(command: list[str]) -> tuple[list[dict[str, str]], str, str, str, str, int]:
    raw = os.environ.pop("OMNIROOT_CONTINUITY", "")
    learning_state = os.environ.pop("OMNIROOT_LEARNING_STATE", "")
    learning_root = os.environ.pop("OMNIROOT_LEARNING_ROOT", "")
    active_session = os.environ.pop("OMNIROOT_INITIAL_SESSION", "")
    invocation_mode = os.environ.pop("OMNIROOT_INVOCATION_MODE", "")
    credential_mode = os.environ.pop("OMNIROOT_CREDENTIAL_MODE", "")
    raw_launcher_argc = os.environ.pop("OMNIROOT_LAUNCHER_ARGC", "")
    try:
        candidates = json.loads(raw)
    except json.JSONDecodeError as error:
        raise OmniRootError("supervisor continuity input is invalid") from error
    try:
        launcher_argc = int(raw_launcher_argc)
    except ValueError as error:
        raise OmniRootError("supervisor continuity input is invalid") from error
    if (not isinstance(candidates, list) or invocation_mode not in {"direct", "gateway"}
            or credential_mode not in {"environment", "launcher"}
            or not 1 <= launcher_argc <= len(command)):
        raise OmniRootError("supervisor continuity input is invalid")
    if (not candidates or len(candidates) >= MAX_CONTINUITY_WRITERS
            or not all(_valid_private_candidate(candidate) for candidate in candidates)):
        raise OmniRootError("supervisor continuity input is invalid")
    return candidates, learning_state, learning_root, active_session, invocation_mode, launcher_argc


def _supervisor_manifest(path: Path) -> dict[str, Any]:
    for _ in range(100):
        manifest = _load_json(path)
        if isinstance(manifest.get("continuity"), dict):
            return manifest
        time.sleep(0.05)
    raise OmniRootError("supervisor manifest was not published")


def _finish_supervision(path: Path, manifest: dict[str, Any], status: str, state: str, reason: str) -> int:
    manifest["status"] = status
    manifest["continuity"]["state"] = state
    manifest["continuity"]["reason"] = reason
    _write_json(path, manifest)
    return 1


def _replacement_command(
    launcher_command: list[str], candidate: dict[str, str], invocation_mode: str, credential_mode: str,
) -> list[str]:
    if invocation_mode == "direct":
        return [*launcher_command, *candidate["arguments"]]
    command = [*launcher_command, candidate["target"], "--port", "20128"]
    if credential_mode == "environment":
        command.extend(["--api-key-env", "OMNIROUTE_API_KEY"])
    command.extend(["--", *candidate["arguments"]])
    return command


def _learning_action(state: str, root: str, session_id: str, *, unavailable: bool) -> None:
    source_root = str(Path(__file__).resolve().parents[4])
    if source_root not in sys.path:
        sys.path.insert(0, source_root)
    if unavailable:
        from scripts.agents.learning_session import attest_participant_unavailable
        attest_participant_unavailable(Path(state), root, session_id, "launch-failure")
    else:
        from scripts.agents.learning_session import register_runtime_participant
        register_runtime_participant(Path(state), root, session_id)


def _supervised_diagnostic(
    command: list[str], *, diagnostic_path: Path, process_path: Path, manifest_path: Path,
    manifest: dict[str, Any], timeout_seconds: int, active_session: str,
    learning_state: str, learning_root: str,
) -> tuple[dict[str, Any] | None, int | None]:
    deadline = _parse_time(manifest.get("deadline"))
    if deadline is None or deadline <= _utc_now():
        return None, _finish_supervision(
            manifest_path, manifest, "blocked", "blocked", "overall deadline expired",
        )
    resumption_secret = os.environ.get("OMNIROOT_RESUMPTION", "")
    try:
        process = subprocess.Popen(  # nosec B603 - sealed command identity is verified.
            command, shell=False, close_fds=True, start_new_session=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
    except OSError:
        os.environ.pop("OMNIROOT_RESUMPTION", None)
        with contextlib.suppress(Exception):
            _learning_action(learning_state, learning_root, active_session, unavailable=True)
        return None, _finish_supervision(
            manifest_path, manifest, "blocked", "blocked", "replacement launch failed",
        )
    os.environ.pop("OMNIROOT_RESUMPTION", None)
    identity = process_identity(process.pid)
    if identity is None:
        _terminate_group(process)
        return None, _finish_supervision(
            manifest_path, manifest, "quarantined", "quarantined",
            "replacement process identity cannot be proven",
        )
    process_evidence = {
        "schemaVersion": SCHEMA_VERSION, "pid": process.pid,
        "pgid": os.getpgid(process.pid), "processIdentity": identity,
    }
    _write_json(process_path, process_evidence)
    remaining_seconds = (deadline - _utc_now()).total_seconds()
    if remaining_seconds <= 0:
        _terminate_group(process)
        return None, _finish_supervision(
            manifest_path, manifest, "blocked", "blocked", "overall deadline expired",
        )
    _collect_diagnostics(
        process, diagnostic_path, timeout_seconds=min(timeout_seconds, remaining_seconds),
        secrets=[os.environ.get("OMNIROUTE_API_KEY", ""), *_resumption_secrets(resumption_secret)],
    )
    process.stdout.close()
    process.stderr.close()
    diagnostic = _validated_diagnostic(_load_json(diagnostic_path))
    if _group_alive(process_evidence["pgid"]):
        try:
            _terminate_group(process)
        except OmniRootError:
            return None, _finish_supervision(
                manifest_path, manifest, "quarantined", "quarantined",
                "prior process group death cannot be proven",
            )
    if diagnostic["exitCode"] == 0:
        manifest["continuity"]["state"] = "review"
        manifest["status"] = "running"
        _write_json(manifest_path, manifest)
        return None, 0
    return diagnostic, None


def _supervise_command(arguments: list[str]) -> int:
    """Outlive delegates and replace retryable failures without owner input."""
    (diagnostic_path, process_path, manifest_path, timeout_seconds,
     expected_identity, command) = _supervisor_arguments(arguments)
    credential_mode = os.environ.get("OMNIROOT_CREDENTIAL_MODE", "")
    (candidates, learning_state, learning_root, active_session,
     invocation_mode, launcher_argc) = _supervisor_environment(command)
    qualified = _resolved_executable(command)
    if qualified is None or qualified[1] != expected_identity:
        raise OmniRootError("qualified launcher changed before execution")
    command = qualified[0]
    launcher_command = command[:launcher_argc]
    manifest = _supervisor_manifest(manifest_path)

    while True:
        diagnostic, result = _supervised_diagnostic(
            command, diagnostic_path=diagnostic_path, process_path=process_path,
            manifest_path=manifest_path, manifest=manifest, timeout_seconds=timeout_seconds,
            active_session=active_session, learning_state=learning_state, learning_root=learning_root,
        )
        if result is not None:
            return result

        launched: dict[str, Any] = {}
        def register(session_id: str) -> None:
            _learning_action(learning_state, learning_root, session_id, unavailable=False)

        def launch(candidate: dict[str, str]) -> dict[str, Any]:
            launched["candidate"] = candidate
            launched["resumption"] = _verified_resumption(candidate, manifest["continuity"])
            return {"pid": -1, "pgid": -1, "processIdentity": "pending"}

        def compensate(session_id: str) -> None:
            _learning_action(learning_state, learning_root, session_id, unavailable=True)

        supervisor_monitor = manifest.get("monitor")
        manifest = _advance_continuity(
            manifest, exit_code=diagnostic["exitCode"], group_dead=True,
            candidates=candidates, register=register, launch=launch, compensate=compensate,
        )
        if manifest.get("status") != "running" or not launched:
            _write_json(manifest_path, manifest)
            return diagnostic["exitCode"]
        manifest["monitor"] = supervisor_monitor
        manifest["continuity"]["state"] = "replacement_running"
        _write_json(manifest_path, manifest)
        active_session = launched["candidate"]["sessionId"]
        candidate = launched["candidate"]
        os.environ["OMNIROOT_RESUMPTION"] = json.dumps(
            launched["resumption"], sort_keys=True, separators=(",", ":"),
        )
        command = _replacement_command(launcher_command, candidate, invocation_mode, credential_mode)


def _validated_diagnostic(value: dict[str, Any]) -> dict[str, Any]:
    if (value.get("schemaVersion") != SCHEMA_VERSION
            or not isinstance(value.get("exitCode"), int)
            or not isinstance(value.get("timedOut"), bool)
            or not all(isinstance(value.get(name), str) for name in ("stdout", "stderr"))
            or not all(isinstance(value.get(name), bool)
                       for name in ("stdoutTruncated", "stderrTruncated"))):
        raise OmniRootError("terminal diagnostic is invalid")
    return value


def _git(worktree: Path, *args: str) -> str:
    if _GIT_EXECUTABLE is None:
        raise OmniRootError("a real clean git worktree is required")
    try:
        completed = subprocess.run(
            [_GIT_EXECUTABLE, "-C", str(worktree), *args],  # nosec B603 - fixed executable and controlled argv.
            check=True, capture_output=True, text=True,
        )
    except (OSError, subprocess.CalledProcessError) as error:
        raise OmniRootError("a real clean git worktree is required") from error
    return completed.stdout.strip()


def _validate_worktree(worktree: Path) -> tuple[Path, str]:
    try:
        resolved = worktree.resolve(strict=True)
    except OSError as error:
        raise OmniRootError("existing isolated worktree is required") from error
    if not resolved.is_dir() or _git(resolved, "rev-parse", "--is-inside-work-tree") != "true":
        raise OmniRootError("a real clean git worktree is required")
    git_dir = Path(_git(resolved, "rev-parse", "--path-format=absolute", "--git-dir"))
    common_dir = Path(_git(resolved, "rev-parse", "--path-format=absolute", "--git-common-dir"))
    if git_dir == common_dir:
        raise OmniRootError("a linked isolated git worktree is required")
    if _git(resolved, "status", "--porcelain", "--untracked-files=all"):
        raise OmniRootError("source worktree must be clean, including untracked files")
    return resolved, _git(resolved, "rev-parse", "HEAD")


def _repository_identity(worktree: Path) -> Path:
    return Path(_git(worktree, "rev-parse", "--path-format=absolute", "--git-common-dir")).resolve()


def _delegate_contract(delegate: dict[str, Any] | None, worktree: Path, target: str, command: list[str], environment: dict[str, str]) -> dict[str, Any]:
    candidate = {} if delegate is None else dict(delegate)
    ownership = _relative_paths(candidate.get("pathOwnership", []))
    if not isinstance(candidate.get("pathOwnership", []), list):
        raise OmniRootError("delegate ownership is invalid")
    values = {
        "identity": candidate.get("identity", "anonymous"), "role": candidate.get("role", "implementer"),
        "capability": candidate.get("capability", "default"), "transport": candidate.get("transport", "omniroute"),
    }
    if not all(isinstance(value, str) and value for value in values.values()):
        raise OmniRootError("delegate identity is invalid")
    assignment = candidate.get("assignment", "")
    if not isinstance(assignment, str):
        raise OmniRootError("delegate assignment is invalid")
    return {
        **values, "assignmentSha256": _sha256(assignment), "pathOwnership": ownership,
        "worktree": str(worktree), "commandSha256": _sha256(command),
        "environmentSha256": _sha256(sorted(environment)), "targetSha256": _sha256(target),
    }


def _overlaps_owned_paths(state_dir: Path, ownership: list[str]) -> bool:
    runs = state_dir / "runs"
    if not runs.is_dir():
        return False
    for path in runs.glob("*.json"):
        manifest = _load_json(path)
        if manifest.get("status") not in {"planned", "running", "stalled", "blocked", "review"}:
            continue
        existing = manifest.get("delegate", {}).get("pathOwnership", [])
        if ownership and isinstance(existing, list):
            for requested in map(Path, ownership):
                for held in map(Path, existing):
                    if requested == held or requested in held.parents or held in requested.parents:
                        return True
    return False


@contextlib.contextmanager
def _reservation(state_dir: Path):
    runs = _private_directory(state_dir / "runs")
    lock = runs / ".reservation.lock"
    try:
        descriptor = os.open(lock, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError as error:
        raise OmniRootError("another dispatch reservation is active") from error
    try:
        os.close(descriptor)
        yield
    finally:
        with contextlib.suppress(OSError):
            lock.unlink()


def dispatch(  # noqa: MC0001 - fail-closed dispatch keeps invariant checks in one auditable boundary.
    *,
    run_id: str,
    worktree: Path,
    state_dir: Path,
    config_path: Path | None = None,
    target: str,
    delegate_args: list[str],
    opener: Callable[..., Any] = _open,
    environ: dict[str, str] | None = None,
    popen: Callable[..., Any] = subprocess.Popen,
    process_identity: Callable[[int], str | None] = process_identity,
    task_id: str | None = None,
    workflow: str | None = None,
    root_session_id: str | None = None,
    base_commit: str | None = None,
    integration_branch: str | None = None,
    integration_worktree: Path | None = None,
    delegate: dict[str, Any] | None = None,
    cadence_seconds: int = 600,
    deadline: str | None = None,
    timeout_seconds: int = 3600,
    learning_state: Path | None = None,
    learning_root_session_id: str | None = None,
    delegate_session_id: str | None = None,
    continuity: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Launch one bounded implementer only after a fresh strict readiness check."""
    _platform_preflight()
    if not _TARGET.fullmatch(target) or not all(isinstance(value, str) and "\x00" not in value for value in delegate_args):
        raise OmniRootError("target or delegated arguments are invalid")
    if workflow not in {"ORCHESTRATOR + SINGLE IMPLEMENTER", "ORCHESTRATOR + PARALLEL IMPLEMENTERS"}:
        raise OmniRootError("dispatch requires an orchestrated workflow")
    if not all(isinstance(value, str) and value and "unspecified" not in value
               for value in (task_id, root_session_id, integration_branch)):
        raise OmniRootError("runtime identity is invalid")
    if not isinstance(cadence_seconds, int) or cadence_seconds not in {300, 600, 900}:
        raise OmniRootError("cadence must be 300, 600, or 900 seconds")
    if not isinstance(timeout_seconds, int) or not 1 <= timeout_seconds <= 86400:
        raise OmniRootError("timeout must be between 1 and 86400 seconds")
    worktree, actual_head = _validate_worktree(Path(worktree))
    if integration_worktree is None:
        raise OmniRootError("integration worktree is required")
    integration = Path(integration_worktree).resolve(strict=True)
    _validate_worktree(integration)
    if integration == worktree:
        raise OmniRootError("delegate and integration worktrees must be distinct")
    if _repository_identity(worktree) != _repository_identity(integration):
        raise OmniRootError("delegate and integration worktrees must belong to the same repository")
    if _git(integration, "branch", "--show-current") != integration_branch:
        raise OmniRootError("integration worktree branch does not match the declared branch")
    frozen_base = base_commit
    if not isinstance(frozen_base, str) or not re.fullmatch(r"[0-9a-f]{40}", frozen_base):
        raise OmniRootError("base commit must be a full git object id")
    _git(worktree, "cat-file", "-e", f"{frozen_base}^{{commit}}")
    _git(worktree, "merge-base", "--is-ancestor", frozen_base, actual_head)
    parsed_deadline = _parse_time(deadline)
    if parsed_deadline is None or parsed_deadline <= _utc_now():
        raise OmniRootError("a future timezone-aware deadline is required")
    if not isinstance(delegate, dict) or not delegate.get("pathOwnership"):
        raise OmniRootError("explicit non-empty delegate ownership is required")
    continuity_manifest = _continuity_contract(continuity)
    if (continuity_manifest is not None
            and (delegate.get("capability") not in _CAPABILITY_RANK
                 or _CAPABILITY_RANK[delegate["capability"]]
                 < _CAPABILITY_RANK[continuity_manifest["requiredCapability"]])):
        raise OmniRootError("initial delegate does not satisfy continuity capability floor")
    environment = os.environ.copy() if environ is None else dict(environ)
    config = _read_config(config_path or default_config_path())
    if config is None:
        raise OmniRootError("OmniRoute is not ready: ROUTE_UNQUALIFIED")
    readiness = probe(config_path=config_path, opener=opener, environ=environment, config=config)
    if readiness["state"] != "READY":
        raise OmniRootError(f"OmniRoute is not ready: {readiness['state']}")
    path = _run_path(Path(state_dir), run_id)
    state_resolved = Path(state_dir).resolve()
    for managed in (worktree, integration):
        if state_resolved == managed or managed in state_resolved.parents:
            raise OmniRootError("state directory must be outside managed worktrees")
    if path.exists():
        raise OmniRootError("run id already exists")
    launcher = _launcher(config)
    if launcher is None:
        raise OmniRootError("launcher is unqualified")
    launcher_argv, credential_mode, invocation_mode = launcher
    qualified = _resolved_executable(launcher_argv)
    if qualified is None:
        raise OmniRootError("launcher is unqualified")
    launcher_argv, launcher_identity = qualified
    launcher_argv, launcher_identity = _seal_launcher(launcher_argv, launcher_identity, Path(state_dir))
    if invocation_mode == "direct":
        argv = [*launcher_argv, *delegate_args]
    else:
        argv = [*launcher_argv, target, "--port", "20128"]
        if credential_mode == "environment":
            argv.extend(["--api-key-env", "OMNIROUTE_API_KEY"])
        argv.extend(["--", *delegate_args])
    dispatch_environment = _dispatch_environment(environment, credential_mode)
    delegate_manifest = _delegate_contract(delegate, worktree, target, argv, dispatch_environment)
    with _reservation(Path(state_dir)):
        if path.exists():
            raise OmniRootError("run id already exists")
        if _overlaps_owned_paths(Path(state_dir), delegate_manifest["pathOwnership"]):
            raise OmniRootError("delegate ownership overlaps a live run")
        runs_dir = Path(state_dir) / "runs"
        for existing_path in runs_dir.glob("*.json"):
            existing = _load_json(existing_path)
            if existing.get("status") in {"planned", "running", "stalled", "blocked", "review"} \
                    and existing.get("delegate", {}).get("worktree") == str(worktree):
                raise OmniRootError("each live run requires a unique delegate worktree")
        _write_json(path, {
            "schemaVersion": SCHEMA_VERSION, "runId": run_id, "status": "planned",
            "delegate": delegate_manifest,
        })
    diagnostic_path = Path(state_dir) / "diagnostics" / f"{run_id}.json"
    delegate_process_path = Path(state_dir) / "processes" / f"{run_id}.json"
    durable_monitor = popen is subprocess.Popen
    if durable_monitor and os.name != "posix":
        with contextlib.suppress(OSError):
            path.unlink()
        raise OmniRootError("bounded process-tree timeout is unsupported on this host")
    if durable_monitor and continuity_manifest is not None:
        launched_argv = [
            sys.executable, str(Path(__file__).resolve()), "_supervise", str(diagnostic_path),
            str(delegate_process_path), str(path), str(timeout_seconds),
            *(str(value) for value in launcher_identity), "--", *argv,
        ]
    elif durable_monitor:
        launched_argv = [
            sys.executable, str(Path(__file__).resolve()), "_capture", str(diagnostic_path),
            str(delegate_process_path), str(timeout_seconds), *(str(value) for value in launcher_identity), "--", *argv,
        ]
    else:
        launched_argv = argv
    launch_environment = dict(dispatch_environment)
    if durable_monitor and continuity_manifest is not None:
        launch_environment.update({
            "OMNIROOT_CONTINUITY": json.dumps(continuity["alternates"], separators=(",", ":")),
            "OMNIROOT_LEARNING_STATE": str(learning_state),
            "OMNIROOT_LEARNING_ROOT": str(learning_root_session_id),
            "OMNIROOT_INITIAL_SESSION": str(delegate_session_id),
            "OMNIROOT_INVOCATION_MODE": invocation_mode,
            "OMNIROOT_CREDENTIAL_MODE": credential_mode,
            "OMNIROOT_LAUNCHER_ARGC": str(len(launcher_argv)),
        })
    registered = False
    try:
        if not _same_executable(launcher_argv, launcher_identity):
            raise OmniRootError("qualified launcher changed before execution")
        if learning_state is None or not all(isinstance(value, str) and value for value in (
                learning_root_session_id, delegate_session_id)):
            raise OmniRootError("learning runtime registration is required")
        try:
            from scripts.agents.learning_session import register_runtime_participant
            register_runtime_participant(Path(learning_state), learning_root_session_id, delegate_session_id)
            registered = True
        except Exception as registration_error:
            raise OmniRootError("delegate learning registration failed") from registration_error
        process = popen(
            launched_argv, cwd=str(worktree), env=launch_environment, shell=False,
            close_fds=True, start_new_session=True,
            stdout=subprocess.DEVNULL if durable_monitor else subprocess.PIPE,
            stderr=subprocess.DEVNULL if durable_monitor else subprocess.PIPE,
        )
    except (OSError, OmniRootError) as error:
        with contextlib.suppress(OSError):
            path.unlink()
        if registered:
            with contextlib.suppress(Exception):
                from scripts.agents.learning_session import attest_participant_unavailable
                attest_participant_unavailable(
                    Path(learning_state), learning_root_session_id, delegate_session_id, "launch-failure"
                )
        if isinstance(error, OmniRootError):
            raise
        raise OmniRootError("OmniRoute launcher could not start") from error
    pid = getattr(process, "pid", None)
    if not isinstance(pid, int) or pid <= 1:
        with contextlib.suppress(OSError):
            path.unlink()
        raise OmniRootError("launcher did not expose a safe process id")
    identity = process_identity(pid)
    if not isinstance(identity, str) or not identity:
        if os.name == "posix" and hasattr(os, "killpg"):
            with contextlib.suppress(OSError):
                os.killpg(pid, signal.SIGKILL)
        with contextlib.suppress(OSError):
            path.unlink()
        raise OmniRootError("durable process identity cannot be proven; use native fallback")
    if durable_monitor:
        _LOCAL_MONITORS[pid] = process
    timestamp = _utc_now().isoformat()
    manifest = {
        "schemaVersion": SCHEMA_VERSION, "runId": run_id, "taskId": task_id, "workflow": workflow,
        "rootSessionId": root_session_id, "baseCommit": frozen_base,
        "integration": {"branch": integration_branch, "worktree": str(integration)},
        "qualification": {"state": readiness["state"], "fingerprint": readiness["qualificationFingerprint"],
                            "serverBuild": readiness["serverBuild"]},
        "delegate": delegate_manifest,
        "monitor": {"pid": pid, "pgid": pid, "processIdentity": identity}, "status": "running",
        "cadenceSeconds": cadence_seconds, "deadline": deadline,
        "timeoutSeconds": timeout_seconds,
        "timestamps": {"startedAt": timestamp, "updatedAt": timestamp}, "head": actual_head,
        "diagnostics": {"path": str(diagnostic_path)},
        "delegateProcess": {"path": str(delegate_process_path)},
        "receipt": {"path": str(_receipt_path(Path(state_dir), run_id)), "sha256": None},
    }
    if continuity_manifest is not None:
        continuity_manifest["participants"] = [{
            "identitySha256": _sha256(delegate.get("identity", "anonymous")),
            "sessionSha256": _sha256(delegate_session_id),
            "capability": delegate.get("capability", "default"),
        }]
        manifest["continuity"] = continuity_manifest
    try:
        _write_json(path, manifest)
    except Exception:
        if os.name == "posix":
            with contextlib.suppress(OSError):
                os.killpg(pid, signal.SIGTERM)
        raise
    if not durable_monitor and callable(getattr(process, "wait", None)) and getattr(process, "stdout", None) is not None:
        threading.Thread(
            target=_collect_diagnostics,
            args=(process, diagnostic_path),
            kwargs={"timeout_seconds": timeout_seconds,
                    "secrets": [environment.get("OMNIROUTE_API_KEY", "")]},
            daemon=True,
        ).start()
    return manifest


def status(run_id: str, state_dir: Path, *, process_identity: Callable[[int], str | None] = process_identity) -> dict[str, Any]:
    """Read current state and quarantine a stale process identity."""
    path = _run_path(Path(state_dir), run_id)
    manifest = _load_json(path)
    if manifest.get("status") not in RUN_STATUSES:
        raise OmniRootError("run status is invalid")
    if manifest.get("status") == "running":
        diagnostic_path = Path(state_dir) / "diagnostics" / f"{run_id}.json"
        diagnostic = _validated_diagnostic(_load_json(diagnostic_path)) if diagnostic_path.is_file() else None
        if diagnostic is not None and isinstance(diagnostic.get("exitCode"), int):
            continuity = manifest.get("continuity")
            monitor = manifest.get("monitor", {})
            if (isinstance(continuity, dict) and continuity.get("state") not in {
                    "review", "blocked", "breaker_open", "quarantined"}
                    and isinstance(monitor.get("pid"), int)
                    and process_identity(monitor["pid"]) == monitor.get("processIdentity")):
                return manifest
            process_path = Path(state_dir) / "processes" / f"{run_id}.json"
            delegate_process = _load_json(process_path) if process_path.is_file() else None
            if not isinstance(delegate_process, dict) or not isinstance(delegate_process.get("pgid"), int) \
                    or _group_alive(delegate_process["pgid"]):
                manifest["status"] = "quarantined"
                manifest["reason"] = "delegate process group death cannot be proven"
                manifest.setdefault("timestamps", {})["updatedAt"] = _utc_now().isoformat()
                _write_json(path, manifest)
                return manifest
            manifest["status"] = "review" if diagnostic["exitCode"] == 0 else "blocked"
            manifest["diagnostics"] = {
                "sha256": _sha256(diagnostic), "exitCode": diagnostic["exitCode"],
                "timedOut": diagnostic.get("timedOut") is True,
                "stdoutTruncated": diagnostic.get("stdoutTruncated") is True,
                "stderrTruncated": diagnostic.get("stderrTruncated") is True,
            }
            manifest.setdefault("timestamps", {})["updatedAt"] = _utc_now().isoformat()
            _write_json(path, manifest)
            monitor_process = _LOCAL_MONITORS.pop(manifest.get("monitor", {}).get("pid"), None)
            if monitor_process is not None:
                with contextlib.suppress(subprocess.TimeoutExpired):
                    monitor_process.wait(timeout=1)
            return manifest
        monitor = manifest.get("monitor", {})
        pid, identity = monitor.get("pid"), monitor.get("processIdentity")
        if not isinstance(pid, int) or not isinstance(identity, str) or process_identity(pid) != identity:
            delegate_path = Path(state_dir) / "processes" / f"{run_id}.json"
            delegate_process = _load_json(delegate_path) if delegate_path.is_file() else None
            if isinstance(delegate_process, dict) and isinstance(delegate_process.get("pgid"), int) \
                    and _group_alive(delegate_process["pgid"]):
                return manifest
            manifest["status"] = "quarantined"
            manifest["reason"] = "process identity cannot be proven"
            manifest.setdefault("timestamps", {})["updatedAt"] = _utc_now().isoformat()
            _write_json(path, manifest)
    receipt = _receipt_path(Path(state_dir), run_id)
    if receipt.is_file():
        return _load_json(receipt)
    return manifest


def cancel(run_id: str, state_dir: Path, *, process_identity: Callable[[int], str | None] = process_identity) -> dict[str, Any]:
    """Terminate only a currently proven process; otherwise quarantine state."""
    manifest = status(run_id, state_dir, process_identity=process_identity)
    if manifest.get("status") != "running":
        return manifest
    monitor = manifest.get("monitor", {})
    monitor_pid = monitor.get("pid")
    process_path = Path(state_dir) / "processes" / f"{run_id}.json"
    delegate_process = _load_json(process_path) if process_path.is_file() else None
    pid = delegate_process.get("pgid") if isinstance(delegate_process, dict) else None
    if os.name != "posix" or not hasattr(os, "killpg"):
        manifest["status"] = "quarantined"
        manifest["reason"] = "process cancellation is unsupported on this host"
    else:
        try:
            if not isinstance(pid, int) or not isinstance(delegate_process.get("processIdentity"), str) \
                    or process_identity(delegate_process["pid"]) != delegate_process["processIdentity"]:
                raise OmniRootError("delegate process identity cannot be proven")
            os.killpg(pid, signal.SIGTERM)
            for _ in range(50):
                if not _group_alive(pid):
                    break
                time.sleep(0.1)
            else:
                os.killpg(pid, signal.SIGKILL)
                for _ in range(50):
                    if not _group_alive(pid):
                        break
                    time.sleep(0.1)
                else:
                    raise OmniRootError("process group survived SIGKILL")
            if isinstance(monitor_pid, int) and process_identity(monitor_pid) == monitor.get("processIdentity"):
                with contextlib.suppress(ProcessLookupError):
                    os.killpg(monitor.get("pgid", monitor_pid), signal.SIGTERM)
        except (OSError, ProcessLookupError, OmniRootError):
            manifest["status"] = "quarantined"
            manifest["reason"] = "process termination could not be proven"
        else:
            manifest["status"] = "cancelled"
    manifest.setdefault("timestamps", {})["updatedAt"] = _utc_now().isoformat()
    _write_json(_run_path(Path(state_dir), run_id), manifest)
    return manifest


def complete(  # noqa: MC0001 - receipt validation remains one fail-closed audit boundary.
    *, run_id: str, state_dir: Path, exit_code: int, changed_paths: list[str], learning_disposition: str,
    head: str | None = None, clean: bool = True, checks: list[str] | None = None,
    blockers: list[str] | None = None, adjacent_findings: list[str] | None = None,
) -> dict[str, Any]:
    """Write the root-verifiable terminal receipt for one delegate."""
    if learning_disposition not in {"candidates", "nothing-durable", "unavailable"}:
        raise OmniRootError("learning disposition is invalid")
    if not isinstance(exit_code, int):
        raise OmniRootError("exit code is invalid")
    if not isinstance(clean, bool) or not all(isinstance(item, str) for item in (checks or []) + (blockers or []) + (adjacent_findings or [])):
        raise OmniRootError("receipt evidence is invalid")
    outcome = "success" if exit_code == 0 else "failed"
    manifest_path = _run_path(Path(state_dir), run_id)
    manifest = _load_json(manifest_path)
    if manifest.get("status") not in {"review", "blocked", "cancelled"}:
        raise OmniRootError("completion requires a terminal review, blocked, or cancelled manifest")
    diagnostic_path = Path(state_dir) / "diagnostics" / f"{run_id}.json"
    diagnostic = _validated_diagnostic(_load_json(diagnostic_path))
    if diagnostic.get("exitCode") != exit_code:
        raise OmniRootError("receipt exit code conflicts with captured process evidence")
    if manifest.get("status") == "cancelled":
        outcome = "cancelled"
    delegate = manifest.get("delegate")
    ownership = delegate.get("pathOwnership") if isinstance(delegate, dict) else None
    worktree_value = delegate.get("worktree") if isinstance(delegate, dict) else None
    if not isinstance(ownership, list) or not isinstance(worktree_value, str):
        raise OmniRootError("manifest lacks ownership-bound worktree evidence")
    normalized = _relative_paths(changed_paths)
    for changed in map(Path, normalized):
        if not any(changed == Path(owned) or Path(owned) in changed.parents for owned in ownership):
            raise OmniRootError("changed path falls outside delegate ownership")
    verified_worktree, verified_head = _validate_worktree(Path(worktree_value))
    integration_value = manifest.get("integration")
    integration_path = integration_value.get("worktree") if isinstance(integration_value, dict) else None
    if not isinstance(integration_path, str):
        raise OmniRootError("manifest lacks integration worktree evidence")
    verified_integration, _ = _validate_worktree(Path(integration_path))
    if verified_worktree == verified_integration:
        raise OmniRootError("delegate and integration worktrees must be distinct")
    if _repository_identity(verified_worktree) != _repository_identity(verified_integration):
        raise OmniRootError("receipt worktrees do not belong to the same repository")
    if clean is not True or head != verified_head:
        raise OmniRootError("receipt HEAD and clean evidence must match the delegate worktree")
    frozen_base = manifest.get("baseCommit")
    if not isinstance(frozen_base, str) or not re.fullmatch(r"[0-9a-f]{40}", frozen_base):
        raise OmniRootError("manifest lacks a frozen dispatch base")
    _git(verified_worktree, "merge-base", "--is-ancestor", frozen_base, verified_head)
    actual = _relative_paths(_git(verified_worktree, "diff", "--name-only", frozen_base, verified_head).splitlines())
    if actual != normalized:
        raise OmniRootError("changed path claim does not equal the frozen-base git diff")
    for changed in actual:
        if not (verified_worktree / changed).is_file():
            raise OmniRootError("changed path claim must name a real file")
    timestamp = _utc_now().isoformat()
    receipt = {
        "schemaVersion": SCHEMA_VERSION, "runId": run_id, "outcome": outcome,
        "status": "completed" if outcome == "success" else "blocked" if outcome == "failed" else "cancelled",
        "exitCode": exit_code, "head": head, "clean": clean, "changedPaths": normalized,
        "checks": checks or [], "blockers": blockers or [], "adjacentFindings": adjacent_findings or [],
        "learningDisposition": learning_disposition, "completedAt": timestamp,
        "diagnostics": {
            "sha256": _sha256(diagnostic), "timedOut": diagnostic.get("timedOut") is True,
            "stdoutTruncated": diagnostic.get("stdoutTruncated") is True,
            "stderrTruncated": diagnostic.get("stderrTruncated") is True,
        },
    }
    continuity = manifest.get("continuity")
    if isinstance(continuity, dict):
        receipt["continuity"] = {
            key: continuity[key] for key in (
                "taskSha256", "authoritySha256", "checkpointSha256", "completedActionSha256s",
                "trackerUrlSha256", "pullRequestUrlSha256", "attempt", "state", "participants",
            ) if key in continuity
        }
    receipt_path = _receipt_path(Path(state_dir), run_id)
    _create_immutable_json(receipt_path, receipt)
    manifest["status"] = receipt["status"]
    manifest.setdefault("timestamps", {})["updatedAt"] = timestamp
    manifest["head"] = head
    manifest["receipt"] = {"path": str(receipt_path), "sha256": _sha256(receipt)}
    _write_json(manifest_path, manifest)
    return receipt


def _print(value: dict[str, Any]) -> int:
    print(json.dumps(value, sort_keys=True))
    return 0


def _private_contract(path: Path) -> dict[str, Any]:
    value = _read_config(path)
    if value is None:
        raise OmniRootError("contract must be one owner-owned 0600 JSON file")
    return value


def main(argv: list[str] | None = None) -> int:
    raw_arguments = sys.argv[1:] if argv is None else argv
    if raw_arguments and raw_arguments[0] == "_capture":
        try:
            return _capture_command(raw_arguments[1:])
        except OmniRootError as error:
            print(str(error), file=sys.stderr)
            return 1
    if raw_arguments and raw_arguments[0] == "_supervise":
        try:
            return _supervise_command(raw_arguments[1:])
        except OmniRootError as error:
            print(str(error), file=sys.stderr)
            return 1
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, default=default_config_path())
    parser.add_argument("--state-dir", type=Path, default=default_state_path())
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("probe")
    dispatch_parser = commands.add_parser("dispatch")
    dispatch_parser.add_argument("--contract", type=Path, required=True)
    status_parser = commands.add_parser("status")
    status_parser.add_argument("--run-id", required=True)
    cancel_parser = commands.add_parser("cancel")
    cancel_parser.add_argument("--run-id", required=True)
    complete_parser = commands.add_parser("complete")
    complete_parser.add_argument("--contract", type=Path, required=True)
    args = parser.parse_args(raw_arguments)
    try:
        if args.command == "probe":
            return _print(probe(config_path=args.config))
        if args.command == "dispatch":
            contract = _private_contract(args.contract)
            required = {"runId", "worktree", "target", "delegateArgs", "taskId", "workflow",
                        "rootSessionId", "baseCommit", "integrationBranch", "integrationWorktree",
                        "delegate", "cadenceSeconds", "deadline", "timeoutSeconds", "learningState",
                        "learningRootSessionId", "delegateSessionId"}
            if set(contract) not in (required, required | {"continuity"}):
                raise OmniRootError("dispatch contract fields are invalid")
            return _print(dispatch(run_id=contract["runId"], worktree=Path(contract["worktree"]),
                state_dir=args.state_dir, config_path=args.config, target=contract["target"],
                delegate_args=contract["delegateArgs"], task_id=contract["taskId"],
                workflow=contract["workflow"], root_session_id=contract["rootSessionId"],
                base_commit=contract["baseCommit"], integration_branch=contract["integrationBranch"],
                integration_worktree=Path(contract["integrationWorktree"]), delegate=contract["delegate"],
                cadence_seconds=contract["cadenceSeconds"], deadline=contract["deadline"],
                timeout_seconds=contract["timeoutSeconds"], learning_state=Path(contract["learningState"]),
                learning_root_session_id=contract["learningRootSessionId"],
                delegate_session_id=contract["delegateSessionId"], continuity=contract.get("continuity")))
        if args.command == "status":
            return _print(status(args.run_id, args.state_dir))
        if args.command == "cancel":
            return _print(cancel(args.run_id, args.state_dir))
        contract = _private_contract(args.contract)
        required = {"runId", "exitCode", "changedPaths", "learningDisposition", "head", "clean",
                    "checks", "blockers", "adjacentFindings"}
        if set(contract) != required:
            raise OmniRootError("complete contract fields are invalid")
        return _print(complete(run_id=contract["runId"], state_dir=args.state_dir,
            exit_code=contract["exitCode"], changed_paths=contract["changedPaths"],
            learning_disposition=contract["learningDisposition"], head=contract["head"],
            clean=contract["clean"], checks=contract["checks"], blockers=contract["blockers"],
            adjacent_findings=contract["adjacentFindings"]))
    except OmniRootError as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
