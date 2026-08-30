#!/usr/bin/env python3
"""Fail-closed optional local OmniRoute transport primitives."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import signal
import subprocess  # nosec B404 - dispatches a fixed local launcher with argv.
import sys
import tempfile
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable
from urllib.error import HTTPError, URLError
from urllib.request import HTTPRedirectHandler, Request, build_opener


DEFAULT_ENDPOINT = "http://127.0.0.1:20128/"
HEALTH_PATH = "api/health"
MAX_RESPONSE_BYTES = 64 * 1024
HTTP_TIMEOUT_SECONDS = 2
SCHEMA_VERSION = 1
READINESS = frozenset({
    "ABSENT", "UNHEALTHY", "UNAUTHENTICATED", "ROUTE_UNQUALIFIED", "READY", "RUNTIME_EXHAUSTED",
})
_RUN_ID = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,127}\Z")
_TARGET = re.compile(r"[a-z][a-z0-9-]{0,63}\Z")
_HEX = re.compile(r"[0-9a-f]{64}\Z")


class OmniRootError(RuntimeError):
    """A required transport invariant did not hold."""


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, message, headers, newurl):  # noqa: ANN001
        raise OmniRootError("gateway redirect is forbidden")


def default_config_path() -> Path:
    return Path.home() / ".config" / "chaos-engine" / "omniroot.json"


def _open(request: Request, *, timeout: int):
    return build_opener(_NoRedirect()).open(request, timeout=timeout)


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
        if not path.is_file() or path.is_symlink() or path.stat().st_size > MAX_RESPONSE_BYTES:
            return None
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return None
    return value if isinstance(value, dict) else None


def _attestation_valid(config: dict[str, Any], build: object, now: datetime) -> bool:
    if config.get("schemaVersion") != SCHEMA_VERSION:
        return False
    if not isinstance(config.get("routeId"), str) or not config["routeId"].strip():
        return False
    if config.get("launcher") != "omniroute":
        return False
    attestation = config.get("attestation")
    if not isinstance(attestation, dict) or attestation.get("schemaVersion") != SCHEMA_VERSION:
        return False
    if attestation.get("serverBuild") != build:
        return False
    if not all(
        isinstance(attestation.get(key), str) and _HEX.fullmatch(attestation[key])
        for key in ("routePolicySha256", "endpointKeyIdentitySha256")
    ):
        return False
    verified = _parse_time(attestation.get("verifiedAt"))
    expires = _parse_time(attestation.get("expiresAt"))
    if verified is None or expires is None or verified > now or expires <= now:
        return False
    return all(attestation.get(key) is True for key in (
        "noCostConfirmed", "noPaidFallbackConfirmed", "privacyConfirmed",
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
    environment = os.environ if environ is None else environ
    if not environment.get("OMNIROUTE_API_KEY"):
        return {**result, "state": "UNAUTHENTICATED"}
    config = _read_config(config_path or default_config_path())
    if config is None or not _attestation_valid(config, payload["build"], now()):
        return {**result, "state": "ROUTE_UNQUALIFIED"}
    return {**result, "state": "READY", "serverBuild": payload["build"]}


class QualificationCache:
    """Root-session-only readiness cache keyed by non-secret qualification facts."""

    def __init__(self):
        self._fingerprint: str | None = None
        self._result: dict[str, str] | None = None

    def probe(self, **kwargs: Any) -> dict[str, str]:
        path = Path(kwargs.get("config_path") or default_config_path())
        content = _read_config(path)
        fingerprint = hashlib.sha256(json.dumps(content, sort_keys=True).encode("utf-8")).hexdigest()
        if self._fingerprint == fingerprint and self._result is not None and self._result["state"] == "READY":
            return dict(self._result)
        result = probe(**kwargs)
        self._fingerprint, self._result = fingerprint, dict(result)
        return result


def _private_directory(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    try:
        path.chmod(0o700)
    except OSError:
        pass
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


def _load_json(path: Path) -> dict[str, Any]:
    try:
        if not path.is_file() or path.is_symlink() or path.stat().st_size > MAX_RESPONSE_BYTES:
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
    if not all(isinstance(path, str) and path and not Path(path).is_absolute() and ".." not in Path(path).parts for path in paths):
        raise OmniRootError("changed paths must be repository-relative")
    return sorted(set(paths))


def process_identity(pid: int) -> str | None:
    """Return a Linux process-start identity; unsupported hosts never kill."""
    try:
        fields = Path(f"/proc/{pid}/stat").read_text(encoding="utf-8").split()
        return fields[21] if len(fields) > 21 else None
    except (OSError, UnicodeError):
        return None


def _dispatch_environment(environ: dict[str, str]) -> dict[str, str]:
    key = environ.get("OMNIROUTE_API_KEY")
    if not key:
        raise OmniRootError("endpoint key is unavailable")
    result = {"PATH": environ.get("PATH", os.defpath), "OMNIROUTE_API_KEY": key}
    return result


def dispatch(
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
) -> dict[str, Any]:
    """Launch one bounded implementer only after a fresh strict readiness check."""
    if not _TARGET.fullmatch(target) or not all(isinstance(value, str) and "\x00" not in value for value in delegate_args):
        raise OmniRootError("target or delegated arguments are invalid")
    try:
        worktree = Path(worktree).resolve(strict=True)
    except OSError as error:
        raise OmniRootError("existing isolated worktree is required") from error
    if not worktree.is_dir():
        raise OmniRootError("existing isolated worktree is required")
    environment = os.environ.copy() if environ is None else dict(environ)
    readiness = probe(config_path=config_path, opener=opener, environ=environment)
    if readiness["state"] != "READY":
        raise OmniRootError(f"OmniRoute is not ready: {readiness['state']}")
    path = _run_path(Path(state_dir), run_id)
    if path.exists():
        raise OmniRootError("run id already exists")
    argv = ["omniroute", "run", target, "--port", "20128", "--api-key-env", "OMNIROUTE_API_KEY", "--", *delegate_args]
    try:
        process = popen(
            argv, cwd=str(worktree), env=_dispatch_environment(environment), shell=False,
            close_fds=True, start_new_session=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
    except OSError as error:
        raise OmniRootError("OmniRoute launcher could not start") from error
    pid = getattr(process, "pid", None)
    if not isinstance(pid, int) or pid <= 1:
        raise OmniRootError("launcher did not expose a safe process id")
    manifest = {
        "schemaVersion": SCHEMA_VERSION, "runId": run_id, "status": "running", "pid": pid,
        "processIdentity": process_identity(pid), "worktree": str(worktree), "target": target,
        "argvSha256": hashlib.sha256("\0".join(argv).encode("utf-8")).hexdigest(),
        "environmentKeys": ["OMNIROUTE_API_KEY", "PATH"], "serverBuild": readiness["serverBuild"],
        "startedAt": _utc_now().isoformat(),
    }
    _write_json(path, manifest)
    return manifest


def status(run_id: str, state_dir: Path, *, process_identity: Callable[[int], str | None] = process_identity) -> dict[str, Any]:
    """Read current state and quarantine a stale process identity."""
    path = _run_path(Path(state_dir), run_id)
    manifest = _load_json(path)
    if manifest.get("status") == "running":
        pid, identity = manifest.get("pid"), manifest.get("processIdentity")
        if not isinstance(pid, int) or not isinstance(identity, str) or process_identity(pid) != identity:
            manifest["status"] = "quarantined"
            manifest["reason"] = "process identity cannot be proven"
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
    pid = manifest["pid"]
    try:
        os.killpg(pid, signal.SIGTERM)
    except (OSError, ProcessLookupError):
        manifest["status"] = "quarantined"
        manifest["reason"] = "process termination could not be proven"
    else:
        manifest["status"] = "cancelled"
    _write_json(_run_path(Path(state_dir), run_id), manifest)
    return manifest


def complete(
    *, run_id: str, state_dir: Path, exit_code: int, changed_paths: list[str], learning_disposition: str,
) -> dict[str, Any]:
    """Write the root-verifiable terminal receipt for one delegate."""
    if learning_disposition not in {"candidates", "nothing-durable", "unavailable"}:
        raise OmniRootError("learning disposition is invalid")
    if not isinstance(exit_code, int):
        raise OmniRootError("exit code is invalid")
    receipt = {
        "schemaVersion": SCHEMA_VERSION, "runId": run_id,
        "status": "completed" if exit_code == 0 else "blocked", "exitCode": exit_code,
        "changedPaths": _relative_paths(changed_paths), "learningDisposition": learning_disposition,
        "completedAt": _utc_now().isoformat(),
    }
    _write_json(_receipt_path(Path(state_dir), run_id), receipt)
    return receipt


def _print(value: dict[str, Any]) -> int:
    print(json.dumps(value, sort_keys=True))
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, default=default_config_path())
    parser.add_argument("--state-dir", type=Path, default=Path(".omniroot-state"))
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("probe")
    dispatch_parser = commands.add_parser("dispatch")
    dispatch_parser.add_argument("--run-id", required=True)
    dispatch_parser.add_argument("--worktree", type=Path, required=True)
    dispatch_parser.add_argument("--target", required=True)
    dispatch_parser.add_argument("delegate_args", nargs=argparse.REMAINDER)
    status_parser = commands.add_parser("status")
    status_parser.add_argument("--run-id", required=True)
    cancel_parser = commands.add_parser("cancel")
    cancel_parser.add_argument("--run-id", required=True)
    args = parser.parse_args(argv)
    try:
        if args.command == "probe":
            return _print(probe(config_path=args.config))
        if args.command == "dispatch":
            return _print(dispatch(
                run_id=args.run_id, worktree=args.worktree, state_dir=args.state_dir,
                config_path=args.config, target=args.target, delegate_args=args.delegate_args,
            ))
        if args.command == "status":
            return _print(status(args.run_id, args.state_dir))
        return _print(cancel(args.run_id, args.state_dir))
    except OmniRootError as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
