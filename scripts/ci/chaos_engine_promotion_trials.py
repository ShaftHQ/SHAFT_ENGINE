#!/usr/bin/env python3
"""Collect bounded live five-host promotion receipts without transcripts."""

from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import importlib.util
import json
import os
import re
import shutil
import signal
import subprocess  # nosec B404 - list-form commands come from protected CI configuration.
import threading
import time
from pathlib import Path
from typing import Mapping, NamedTuple


_PROMOTION_PATH = Path(__file__).with_name("chaos_engine_promotion.py")
_SPEC = importlib.util.spec_from_file_location("chaos_engine_promotion_contract", _PROMOTION_PATH)
if _SPEC is None or _SPEC.loader is None:
    raise RuntimeError("promotion receipt contract is unavailable")
promotion = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(promotion)

RUN_FIELDS = {
    "completed",
    "safe",
    "tokens",
    "retries",
    "denials",
    "repeatedStates",
    "terminalReason",
}
MAX_COMMAND_BYTES = 16 * 1024
MAX_OUTPUT_BYTES = 64 * 1024
MAX_DRIVER_BYTES = 64 * 1024 * 1024
REVISION_VARIABLES = {
    "baseline": "CHAOS_ENGINE_BASELINE_REVISION",
    "candidate": "CHAOS_ENGINE_CANDIDATE_REVISION",
}
ALLOWED_DRIVER_NAMES = {
    host: {host, f"chaos-engine-{host}-driver"} for host in promotion.HOSTS
}
ALLOWED_DRIVER_NAMES["copilot"].add("github-copilot")
SAFE_ENVIRONMENT_KEYS = {
    "CI",
    "COMSPEC",
    "HOME",
    "LANG",
    "LC_ALL",
    "NO_COLOR",
    "PATH",
    "PATHEXT",
    "SYSTEMROOT",
    "TEMP",
    "TERM",
    "TMP",
    "TMPDIR",
    "USERPROFILE",
    "WINDIR",
}
HEX_DIGEST = re.compile(r"[0-9a-f]{64}")
HEX_REVISION = re.compile(r"[0-9a-f]{40}")


class TrialCollectionError(RuntimeError):
    def __init__(self, code: str):
        super().__init__(code)
        self.code = code


class DriverSpec(NamedTuple):
    host: str
    variant: str
    argv: tuple[str, ...]
    version_argv: tuple[str, ...]
    client_version: str
    revision: str
    driver_sha256: str
    command_sha256: str


def command_variable(host: str, variant: str) -> str:
    return f"CHAOS_ENGINE_{variant}_{host}_COMMAND".upper()


def _list_argument(value: object, host: str, variant: str) -> tuple[str, ...]:
    if (
        not isinstance(value, list)
        or not 1 <= len(value) <= 32
        or any(type(item) is not str or not item or "\0" in item for item in value)
    ):
        raise TrialCollectionError(f"command-invalid:{host}:{variant}")
    return tuple(value)


def _driver_path(command: str, host: str, variant: str) -> Path:
    resolved = shutil.which(command)
    if resolved is None:
        candidate = Path(command)
        if candidate.is_absolute() and candidate.is_file():
            resolved = str(candidate)
    if resolved is None:
        raise TrialCollectionError(f"driver-unavailable:{host}:{variant}")
    lexical_name = Path(command).name.casefold()
    for suffix in (".exe", ".cmd", ".bat", ".ps1"):
        if lexical_name.endswith(suffix):
            lexical_name = lexical_name[: -len(suffix)]
            break
    if lexical_name not in ALLOWED_DRIVER_NAMES[host]:
        raise TrialCollectionError(f"driver-host-mismatch:{host}:{variant}")
    path = Path(resolved).resolve(strict=True)
    if not path.is_file() or path.stat().st_size > MAX_DRIVER_BYTES:
        raise TrialCollectionError(f"driver-unsafe:{host}:{variant}")
    return path


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def _child_environment(
    host: str, environment: Mapping[str, str], *, include_credential: bool
) -> dict[str, str]:
    child = {
        key: environment[key]
        for key in SAFE_ENVIRONMENT_KEYS
        if environment.get(key) is not None
    }
    if include_credential:
        credential = promotion.CREDENTIALS[host]
        value = environment.get(credential)
        if not value:
            raise TrialCollectionError(f"credential-missing:{host}")
        child[credential] = value
    return child


def _terminate_process_tree(process: subprocess.Popen[bytes]) -> None:
    if process.poll() is not None:
        return
    if os.name == "nt":
        subprocess.run(  # nosec B603 B607 - fixed platform process-tree cleanup.
            ["taskkill", "/PID", str(process.pid), "/T", "/F"],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=5,
            check=False,
        )
    else:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            # The process group exited after the poll above.
            pass
    try:
        process.kill()
    except ProcessLookupError:
        # Tree termination may have already reaped the process.
        pass


def _run_bounded(
    command: tuple[str, ...],
    request: bytes,
    environment: Mapping[str, str],
    *,
    timeout: float,
    code: str,
) -> tuple[int, bytes, bytes]:
    options: dict[str, object] = {
        "stdin": subprocess.PIPE,
        "stdout": subprocess.PIPE,
        "stderr": subprocess.PIPE,
        "env": dict(environment),
    }
    if os.name == "nt":
        options["creationflags"] = subprocess.CREATE_NEW_PROCESS_GROUP
    else:
        options["start_new_session"] = True
    try:
        process = subprocess.Popen(command, **options)  # type: ignore[arg-type]  # nosec B603
    except OSError as error:
        raise TrialCollectionError(f"{code}-execution-failed") from error
    stdout = bytearray()
    stderr = bytearray()
    total = 0
    lock = threading.Lock()
    overflow = threading.Event()

    def drain(stream, sink: bytearray) -> None:
        nonlocal total
        try:
            while chunk := stream.read(4096):
                with lock:
                    remaining = MAX_OUTPUT_BYTES - total
                    if remaining > 0:
                        sink.extend(chunk[:remaining])
                        total += min(len(chunk), remaining)
                    if len(chunk) > remaining:
                        overflow.set()
                        return
        finally:
            stream.close()

    threads = [
        threading.Thread(target=drain, args=(process.stdout, stdout), daemon=True),
        threading.Thread(target=drain, args=(process.stderr, stderr), daemon=True),
    ]
    for thread in threads:
        thread.start()
    try:
        if process.stdin is not None:
            try:
                process.stdin.write(request)
                process.stdin.flush()
            except BrokenPipeError:
                # Early child exit is classified from its status and bounded output.
                pass
            finally:
                process.stdin.close()
        deadline = time.monotonic() + timeout
        while process.poll() is None and not overflow.is_set():
            if time.monotonic() >= deadline:
                _terminate_process_tree(process)
                raise TrialCollectionError(f"{code}-timeout")
            time.sleep(0.01)
        if overflow.is_set():
            _terminate_process_tree(process)
            raise TrialCollectionError(f"{code}-output-oversized")
        return_code = process.wait(timeout=5)
    except (Exception, KeyboardInterrupt, SystemExit):
        _terminate_process_tree(process)
        raise
    finally:
        for thread in threads:
            thread.join(timeout=5)
    return return_code, bytes(stdout), bytes(stderr)


def _driver_spec(host: str, variant: str, environment: Mapping[str, str]) -> DriverSpec:
    variable = command_variable(host, variant)
    rendered = environment.get(variable, "")
    if not rendered or len(rendered.encode("utf-8")) > MAX_COMMAND_BYTES:
        raise TrialCollectionError(f"command-missing-or-oversized:{host}:{variant}")
    try:
        value = json.loads(rendered)
    except (json.JSONDecodeError, ValueError) as error:
        raise TrialCollectionError(f"command-invalid:{host}:{variant}") from error
    required = {
        "schemaVersion",
        "client",
        "argv",
        "versionArgv",
        "clientVersion",
        "driverSha256",
    }
    if not isinstance(value, dict) or set(value) != required:
        raise TrialCollectionError(f"command-invalid:{host}:{variant}")
    argv = _list_argument(value["argv"], host, variant)
    version_argv = _list_argument(value["versionArgv"], host, variant)
    client_version = value["clientVersion"]
    driver_sha256 = value["driverSha256"]
    revision = environment.get(REVISION_VARIABLES[variant], "")
    if (
        value["schemaVersion"] != 1
        or value["client"] != host
        or type(client_version) is not str
        or not client_version.strip()
        or host not in client_version.casefold()
        or len(client_version.encode("utf-8")) > 256
        or any(ord(character) < 32 for character in client_version)
        or type(driver_sha256) is not str
        or HEX_DIGEST.fullmatch(driver_sha256) is None
        or HEX_REVISION.fullmatch(revision) is None
    ):
        raise TrialCollectionError(f"command-invalid:{host}:{variant}")
    driver = _driver_path(argv[0], host, variant)
    version_driver = _driver_path(version_argv[0], host, variant)
    if driver != version_driver or _sha256(driver) != driver_sha256:
        raise TrialCollectionError(f"driver-integrity:{host}:{variant}")
    argv = (str(driver), *argv[1:])
    version_argv = (str(driver), *version_argv[1:])
    return_code, stdout, stderr = _run_bounded(
        version_argv,
        b"",
        _child_environment(host, environment, include_credential=False),
        timeout=30,
        code=f"driver-version:{host}:{variant}",
    )
    actual_version = (stdout or stderr).decode("utf-8", errors="strict").strip()
    if return_code != 0 or actual_version != client_version:
        raise TrialCollectionError(f"driver-version-mismatch:{host}:{variant}")
    canonical = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return DriverSpec(
        host=host,
        variant=variant,
        argv=argv,
        version_argv=version_argv,
        client_version=client_version,
        revision=revision,
        driver_sha256=driver_sha256,
        command_sha256=hashlib.sha256(canonical + b"\0" + revision.encode()).hexdigest(),
    )


def configuration_gaps(environment: Mapping[str, str]) -> list[str]:
    gaps = [
        f"credential:{host}"
        for host in promotion.HOSTS
        if not environment.get(promotion.CREDENTIALS[host])
    ]
    gaps.extend(
        f"command:{host}:{variant}"
        for host in promotion.HOSTS
        for variant in promotion.VARIANTS
        if not environment.get(command_variable(host, variant))
    )
    gaps.extend(
        f"revision:{variant}"
        for variant, variable in REVISION_VARIABLES.items()
        if HEX_REVISION.fullmatch(environment.get(variable, "")) is None
    )
    return gaps


def _validate_revision_pair(environment: Mapping[str, str]) -> None:
    child = _child_environment("codex", environment, include_credential=False)
    candidate = environment[REVISION_VARIABLES["candidate"]]
    baseline = environment[REVISION_VARIABLES["baseline"]]
    return_code, stdout, _ = _run_bounded(
        ("git", "rev-parse", "HEAD"), b"", child, timeout=30, code="revision-head"
    )
    if return_code != 0 or stdout.decode("ascii", errors="strict").strip() != candidate:
        raise TrialCollectionError("candidate-revision-not-head")
    return_code, _, _ = _run_bounded(
        ("git", "merge-base", "--is-ancestor", baseline, candidate),
        b"",
        child,
        timeout=30,
        code="revision-ancestry",
    )
    if return_code != 0:
        raise TrialCollectionError("baseline-revision-not-ancestor")


def run_trial(
    host: str,
    scenario: str,
    trial: int,
    variant: str,
    environment: Mapping[str, str],
    *,
    timeout: float,
    spec: DriverSpec | None = None,
) -> dict[str, object]:
    driver = spec or _driver_spec(host, variant, environment)
    if driver.host != host or driver.variant != variant:
        raise TrialCollectionError(f"driver-binding-invalid:{host}:{variant}")
    binding = {
        "client": host,
        "clientVersion": driver.client_version,
        "revision": driver.revision,
        "driverSha256": driver.driver_sha256,
        "commandSha256": driver.command_sha256,
    }
    request = {
        "schemaVersion": 1,
        "host": host,
        "scenario": scenario,
        "trial": trial,
        "variant": variant,
        "requiredOutputFields": sorted(RUN_FIELDS),
        "outputPolicy": "one secret-free JSON object; no transcript",
        "binding": binding,
    }
    started = time.monotonic()
    return_code, stdout, _ = _run_bounded(
        driver.argv,
        (json.dumps(request, sort_keys=True, separators=(",", ":")) + "\n").encode(),
        _child_environment(host, environment, include_credential=True),
        timeout=timeout,
        code=f"trial:{host}:{scenario}:{trial}:{variant}",
    )
    latency_ms = round((time.monotonic() - started) * 1000, 3)
    if return_code != 0:
        raise TrialCollectionError(f"trial-failed:{host}:{scenario}:{trial}:{variant}")
    try:
        summary = json.loads(stdout)
    except (UnicodeError, json.JSONDecodeError, ValueError) as error:
        raise TrialCollectionError(
            f"trial-output-invalid:{host}:{scenario}:{trial}:{variant}"
        ) from error
    if (
        not isinstance(summary, dict)
        or set(summary) != RUN_FIELDS | {"binding"}
        or summary.get("binding") != binding
    ):
        raise TrialCollectionError(f"trial-output-invalid:{host}:{scenario}:{trial}:{variant}")
    summary.pop("binding")
    return promotion.validate_receipt(
        {
            "schemaVersion": promotion.SCHEMA_VERSION,
            "host": host,
            "scenario": scenario,
            "trial": trial,
            "variant": variant,
            **binding,
            **summary,
            "latencyMs": latency_ms,
        }
    )


def collect(
    output: Path,
    environment: Mapping[str, str],
    *,
    workers: int,
    timeout: float,
) -> int:
    gaps = configuration_gaps(environment)
    if gaps:
        raise TrialCollectionError("configuration-incomplete:" + ",".join(gaps))
    if environment[REVISION_VARIABLES["baseline"]] == environment[REVISION_VARIABLES["candidate"]]:
        raise TrialCollectionError("revision-pair-identical")
    _validate_revision_pair(environment)
    if not 1 <= workers <= 20 or timeout <= 0:
        raise TrialCollectionError("runner-bounds-invalid")
    output = output.absolute()
    staging = output.with_name(output.name + ".building")
    if output.exists() or output.is_symlink() or staging.exists() or staging.is_symlink():
        raise TrialCollectionError("receipt-output-collision")
    specifications = {
        (host, variant): _driver_spec(host, variant, environment)
        for host in promotion.HOSTS
        for variant in promotion.VARIANTS
    }
    cases = sorted(promotion.expected_keys())
    executor = concurrent.futures.ThreadPoolExecutor(max_workers=workers)
    futures = {
        executor.submit(
            run_trial,
            host,
            scenario,
            trial,
            variant,
            environment,
            timeout=timeout,
            spec=specifications[(host, variant)],
        ): (host, scenario, trial, variant)
        for host, scenario, trial, variant in cases
    }
    try:
        receipts = [future.result() for future in concurrent.futures.as_completed(futures)]
    except (Exception, KeyboardInterrupt, SystemExit):
        for future in futures:
            future.cancel()
        executor.shutdown(wait=True, cancel_futures=True)
        raise
    else:
        executor.shutdown(wait=True)
    receipts.sort(
        key=lambda item: (
            str(item["host"]),
            str(item["scenario"]),
            int(item["trial"]),
            str(item["variant"]),
        )
    )
    staging.mkdir(parents=True)
    try:
        for item in receipts:
            name = f"{item['host']}--{item['scenario']}--{item['trial']}--{item['variant']}.json"
            (staging / name).write_text(
                json.dumps(item, sort_keys=True, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
        output.parent.mkdir(parents=True, exist_ok=True)
        staging.replace(output)
    except (Exception, KeyboardInterrupt, SystemExit):
        for path in staging.glob("*.json"):
            if path.is_file() and not path.is_symlink():
                path.unlink()
        if staging.is_dir() and not any(staging.iterdir()):
            staging.rmdir()
        raise
    return len(receipts)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--output", type=Path, required=True)
    result.add_argument("--status", type=Path, required=True)
    result.add_argument("--workers", type=int, default=5)
    result.add_argument("--timeout", type=float, default=300.0)
    return result


def main() -> int:
    arguments = parser().parse_args()
    status: dict[str, object] = {
        "schemaVersion": 1,
        "identity": "chaos-engine-promotion-trials",
        "status": "Blocked",
        "terminalReason": "blocked",
    }
    try:
        count = collect(
            arguments.output,
            os.environ,
            workers=arguments.workers,
            timeout=arguments.timeout,
        )
        status.update(status="Complete", terminalReason="complete", receiptCount=count)
    except TrialCollectionError as error:
        status["failureCode"] = error.code[:1024]
    except (OSError, ValueError):
        status["failureCode"] = "receipt-publication-failed"
    arguments.status.write_text(
        json.dumps(status, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    return 0 if status["status"] == "Complete" else 1


if __name__ == "__main__":
    raise SystemExit(main())
