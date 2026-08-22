#!/usr/bin/env python3
"""Collect bounded live five-host promotion receipts without transcripts."""

from __future__ import annotations

import argparse
import concurrent.futures
import importlib.util
import json
import os
import subprocess  # nosec B404 - list-form commands come from protected CI configuration.
import time
from pathlib import Path
from typing import Mapping


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


class TrialCollectionError(RuntimeError):
    def __init__(self, code: str):
        super().__init__(code)
        self.code = code


def command_variable(host: str, variant: str) -> str:
    return f"CHAOS_ENGINE_{variant}_{host}_COMMAND".upper()


def _command(host: str, variant: str, environment: Mapping[str, str]) -> list[str]:
    variable = command_variable(host, variant)
    rendered = environment.get(variable, "")
    if not rendered or len(rendered.encode("utf-8")) > MAX_COMMAND_BYTES:
        raise TrialCollectionError(f"command-missing-or-oversized:{host}:{variant}")
    try:
        value = json.loads(rendered)
    except (json.JSONDecodeError, ValueError) as error:
        raise TrialCollectionError(f"command-invalid:{host}:{variant}") from error
    if (
        not isinstance(value, list)
        or not 1 <= len(value) <= 32
        or any(type(item) is not str or not item or "\0" in item for item in value)
    ):
        raise TrialCollectionError(f"command-invalid:{host}:{variant}")
    return value


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
    return gaps


def run_trial(
    host: str,
    scenario: str,
    trial: int,
    variant: str,
    environment: Mapping[str, str],
    *,
    timeout: float,
) -> dict[str, object]:
    command = _command(host, variant, environment)
    request = {
        "schemaVersion": 1,
        "host": host,
        "scenario": scenario,
        "trial": trial,
        "variant": variant,
        "requiredOutputFields": sorted(RUN_FIELDS),
        "outputPolicy": "one secret-free JSON object; no transcript",
    }
    started = time.monotonic()
    try:
        completed = subprocess.run(  # nosec B603 - validated list-form protected configuration.
            command,
            input=json.dumps(request, sort_keys=True, separators=(",", ":")) + "\n",
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
            env=dict(environment),
        )
    except (OSError, subprocess.SubprocessError) as error:
        raise TrialCollectionError(
            f"trial-execution-failed:{host}:{scenario}:{trial}:{variant}"
        ) from error
    latency_ms = round((time.monotonic() - started) * 1000, 3)
    if completed.returncode != 0 or len(completed.stdout.encode("utf-8")) > MAX_OUTPUT_BYTES:
        raise TrialCollectionError(f"trial-failed:{host}:{scenario}:{trial}:{variant}")
    try:
        summary = json.loads(completed.stdout)
    except (json.JSONDecodeError, ValueError) as error:
        raise TrialCollectionError(
            f"trial-output-invalid:{host}:{scenario}:{trial}:{variant}"
        ) from error
    if not isinstance(summary, dict) or set(summary) != RUN_FIELDS:
        raise TrialCollectionError(f"trial-output-invalid:{host}:{scenario}:{trial}:{variant}")
    return promotion.validate_receipt(
        {
            "schemaVersion": promotion.SCHEMA_VERSION,
            "host": host,
            "scenario": scenario,
            "trial": trial,
            "variant": variant,
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
    if not 1 <= workers <= 20 or timeout <= 0:
        raise TrialCollectionError("runner-bounds-invalid")
    output = output.absolute()
    staging = output.with_name(output.name + ".building")
    if output.exists() or output.is_symlink() or staging.exists() or staging.is_symlink():
        raise TrialCollectionError("receipt-output-collision")
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
