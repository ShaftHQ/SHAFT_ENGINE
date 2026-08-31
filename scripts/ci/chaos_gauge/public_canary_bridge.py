#!/usr/bin/env python3
"""Fail-closed credential and evidence boundary for public ChaosGauge canaries."""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import re
import subprocess
import urllib.error
import urllib.request
from pathlib import Path
from typing import Callable

from scripts.ci.validate_shaft_pilot_release import CREDENTIAL_PATTERNS, SECRET_CANARIES


PRIVATE_REPOSITORY = "ShaftHQ/ChaosGauge-private"
PRIVATE_COMMIT = "08551a3db4376438acddd77422554ce710a58624"
RUN_ID = re.compile(r"[1-9][0-9]{0,18}")


def _error(message: str) -> ValueError:
    return ValueError(f"public canary bridge: {message}")


def _metadata(token: str, opener: Callable[..., object] = urllib.request.urlopen) -> tuple[dict[str, object], str]:
    if not token:
        raise _error("BOT_TOKEN is unavailable")
    request = urllib.request.Request(
        f"https://api.github.com/repos/{PRIVATE_REPOSITORY}",
        headers={
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {token}",
            "X-GitHub-Api-Version": "2026-03-10",
        },
    )
    try:
        with opener(request, timeout=20) as response:
            value = json.loads(response.read().decode("utf-8"))
            scopes = str(response.headers.get("X-OAuth-Scopes", ""))
    except (OSError, urllib.error.HTTPError, json.JSONDecodeError) as error:
        raise _error("BOT_TOKEN metadata is unavailable") from error
    if not isinstance(value, dict):
        raise _error("BOT_TOKEN metadata is invalid")
    return value, scopes


def preflight(token: str, opener: Callable[..., object] = urllib.request.urlopen) -> None:
    """Prove private read/release capability with metadata only; never probe-write."""
    metadata, scopes = _metadata(token, opener)
    permissions = metadata.get("permissions")
    if (
        metadata.get("private") is not True
        or not isinstance(permissions, dict)
        or permissions.get("pull") is not True
        or permissions.get("push") is not True
    ):
        raise _error("BOT_TOKEN lacks private release capability")
    granted = {scope.strip() for scope in scopes.split(",") if scope.strip()}
    if not {"repo", "workflow"}.issubset(granted):
        raise _error("BOT_TOKEN release scopes are not provable")


def _canary_module():
    path = Path(__file__).with_name("canary.py")
    spec = importlib.util.spec_from_file_location("chaos_gauge_canary", path)
    if spec is None or spec.loader is None:
        raise _error("canary validation is unavailable")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _safe_file(path: Path) -> bytes:
    if not path.is_file() or path.is_symlink():
        raise _error("evidence file is unavailable")
    return path.read_bytes()


def _scan(label: str, content: bytes) -> None:
    if any(marker in content for marker in SECRET_CANARIES) or any(pattern.search(content) for pattern in CREDENTIAL_PATTERNS):
        raise _error(f"{label} contains a secret-shaped value")


def validate(raw: Path, receipt: Path, repository: Path) -> None:
    """Reject unsafe raw data and receipt drift before any upload."""
    raw_content, receipt_content = _safe_file(raw), _safe_file(receipt)
    _scan("raw evidence", raw_content)
    _scan("sanitized receipt", receipt_content)
    try:
        value = json.loads(receipt_content.decode("utf-8"))
        _canary_module().validate_public_evidence(value, repository=repository)
    except (OSError, UnicodeDecodeError, ValueError, json.JSONDecodeError) as error:
        raise _error("sanitized receipt is invalid") from error


def publish(raw: Path, receipt: Path, repository: Path, run_id: str, token: str, run: Callable[..., object] = subprocess.run) -> None:
    """Store exactly raw and sanitized evidence in one private draft release."""
    if not RUN_ID.fullmatch(run_id):
        raise _error("run ID is invalid")
    preflight(token)
    validate(raw, receipt, repository)
    run(
        [
            "gh", "release", "create", f"chaosgauge-canary-{run_id}", str(raw), str(receipt),
            "--repo", PRIVATE_REPOSITORY, "--target", PRIVATE_COMMIT,
            "--draft", "--title", f"ChaosGauge excluded canary {run_id}",
            "--notes", "Private raw evidence and validated sanitized receipt.",
        ],
        check=True,
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("preflight")
    validate_parser = commands.add_parser("validate")
    publish_parser = commands.add_parser("publish")
    for command in (validate_parser, publish_parser):
        command.add_argument("--raw", type=Path, required=True)
        command.add_argument("--receipt", type=Path, required=True)
        command.add_argument("--repository", type=Path, required=True)
    publish_parser.add_argument("--run-id", required=True)
    args = parser.parse_args()
    token = os.environ.get("BOT_TOKEN", "")
    if args.command == "preflight":
        preflight(token)
    elif args.command == "validate":
        validate(args.raw, args.receipt, args.repository)
    else:
        publish(args.raw, args.receipt, args.repository, args.run_id, token)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
