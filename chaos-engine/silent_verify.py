#!/usr/bin/env python3
"""Silent-on-success / errors-only verification wrappers (#5654 / HumanLayer)."""

from __future__ import annotations

import argparse
import importlib.util
import json
import subprocess  # nosec B404 - fixed argv list only, never shell=True.
import sys
from collections.abc import Callable, Sequence
from pathlib import Path
from typing import Any

SUCCESS_EXIT = 0
FAILURE_EXIT = 2


def silent_result(
    ok: bool,
    *,
    failure_message: str,
    stream=None,
) -> int:
    """Success → silent exit 0; failure → one actionable stderr line + exit 2."""
    target = sys.stderr if stream is None else stream
    if ok:
        return SUCCESS_EXIT
    line = " ".join(str(failure_message).split()).strip() or "verification failed"
    target.write(line + "\n")
    return FAILURE_EXIT


def run_argv(argv: Sequence[str], *, cwd: Path | None = None) -> int:
    """Run a fixed argv list; swallow green stdout; keep one failure line."""
    if not argv:
        return silent_result(False, failure_message="silent-verify: empty command")
    completed = subprocess.run(  # nosec B603
        list(argv),
        cwd=str(cwd) if cwd is not None else None,
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode == 0:
        return SUCCESS_EXIT
    detail = (completed.stderr or completed.stdout or "").strip().splitlines()
    message = detail[-1] if detail else f"command failed (exit {completed.returncode})"
    return silent_result(False, failure_message=message)


def check_callable(fn: Callable[[], Any], *, failure_message: str | None = None) -> int:
    try:
        result = fn()
    except Exception as error:  # noqa: BLE001 - surface one line only
        return silent_result(False, failure_message=failure_message or str(error))
    if result is False:
        return silent_result(False, failure_message=failure_message or "check returned false")
    return SUCCESS_EXIT


def _load(name: str):
    path = Path(__file__).resolve().with_name(name)
    spec = importlib.util.spec_from_file_location(f"chaos_engine_silent_{name}", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"{name} missing")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def verify_session_start_budget(project: Path | None = None) -> int:
    """SessionStart locator budget check used on Check/Stop hot path."""
    hook_path = Path(__file__).resolve().parent / "hooks" / "lifecycle.py"
    spec = importlib.util.spec_from_file_location("ce_silent_lifecycle", hook_path)
    if spec is None or spec.loader is None:
        return silent_result(False, failure_message="lifecycle.py missing for SessionStart budget check")
    lifecycle = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(lifecycle)
    context = lifecycle.session_start_context("silent-verify", "activation")
    encoded = context.encode("utf-8")
    budget = int(lifecycle.SESSION_START_MAX_BYTES)
    if len(encoded) > budget:
        return silent_result(
            False,
            failure_message=f"SessionStart budget exceeded: {len(encoded)}>{budget}",
        )
    # Record bytes for metrics (#5653) when project root is available.
    try:
        counters = _load("learning_counters.py")
        counters.record_session_start_bytes(len(encoded), project=project)
    except Exception:  # noqa: BLE001 - metrics must never fail the verify
        pass
    return SUCCESS_EXIT


def verify_learning_session_finalize(session_id: str, *, disposition: str = "issues-first") -> int:
    learning_session = _load("learning_session.py")
    try:
        learning_session.finalize(session_id, disposition=disposition)
    except Exception as error:  # noqa: BLE001
        return silent_result(False, failure_message=str(error))
    return SUCCESS_EXIT


def verify_eval_parity(fixtures: Path | None = None) -> int:
    root = Path(__file__).resolve().parents[1]
    script = root / "scripts" / "ci" / "chaos_engine_eval_parity.py"
    if not script.is_file():
        # Portable install may lack monorepo scripts — treat as skip-success for overlay.
        return SUCCESS_EXIT
    argv = [sys.executable, str(script)]
    if fixtures is not None:
        argv.extend(["--fixtures", str(fixtures)])
    return run_argv(argv, cwd=root)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("session-start-budget")
    final = sub.add_parser("learning-session-finalize")
    final.add_argument("--session-id", required=True)
    final.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    evalp = sub.add_parser("eval-parity")
    evalp.add_argument("--fixtures", type=Path, default=None)
    run = sub.add_parser("run")
    run.add_argument("argv", nargs=argparse.REMAINDER, help="command after --")
    args = parser.parse_args(argv)
    if args.command == "session-start-budget":
        return verify_session_start_budget()
    if args.command == "learning-session-finalize":
        return verify_learning_session_finalize(args.session_id, disposition=args.disposition)
    if args.command == "eval-parity":
        return verify_eval_parity(args.fixtures)
    # run: expect leading --
    command = list(args.argv)
    if command and command[0] == "--":
        command = command[1:]
    return run_argv(command)


if __name__ == "__main__":
    raise SystemExit(main())
