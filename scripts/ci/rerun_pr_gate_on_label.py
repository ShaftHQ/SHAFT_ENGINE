#!/usr/bin/env python3
"""
Re-run PR Gate when a pull request gains the ``ci:installer-macos`` label (#6208).

PR Gate no longer triggers on label events (#6190), and its ``changes`` job
reads the opt-in label live when it starts (#6187). Adding the label therefore
did nothing until the next push. This helper re-runs the newest PR Gate run
for the head commit, cancelling it first when it is still running, so the
fresh ``changes`` job sees the label and adds the macOS installer legs.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess  # nosec B404 - fixed gh argument vectors, never a shell.
import sys
import time
from typing import Any, Callable

WORKFLOW = "pr-gate.yml"
ACTIVE_STATUSES = frozenset({"queued", "in_progress", "waiting", "pending", "requested"})


def plan_action(run: dict[str, Any] | None) -> str:
    """Return ``none``, ``rerun`` or ``cancel-then-rerun`` for the newest run."""
    if not run:
        # No run yet: when it starts, its changes job reads the label itself.
        return "none"
    if run.get("status") in ACTIVE_STATUSES:
        return "cancel-then-rerun"
    return "rerun"


def newest_run(runs: list[dict[str, Any]]) -> dict[str, Any] | None:
    """Pick the most recently created run, or ``None``."""
    return max(runs, key=lambda run: str(run.get("created_at", "")), default=None)


class Gh:
    """Thin wrapper over an absolute-path ``gh`` executable."""

    def __init__(self, dry_run: bool = False) -> None:
        """Resolve ``gh`` once; ``dry_run`` prints mutating calls instead."""
        self.dry_run = dry_run
        self.executable = shutil.which("gh") or "gh"

    def __call__(self, *args: str, mutate: bool = False) -> str:
        """Run one ``gh`` call and return its stdout."""
        argv = [self.executable, *args]
        if self.dry_run and mutate:
            print("dry-run: " + " ".join(argv))
            return ""
        completed = subprocess.run(  # nosec B603 - fixed argument vector, no shell.
            argv, check=True, capture_output=True, text=True
        )
        return completed.stdout


def pr_gate_runs(gh: Callable[..., str], repo: str, head_sha: str) -> list[dict[str, Any]]:
    """PR Gate ``pull_request`` runs for one head commit."""
    payload = gh(
        "api",
        f"repos/{repo}/actions/workflows/{WORKFLOW}/runs?head_sha={head_sha}&event=pull_request&per_page=20",
    )
    return list(json.loads(payload or "{}").get("workflow_runs", []))


def wait_until_completed(
    gh: Callable[..., str],
    repo: str,
    run_id: int,
    *,
    timeout: float = 300,
    interval: float = 10,
    sleep: Callable[[float], None] = time.sleep,
) -> bool:
    """Poll one run until GitHub reports it completed (cancellation is asynchronous)."""
    waited = 0.0
    while waited <= timeout:
        status = gh("api", f"repos/{repo}/actions/runs/{run_id}", "--jq", ".status").strip()
        if status == "completed":
            return True
        sleep(interval)
        waited += interval
    return False


def main(argv: list[str] | None = None) -> int:
    """Cancel (if running) and re-run the newest PR Gate run for ``--head-sha``."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[1])
    parser.add_argument("--repo", required=True)
    parser.add_argument("--head-sha", required=True)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    gh = Gh(dry_run=args.dry_run)
    run = newest_run(pr_gate_runs(gh, args.repo, args.head_sha))
    action = plan_action(run)
    print(f"installer-macos rerun: {action}" + (f" (run {run['id']})" if run else ""))
    if action == "none":
        return 0
    run_id = int(run["id"])
    if action == "cancel-then-rerun":
        gh("run", "cancel", str(run_id), "-R", args.repo, mutate=True)
        if not args.dry_run and not wait_until_completed(gh, args.repo, run_id):
            print(f"::error::PR Gate run {run_id} did not finish cancelling in time", file=sys.stderr)
            return 1
    gh("run", "rerun", str(run_id), "-R", args.repo, mutate=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
