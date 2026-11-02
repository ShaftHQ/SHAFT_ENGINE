#!/usr/bin/env python3
"""Detect merge-queue AWAITING_CHECKS stalls with no merge_group runs (#7704).

GitHub sometimes enqueues a PR and never schedules the required merge_group
workflows (PR Gate / Release-note governance). The entry sits in
AWAITING_CHECKS indefinitely and blocks the queue. The known heal is one
dequeue + re-enqueue cycle (see chaos-engine/references/ci-status-economy.md).

This module is the pure predicate + CLI wrapper. The Dependabot auto-merge
schedule can call ``heal`` with live ``gh`` output; unit tests feed fixtures.
"""

from __future__ import annotations

import argparse
import json
import sys
from typing import Any, Mapping, Sequence

REQUIRED_MERGE_GROUP_WORKFLOWS = frozenset({
    "PR Gate",
    "Release-note governance",
})

STALL_STATES = frozenset({"AWAITING_CHECKS", "WAITING"})


def is_stalled_awaiting_checks(
    entry: Mapping[str, Any],
    merge_group_runs: Sequence[Mapping[str, Any]] | None = None,
) -> bool:
    """True when a queue entry waits on checks but no merge_group run started.

    ``entry`` keys used: ``state`` (merge queue entry state), optional
    ``mergeStateStatus``. ``merge_group_runs`` is the list of workflow runs for
    that merge group head with ``event == merge_group``; empty/None means none.
    """
    state = str(entry.get("state") or entry.get("mergeStateStatus") or "").upper()
    if state not in STALL_STATES:
        return False
    runs = list(merge_group_runs or [])
    if not runs:
        return True
    names = {
        str(run.get("name") or run.get("workflowName") or "")
        for run in runs
    }
    return REQUIRED_MERGE_GROUP_WORKFLOWS.isdisjoint(names)


def required_workflows_declare_merge_group(workflows: Mapping[str, Any]) -> list[str]:
    """Return workflow file names that lack an ``on.merge_group`` trigger.

    ``workflows`` maps basename -> parsed YAML mapping (must include ``on``).
    """
    missing: list[str] = []
    for name, document in workflows.items():
        triggers = document.get("on") or document.get(True) or {}
        if not isinstance(triggers, Mapping) or "merge_group" not in triggers:
            missing.append(name)
    return sorted(missing)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    predict = sub.add_parser("predict", help="exit 0 when stalled, 1 when healthy")
    predict.add_argument("--entry-json", required=True, help="path to queue entry JSON")
    predict.add_argument(
        "--runs-json",
        default="",
        help="path to merge_group workflow runs JSON array (default: empty)",
    )

    args = parser.parse_args(argv)
    if args.command == "predict":
        entry = json.loads(open(args.entry_json, encoding="utf-8").read())
        runs: list[dict[str, Any]] = []
        if args.runs_json:
            runs = json.loads(open(args.runs_json, encoding="utf-8").read())
        stalled = is_stalled_awaiting_checks(entry, runs)
        print("stalled" if stalled else "healthy")
        return 0 if stalled else 1
    return 2


if __name__ == "__main__":
    sys.exit(main())
