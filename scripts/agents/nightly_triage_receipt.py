#!/usr/bin/env python3
"""Decide whether a scheduled-nightly triage is still fresh.

The helper does not call GitHub. The agent passes the last receipt and the
latest schedule run ids it already fetched. A fresh SKIP means do not refetch
job logs and do not open a product fix.

    python3 scripts/agents/nightly_triage_receipt.py \
        --receipt receipt.json --runs runs.json --now 2026-10-18T18:08:10Z

``receipt.json`` holds ``run_ids``, ``decision``, and ``next_cron``.
``runs.json`` holds the same ``run_ids`` object. Both timestamps are UTC
``YYYY-MM-DDTHH:MM:SSZ``. Prints SKIP or TRIAGE.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def freshness(receipt: dict | None, run_ids: dict, now: str) -> str:
    """Return SKIP when the receipt still answers the next decision, else TRIAGE."""
    if not isinstance(receipt, dict):
        return "TRIAGE"
    if receipt.get("run_ids") != run_ids:
        return "TRIAGE"
    decision = receipt.get("decision")
    next_cron = receipt.get("next_cron")
    if not isinstance(decision, str) or not decision.strip():
        return "TRIAGE"
    if not isinstance(next_cron, str) or not next_cron.strip():
        return "TRIAGE"
    if now >= next_cron:
        return "TRIAGE"
    return "SKIP"


def _load(path: str) -> dict:
    payload = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise SystemExit(f"{path} must be a JSON object")
    return payload


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--receipt", required=True, help="JSON file with run_ids, decision, next_cron")
    parser.add_argument("--runs", required=True, help="JSON file with the current run_ids object")
    parser.add_argument("--now", required=True, help="Current UTC time as YYYY-MM-DDTHH:MM:SSZ")
    args = parser.parse_args(argv)
    receipt = _load(args.receipt)
    runs = _load(args.runs)
    run_ids = runs.get("run_ids", runs)
    if not isinstance(run_ids, dict):
        raise SystemExit("--runs must be a run_ids object or an object containing run_ids")
    print(freshness(receipt, run_ids, args.now))
    return 0


if __name__ == "__main__":
    sys.exit(main())
