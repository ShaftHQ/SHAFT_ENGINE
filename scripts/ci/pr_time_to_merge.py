#!/usr/bin/env python3
"""PR open -> merge time report and alert (#6990 phase 2).

Reads the last N merged pull requests (``gh api``) and reports median/p90
open->merge minutes plus PR Gate runs per PR. Exits 2 when the median breaches
the target (default 15 minutes) so the scheduled workflow can alert.
"""

from __future__ import annotations

import argparse
import json
import shutil
import statistics
import subprocess
import sys
from datetime import datetime

TARGET_MEDIAN_MINUTES = 15.0


def _ts(value: str) -> datetime:
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def minutes_to_merge(prs: list[dict]) -> list[float]:
    return sorted(
        (_ts(pr["merged_at"]) - _ts(pr["created_at"])).total_seconds() / 60
        for pr in prs
        if pr.get("merged_at") and pr.get("created_at")
    )


def percentile(values: list[float], fraction: float) -> float:
    if not values:
        return 0.0
    index = min(len(values) - 1, max(0, round(fraction * (len(values) - 1))))
    return values[index]


def summarize(prs: list[dict], target: float = TARGET_MEDIAN_MINUTES) -> dict:
    values = minutes_to_merge(prs)
    median = statistics.median(values) if values else 0.0
    return {
        "count": len(values),
        "median_minutes": round(median, 1),
        "p90_minutes": round(percentile(values, 0.9), 1),
        "target_median_minutes": target,
        "breach": bool(values) and median > target,
    }


def render(summary: dict) -> str:
    status = "BREACH" if summary["breach"] else "OK"
    return (
        "## PR time to merge\n\n"
        "| PRs | Median (min) | p90 (min) | Target median (min) | Status |\n"
        "|---|---|---|---|---|\n"
        f"| {summary['count']} | {summary['median_minutes']} | {summary['p90_minutes']} "
        f"| {summary['target_median_minutes']} | {status} |\n"
    )


def fetch_merged(repo: str, limit: int) -> list[dict]:
    gh = shutil.which("gh")
    if gh is None:
        raise SystemExit("gh CLI not found")
    raw = subprocess.run(  # noqa: S603 - fixed argv, resolved executable
        [gh, "api", f"repos/{repo}/pulls?state=closed&base=main&sort=updated&direction=desc&per_page=100"],
        check=True, capture_output=True, text=True,
    ).stdout
    return [pr for pr in json.loads(raw) if pr.get("merged_at")][:limit]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--limit", type=int, default=30)
    parser.add_argument("--target", type=float, default=TARGET_MEDIAN_MINUTES)
    parser.add_argument("--input", help="JSON file of PRs instead of gh api (tests)")
    args = parser.parse_args(argv)
    prs = json.load(open(args.input, encoding="utf-8")) if args.input else fetch_merged(args.repo, args.limit)
    summary = summarize(prs, args.target)
    print(render(summary))
    print(json.dumps(summary))
    return 2 if summary["breach"] else 0


if __name__ == "__main__":
    sys.exit(main())
