#!/usr/bin/env python3
"""Fail when the decision-quality score drops below its committed baseline (#6411).

Score: protective parity fixtures (deny or exit 2) plus calibration correctness.
Override an intended drop with the PR label ``decision-quality-override``.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PARITY = ROOT / "chaos-engine/evals/parity-fixtures.json"
AGGREGATE = ROOT / "chaos-engine/decision-quality-calibration.aggregate.json"
BASELINE = ROOT / "chaos-engine/decision-quality-score.baseline.json"
OVERRIDE_LABEL = "decision-quality-override"


def score(parity: dict, aggregate: dict) -> dict[str, float]:
    """Return the aggregate decision-quality score."""
    protective = sum(
        1
        for fixture in parity.get("fixtures", [])
        if fixture.get("expect", {}).get("decision") == "deny"
        or fixture.get("expect", {}).get("exit_code") == 2
    )
    correctness = float(aggregate.get("metrics", {}).get("chaos-engine", {}).get("correctness", 0.0))
    return {"protective_fixtures": float(protective), "correctness": correctness}


def regressions(current: dict[str, float], baseline: dict) -> list[str]:
    """Name every metric that fell more than the tolerance below the baseline."""
    tolerance = float(baseline.get("tolerance", 0.0))
    return [
        f"{name}: {current.get(name, 0.0)} < baseline {value} - tolerance {tolerance}"
        for name, value in baseline["scores"].items()
        if current.get(name, 0.0) < float(value) - tolerance
    ]


def verdict(current: dict[str, float], baseline: dict, labels: list[str]) -> int:
    """Return 0 when the score holds or the override label is present."""
    found = regressions(current, baseline)
    for line in found:
        print(f"decision-quality regression: {line}", file=sys.stderr)
    return 0 if not found or OVERRIDE_LABEL in labels else 1


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--labels", default="[]", help="JSON list of PR labels")
    args = parser.parse_args(argv)
    current = score(json.loads(PARITY.read_text(encoding="utf-8")), json.loads(AGGREGATE.read_text(encoding="utf-8")))
    return verdict(current, json.loads(BASELINE.read_text(encoding="utf-8")), json.loads(args.labels))


if __name__ == "__main__":
    raise SystemExit(main())
