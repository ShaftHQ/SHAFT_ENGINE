#!/usr/bin/env python3
"""Summarize and optionally gate JaCoCo CSV coverage metrics."""

import argparse
import csv
import json
from dataclasses import dataclass
from pathlib import Path

METRICS = ("INSTRUCTION", "BRANCH", "LINE", "METHOD")


@dataclass(frozen=True)
class CoverageMetric:
    """Aggregated coverage for one JaCoCo metric."""

    name: str
    missed: int
    covered: int

    @property
    def total(self) -> int:
        return self.missed + self.covered

    @property
    def percent(self) -> float:
        if self.total == 0:
            return 100.0
        return self.covered * 100 / self.total


def load_metrics(csv_path: Path) -> list[CoverageMetric]:
    """Load aggregate JaCoCo metrics from a JaCoCo CSV report."""
    with csv_path.open(encoding="utf-8", newline="") as report:
        rows = list(csv.DictReader(report))

    metrics: list[CoverageMetric] = []
    for metric in METRICS:
        missed_column = f"{metric}_MISSED"
        covered_column = f"{metric}_COVERED"
        missed = sum(int(row[missed_column]) for row in rows)
        covered = sum(int(row[covered_column]) for row in rows)
        metrics.append(CoverageMetric(metric.lower(), missed, covered))
    return metrics


def to_markdown(metrics: list[CoverageMetric], gate_metric: str, minimum: float) -> str:
    """Render a GitHub-friendly Markdown coverage summary."""
    lines = [
        "# JaCoCo Coverage Summary",
        "",
        "| Metric | Covered | Total | Coverage | Gate |",
        "|---|---:|---:|---:|---|",
    ]
    for metric in metrics:
        gate = ""
        if metric.name == gate_metric:
            gate = f"minimum {minimum:.2f}%"
        lines.append(f"| {metric.name} | {metric.covered} | {metric.total} | {metric.percent:.2f}% | {gate} |")
    return "\n".join(lines)


def to_json(metrics: list[CoverageMetric]) -> str:
    """Render coverage metrics as JSON for automation."""
    return json.dumps(
        {
            metric.name: {
                "missed": metric.missed,
                "covered": metric.covered,
                "total": metric.total,
                "percent": round(metric.percent, 4),
            }
            for metric in metrics
        },
        indent=2,
        sort_keys=True,
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Summarize and gate JaCoCo CSV coverage metrics.")
    parser.add_argument("csv_path", type=Path, help="Path to jacoco.csv")
    parser.add_argument("--metric", choices=tuple(metric.lower() for metric in METRICS), default="line", help="Metric to gate")
    parser.add_argument("--minimum", type=float, default=80.0, help="Minimum required percentage for the gated metric")
    parser.add_argument("--format", choices=("markdown", "json"), default="markdown", help="Output format")
    parser.add_argument("--enforce", action="store_true", help="Exit with status 1 when the gated metric is below minimum")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    metrics = load_metrics(args.csv_path)
    print(to_json(metrics) if args.format == "json" else to_markdown(metrics, args.metric, args.minimum))
    gated_metric = next(metric for metric in metrics if metric.name == args.metric)
    if args.enforce and gated_metric.percent < args.minimum:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
