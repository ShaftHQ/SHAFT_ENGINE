"""Compare two JMH JSON results (#7009): flag benchmarks slower than the baseline by more than a threshold.

Usage: jmh_compare.py BASELINE.json CURRENT.json [--threshold 0.15]
Prints a Markdown table; exits 1 when any benchmark regressed beyond the threshold, 0 otherwise
(including when there is no baseline yet).
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def scores(path: Path) -> dict[str, float]:
    """Benchmark name -> primary score (lower is better for avgt)."""
    return {row["benchmark"].rsplit(".", 1)[-1]: float(row["primaryMetric"]["score"])
            for row in json.loads(path.read_text(encoding="utf-8"))}


def regressions(baseline: dict[str, float], current: dict[str, float], threshold: float) -> list[tuple[str, float]]:
    """(name, ratio) for each shared benchmark whose current/baseline ratio exceeds 1 + threshold."""
    return [(name, current[name] / baseline[name]) for name in sorted(current)
            if name in baseline and baseline[name] > 0 and current[name] / baseline[name] > 1 + threshold]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path)
    parser.add_argument("current", type=Path)
    parser.add_argument("--threshold", type=float, default=0.15)
    args = parser.parse_args(argv)
    current = scores(args.current)
    if not args.baseline.is_file():
        print("No baseline yet; this run becomes the baseline.")
        return 0
    baseline = scores(args.baseline)
    print("| Benchmark | Baseline | Current | Ratio |\n| --- | ---: | ---: | ---: |")
    for name in sorted(current):
        base = baseline.get(name)
        ratio = f"{current[name] / base:.2f}" if base else "new"
        print(f"| {name} | {base if base is not None else '-'} | {current[name]:.3f} | {ratio} |")
    bad = regressions(baseline, current, args.threshold)
    for name, ratio in bad:
        print(f"REGRESSION: {name} is {ratio:.2f}x the baseline (threshold {1 + args.threshold:.2f}x)")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
