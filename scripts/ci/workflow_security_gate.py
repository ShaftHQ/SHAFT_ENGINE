#!/usr/bin/env python3
"""Workflow security gate: actionlint + zizmor with a ratcheting baseline (#7000).

actionlint must be clean. zizmor high-severity findings are counted per
(workflow file, rule); a count above the committed baseline fails, so
existing debt is tolerated while new findings block. Pass ``--write`` to
lower the baseline after fixing findings (never to absorb new ones).
"""
from __future__ import annotations

import argparse
import collections
import json
import subprocess  # nosec B404 - fixed tool invocations
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / ".github/zizmor-baseline.json"


def finding_keys(findings: list[dict]) -> collections.Counter:
    counts: collections.Counter = collections.Counter()
    for finding in findings:
        locations = finding.get("locations") or [{}]
        symbolic = locations[0].get("symbolic", {})
        local = symbolic.get("key", {}).get("Local", {})
        given = local.get("verbatim_path") or local.get("given_path") or "?"
        counts[f"{Path(given).name}::{finding.get('ident', '?')}"] += 1
    return counts


def regressions(current: collections.Counter, baseline: dict[str, int]) -> list[str]:
    return sorted(f"{key}: {count} > baseline {baseline.get(key, 0)}"
                  for key, count in current.items() if count > baseline.get(key, 0))


def run_zizmor() -> list[dict]:
    result = subprocess.run(  # nosec B603 B607 - fixed args, pinned tool
        ["zizmor", "--offline", "--min-severity", "high", "--format", "json", ".github/workflows"],
        cwd=ROOT, capture_output=True, text=True, check=False)
    return json.loads(result.stdout or "[]")


def run_actionlint(binary: str) -> int:
    # shellcheck integration off: its result varies with the runner image.
    return subprocess.run([binary, "-shellcheck="], cwd=ROOT, check=False).returncode  # nosec B603


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--actionlint", default="actionlint")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args(argv)
    current = finding_keys(run_zizmor())
    if args.write:
        BASELINE.write_text(json.dumps(dict(sorted(current.items())), indent=2) + "\n", encoding="utf-8")
        return 0
    status = 0
    if run_actionlint(args.actionlint) != 0:
        print("::error::actionlint reported findings")
        status = 1
    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    for line in regressions(current, baseline):
        print(f"::error::new zizmor high finding {line}")
        status = 1
    return status


if __name__ == "__main__":
    sys.exit(main())
