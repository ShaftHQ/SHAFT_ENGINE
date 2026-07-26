#!/usr/bin/env python3
"""Validate that every GitHub Actions workflow job bounds its own runtime.

Issue #4081: MacOSX_Safari_Local hung after a heap OOM and burned the full
6-hour default job ceiling on a macos-latest runner (10x billing multiplier)
because nothing bounded it. This is the regression guard: it fails whenever
any job in .github/workflows/*.yml has no explicit timeout-minutes. Jobs that
call a reusable workflow via `uses:` are exempt -- GitHub Actions rejects
timeout-minutes on that job shape, the called workflow bounds itself.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS_GLOB = ".github/workflows/*.yml"


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable validation issue."""
    return {"code": code, "path": path, "message": message}


def validate_repository(root: Path = ROOT) -> list[dict[str, str]]:
    """Flag every workflow job that has no explicit timeout-minutes."""
    errors: list[dict[str, str]] = []
    workflows_dir = root / ".github/workflows"
    if not workflows_dir.is_dir():
        return errors
    for workflow_path in sorted(root.glob(WORKFLOWS_GLOB)):
        relative = workflow_path.relative_to(root).as_posix()
        try:
            document = yaml.safe_load(workflow_path.read_text(encoding="utf-8"))
        except yaml.YAMLError as error:
            errors.append(issue("workflow-timeout-parse", relative, str(error)))
            continue
        jobs = (document or {}).get("jobs", {})
        if not isinstance(jobs, dict):
            continue
        for job_name, job in jobs.items():
            if not isinstance(job, dict) or "uses" in job:
                continue
            if "timeout-minutes" not in job:
                errors.append(
                    issue(
                        "workflow-timeout-missing",
                        relative,
                        f"job '{job_name}' has no timeout-minutes",
                    )
                )
    return errors


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    errors = validate_repository(args.root.resolve())
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors}, indent=2))
    elif errors:
        for error in errors:
            print(f"{error['code']}: {error['path']}: {error['message']}", file=sys.stderr)
    else:
        print("All workflow jobs declare timeout-minutes.")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
