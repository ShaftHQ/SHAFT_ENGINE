#!/usr/bin/env python3
"""Select Agent Plugin Live Acceptance jobs for workflow_dispatch (#6283)."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path

MONDAY_SCHEDULE = "15 4 * * 1"
INSTALLER_PART1_GATE = (
    "installer-part-1-windows-2025",
    "installer-part-1-macos-15",
)
GATE_CELLS = {
    "installer-part-1-windows-2025": {"os": "windows-2025", "part": 1},
    "installer-part-1-macos-15": {"os": "macos-15", "part": 1},
}
WEEKLY_JOBS = (
    "deterministic-harness-full",
    "harness-platform-contracts",
    "agnix-conformance",
    "external-guardrail-corpus",
    "native-client-load",
    "chaos-engine-promotion",
)
ALWAYS_JOBS = (
    "chaos-engine-cross-platform",
    "chaos-engine-live-installer",
)
CROSS_OS = ("ubuntu-22.04", "macos-15", "windows-2025")


def parse_job_filter(raw: str) -> list[str]:
    """Split a dispatch `jobs` input. Empty means the caller did not filter."""
    return [part.strip() for part in str(raw or "").split(",") if part.strip()]


def full_cross_matrix() -> list[dict]:
    return [{"os": os_name, "part": part} for os_name in CROSS_OS for part in (1, 2)]


def acceptance_selection(event_name: str, schedule: str, jobs: str) -> dict:
    """Jobs and installer matrix cells this event is allowed to run.

    A workflow_dispatch that names installer part 1 on windows-2025 and
    macos-15 runs only those cells. It does not run the other matrix cells
    or the weekly full harness job. An empty dispatch input keeps today's
    full run. Schedule keeps the Monday-only weekly jobs.
    """
    requested = parse_job_filter(jobs)
    filtered = event_name == "workflow_dispatch" and bool(requested)
    weekly_window = event_name != "schedule" or schedule == MONDAY_SCHEDULE
    if not filtered:
        enabled = set(ALWAYS_JOBS)
        if weekly_window:
            enabled.update(WEEKLY_JOBS)
        cells = full_cross_matrix()
        return {
            "enabled": sorted(enabled),
            "cross_matrix": cells,
            "cross_exclude": [],
        }

    enabled: set[str] = set()
    cells: list[dict] = []
    named_full_cross = False
    for name in requested:
        if name in GATE_CELLS:
            cells.append(dict(GATE_CELLS[name]))
            enabled.add("chaos-engine-cross-platform")
        elif name == "chaos-engine-cross-platform":
            named_full_cross = True
            enabled.add(name)
        elif name in ALWAYS_JOBS or name in WEEKLY_JOBS:
            enabled.add(name)
    if named_full_cross:
        cells = full_cross_matrix()
    elif "chaos-engine-cross-platform" not in enabled:
        cells = []
    return {
        "enabled": sorted(enabled),
        "cross_matrix": cells,
        "cross_exclude": [
            {"os": os_name, "part": part}
            for os_name in CROSS_OS
            for part in (1, 2)
            if {"os": os_name, "part": part} not in cells
        ],
    }


def dispatch_command(repo: str, ref: str, jobs: tuple[str, ...] | list[str]) -> list[str]:
    """`gh workflow run` for exactly the gate names. No extra jobs input."""
    spec = ",".join(jobs)
    return [
        "gh",
        "workflow",
        "run",
        "agent-plugin-acceptance.yml",
        "--repo",
        repo,
        "--ref",
        ref,
        "-f",
        f"jobs={spec}",
    ]


def format_job_conclusions(jobs: list[dict]) -> str:
    """One conclusion line per finished job. In-progress rows are omitted."""
    lines = [
        f"{job.get('name')}: {job.get('conclusion')}"
        for job in jobs
        if str(job.get("conclusion") or "").strip()
    ]
    return "\n".join(lines)


def watch_acceptance_jobs(snapshots: list[list[dict]]) -> str:
    """Report the latest conclusions only. Intermediate polls stay silent."""
    if not snapshots:
        return ""
    return format_job_conclusions(snapshots[-1])


def _output_flag(job_id: str) -> str:
    return "run_" + job_id.replace("-", "_")


def write_github_output(path: Path, selection: dict) -> None:
    enabled = set(selection["enabled"])
    lines = [
        f"{_output_flag(job_id)}={'true' if job_id in enabled else 'false'}"
        for job_id in (*ALWAYS_JOBS, *WEEKLY_JOBS)
    ]
    lines.append("cross_matrix<<EOF")
    lines.append(json.dumps(selection["cross_matrix"]))
    lines.append("EOF")
    lines.append("cross_exclude<<EOF")
    lines.append(json.dumps(selection["cross_exclude"]))
    lines.append("EOF")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--event-name", default=os.environ.get("GITHUB_EVENT_NAME", ""))
    parser.add_argument("--schedule", default=os.environ.get("ACCEPTANCE_SCHEDULE", ""))
    parser.add_argument("--jobs", default=os.environ.get("ACCEPTANCE_JOBS", ""))
    parser.add_argument("--github-output", type=Path)
    arguments = parser.parse_args(argv)
    selection = acceptance_selection(arguments.event_name, arguments.schedule, arguments.jobs)
    if arguments.github_output is not None:
        write_github_output(arguments.github_output, selection)
    else:
        print(json.dumps(selection))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
