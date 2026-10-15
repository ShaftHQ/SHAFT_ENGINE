#!/usr/bin/env python3
"""
React to a post-merge PR Gate run on ``main`` (issue #6185).

A red leg files, or updates, one rolling issue per failing leg (label
``ci-main-red``, assigned to the owner). A leg that passes again on a later
``main`` run closes its issue. The reaction never opens a revert: main rolls
forward, and the rolling issue is the tracker (#6990, nightly-tracker pattern). ChaosEngine installs from ``main``, so the
post-merge run is the only safety net for legs that no longer run on PRs.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess  # nosec B404 - fixed gh/git argument vectors, never a shell.
import sys
from dataclasses import dataclass
from typing import Any

LABEL = "ci-main-red"
OWNER = "MohabMohie"
TITLE_PREFIX = "Main red: "
IGNORED_JOBS = frozenset({"PR Gate Summary", "Main red reaction"})
FAILED_CONCLUSIONS = frozenset({"failure", "timed_out"})


@dataclass(frozen=True)
class Job:
    name: str
    conclusion: str
    url: str


@dataclass(frozen=True)
class Plan:
    create: tuple[Job, ...]
    comment: tuple[tuple[int, Job], ...]
    close: tuple[tuple[int, str], ...]


def parse_jobs(lines: str) -> list[Job]:
    """Parse ``gh api --jq`` JSON lines of ``{name, conclusion, html_url}``."""
    jobs = []
    for line in lines.splitlines():
        if not line.strip():
            continue
        item = json.loads(line)
        jobs.append(
            Job(
                str(item.get("name") or ""),
                str(item.get("conclusion") or ""),
                str(item.get("html_url") or ""),
            )
        )
    return jobs


def issue_title(leg: str) -> str:
    return TITLE_PREFIX + leg


def leg_from_title(title: str) -> str | None:
    return title[len(TITLE_PREFIX):] if title.startswith(TITLE_PREFIX) else None


def build_plan(jobs: list[Job], open_issues: dict[str, int]) -> Plan:
    """Decide issue actions from this run's job conclusions."""
    legs = [job for job in jobs if job.name and job.name not in IGNORED_JOBS]
    failing = [job for job in legs if job.conclusion in FAILED_CONCLUSIONS]
    failing_names = {job.name for job in failing}
    passing = {job.name for job in legs if job.conclusion == "success"} - failing_names
    create = tuple(job for job in failing if issue_title(job.name) not in open_issues)
    comment = tuple(
        (open_issues[issue_title(job.name)], job)
        for job in failing
        if issue_title(job.name) in open_issues
    )
    close = tuple(
        (number, leg)
        for title, number in sorted(open_issues.items())
        if (leg := leg_from_title(title)) is not None and leg in passing
    )
    return Plan(create, comment, close)


def issue_body(job: Job, context: dict[str, str]) -> str:
    return "\n".join(
        (
            f"Post-merge **PR Gate** leg `{job.name}` failed on `main`.",
            "",
            f"- Run: {context['run_url']} (job: {job.url or 'n/a'})",
            f"- Merge commit: {context['sha']}",
            f"- Originating PR: {context['pr'] or 'unknown'}",
            "",
            "Treat as P0: ChaosEngine adopters install from `main`. Roll forward with "
            "a fix; never revert. This issue closes automatically when the leg "
            "passes again on a later `main` run.",
        )
    )


class Runner:
    """Thin wrapper over absolute-path ``gh``/``git`` invocations."""

    def __init__(self, dry_run: bool = False) -> None:
        """Resolve ``gh``/``git`` once; ``dry_run`` prints mutating calls instead."""
        self.dry_run = dry_run
        self.gh = shutil.which("gh") or "gh"
        self.git = shutil.which("git") or "git"

    def run(
        self, argv: list[str], *, env: dict[str, str] | None = None, mutate: bool = False
    ) -> str:
        if self.dry_run and mutate:
            print("dry-run: " + " ".join(argv))
            return ""
        completed = subprocess.run(  # nosec B603 - fixed argument vector, no shell.
            argv,
            check=True,
            capture_output=True,
            text=True,
            env={**os.environ, **(env or {})},
        )
        return completed.stdout.strip()


def fetch_jobs(runner: Runner, repo: str, run_id: str, attempt: str) -> list[Job]:
    output = runner.run(
        [
            runner.gh, "api", "--paginate",
            f"repos/{repo}/actions/runs/{run_id}/attempts/{attempt}/jobs",
            "--jq", ".jobs[] | {name, conclusion, html_url}",
        ]
    )
    return parse_jobs(output)


def fetch_open_issues(runner: Runner, repo: str) -> dict[str, int]:
    output = runner.run(
        [
            runner.gh, "issue", "list", "--repo", repo, "--state", "open",
            "--label", LABEL, "--limit", "100", "--json", "number,title",
        ]
    )
    return {item["title"]: int(item["number"]) for item in json.loads(output or "[]")}


def originating_pr(runner: Runner, repo: str, sha: str) -> str:
    try:
        output = runner.run(
            [
                runner.gh, "api", f"repos/{repo}/commits/{sha}/pulls",
                "--jq", '.[0] | select(.) | "#\\(.number) \\(.title)"',
            ]
        )
    except subprocess.CalledProcessError:
        return ""
    return output


def apply_plan(runner: Runner, repo: str, plan: Plan, context: dict[str, str]) -> None:
    if plan.create or plan.comment:
        runner.run(
            [
                runner.gh, "label", "create", LABEL, "--repo", repo, "--force",
                "--color", "B60205",
                "--description", "Post-merge PR Gate leg failed on main (P0)",
            ],
            mutate=True,
        )
    for job in plan.create:
        runner.run(
            [
                runner.gh, "issue", "create", "--repo", repo,
                "--title", issue_title(job.name), "--label", LABEL,
                "--assignee", OWNER, "--body", issue_body(job, context),
            ],
            mutate=True,
        )
    for number, job in plan.comment:
        runner.run(
            [
                runner.gh, "issue", "comment", str(number), "--repo", repo,
                "--body", "Still failing.\n\n" + issue_body(job, context),
            ],
            mutate=True,
        )
    for number, leg in plan.close:
        runner.run(
            [
                runner.gh, "issue", "close", str(number), "--repo", repo,
                "--reason", "completed", "--comment",
                f"Recovered: `{leg}` passed on {context['run_url']} ({context['sha']}).",
            ],
            mutate=True,
        )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--run-attempt", default="1")
    parser.add_argument("--sha", required=True)
    parser.add_argument("--server-url", default="https://github.com")
    parser.add_argument("--dry-run", action="store_true")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    runner = Runner(dry_run=args.dry_run)
    jobs = fetch_jobs(runner, args.repo, args.run_id, args.run_attempt)
    plan = build_plan(jobs, fetch_open_issues(runner, args.repo))
    context: dict[str, Any] = {
        "run_url": f"{args.server_url}/{args.repo}/actions/runs/{args.run_id}",
        "sha": args.sha,
        "pr": originating_pr(runner, args.repo, args.sha),
    }
    apply_plan(runner, args.repo, plan, context)
    print(
        f"main-red-reaction create={len(plan.create)} comment={len(plan.comment)} "
        f"close={len(plan.close)}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
