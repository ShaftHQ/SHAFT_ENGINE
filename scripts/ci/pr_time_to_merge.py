#!/usr/bin/env python3
"""PR time-to-merge report and alert (#6990 / #7184).

Reads the last N merged pull requests (``gh api`` / GraphQL) and reports:

* **Primary (breach):** median last-commit → merge minutes for non-bot authors
  (aligns with the ci-cd skill target for auto-merge PRs once the final push is
  up). Open→merge includes human review waits that auto-merge PRs skip.
* Open→merge for all authors and for non-bot authors (context).
* Average PR Gate ``pull_request`` workflow runs per sampled PR.

Exits 2 when the primary median breaches the target (default 15 minutes) so the
scheduled workflow can alert.
"""

from __future__ import annotations

import argparse
import json
import shutil
import statistics
import subprocess  # nosec B404 - fixed gh argument vector, never a shell.
import sys
from datetime import datetime

TARGET_MEDIAN_MINUTES = 15.0
BOT_LOGINS = frozenset(
    {
        "dependabot",
        "dependabot[bot]",
        "renovate",
        "renovate[bot]",
        "github-actions[bot]",
        "copilot-swe-agent[bot]",
    }
)


def _ts(value: str) -> datetime:
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def author_login(pr: dict) -> str:
    user = pr.get("user") or pr.get("author") or {}
    if isinstance(user, dict):
        return str(user.get("login") or "")
    return str(user or "")


def is_bot(pr: dict) -> bool:
    login = author_login(pr).casefold()
    if login in {item.casefold() for item in BOT_LOGINS}:
        return True
    return login.endswith("[bot]")


def minutes_between(start: str | None, end: str | None) -> float | None:
    if not start or not end:
        return None
    minutes = (_ts(end) - _ts(start)).total_seconds() / 60
    # Clock skew / bad GraphQL timestamps can yield negatives; exclude them so
    # they cannot pull the median down (#7184 Coach).
    if minutes < 0:
        return None
    return minutes


def minutes_to_merge(prs: list[dict]) -> list[float]:
    values = []
    for pr in prs:
        minutes = minutes_between(pr.get("created_at") or pr.get("createdAt"), pr.get("merged_at") or pr.get("mergedAt"))
        if minutes is not None:
            values.append(minutes)
    return sorted(values)


def last_commit_to_merge(prs: list[dict]) -> list[float]:
    values = []
    for pr in prs:
        start = pr.get("last_commit_at") or pr.get("lastCommitAt")
        end = pr.get("merged_at") or pr.get("mergedAt")
        minutes = minutes_between(start, end)
        if minutes is not None:
            values.append(minutes)
    return sorted(values)


def percentile(values: list[float], fraction: float) -> float:
    if not values:
        return 0.0
    index = min(len(values) - 1, max(0, round(fraction * (len(values) - 1))))
    return values[index]


def _bucket(values: list[float], target: float) -> dict:
    median = statistics.median(values) if values else 0.0
    return {
        "count": len(values),
        "median_minutes": round(median, 1),
        "p90_minutes": round(percentile(values, 0.9), 1),
        "target_median_minutes": target,
        "breach": bool(values) and median > target,
    }


def summarize(
    prs: list[dict],
    target: float = TARGET_MEDIAN_MINUTES,
    gate_runs: dict[int, int] | None = None,
) -> dict:
    human = [pr for pr in prs if not is_bot(pr)]
    primary = _bucket(last_commit_to_merge(human), target)
    # Fall back to open→merge non-bot when last-commit dates are absent (legacy --input).
    if primary["count"] == 0:
        primary = _bucket(minutes_to_merge(human or prs), target)
        primary["metric"] = "open_to_merge_fallback"
    else:
        primary["metric"] = "last_commit_to_merge_non_bot"

    open_all = _bucket(minutes_to_merge(prs), target)
    open_human = _bucket(minutes_to_merge(human), target)

    avg_runs = 0.0
    if gate_runs and primary["count"]:
        numbers = [int(pr["number"]) for pr in human if pr.get("number") is not None]
        if numbers:
            avg_runs = round(sum(gate_runs.get(n, 0) for n in numbers) / len(numbers), 2)

    return {
        "count": primary["count"],
        "median_minutes": primary["median_minutes"],
        "p90_minutes": primary["p90_minutes"],
        "target_median_minutes": target,
        "breach": primary["breach"],
        "metric": primary["metric"],
        "open_to_merge_all": open_all,
        "open_to_merge_non_bot": open_human,
        "avg_pr_gate_runs": avg_runs,
    }


def render(summary: dict) -> str:
    status = "BREACH" if summary["breach"] else "OK"
    open_all = summary.get("open_to_merge_all") or {}
    open_human = summary.get("open_to_merge_non_bot") or {}
    return (
        "## PR time to merge\n\n"
        f"Primary metric: `{summary.get('metric', 'last_commit_to_merge_non_bot')}` "
        "(skill: ≤15 min for auto-merge / ready PRs).\n\n"
        "| Slice | PRs | Median (min) | p90 (min) | Target | Status |\n"
        "|---|---|---|---|---|---|\n"
        f"| last-commit→merge (non-bot) | {summary['count']} | {summary['median_minutes']} | "
        f"{summary['p90_minutes']} | {summary['target_median_minutes']} | {status} |\n"
        f"| open→merge (non-bot) | {open_human.get('count', 0)} | {open_human.get('median_minutes', 0)} | "
        f"{open_human.get('p90_minutes', 0)} | {summary['target_median_minutes']} | "
        f"{'BREACH' if open_human.get('breach') else 'OK'} |\n"
        f"| open→merge (all) | {open_all.get('count', 0)} | {open_all.get('median_minutes', 0)} | "
        f"{open_all.get('p90_minutes', 0)} | {summary['target_median_minutes']} | "
        f"{'BREACH' if open_all.get('breach') else 'OK'} |\n\n"
        f"Average PR Gate pull_request runs per sampled non-bot PR: "
        f"{summary.get('avg_pr_gate_runs', 0)}\n"
    )


def _run_gh(args: list[str]) -> str:
    gh = shutil.which("gh")
    if gh is None:
        raise SystemExit("gh CLI not found")
    return subprocess.run(  # nosec B603 - fixed argv, resolved executable
        [gh, *args],
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def fetch_merged(repo: str, limit: int) -> list[dict]:
    """Newest merged PRs by merged_at, with last commit timestamp."""
    owner, _, name = repo.partition("/")
    query = """
    query($owner: String!, $name: String!) {
      repository(owner: $owner, name: $name) {
        pullRequests(states: MERGED, first: 100, orderBy: {field: UPDATED_AT, direction: DESC}) {
          nodes {
            number
            createdAt
            mergedAt
            author { login }
            headRefName
            commits(last: 1) { nodes { commit { committedDate } } }
          }
        }
      }
    }
    """
    raw = _run_gh(
        [
            "api",
            "graphql",
            "-f",
            f"query={query}",
            "-F",
            f"owner={owner}",
            "-F",
            f"name={name}",
        ]
    )
    nodes = json.loads(raw)["data"]["repository"]["pullRequests"]["nodes"]
    prs = []
    for node in nodes:
        if not node.get("mergedAt"):
            continue
        commits = (node.get("commits") or {}).get("nodes") or []
        last = commits[-1]["commit"]["committedDate"] if commits else None
        prs.append(
            {
                "number": node["number"],
                "created_at": node["createdAt"],
                "merged_at": node["mergedAt"],
                "user": {"login": (node.get("author") or {}).get("login") or ""},
                "last_commit_at": last,
                "head_branch": node.get("headRefName") or "",
            }
        )
    prs.sort(key=lambda item: item["merged_at"], reverse=True)
    return prs[:limit]


def fetch_gate_runs(repo: str, prs: list[dict]) -> dict[int, int]:
    """Count recent PR Gate pull_request runs per sampled PR.

    Completed runs often omit ``pull_requests`` after the branch is deleted, so
    match on ``head_branch`` and on a ``(#N)`` title suffix.
    """
    numbers = {int(pr["number"]) for pr in prs if pr.get("number") is not None}
    if not numbers:
        return {}
    counts = {number: 0 for number in numbers}
    branch_to_number = {
        str(pr.get("head_branch") or ""): int(pr["number"])
        for pr in prs
        if pr.get("number") is not None and pr.get("head_branch")
    }
    try:
        raw = _run_gh(
            [
                "api",
                f"repos/{repo}/actions/workflows/pr-gate.yml/runs"
                "?event=pull_request&per_page=100&status=completed",
            ]
        )
    except subprocess.CalledProcessError:
        return counts
    for run in json.loads(raw).get("workflow_runs") or []:
        matched = None
        for pull in run.get("pull_requests") or []:
            number = pull.get("number")
            if number in counts:
                matched = number
                break
        if matched is None:
            branch = run.get("head_branch") or ""
            matched = branch_to_number.get(branch)
        if matched is None:
            title = run.get("display_title") or ""
            for number in numbers:
                if f"(#{number})" in title:
                    matched = number
                    break
        if matched is not None:
            counts[matched] += 1
    return counts


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--limit", type=int, default=30)
    parser.add_argument("--target", type=float, default=TARGET_MEDIAN_MINUTES)
    parser.add_argument("--input", help="JSON file of PRs instead of gh api (tests)")
    args = parser.parse_args(argv)
    gate_runs: dict[int, int] | None = None
    if args.input:
        with open(args.input, encoding="utf-8") as handle:
            prs = json.load(handle)
    else:
        prs = fetch_merged(args.repo, args.limit)
        gate_runs = fetch_gate_runs(args.repo, [pr for pr in prs if not is_bot(pr)])
    summary = summarize(prs, args.target, gate_runs)
    print(render(summary))
    print(json.dumps(summary))
    return 2 if summary["breach"] else 0


if __name__ == "__main__":
    sys.exit(main())
