#!/usr/bin/env python3
"""Fail the PR gate when Codecov patch/project status on the PR head is red (#6990).

Codecov statuses cannot be ruleset-required: Codecov does not report on
merge_group, so a required Codecov context would hang the merge queue. The
required ``PR Gate Summary`` runs this on pull_request only; merge_group skips
it. Polling is bounded: an absent status (no upload) or one still pending at
the deadline fails when the PR changes Java main sources and passes with a
warning otherwise; the poll is bounded, so the gate never hangs. Codecov
re-notifies per upload, so a state must settle before it counts.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import time
import urllib.request

CONTEXTS = ("codecov/patch", "codecov/project")


def _wanted(name: str) -> bool:
    return any(name == c or name.startswith(c + "/") for c in CONTEXTS)


def latest_states(check_runs: list[dict], statuses: list[dict]) -> dict[str, str]:
    """Newest state per Codecov context.

    The Codecov GitHub App reports check runs; the legacy integration posts
    commit statuses. Both are read; check runs win. Values use the status
    vocabulary: success, failure, error, pending.
    """
    states: dict[str, str] = {}
    for status in statuses:  # API lists newest first
        context = status.get("context", "")
        if _wanted(context) and context not in states:
            states[context] = status.get("state", "")
    for run in check_runs:
        name = run.get("name", "")
        if not _wanted(name):
            continue
        if run.get("status") != "completed":
            states[name] = "pending"
        elif run.get("conclusion") in ("success", "neutral", "skipped"):
            states[name] = "success"
        else:
            states[name] = "failure"
    return states


def verdict(states: dict[str, str]) -> tuple[str, list[str]]:
    """Return ('fail'|'pending'|'pass', failing contexts)."""
    failing = sorted(c for c, s in states.items() if s in ("failure", "error"))
    if failing:
        return "fail", failing
    if not states or any(s == "pending" for s in states.values()):
        return "pending", []
    return "pass", []


def _get(url: str, token: str):
    request = urllib.request.Request(
        url, headers={"Authorization": f"Bearer {token}", "Accept": "application/vnd.github+json"}
    )
    with urllib.request.urlopen(request, timeout=30) as response:  # nosec B310 - fixed https host
        return json.load(response)


def is_java_main(path: str) -> bool:
    """A production Java source: its coverage must be reported."""
    return path.endswith(".java") and "src/main/" in path


def pr_changes_java_main(repo: str, pr: int, token: str) -> bool:
    page = 1
    while True:
        files = _get(f"https://api.github.com/repos/{repo}/pulls/{pr}/files?per_page=100&page={page}", token)
        if any(is_java_main(f.get("filename", "")) for f in files):
            return True
        if len(files) < 100:
            return False
        page += 1


def fetch(repo: str, sha: str, token: str) -> dict[str, str]:
    base = f"https://api.github.com/repos/{repo}/commits/{sha}"
    runs = _get(f"{base}/check-runs?per_page=100", token).get("check_runs", [])
    return latest_states(runs, _get(f"{base}/statuses?per_page=100", token))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY", ""))
    parser.add_argument("--sha", required=True)
    parser.add_argument("--timeout", type=int, default=240)
    parser.add_argument("--pr", type=int, default=0,
                        help="PR number; a PR touching Java main sources must get a final Codecov verdict")
    parser.add_argument("--interval", type=int, default=15)
    parser.add_argument("--settle", type=int, default=60,
                        help="seconds a final state must stay unchanged; Codecov re-notifies per upload")
    args = parser.parse_args(argv)
    token = os.environ.get("GITHUB_TOKEN", "")
    strict = bool(args.pr) and pr_changes_java_main(args.repo, args.pr, token)
    deadline = time.monotonic() + args.timeout
    seen: dict[str, str] | None = None
    stable_since = time.monotonic()
    while True:
        states = fetch(args.repo, args.sha, token)
        now = time.monotonic()
        if states != seen:
            seen, stable_since = states, now
        result, failing = verdict(states)
        settled = now - stable_since >= args.settle
        if result == "fail" and (settled or now >= deadline):
            print(f"::error::Codecov red on PR head: {', '.join(failing)} (patch target 80%, project threshold 0.5%). Add tests, then push.")
            return 1
        if result == "pass" and settled:
            print(f"Codecov green: {states}")
            return 0
        if now >= deadline:
            if result == "pass":
                print(f"Codecov green (not settled): {states}")
            elif strict:
                print(f"::error::PR changes Java main sources but Codecov is not final ({states or 'none posted'}). Re-run the gate once Codecov reports.")
                return 1
            else:
                print(f"::warning::Codecov not final before deadline ({states or 'none posted'}); no Java main change, not blocking")
            return 0
        time.sleep(args.interval)


if __name__ == "__main__":
    sys.exit(main())
