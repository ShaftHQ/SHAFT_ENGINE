#!/usr/bin/env python3
"""Provably-bounded watcher for a PR's GitHub checks."""

# AGENTS.md bans unbounded Stop-hook looping on Windows (no ``ralph-loop``,
# Maven fork-storm risk); this script's termination safety is by
# construction, not by convention: the poll loop is a plain ``for`` over
# ``range(max_polls)`` (never ``while True``), and ``max_polls`` is
# hard-clamped to ``HARD_MAX_POLLS`` even when a caller asks for more.
#
# WHAT IT DOES:
#     Polls ``gh pr checks <pr> --json name,state,link`` for the given (or
#     auto-resolved current-branch) PR at most ``--max-polls`` times,
#     sleeping ``--interval`` seconds between polls. If the installed
#     ``gh`` does not support ``--json`` on ``pr checks``, falls back to
#     ``gh api repos/<repo>/commits/<head-sha>/check-runs`` using the head
#     SHA from ``gh pr view --json headRefOid``.
#
# CLASSIFICATION (per poll):
#     RED     -- any check reports a failed/cancelled/timed-out-shaped
#                state (see ``RED_STATES``; includes ``ACTION_REQUIRED``,
#                which GitHub Apps such as Codacy use for a
#                completed-but-failing run).
#     GREEN   -- all checks are completed and none are RED.
#     PENDING -- otherwise (some check is still queued/running); sleep
#                ``--interval`` seconds and poll again, unless the poll
#                budget is exhausted.
#
# EXIT CONTRACT:
#     0   GREEN. Prints "all checks green" to stdout.
#     1   RED. Prints EXACTLY one JSON object to stdout:
#         ``{"failingJobs": [{"name": "...", "runUrl": "..."}]}`` -- this is
#         the ``args`` shape ``.claude/workflows/shaft-release-ci-fix.js``
#         consumes (see its ``args.failingJobs`` footer comment).
#     2   Still PENDING after ``max_polls`` polls. Prints
#         "timed out waiting, N checks still pending" to stderr.
#     3   Any ``gh``/environment error (gh missing, repo/PR unresolvable,
#         malformed JSON, etc.). Prints the error to stderr.
#
# Usage:
#     python3 scripts/ci/watch_pr_checks.py                  # current branch's PR
#     python3 scripts/ci/watch_pr_checks.py --pr 3368         # explicit PR
#     python3 scripts/ci/watch_pr_checks.py --poll-once       # single poll, no sleep
#     python3 scripts/ci/watch_pr_checks.py --max-polls 5 --interval 30

from __future__ import annotations

import argparse
import json
import re
import shutil
# subprocess is used only for read-only `gh`/`git` invocations below, always
# as a list of args (never shell=True) with the executable resolved to an
# absolute path via shutil.which.
import subprocess  # nosec B404
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

DEFAULT_MAX_POLLS = 20
HARD_MAX_POLLS = 60
DEFAULT_INTERVAL_SECONDS = 60
MIN_INTERVAL_SECONDS = 10
MAX_INTERVAL_SECONDS = 600

# gh's `state` field (and our normalization of the check-runs API fallback)
# use these upper-case vocabularies. ACTION_REQUIRED, STARTUP_FAILURE and
# STALE are completed-but-bad conclusions GitHub Apps use in place of a
# plain "failure" and are treated as RED for the same reason a plain
# failure is.
RED_STATES = {
    "FAILURE",
    "CANCELLED",
    "TIMED_OUT",
    "ACTION_REQUIRED",
    "STARTUP_FAILURE",
    "STALE",
}
PENDING_STATES = {
    "PENDING",
    "IN_PROGRESS",
    "QUEUED",
    "EXPECTED",
    "REQUESTED",
    "WAITING",
}

_GIT_URL_RE = re.compile(r"[:/]([^/:]+)/([^/]+?)(?:\.git)?/?$")


class CheckWatchError(RuntimeError):
    """Raised for gh/environment failures that map to exit code 3."""


def resolve_gh() -> str:
    """Resolve the gh executable via shutil.which or raise."""
    gh_executable = shutil.which("gh")
    if gh_executable is None:
        raise CheckWatchError("gh is not on PATH")
    return gh_executable


def run_gh(gh_executable: str, args: list[str], root: Path) -> subprocess.CompletedProcess:
    """Run one gh command with list-args and return the completed process."""
    # Executable is an absolute path from shutil.which; every element of
    # `args` is built internally from validated CLI flags/numbers, never
    # raw untrusted text, and no shell is involved.
    return subprocess.run(  # nosec B603
        [gh_executable, *args],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )


def parse_owner_repo_from_git_url(url: str) -> str:
    """Extract owner/repo from an SSH or HTTPS git remote URL."""
    match = _GIT_URL_RE.search(url.strip())
    if not match:
        raise CheckWatchError(f"cannot parse owner/repo from git remote url: {url!r}")
    owner, name = match.group(1), match.group(2)
    return f"{owner}/{name}"


def resolve_repo(gh_executable: str, root: Path, explicit_repo: str | None) -> str:
    """Resolve the owner/repo slug to operate against."""
    # Prefers --repo if given, then `gh repo view --json nameWithOwner`,
    # then a parse of `git remote get-url origin`.
    if explicit_repo:
        return explicit_repo

    proc = run_gh(gh_executable, ["repo", "view", "--json", "nameWithOwner"], root)
    if proc.returncode == 0:
        try:
            return json.loads(proc.stdout)["nameWithOwner"]
        except (json.JSONDecodeError, KeyError):
            pass  # fall through to the git-remote fallback below

    git_executable = shutil.which("git")
    if git_executable is None:
        raise CheckWatchError(
            "cannot resolve repo: `gh repo view` failed and git is not on PATH"
        )
    # Same list-args/no-shell justification as run_gh above.
    remote = subprocess.run(  # nosec B603
        [git_executable, "remote", "get-url", "origin"],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    if remote.returncode != 0:
        raise CheckWatchError(
            "cannot resolve repo: `gh repo view` and `git remote get-url origin` both failed"
        )
    return parse_owner_repo_from_git_url(remote.stdout)


def resolve_pr_number(gh_executable: str, root: Path, repo: str, explicit_pr: int | None) -> int:
    """Resolve the PR number: explicit value, else the current branch's open PR."""
    if explicit_pr is not None:
        return explicit_pr
    proc = run_gh(gh_executable, ["pr", "view", "--repo", repo, "--json", "number"], root)
    if proc.returncode != 0:
        raise CheckWatchError(
            "cannot resolve current branch's PR (pass --pr explicitly): "
            f"{proc.stderr.strip() or proc.stdout.strip()}"
        )
    try:
        return int(json.loads(proc.stdout)["number"])
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise CheckWatchError(f"unexpected `gh pr view` output: {error}") from error


def normalize_check_run(run: dict) -> dict:
    """Normalize one GitHub check-runs API entry to the {name,state,link} shape."""
    status = str(run.get("status") or "").lower()
    if status != "completed":
        state = "IN_PROGRESS"
    else:
        state = str(run.get("conclusion") or "NEUTRAL").upper()
    return {
        "name": run.get("name", ""),
        "state": state,
        "link": run.get("html_url", ""),
    }


def poll_via_check_runs_api(gh_executable: str, root: Path, repo: str, pr: int) -> list[dict]:
    """Fall back to the check-runs REST API when `gh pr checks --json` is unsupported."""
    view = run_gh(gh_executable, ["pr", "view", str(pr), "--repo", repo, "--json", "headRefOid"], root)
    if view.returncode != 0:
        raise CheckWatchError(
            f"cannot resolve head SHA for PR {pr}: {view.stderr.strip() or view.stdout.strip()}"
        )
    try:
        head_sha = json.loads(view.stdout)["headRefOid"]
    except (json.JSONDecodeError, KeyError) as error:
        raise CheckWatchError(f"unexpected `gh pr view` output: {error}") from error

    api = run_gh(gh_executable, ["api", f"repos/{repo}/commits/{head_sha}/check-runs"], root)
    if api.returncode != 0:
        raise CheckWatchError(
            f"gh api check-runs fallback failed: {api.stderr.strip() or api.stdout.strip()}"
        )
    try:
        payload = json.loads(api.stdout)
    except json.JSONDecodeError as error:
        raise CheckWatchError(f"gh api check-runs returned unparseable JSON: {error}") from error
    return [normalize_check_run(run) for run in payload.get("check_runs", [])]


def poll_once(gh_executable: str, root: Path, repo: str, pr: int) -> list[dict]:
    """Fetch the current check list for a single poll, preferring `gh pr checks --json`."""
    proc = run_gh(
        gh_executable,
        ["pr", "checks", str(pr), "--repo", repo, "--json", "name,state,link"],
        root,
    )
    # gh documents exit code 8 as "checks pending" even on a successful,
    # parseable --json response, so both 0 and 8 are success here.
    if proc.returncode in (0, 8):
        try:
            return json.loads(proc.stdout or "[]")
        except json.JSONDecodeError as error:
            raise CheckWatchError(f"gh pr checks returned unparseable JSON: {error}") from error

    stderr_lower = (proc.stderr or "").lower()
    if "unknown json field" in stderr_lower or "unsupported flag" in stderr_lower or (
        "--json" in stderr_lower and "unknown" in stderr_lower
    ):
        return poll_via_check_runs_api(gh_executable, root, repo, pr)

    raise CheckWatchError(f"gh pr checks failed: {proc.stderr.strip() or proc.stdout.strip()}")


def classify_checks(checks: list[dict]) -> tuple[str, list[dict]]:
    """Classify one poll's checks into RED, GREEN, or PENDING."""
    # Returns (bucket, failing_checks); failing_checks is only populated
    # for RED. An empty check list (nothing reported yet) is PENDING, not
    # GREEN.
    if not checks:
        return "PENDING", []
    failing = [check for check in checks if str(check.get("state", "")).upper() in RED_STATES]
    if failing:
        return "RED", failing
    if any(str(check.get("state", "")).upper() in PENDING_STATES for check in checks):
        return "PENDING", []
    return "GREEN", []


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--pr",
        type=int,
        default=None,
        help="PR number. Default: resolve the current branch's open PR via `gh pr view`.",
    )
    parser.add_argument(
        "--repo",
        type=str,
        default=None,
        help="owner/repo slug. Default: `gh repo view`, falling back to the git remote.",
    )
    parser.add_argument(
        "--max-polls",
        type=int,
        default=DEFAULT_MAX_POLLS,
        help=f"Maximum poll attempts (default {DEFAULT_MAX_POLLS}, hard-capped at "
        f"{HARD_MAX_POLLS} regardless of the value passed).",
    )
    parser.add_argument(
        "--interval",
        type=int,
        default=DEFAULT_INTERVAL_SECONDS,
        help=f"Seconds to sleep between polls (default {DEFAULT_INTERVAL_SECONDS}, "
        f"clamped to [{MIN_INTERVAL_SECONDS}, {MAX_INTERVAL_SECONDS}]).",
    )
    parser.add_argument(
        "--poll-once",
        action="store_true",
        help="Poll exactly one time and exit immediately (no sleep); for cheap "
        "testing or a single agent-driven check.",
    )
    parser.add_argument("--root", type=Path, default=ROOT)
    return parser


def clamp_max_polls(requested: int) -> int:
    """Clamp --max-polls into [1, HARD_MAX_POLLS], warning on stderr."""
    clamped = requested
    if clamped > HARD_MAX_POLLS:
        print(
            f"watch_pr_checks: clamping --max-polls {requested} to hard cap {HARD_MAX_POLLS}",
            file=sys.stderr,
        )
        clamped = HARD_MAX_POLLS
    if clamped < 1:
        print(f"watch_pr_checks: clamping --max-polls {requested} to 1", file=sys.stderr)
        clamped = 1
    return clamped


def clamp_interval(requested: int) -> int:
    """Clamp --interval into [MIN_INTERVAL_SECONDS, MAX_INTERVAL_SECONDS], warning on stderr."""
    clamped = max(MIN_INTERVAL_SECONDS, min(MAX_INTERVAL_SECONDS, requested))
    if clamped != requested:
        print(
            f"watch_pr_checks: clamping --interval {requested}s to {clamped}s",
            file=sys.stderr,
        )
    return clamped


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    root = args.root.resolve()

    max_polls = clamp_max_polls(args.max_polls)
    interval = clamp_interval(args.interval)
    poll_budget = 1 if args.poll_once else max_polls

    try:
        gh_executable = resolve_gh()
        repo = resolve_repo(gh_executable, root, args.repo)
        pr = resolve_pr_number(gh_executable, root, repo, args.pr)
    except CheckWatchError as error:
        print(f"watch_pr_checks: {error}", file=sys.stderr)
        return 3

    checks: list[dict] = []
    # THE bounded loop: a plain `for` over a fixed range, never `while True`.
    # There is no path through this function that can iterate more than
    # `poll_budget` (<= HARD_MAX_POLLS) times.
    for attempt in range(poll_budget):
        try:
            checks = poll_once(gh_executable, root, repo, pr)
        except CheckWatchError as error:
            print(f"watch_pr_checks: {error}", file=sys.stderr)
            return 3

        bucket, failing = classify_checks(checks)
        if bucket == "GREEN":
            print("all checks green")
            return 0
        if bucket == "RED":
            payload = {
                "failingJobs": [
                    {"name": check.get("name", ""), "runUrl": check.get("link", "")}
                    for check in failing
                ]
            }
            print(json.dumps(payload))
            return 1

        is_last_attempt = attempt == poll_budget - 1
        if not is_last_attempt:
            time.sleep(interval)

    pending_count = sum(1 for check in checks if str(check.get("state", "")).upper() in PENDING_STATES)
    print(f"watch_pr_checks: timed out waiting, {pending_count} checks still pending", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
