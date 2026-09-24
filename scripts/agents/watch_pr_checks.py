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
#     First collapse to one effective check per ``name``: drop
#     ``CANCELLED`` when a same-named check is pending or successful
#     (superseded workflow re-runs), then prefer PENDING > SUCCESS >
#     remaining RED. See ``collapse_checks_by_name`` / #5753.
#     RED     -- any effective check reports a failed/cancelled/timed-out-
#                shaped state (see ``RED_STATES``; includes
#                ``ACTION_REQUIRED``, which GitHub Apps such as Codacy use
#                for a completed-but-failing run).
#     GREEN   -- all effective checks are completed and none are RED.
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
# DIGEST (#6162 / #6163):
#     ``--digest`` prints ONE bounded JSON digest on terminal states
#     (``{sha, state, failing[<=10], failing_total, pending_count,
#     codacy_action_required, updated_at}``; RED keeps ``failingJobs`` in the
#     same object). ``--digest-out PATH`` / ``--status-lease`` publish the digest
#     and the one-status-channel lease on state change only -- never a
#     heartbeat and never the raw ``statusCheckRollup``.
#
# Usage:
#     python3 scripts/ci/watch_pr_checks.py                  # current branch's PR
#     python3 scripts/ci/watch_pr_checks.py --pr 3368         # explicit PR
#     python3 scripts/ci/watch_pr_checks.py --poll-once       # single poll, no sleep
#     python3 scripts/ci/watch_pr_checks.py --max-polls 5 --interval 30

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
# subprocess is used only for read-only `gh`/`git` invocations below, always
# as a list of args (never shell=True) with the executable resolved to an
# absolute path via shutil.which.
import subprocess  # nosec B404
import sys
import time
from pathlib import Path

try:
    from scripts.agents.repository_context import (
        RepositoryContextError,
        parse_pr_reference,
        resolve_repository_context,
    )
except ModuleNotFoundError:
    from repository_context import RepositoryContextError, parse_pr_reference, resolve_repository_context

try:
    from scripts.agents.status_lease import write_lease
except ModuleNotFoundError:
    from status_lease import write_lease

DEFAULT_MAX_POLLS = 20
HARD_MAX_POLLS = 60
DEFAULT_INTERVAL_SECONDS = 60
MIN_INTERVAL_SECONDS = 10
MAX_INTERVAL_SECONDS = 600
# #6196: the slowest required leg (fresh installer on macOS) takes ~35 min, so
# the default watch window covers 45 min plus one extra poll. Cancelled-only
# groups are treated as pending for the first REGISTRATION_GRACE_SECONDS: a
# superseded run is cancelled before its replacement registers.
SLOWEST_REQUIRED_CHECK_MINUTES = 45
REGISTRATION_GRACE_SECONDS = 180


def default_max_polls(interval: int) -> int:
    """Polls needed so the default window outlasts the slowest required check."""
    interval = max(1, int(interval))
    return -(-SLOWEST_REQUIRED_CHECK_MINUTES * 60 // interval) + 1

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
SUCCESS_STATES = {"SUCCESS", "NEUTRAL", "SKIPPED"}
KNOWN_STATES = RED_STATES | PENDING_STATES | SUCCESS_STATES
_TRANSIENT_GITHUB_HTTP = re.compile(r"\bHTTP\s*(?:429|503)\b", re.IGNORECASE)
_STATUS_TABLE = re.compile(
    r"(?m)^\|?\s*:?-{3,}:?\s*(?:\|\s*:?-{3,}:?\s*)+\|?\s*$"
)
_FORCE_MERGE = re.compile(r"(?:^|\s)--admin(?:\s|$)")


def unattended_watch_command(pr: int | str, *, repo: str | None = None) -> str:
    """One blocking watch. Not a turn-ending status poll."""
    parts = [
        "python3",
        "scripts/agents/watch_pr_checks.py",
        "--pr",
        str(pr),
        "--until-merged",
    ]
    if repo:
        parts.extend(["--repo", repo])
    command = " ".join(parts)
    if _FORCE_MERGE.search(command) or "--poll-once" in command:
        raise CheckWatchError("unattended watch must not force-merge or poll once")
    return command


def reject_repeated_status_table(text: str) -> None:
    """A second markdown status table in one watch is a contract failure."""
    if len(_STATUS_TABLE.findall(text or "")) > 1:
        raise CheckWatchError("repeated PR status table in one watch")


def classify_unattended(checks: list[dict], pull: dict | None) -> tuple[str, list[dict]]:
    """Pending stays pending while auto-merge is armed. Merged is its own line."""
    view = pull or {}
    state = str(view.get("state") or "").upper()
    if state == "MERGED" or view.get("mergedAt"):
        return "MERGED", []
    merge_state = str(view.get("mergeStateStatus") or "").upper()
    if merge_state in {"DIRTY", "BEHIND"}:
        return "RED", [{"name": "mergeStateStatus", "link": merge_state}]
    return classify_checks(checks)

class CheckWatchError(RuntimeError):
    """Raised for gh/environment failures that map to exit code 3."""


def is_transient_github_http_error(error: BaseException) -> bool:
    """True when a read-only gh poll failed with HTTP 429 or 503."""
    return bool(_TRANSIENT_GITHUB_HTTP.search(str(error)))


def validate_checks(payload: object, source: str) -> list[dict]:
    """Validate the stable check shape before classification can go green."""
    if not isinstance(payload, list):
        raise CheckWatchError(f"{source} returned a non-list check payload")
    checks: list[dict] = []
    for index, check in enumerate(payload):
        if not isinstance(check, dict):
            raise CheckWatchError(f"{source} check {index} is not an object")
        state = check.get("state")
        if not isinstance(state, str) or not state.strip():
            raise CheckWatchError(f"{source} check {index} has no usable state")
        normalized_state = state.upper()
        if normalized_state not in KNOWN_STATES:
            raise CheckWatchError(
                f"{source} check {index} has unknown state {state!r}"
            )
        name = check.get("name")
        link = check.get("link", "")
        if not isinstance(name, str) or not name.strip() or not isinstance(link, str):
            raise CheckWatchError(f"{source} check {index} has an unusable name or link")
        checks.append({**check, "state": normalized_state})
    return checks


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
    if not isinstance(run, dict):
        raise CheckWatchError("gh api check-runs entry is not an object")
    status_value = run.get("status")
    if not isinstance(status_value, str) or not status_value:
        raise CheckWatchError("gh api check-runs entry has no usable status")
    status = status_value.lower()
    if status != "completed":
        state = "IN_PROGRESS"
    else:
        conclusion = run.get("conclusion")
        if not isinstance(conclusion, str) or not conclusion:
            raise CheckWatchError("completed gh api check-runs entry has no usable conclusion")
        state = conclusion.upper()
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
    if not isinstance(payload, dict) or not isinstance(payload.get("check_runs"), list):
        raise CheckWatchError("gh api check-runs returned an invalid payload shape")
    return validate_checks(
        [normalize_check_run(run) for run in payload["check_runs"]],
        "gh api check-runs",
    )


def fetch_pull(gh_executable: str, root: Path, repo: str, pr: int) -> dict:
    """Read merge state inside the same watch. Not a second status channel."""
    proc = run_gh(
        gh_executable,
        [
            "pr",
            "view",
            str(pr),
            "--repo",
            repo,
            "--json",
            "state,mergedAt,autoMergeRequest,mergeStateStatus",
        ],
        root,
    )
    if proc.returncode != 0:
        raise CheckWatchError(
            f"gh pr view failed: {proc.stderr.strip() or proc.stdout.strip()}"
        )
    try:
        payload = json.loads(proc.stdout or "{}")
    except json.JSONDecodeError as error:
        raise CheckWatchError(f"gh pr view returned unparseable JSON: {error}") from error
    if not isinstance(payload, dict):
        raise CheckWatchError("gh pr view returned a non-object payload")
    return payload


def fetch_head_sha(gh_executable: str, root: Path, repo: str, pr: int) -> str | None:
    """Head SHA for the digest only; a lookup failure degrades to None, never to exit 3."""
    proc = run_gh(gh_executable, ["pr", "view", str(pr), "--repo", repo, "--json", "headRefOid"], root)
    if proc.returncode != 0:
        return None
    try:
        payload = json.loads(proc.stdout or "{}")
    except json.JSONDecodeError:
        return None
    value = payload.get("headRefOid") if isinstance(payload, dict) else None
    return value if isinstance(value, str) and value else None


def _publish(args, root: Path, pr: int, digest: dict) -> None:
    """Write the shared digest file and the status lease on state change; never stdout."""
    if args.digest_out:
        path = Path(args.digest_out)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(digest, sort_keys=True) + "\n", encoding="utf-8")
    if args.status_lease:
        write_lease(root, pr, pid=os.getpid(), state=digest["state"], digest_path=str(args.digest_out) if args.digest_out else None)


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
            return validate_checks(json.loads(proc.stdout or "[]"), "gh pr checks")
        except json.JSONDecodeError as error:
            raise CheckWatchError(f"gh pr checks returned unparseable JSON: {error}") from error

    stderr_lower = (proc.stderr or "").lower()
    if "unknown json field" in stderr_lower or "unsupported flag" in stderr_lower or (
        "--json" in stderr_lower and "unknown" in stderr_lower
    ):
        return poll_via_check_runs_api(gh_executable, root, repo, pr)

    raise CheckWatchError(f"gh pr checks failed: {proc.stderr.strip() or proc.stdout.strip()}")


def collapse_checks_by_name(checks: list[dict]) -> list[dict]:
    """Prefer one effective check per name, ignoring superseded cancellations.

    When a required workflow is cancelled and replaced on the same head,
    GitHub's rollup still lists the cancelled summary alongside the new
    pending/successful run. Keep watching through that supersession: drop
    ``CANCELLED`` entries whenever a same-named check is pending or
    successful, then prefer PENDING > SUCCESS > remaining RED (#5753).
    """
    by_name: dict[str, list[dict]] = {}
    for check in checks:
        name = str(check.get("name", "")).strip() or ""
        by_name.setdefault(name, []).append(check)

    effective: list[dict] = []
    for group in by_name.values():
        non_cancelled = [
            check
            for check in group
            if str(check.get("state", "")).upper() != "CANCELLED"
        ]
        chosen_group = non_cancelled if non_cancelled else group

        pending = [
            check
            for check in chosen_group
            if str(check.get("state", "")).upper() in PENDING_STATES
        ]
        if pending:
            effective.append(pending[0])
            continue
        success = [
            check
            for check in chosen_group
            if str(check.get("state", "")).upper() in SUCCESS_STATES
        ]
        if success:
            effective.append(success[0])
            continue
        effective.append(chosen_group[0])
    return effective


def _without_unexpanded_matrix_jobs(checks: list[dict]) -> list[dict]:
    """Drop matrix jobs cancelled before GitHub expanded ``${{ }}`` in the name."""
    expanded = [check for check in checks if "${{" not in str(check.get("name", ""))]
    return expanded if expanded else checks


def classify_checks(checks: list[dict], *, registration_grace: bool = False) -> tuple[str, list[dict]]:
    """Classify one poll's checks into RED, GREEN, or PENDING.

    With ``registration_grace`` a RED made only of CANCELLED checks is PENDING:
    the superseding run has not registered its replacement check yet (#6196).
    """
    # Returns (bucket, failing_checks); failing_checks is only populated
    # for RED. An empty check list (nothing reported yet) is PENDING, not
    # GREEN. Collapse same-named superseded cancellations first (#5753).
    # A PR Gate Summary failure while another check is still running is the
    # cancelled workflow's summary, not a new red.
    if not checks:
        return "PENDING", []
    effective = collapse_checks_by_name(_without_unexpanded_matrix_jobs(checks))
    failing = [
        check for check in effective if str(check.get("state", "")).upper() in RED_STATES
    ]
    pending = any(str(check.get("state", "")).upper() in PENDING_STATES for check in effective)
    if failing and pending:
        failing = [check for check in failing if str(check.get("name", "")) != "PR Gate Summary"]
    if failing and registration_grace and all(
        str(check.get("state", "")).upper() == "CANCELLED" for check in failing
    ):
        return "PENDING", []
    if failing:
        return "RED", failing
    if pending:
        return "PENDING", []
    return "GREEN", []


DIGEST_FAILING_CAP = 10
DIGEST_MAX_BYTES = 4096
DIGEST_NAME_CHARS = 80
DIGEST_URL_CHARS = 160
CODACY_NAME_MARKER = "codacy"


def _clip(value: object, limit: int) -> str:
    """Bound one digest field so the whole digest stays under DIGEST_MAX_BYTES."""
    text = str(value or "")
    return text if len(text) <= limit else text[: limit - 1] + "\u2026"



def build_digest(sha: str | None, bucket: str, checks: list[dict], failing: list[dict], *, now: float | None = None) -> dict:
    """Agent-facing CI status digest (#6162): bounded, never the raw statusCheckRollup."""
    effective = collapse_checks_by_name(checks) if checks else []
    pending_count = sum(1 for check in effective if str(check.get("state")).upper() in PENDING_STATES)
    codacy = sorted(
        _clip(check.get("name"), DIGEST_NAME_CHARS)
        for check in effective
        if CODACY_NAME_MARKER in str(check.get("name", "")).lower()
        and str(check.get("state", "")).upper() == "ACTION_REQUIRED"
    )
    rows = [
        {"name": _clip(check.get("name"), DIGEST_NAME_CHARS), "url": _clip(check.get("link"), DIGEST_URL_CHARS)}
        for check in failing[:DIGEST_FAILING_CAP]
    ]
    return {
        "sha": sha,
        "state": bucket.lower(),
        "failing": rows,
        "failing_total": len(failing),
        "pending_count": pending_count,
        "codacy_action_required": codacy[:DIGEST_FAILING_CAP],
        "updated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(time.time() if now is None else now)),
    }


def format_digest_line(digest: dict) -> str:
    """One human line for the digest."""
    sha = (digest.get("sha") or "unknown")[:12]
    names = ", ".join(row["name"] for row in digest.get("failing", []))
    line = f"{digest.get('state')} sha={sha} failing={digest.get('failing_total', 0)} pending={digest.get('pending_count', 0)}"
    if digest.get("codacy_action_required"):
        line += f" codacy_action_required={len(digest['codacy_action_required'])} (blocking)"
    if names:
        line += f" :: {names}"
    return line


def rollup_is_waste(payload: str, *, budget: int = DIGEST_MAX_BYTES) -> bool:
    """True when a tool result pastes a raw statusCheckRollup larger than the digest budget."""
    return '"statusCheckRollup"' in payload and len(payload.encode("utf-8")) > budget


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--pr",
        type=str,
        default=None,
        help="PR number or URL. Default: resolve the current branch's open PR via `gh pr view`.",
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
        default=None,
        help=f"Maximum poll attempts (default: enough polls to cover "
        f"{SLOWEST_REQUIRED_CHECK_MINUTES} min at --interval; hard-capped at "
        f"max({HARD_MAX_POLLS}, that default)).",
    )
    parser.add_argument(
        "--max-minutes",
        type=int,
        default=None,
        help="Watch window in minutes; overrides --max-polls (#6196).",
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
    parser.add_argument(
        "--until-merged",
        action="store_true",
        help="Block until the PR is merged or a check is red. Green checks stay "
        "pending. Auto-merge armed does not end the watch.",
    )
    parser.add_argument(
        "--digest",
        action="store_true",
        help="Print one bounded JSON digest on terminal states (#6162). RED keeps failingJobs.",
    )
    parser.add_argument(
        "--digest-out",
        type=str,
        default=None,
        help="Write the digest JSON to this path on state change (shared status channel, #6163).",
    )
    parser.add_argument(
        "--status-lease",
        action="store_true",
        help="Own the status channel: record .chaos-engine/runtime/status-lease-<pr>.json (#6163).",
    )
    parser.add_argument("--root", type=Path, default=Path.cwd())
    return parser


def clamp_max_polls(requested: int, interval: int = DEFAULT_INTERVAL_SECONDS) -> int:
    """Clamp --max-polls into [1, max(HARD_MAX_POLLS, default window)], warning on stderr."""
    clamped = requested
    cap = max(HARD_MAX_POLLS, default_max_polls(interval))
    if clamped > cap:
        print(
            f"watch_pr_checks: clamping --max-polls {requested} to hard cap {cap}",
            file=sys.stderr,
        )
        clamped = cap
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


def main(argv: list[str] | None = None) -> int:
    """Run the CLI."""
    args = build_parser().parse_args(argv)
    root = args.root.resolve()

    interval = clamp_interval(args.interval)
    requested_polls = args.max_polls if args.max_polls is not None else default_max_polls(interval)
    if args.max_minutes is not None:
        requested_polls = -(-max(1, args.max_minutes) * 60 // interval) + 1
    max_polls = clamp_max_polls(requested_polls, interval)
    poll_budget = 1 if args.poll_once else max_polls
    started = time.monotonic()

    try:
        gh_executable = resolve_gh()
        context = resolve_repository_context(
            explicit_repo=args.repo,
            pr=args.pr,
            explicit_root=root,
            cwd=Path.cwd(),
        )
        repo = context.repo
        _, _, bare_numeric_pr = parse_pr_reference(args.pr)
        if bare_numeric_pr and args.repo is None:
            print(
                f"watch_pr_checks: numeric --pr {context.pr_number} inferred repository {repo}",
                file=sys.stderr,
            )
        pr = resolve_pr_number(gh_executable, context.root, repo, context.pr_number)
        root = context.root
        wants_digest = args.digest or args.digest_out or args.status_lease
        head_sha = fetch_head_sha(gh_executable, root, repo, pr) if wants_digest else None
    except (CheckWatchError, RepositoryContextError) as error:
        print(f"watch_pr_checks: {error}", file=sys.stderr)
        return 3

    checks: list[dict] = []
    transcript: list[str] = []

    def emit(line: str) -> None:
        transcript.append(line)
        reject_repeated_status_table("\n".join(transcript))
        print(line)

    # THE bounded loop: a plain `for` over a fixed range, never `while True`.
    # There is no path through this function that can iterate more than
    # `poll_budget` (<= HARD_MAX_POLLS) times. Pending polls print nothing.
    last_bucket = None
    for attempt in range(poll_budget):
        try:
            checks = poll_once(gh_executable, root, repo, pr)
            pull = fetch_pull(gh_executable, root, repo, pr) if args.until_merged else None
        except CheckWatchError as error:
            print(f"watch_pr_checks: {error}", file=sys.stderr)
            if is_transient_github_http_error(error) and attempt < poll_budget - 1:
                time.sleep(interval)
                continue
            return 3

        if args.until_merged:
            bucket, failing = classify_unattended(checks, pull)
        else:
            grace = time.monotonic() - started < REGISTRATION_GRACE_SECONDS and not args.poll_once
            bucket, failing = classify_checks(checks, registration_grace=grace)
        digest = build_digest(head_sha, bucket, checks, failing)
        if bucket != last_bucket:
            _publish(args, root, pr, digest)
            last_bucket = bucket
        if bucket == "MERGED":
            emit("MERGED")
            return 0
        if bucket == "GREEN" and not args.until_merged:
            emit(json.dumps(digest) if args.digest else "all checks green")
            return 0
        if bucket == "GREEN":
            bucket = "PENDING"
        if bucket == "RED":
            payload = {
                "failingJobs": [
                    {"name": check.get("name", ""), "runUrl": check.get("link", "")}
                    for check in failing
                ]
            }
            if args.digest:
                payload.update(digest)
            emit(json.dumps(payload))
            return 1

        is_last_attempt = attempt == poll_budget - 1
        if not is_last_attempt:
            time.sleep(interval)

    pending_count = sum(1 for check in checks if str(check.get("state", "")).upper() in PENDING_STATES)
    _publish(args, root, pr, build_digest(head_sha, "PENDING", checks, []))
    print(f"watch_pr_checks: timed out waiting, {pending_count} checks still pending", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
