#!/usr/bin/env python3
"""Report worktrees whose work is pending, already upstream, or corrupt."""
# Uncommitted changes in a worktree are indistinguishable from finished work
# until something says otherwise. Issue #4437: seven worktrees accumulated in
# one checkout, three holding uncommitted changes. Two of those held
# documentation that had already landed on `origin/main` through another path,
# and one held 652 entirely NUL-filled files that `git status` reported as
# ordinary ' M' entries. Establishing that cost a full byte-level
# investigation and landed zero commits.
#
# This module turns the conditions into reportable states:
#
#   corrupt     -- changed files are almost entirely NUL bytes.
#   abandoned   -- another worktree holds uncommitted changes, carries no patch
#                  that is not already upstream, and has no open pull request;
#                  or it holds commits on a detached HEAD no branch references.
#                  Nobody is coming back for it.
#   superseded  -- a linked worktree is clean and every commit it carries is
#                  already upstream by content. It landed through another path.
#   uncommitted -- changes exist that no commit holds yet, in a worktree that
#                  is otherwise live (including the one you are working in).
#   unknown     -- git could not answer; claim nothing about it.
#
# Advisories are reported, never fatal: concurrent sessions legitimately hold
# their own worktrees, so this must inform an agent rather than block it.
# Relationship to upstream uses `git cherry`, not ahead/behind: the 2026-08-04
# worktrees carried commits whose patches were already on `origin/main`.

from __future__ import annotations

import argparse
import json
import shutil
import subprocess  # nosec B404 - fixed, read-only git and gh queries.
import sys
import time
from collections.abc import Callable
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.agents.guard import scan_for_nul_corruption  # noqa: E402

UPSTREAM_BRANCH = "main"
UPSTREAM_REF = f"origin/{UPSTREAM_BRANCH}"
GIT_TIMEOUT_SECONDS = 30
PULL_REQUEST_TIMEOUT_SECONDS = 30
# A checkout with more linked worktrees than this is already the problem this
# report exists to surface; scanning every one of them is not worth the wait.
MAX_REPORTED_WORKTREES = 50
# Issue #4450: `ChaosEngine/user-evaluation-20260802` was pushed to origin, its
# worktree was cleaned up, and no pull request was ever opened -- invisible to
# this report (local worktrees only) and to `--check-pull-requests` (open pull
# requests for worktrees that still exist, only). A short grace period keeps a
# branch pushed minutes ago, mid-session, from being flagged before its author
# has had a chance to open a pull request.
DEFAULT_STALE_DAYS = 3
SECONDS_PER_DAY = 86400
# A local worktree can hold the only copy of uncommitted work, so its advisory
# clock is deliberately much shorter than the three-day remote-branch clock.
FOREIGN_WORKTREE_STALE_HOURS = 12
# Keep activity inspection bounded like the NUL scan. A truncated list still
# has a known activity floor; it is not silently treated as unknown.
MAX_ACTIVITY_PATHS = 2000

ADVISORY_STATES = ("corrupt", "abandoned", "superseded", "uncommitted", "unknown", "orphaned")


def _git(cwd: Path, *arguments: str) -> str | None:
    """Run one read-only git query, or return None when it cannot be trusted."""
    # `core.longpaths=true` matches the guard's R9 requirement: without it git
    # aborts with `Filename too long` on this repository's over-long
    # `.memory/**` paths, and a failed status query would otherwise read as
    # "clean".
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed read-only git query.
            ["git", "-c", "core.longpaths=true", *arguments],
            cwd=cwd,
            capture_output=True,
            text=True,
            timeout=GIT_TIMEOUT_SECONDS,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return completed.stdout if completed.returncode == 0 else None


def _parse_worktree_list(output: str) -> list[dict]:
    """Parse `git worktree list --porcelain` into worktree records."""
    # `locked` and `prunable` are kept, not discarded: they are the only
    # signals distinguishing a worktree a live session holds, or one whose
    # directory is already gone, from one that is genuinely idle.
    entries: list[dict] = []
    current: dict = {}

    def blank() -> dict:
        return {
            "path": None,
            "branch": None,
            "locked": False,
            "lock_reason": None,
            "prunable": False,
        }

    for line in output.splitlines():
        if not line.strip():
            if current:
                entries.append(current)
                current = {}
            continue
        key, _, value = line.partition(" ")
        if key == "worktree":
            if current:
                entries.append(current)
            current = blank()
            current["path"] = value
        elif key == "branch":
            current["branch"] = value.removeprefix("refs/heads/")
        elif key == "detached":
            current["branch"] = None
        elif key == "locked":
            current["locked"] = True
            current["lock_reason"] = value or None
        elif key == "prunable":
            current["prunable"] = True
    if current:
        entries.append(current)
    return [entry for entry in entries if entry.get("path")]


def _uncommitted_paths(worktree: Path) -> list[str] | None:
    """Changed paths, or None when git could not answer."""
    # None is not zero. Reading a failed query as "clean" would feed the
    # superseded verdict, which tells the reader to delete the branch.
    output = _git(worktree, "status", "--porcelain=v1", "-z")
    if output is None:
        return None
    paths: list[str] = []
    records = output.split("\0")
    index = 0
    while index < len(records):
        record = records[index]
        index += 1
        if not record:
            continue
        if len(record) < 4 or record[2] != " ":
            return None
        paths.append(record[3:])
        # A rename/copy record carries its source path after the destination.
        # It is not a second changed path in this worktree, and may no longer
        # exist, so skip it without another status query.
        if "R" in record[:2] or "C" in record[:2]:
            if index >= len(records):
                return None
            index += 1
    return paths


def _uncommitted_files(worktree: Path) -> int | None:
    """Count changed paths from the reporter's one status query."""
    paths = _uncommitted_paths(worktree)
    return len(paths) if paths is not None else None


def _existing_parent_mtime(path: Path, worktree: Path) -> float | None:
    """Newest available parent mtime for a deleted path, bounded to `worktree`."""
    parent = path.parent
    while True:
        try:
            return parent.stat().st_mtime
        except FileNotFoundError:
            if parent == worktree:
                return None
            parent = parent.parent
        except OSError:
            return None


def _uncommitted_path_activity_epochs(worktree: Path, paths: list[str] | None) -> list[float]:
    """Mtimes for changed paths, using a deleted path's nearest existing parent."""
    if paths is None:
        return []
    try:
        resolved_worktree = worktree.resolve()
    except OSError:
        return []

    signals: list[float] = []
    for relative_path in paths[:MAX_ACTIVITY_PATHS]:
        try:
            candidate = (worktree / relative_path).resolve()
            candidate.relative_to(resolved_worktree)
            signals.append(candidate.stat().st_mtime)
        except FileNotFoundError:
            parent_mtime = _existing_parent_mtime(candidate, resolved_worktree)
            if parent_mtime is not None:
                signals.append(parent_mtime)
        except (OSError, ValueError):
            continue
    return signals


def _linked_worktree_creation_epoch(worktree: Path) -> float | None:
    """Return a linked worktree's private `.git` marker mtime, when readable."""
    try:
        git_marker = worktree / ".git"
        # The primary checkout has the shared admin directory instead, and
        # fetches elsewhere can refresh it indefinitely.
        return git_marker.stat().st_mtime if git_marker.is_file() else None
    except OSError:
        return None


def _activity_epoch(
    root: Path, worktree: Path, committish: str | None, paths: list[str] | None
) -> float | None:
    """Freshest non-self-contaminating activity signal for one worktree."""
    signals = _uncommitted_path_activity_epochs(worktree, paths)
    if committish is not None:
        last_commit_epoch = _last_commit_epoch(root, committish)
        if last_commit_epoch is not None:
            signals.append(float(last_commit_epoch))
    creation_epoch = _linked_worktree_creation_epoch(worktree)
    if creation_epoch is not None:
        signals.append(creation_epoch)
    return max(signals) if signals else None


def _unique_commits(root: Path, committish: str | None) -> int | None:
    """Commits on `committish` whose patch is not already upstream, or None."""
    # `git cherry` marks a patch-identical commit with '-' even when its hash
    # differs, which is exactly the case ahead/behind gets wrong: a branch can
    # be one commit "ahead" of upstream and carry nothing new.
    if committish is None:
        return None
    if _git(root, "rev-parse", "--verify", "--quiet", UPSTREAM_REF) is None:
        return None
    output = _git(root, "cherry", UPSTREAM_REF, committish)
    if output is None:
        return None
    return len([line for line in output.splitlines() if line.startswith("+")])


def _ahead_behind(root: Path, committish: str | None) -> tuple[int | None, int | None]:
    """Commit counts relative to upstream, or (None, None) when unknown."""
    if committish is None:
        return None, None
    output = _git(
        root, "rev-list", "--left-right", "--count", f"{UPSTREAM_REF}...{committish}"
    )
    if output is None:
        return None, None
    parts = output.split()
    if len(parts) != 2 or not all(part.isdigit() for part in parts):
        return None, None
    return int(parts[1]), int(parts[0])  # ahead, behind


def _remote_only_branch_names(root: Path, worktree_branches: set[str]) -> list[str]:
    """Origin branches no linked worktree references, upstream excluded."""
    # `git worktree list` already accounts for every branch that has a
    # worktree; this only has to name the ones nothing local still holds.
    output = _git(root, "for-each-ref", "--format=%(refname:strip=3)", "refs/remotes/origin")
    if output is None:
        return []
    names = [line.strip() for line in output.splitlines() if line.strip()]
    return [
        name
        for name in names
        if name and name not in (UPSTREAM_BRANCH, "HEAD") and name not in worktree_branches
    ]


def _last_commit_epoch(root: Path, ref: str) -> int | None:
    """Committer-date epoch of `ref`'s tip, or None when git could not answer."""
    output = _git(root, "log", "-1", "--format=%ct", ref)
    if output is None:
        return None
    value = output.strip()
    return int(value) if value.isdigit() else None


def _classify_remote_only(entry: dict) -> str:
    """An open pull request means it is already visible; anything else is not."""
    pull_request_status_known = entry["open_pull_requests"] is not None
    has_open_pull_request = bool(entry["open_pull_requests"])
    if pull_request_status_known and has_open_pull_request:
        return "clean"
    return "orphaned"


def _collect_remote_only_entries(
    root: Path,
    worktree_branches: set[str],
    *,
    open_pull_requests: Callable[[str], int] | None,
    stale_days: float,
    now: float | None,
) -> list[dict]:
    """Stale origin branches with no worktree, as reportable entries."""
    reference_time = time.time() if now is None else now
    threshold = reference_time - (stale_days * SECONDS_PER_DAY)

    entries: list[dict] = []
    names = _remote_only_branch_names(root, worktree_branches)
    for name in names[:MAX_REPORTED_WORKTREES]:
        last_commit_epoch = _last_commit_epoch(root, f"refs/remotes/origin/{name}")
        if last_commit_epoch is None or last_commit_epoch >= threshold:
            continue  # too young to call idle, or git could not answer

        pull_requests: int | None = None
        if open_pull_requests is not None:
            try:
                pull_requests = int(open_pull_requests(name))
            except Exception:  # noqa: BLE001 - a lookup failure must not hide the report
                pull_requests = None

        entry = {
            "path": f"origin/{name}",
            "branch": name,
            "is_main": False,
            "is_current": False,
            "locked": False,
            "is_remote_only": True,
            "last_commit_epoch": last_commit_epoch,
            "age_days": (reference_time - last_commit_epoch) / SECONDS_PER_DAY,
            "open_pull_requests": pull_requests,
        }
        entry["state"] = _classify_remote_only(entry)
        if entry["state"] != "clean":
            entries.append(entry)
    return entries


def open_pull_requests_via_gh(branch: str) -> int:
    """Open pull requests for `branch`, via the GitHub CLI when it is available."""
    if shutil.which("gh") is None:
        raise RuntimeError("gh is unavailable")
    completed = subprocess.run(  # nosec B603 B607 - fixed read-only gh query.
        ["gh", "pr", "list", "--head", branch, "--state", "open", "--json", "number"],
        capture_output=True,
        text=True,
        timeout=PULL_REQUEST_TIMEOUT_SECONDS,
        check=False,
    )
    if completed.returncode != 0:
        raise RuntimeError(completed.stderr.strip() or "gh pr list failed")
    return len(json.loads(completed.stdout or "[]"))


def _classify(entry: dict) -> str:
    """Name the one condition that decides what to do with this worktree."""
    # Every verdict beyond "there are uncommitted files here" needs positive
    # evidence, because the advice attached to it is destructive. A worktree
    # that is merely clean and carries no unique patch is indistinguishable
    # from one another session created seconds ago from origin/main, so absence
    # of work is never treated as proof that work has landed.
    if entry["corrupt_files"]:
        return "corrupt"

    uncommitted = entry["uncommitted_files"]
    # A live session holds a locked worktree; the entrypoint says report it and
    # leave it alone, never propose removing it.
    protected = entry["is_current"] or entry["is_main"] or entry["locked"]
    already_upstream = entry["unique_commits"] == 0
    pull_request_status_known = entry["open_pull_requests"] is not None
    has_open_pull_request = bool(entry["open_pull_requests"])
    carries_commits = bool(entry["ahead"])

    if uncommitted:
        if (
            not protected
            and already_upstream
            and pull_request_status_known
            and not has_open_pull_request
        ):
            return "abandoned"
        return "uncommitted"
    if uncommitted is None:
        return "unknown"  # git could not answer; claim nothing about it

    # Work reachable from no branch at all: a detached worktree whose commits
    # are not upstream is the only copy of them.
    if (
        entry["branch"] is None
        and not protected
        and entry["unique_commits"]
        and not has_open_pull_request
    ):
        return "abandoned"
    # Superseded needs commits that exist and are already upstream -- not the
    # mere absence of any.
    if (
        not protected
        and entry["branch"] != UPSTREAM_BRANCH
        and carries_commits
        and already_upstream
    ):
        return "superseded"
    if entry["unique_commits"]:
        return "pending"
    return "clean"


def collect_worktree_report(
    root: Path,
    *,
    open_pull_requests: Callable[[str], int] | None = None,
    stale_days: float = DEFAULT_STALE_DAYS,
    now: float | None = None,
) -> list[dict]:
    """Describe every worktree of `root`'s repository, `root`'s own included,

    plus any origin branch old enough to call idle that no worktree here
    references (issue #4450) -- local `git` only, same as the rest of this
    report; `open_pull_requests` is the one call that can leave the machine,
    and it is reused here exactly as it is for worktrees above.
    """
    # Returns an empty list -- never an error -- when the directory is not a
    # repository or git cannot answer, so a caller can always report the
    # result.
    listing = _git(root, "worktree", "list", "--porcelain")
    if listing is None:
        return []

    # Ask git which worktree `root` belongs to rather than assuming `root` is
    # a worktree root: run from a subdirectory, comparing paths directly would
    # mark the live worktree "not current" and then, once it is dirty, report
    # the session's own in-progress work as abandoned.
    toplevel = _git(root, "rev-parse", "--show-toplevel")
    try:
        current = Path(toplevel.strip()).resolve() if toplevel else root.resolve()
    except OSError:
        return []

    reference_time = time.time() if now is None else now
    report: list[dict] = []
    for index, record in enumerate(_parse_worktree_list(listing)[:MAX_REPORTED_WORKTREES]):
        if record["prunable"]:
            continue  # the directory is already gone; `git worktree prune` owns it
        worktree = Path(record["path"])
        try:
            resolved = worktree.resolve()
        except OSError:
            continue
        branch = record["branch"]
        # A detached worktree still has commits worth accounting for, so fall
        # back to its HEAD rather than reporting nothing about it.
        head = (_git(worktree, "rev-parse", "HEAD") or "").strip() or None
        committish = branch or head
        corrupt, _, scan_truncated = scan_for_nul_corruption(str(worktree))
        ahead, behind = _ahead_behind(root, committish)
        uncommitted_paths = _uncommitted_paths(worktree)
        last_activity_epoch = _activity_epoch(root, worktree, committish, uncommitted_paths)

        pull_requests: int | None = None
        if open_pull_requests is not None and branch is not None:
            try:
                pull_requests = int(open_pull_requests(branch))
            except Exception:  # noqa: BLE001 - a lookup failure must not hide the report
                pull_requests = None

        entry = {
            "path": resolved.as_posix(),
            "branch": branch,
            "is_main": index == 0,
            "is_current": resolved == current,
            "locked": record["locked"],
            "lock_reason": record["lock_reason"],
            "uncommitted_files": (
                len(uncommitted_paths) if uncommitted_paths is not None else None
            ),
            "last_activity_epoch": last_activity_epoch,
            "age_hours": (
                max(0.0, reference_time - last_activity_epoch) / 3600
                if last_activity_epoch is not None
                else None
            ),
            "corrupt_files": len(corrupt),
            "corrupt_paths": corrupt[:5],
            "scan_truncated": scan_truncated,
            "ahead": ahead,
            "behind": behind,
            "unique_commits": _unique_commits(root, committish),
            "open_pull_requests": pull_requests,
        }
        entry["state"] = _classify(entry)
        report.append(entry)

    worktree_branches = {entry["branch"] for entry in report if entry["branch"]}
    report.extend(
        _collect_remote_only_entries(
            root,
            worktree_branches,
            open_pull_requests=open_pull_requests,
            stale_days=stale_days,
            now=reference_time,
        )
    )
    return report


def _describe(entry: dict) -> str:
    branch = entry["branch"] or "detached HEAD"
    location = entry["path"]
    uncommitted = entry.get("uncommitted_files")

    if entry["state"] == "orphaned":
        age = int(entry["age_days"])
        caveat = (
            "no open pull request" if entry["open_pull_requests"] is not None
            else "open pull requests were not checked -- rerun with "
            "--check-pull-requests to confirm"
        )
        return (
            f"branch-orphaned: {location} ({branch}): {age} day(s) since its last "
            f"commit, no local worktree holds it, and {caveat}. This branch is "
            "invisible to every other hygiene check. Its commit count relative to "
            f"{UPSTREAM_REF} is not evidence either way -- this repo squash-merges, "
            "so landed work can still show commits ahead of main. Diff its tip "
            "against main over exactly the files it touched, then check whether "
            "anything still differing ever existed on main, before opening a pull "
            "request for it or deleting it."
        )
    if entry["state"] == "corrupt":
        shown = ", ".join(entry["corrupt_paths"]) or "changed files"
        return (
            f"worktree-corrupt: {location} ({branch}): {entry['corrupt_files']} "
            f"changed file(s) are almost entirely NUL bytes ({shown}). Files of "
            "a plausible size filled with NUL are the signature of an unclean "
            "shutdown, and git reports them as ordinary modifications. Do not "
            "commit them. Restore only each confirmed corrupt path, never the "
            "whole worktree; for example: `git restore --source=HEAD --staged "
            "--worktree -- <confirmed-corrupt-path>`. Then re-create anything that "
            "existed only in corrupt files."
        )
    if entry["state"] == "abandoned":
        if entry["branch"] is None:
            held = (
                f"{entry['unique_commits']} commit(s) on a detached HEAD that "
                "no branch references"
            )
        else:
            held = (
                f"{uncommitted} uncommitted file(s) and no commit that is not "
                f"already on {UPSTREAM_REF}"
            )
        caveat = (
            "" if entry["open_pull_requests"] is not None
            else " (open pull requests were not checked -- rerun with "
            "--check-pull-requests to confirm)"
        )
        return (
            f"worktree-abandoned: {location} ({branch}): {held}{caveat}. "
            "Nothing but this working tree holds that work. Commit, push, and "
            "open a pull request from that worktree, or confirm it is "
            "redundant -- do not leave it to a cleanup pass to decide."
        )
    if entry["state"] == "superseded":
        return (
            f"worktree-superseded: {location} ({branch}): clean, and all "
            f"{entry['ahead']} of its commit(s) are already on {UPSTREAM_REF} "
            "by content. Its work landed through another path. If no session "
            "is using it, remove the worktree and delete the branch."
        )
    if entry["state"] == "unknown":
        return (
            f"worktree-unknown: {location} ({branch}): git could not report "
            "this worktree's status, so nothing can be concluded about the "
            "work in it. Inspect it before any cleanup pass touches it."
        )
    caveat = (
        " Open pull requests were not checked; rerun with "
        "`--check-pull-requests` before deciding whether it is stale."
        if entry["open_pull_requests"] is None and entry["branch"] is not None
        else ""
    )
    return (
        f"worktree-uncommitted: {location} ({branch}): {uncommitted} "
        "uncommitted file(s). Uncommitted work is not done -- commit and push "
        f"it before ending the turn, or discard it deliberately.{caveat}"
    )


def format_advisories(report: list[dict]) -> list[str]:
    """One actionable line per worktree that needs attention."""
    advisories = []
    for entry in report:
        if entry["state"] not in ADVISORY_STATES:
            continue
        advisory = _describe(entry)
        if entry.get("scan_truncated"):
            advisory += (
                " NUL scan inspected only the first 2000 candidate paths; stage "
                "smaller named path sets so the guard can verify every file."
            )
        advisories.append(advisory)
    return advisories


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument(
        "--check-pull-requests",
        action="store_true",
        help="Ask the GitHub CLI whether each branch has an open pull request.",
    )
    return parser


def main() -> int:
    """Run the CLI. Always exits 0: this reports, it does not gate."""
    args = build_parser().parse_args()
    report = collect_worktree_report(
        args.root.resolve(),
        open_pull_requests=open_pull_requests_via_gh if args.check_pull_requests else None,
    )
    if args.format == "json":
        print(
            json.dumps(
                {
                    "foreign_worktree_stale_hours": FOREIGN_WORKTREE_STALE_HOURS,
                    "worktrees": report,
                    "advisories": format_advisories(report),
                },
                indent=2,
            )
        )
        return 0
    advisories = format_advisories(report)
    if not advisories:
        print(f"Worktree hygiene is clean: {len(report)} worktree(s), nothing to report.")
        return 0
    for advisory in advisories:
        print(advisory)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
