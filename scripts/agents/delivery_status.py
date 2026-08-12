"""Validate live delivery and scoped-cleanup evidence for every owned PR."""

from __future__ import annotations

from datetime import UTC, datetime
from pathlib import Path
import shutil
import subprocess  # nosec B404 - fixed read-only git commands.

try:
    from scripts.agents.github_client import GitHubClient
    from scripts.agents.pr_audit import audit_snapshot, collect_pr_snapshot
except ModuleNotFoundError:
    from github_client import GitHubClient
    from pr_audit import audit_snapshot, collect_pr_snapshot


def _text(value) -> bool:
    return isinstance(value, str) and bool(value.strip())


def evaluate_delivery(
    manifest: object, statuses: object, cleanup_observation: object,
    *, execution_repository: str | None = None, execution_head: str | None = None,
) -> dict:
    reasons: list[str] = []
    unavailable = False
    owned = manifest.get("ownedPullRequests") if isinstance(manifest, dict) else None
    if not isinstance(owned, list) or not owned:
        return {"schemaVersion": 1, "kind": "delivery-status", "repository": execution_repository, "headOid": execution_head, "decision": "unavailable", "reasons": ["invalid owned pull-request manifest"], "mergedCount": 0, "observedAt": datetime.now(UTC).isoformat(), "pullRequests": [], "cleanup": cleanup_observation}
    if not isinstance(statuses, list):
        statuses = []
    by_key = {
        f"{item.get('repository')}#{item.get('number')}": item
        for item in statuses if isinstance(item, dict)
    }
    seen: set[str] = set()
    merged_count = 0
    results = []
    for item in owned:
        if not isinstance(item, dict) or not _text(item.get("repository")) or not isinstance(item.get("number"), int) or not _text(item.get("headOid")):
            reasons.append("invalid owned pull-request entry")
            unavailable = True
            continue
        key = f"{item['repository']}#{item['number']}"
        if key in seen:
            reasons.append(f"duplicate owned pull request {key}")
            unavailable = True
            continue
        for dependency in item.get("dependsOn", []):
            if dependency not in seen:
                reasons.append(f"dependency {dependency} must precede {key}")
        seen.add(key)
        status = by_key.get(key)
        if not isinstance(status, dict):
            reasons.append(f"live status unavailable for {key}")
            unavailable = True
            continue
        authority = item.get("authorityEvidence")
        authority_valid = (
            isinstance(authority, dict)
            and authority.get("source") in {"user-instruction", "native-memory"}
            and _text(authority.get("locator"))
            and _text(authority.get("recordedAt"))
            and isinstance(authority.get("repositories"), list)
            and item["repository"] in authority["repositories"]
        )
        if item.get("mergeAuthorized") is not True or not authority_valid:
            reasons.append(f"merge authority is not recorded for {key}; keep the goal incomplete")
        if status.get("headOid") != item["headOid"]:
            reasons.append(f"head changed for {key}")
        if status.get("auditDecision") != "allow":
            reasons.append(f"feedback audit is not clear for {key}")
        delivered = (
            status.get("state") in {"CLOSED", "MERGED"}
            and status.get("isDraft") is False
            and _text(status.get("mergedAt"))
        )
        if not delivered:
            reasons.append(f"live mergedAt is absent for {key}; draft, green, or armed is intermediate")
        else:
            merged_count += 1
        results.append(status)
    cleanup = cleanup_observation
    if not isinstance(cleanup, dict):
        reasons.append("live cleanup observation is missing")
        unavailable = True
    else:
        if cleanup.get("primarySynced") is not True:
            reasons.append("primary checkout is not synchronized by fast-forward")
        if cleanup.get("unrelatedDirtyPreserved") is not True:
            reasons.append("unrelated dirty worktree preservation is not proven")
        for field in ("taskWorktreesAbsent", "taskBranchesAbsent"):
            if cleanup.get(field) is not True:
                reasons.append(f"scoped cleanup is incomplete: {field}")
    return {
        "schemaVersion": 1, "kind": "delivery-status",
        "repository": execution_repository, "headOid": execution_head,
        "observedAt": datetime.now(UTC).isoformat(),
        "decision": "unavailable" if unavailable else ("block" if reasons else "allow"),
        "reasons": reasons, "mergedCount": merged_count, "pullRequests": results,
        "cleanup": cleanup,
    }


def collect_delivery(manifest: dict, *, default_root: Path) -> list[dict]:
    if not isinstance(manifest, dict):
        raise ValueError("delivery manifest must be an object")
    statuses = []
    for owned in manifest.get("ownedPullRequests", []):
        repository = owned.get("repository")
        number = owned.get("number")
        root = Path(owned.get("root") or default_root)
        snapshot = collect_pr_snapshot(GitHubClient(repository, root=root), number)
        audit = audit_snapshot(
            snapshot, owned.get("dispositions", {}), expected_head=owned.get("headOid")
        )
        statuses.append({
            **{key: snapshot.get(key) for key in (
                "repository", "number", "headOid", "state", "isDraft", "autoMergeRequest",
                "mergeStateStatus", "mergedAt",
            )},
            "auditDecision": audit["decision"],
        })
    return statuses


def inspect_cleanup(manifest: dict, *, runner=None, executable: str | None = None) -> dict:
    """Observe only the named repositories/worktrees/branches; never clean them."""
    if not isinstance(manifest, dict):
        raise ValueError("delivery manifest must be an object")
    cleanup = manifest.get("cleanup")
    repositories = cleanup.get("repositories") if isinstance(cleanup, dict) else None
    if not isinstance(repositories, list) or not repositories:
        raise ValueError("cleanup.repositories must name live cleanup targets")
    runner = subprocess.run if runner is None else runner
    git = executable or shutil.which("git")
    if not git:
        raise ValueError("git is required for cleanup inspection")
    observations = []
    all_synced = all_worktrees_absent = all_branches_absent = all_dirty_preserved = True
    for target in repositories:
        if not isinstance(target, dict) or not _text(target.get("root")) or not _text(target.get("defaultBranch")):
            raise ValueError("invalid cleanup repository target")
        root = Path(target["root"]).resolve()
        def git_read(*arguments):
            result = runner([git, *arguments], cwd=root, capture_output=True, text=True, timeout=10, check=False)
            if result.returncode:
                raise ValueError(result.stderr.strip() or f"git {' '.join(arguments)} failed")
            return result.stdout
        branch = target["defaultBranch"]
        local = git_read("rev-parse", branch).strip()
        remote = git_read("rev-parse", f"origin/{branch}").strip()
        synced = local == remote
        worktree_output = git_read("worktree", "list", "--porcelain")
        present_worktrees = {
            str(Path(line.removeprefix("worktree ")).resolve()).lower()
            for line in worktree_output.splitlines() if line.startswith("worktree ")
        }
        named_worktrees = [str(Path(value).resolve()).lower() for value in target.get("taskWorktrees", [])]
        worktrees_absent = not any(value in present_worktrees for value in named_worktrees)
        branch_output = git_read("branch", "--format=%(refname:short)").splitlines()
        named_branches = target.get("taskBranches", [])
        branches_absent = not any(value in branch_output for value in named_branches)
        preserved = []
        for value in target.get("unrelatedDirtyWorktrees", []):
            path = Path(value).resolve()
            dirty = runner([git, "status", "--porcelain"], cwd=path, capture_output=True, text=True, timeout=10, check=False)
            preserved.append(dirty.returncode == 0 and bool(dirty.stdout.strip()))
        dirty_preserved = all(preserved)
        all_synced &= synced
        all_worktrees_absent &= worktrees_absent
        all_branches_absent &= branches_absent
        all_dirty_preserved &= dirty_preserved
        observations.append({"root": str(root), "primarySynced": synced, "taskWorktreesAbsent": worktrees_absent, "taskBranchesAbsent": branches_absent, "unrelatedDirtyPreserved": dirty_preserved})
    return {"primarySynced": all_synced, "taskWorktreesAbsent": all_worktrees_absent, "taskBranchesAbsent": all_branches_absent, "unrelatedDirtyPreserved": all_dirty_preserved, "repositories": observations}
