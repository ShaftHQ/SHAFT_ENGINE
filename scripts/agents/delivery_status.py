"""Validate live delivery and scoped-cleanup evidence for every owned PR."""

from __future__ import annotations

from datetime import UTC, datetime
from pathlib import Path
import json
import os
import re
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


def _normalized_path(value: str | Path, platform_name: str | None = None) -> str:
    resolved = str(Path(value).resolve())
    return resolved.casefold() if (platform_name or os.name) == "nt" else resolved


def _github_repository(remote: str) -> str | None:
    match = re.fullmatch(
        r"(?:https?://github\.com/|ssh://git@github\.com/|git@github\.com:)([^/\s]+/[^/\s]+?)(?:\.git)?",
        remote.strip(),
        flags=re.IGNORECASE,
    )
    return match.group(1) if match else None


def _authority_receipt(root: Path, repository: str) -> dict | None:
    dot_git = root.resolve() / ".git"
    try:
        git_dir = Path(dot_git.read_text(encoding="utf-8").partition("gitdir:")[2].strip()).resolve() if dot_git.is_file() else dot_git.resolve()
        current = git_dir / "chaos-engine" / "user-authority.json"
        legacy = git_dir / ("act-as-" + "mohab") / "user-authority.json"
        receipt = json.loads((current if current.is_file() else legacy).read_text(encoding="utf-8"))
        observed = datetime.fromisoformat(str(receipt.get("observedAt")))
    except (OSError, ValueError, TypeError, json.JSONDecodeError):
        return None
    if (
        receipt.get("schemaVersion") == 1 and receipt.get("kind") == "user-merge-authority"
        and receipt.get("repository") == repository and receipt.get("decision") in {"allow", "deny", "neutral"}
        and observed.tzinfo is not None
    ):
        return receipt
    return None


def _authority_valid(authority: object, repository: str, root: Path) -> bool:
    current = _authority_receipt(root, repository)
    if current and current["decision"] == "deny":
        return False
    if current and current["decision"] == "allow":
        return True
    if not isinstance(authority, dict) or authority.get("source") != "native-memory":
        return False
    repositories = authority.get("repositories")
    try:
        recorded = datetime.fromisoformat(str(authority.get("recordedAt")))
    except ValueError:
        return False
    structurally_valid = (
        recorded.tzinfo is not None and _text(authority.get("locator"))
        and isinstance(repositories, list) and all(_text(item) for item in repositories)
        and repository in repositories
    )
    if not structurally_valid:
        return False
    memory_root = root.resolve() / ".memory"
    for sidecar in (memory_root / "memory").glob("**/*.json"):
        try:
            record = json.loads(sidecar.read_text(encoding="utf-8"))
            if (
                record.get("id") != authority["locator"] or record.get("status") != "active"
                or record.get("type") != "decision" or "merge-authority" not in record.get("tags", [])
                or authority["recordedAt"] != record.get("updated_at")
            ):
                continue
            body_path = (memory_root / record["body_path"]).resolve()
            body_path.relative_to(memory_root.resolve())
            body = body_path.read_text(encoding="utf-8").lower()
        except (OSError, ValueError, TypeError, KeyError, json.JSONDecodeError):
            continue
        repo_name = repository.rsplit("/", 1)[-1].lower().replace("_", "-").replace(".", "-")
        scope_project = str(record.get("scope", {}).get("project", "")).lower().removeprefix("project.").replace("_", "-").replace(".", "-")
        normalized_body = body.replace("_", "-").replace(".", " ")
        record_id = str(record.get("id", "")).lower().replace("_", "-").replace(".", "-")
        explicitly_standing = "standing merge authorization" in body or "merge autonomously" in body
        if explicitly_standing and (
            scope_project == repo_name or repo_name in record_id
            or repo_name.replace("-", " ") in normalized_body
            or (repo_name == "shaft-engine" and "shaft-engine" in normalized_body)
        ):
            return True
    return False


def validate_authority(manifest: object, repository: str, number: int, head: str, *, root: Path = Path(".")) -> dict:
    reasons = []
    owned = manifest.get("ownedPullRequests") if isinstance(manifest, dict) else None
    match = next((item for item in owned or [] if isinstance(item, dict) and item.get("repository") == repository and item.get("number") == number and item.get("headOid") == head), None)
    authority = match.get("authorityEvidence") if isinstance(match, dict) else None
    valid = (
        isinstance(match, dict) and match.get("mergeAuthorized") is True
        and isinstance(number, int) and not isinstance(number, bool) and number > 0
        and _text(head) and _authority_valid(authority, repository, root)
    )
    if not valid:
        reasons.append("merge authority is absent or does not cover the exact repository, PR, and head")
    return {"schemaVersion": 1, "kind": "merge-authority", "repository": repository, "pullRequest": number, "headOid": head, "observedAt": datetime.now(UTC).isoformat(), "decision": "allow" if not reasons else "block", "reasons": reasons, "authorityEvidence": authority}


def evaluate_delivery(  # noqa: MC0001
    manifest: object, statuses: object, cleanup_observation: object,
    *, execution_repository: str | None = None, execution_head: str | None = None,
) -> dict:
    reasons: list[str] = []
    delivery_unavailable = False
    owned = manifest.get("ownedPullRequests") if isinstance(manifest, dict) else None
    if not isinstance(owned, list) or not owned:
        return {"schemaVersion": 1, "kind": "delivery-status", "repository": execution_repository, "headOid": execution_head, "decision": "unavailable", "deliveryDecision": "unavailable", "cleanupDecision": "unavailable", "reasons": ["invalid owned pull-request manifest"], "mergedCount": 0, "observedAt": datetime.now(UTC).isoformat(), "pullRequests": [], "cleanup": cleanup_observation}
    if not isinstance(statuses, list):
        statuses = []
    by_key = {
        f"{item.get('repository')}#{item.get('number')}": item
        for item in statuses if isinstance(item, dict)
    }
    seen: set[str] = set()
    merged_count = 0
    results = []
    owned_heads: set[tuple[str, str]] = set()
    for item in owned:
        if not isinstance(item, dict) or not _text(item.get("repository")) or not isinstance(item.get("number"), int) or not _text(item.get("headOid")):
            reasons.append("invalid owned pull-request entry")
            delivery_unavailable = True
            continue
        key = f"{item['repository']}#{item['number']}"
        if key in seen:
            reasons.append(f"duplicate owned pull request {key}")
            delivery_unavailable = True
            continue
        for dependency in item.get("dependsOn", []):
            if dependency not in seen:
                reasons.append(f"dependency {dependency} must precede {key}")
        seen.add(key)
        owned_heads.add((item["repository"], item["headOid"]))
        status = by_key.get(key)
        if not isinstance(status, dict):
            reasons.append(f"live status unavailable for {key}")
            delivery_unavailable = True
            continue
        authority = item.get("authorityEvidence")
        authority_valid = _authority_valid(authority, item["repository"], Path(item.get("root") or "."))
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
    legacy_commit_proofs: list[dict[str, str]] = []
    supplied_legacy_proofs = manifest.get("legacyCommitProofs", [])
    if not isinstance(supplied_legacy_proofs, list):
        reasons.append("legacy commit proofs must be a list")
        delivery_unavailable = True
    else:
        for proof in supplied_legacy_proofs:
            valid = bool(
                isinstance(proof, dict)
                and set(proof) == {"repository", "headOid"}
                and _text(proof.get("repository"))
                and re.fullmatch(r"[0-9a-f]{40}", str(proof.get("headOid", "")))
                and (proof["repository"], proof["headOid"]) in owned_heads
            )
            if not valid:
                reasons.append("legacy commit proof is not bound to an owned pull-request head")
                delivery_unavailable = True
                continue
            legacy_commit_proofs.append(
                {"repository": proof["repository"], "head": proof["headOid"]}
            )
    delivery_reasons = list(reasons)
    cleanup_reasons: list[str] = []
    cleanup_decision = "unavailable"
    cleanup_unavailable = False
    cleanup = cleanup_observation
    if not isinstance(cleanup, dict):
        cleanup_reasons.append("live cleanup observation is missing")
        cleanup_unavailable = True
    else:
        if cleanup.get("primarySynced") is not True:
            cleanup_reasons.append("primary checkout is not synchronized by fast-forward")
        if cleanup.get("unrelatedDirtyPreserved") is not True:
            cleanup_reasons.append("unrelated dirty worktree preservation is not proven")
        outcome = cleanup.get("outcome", "complete")
        if outcome == "complete":
            for field in ("taskWorktreesAbsent", "taskBranchesAbsent"):
                if cleanup.get(field) is not True:
                    cleanup_reasons.append(f"scoped cleanup is incomplete: {field}")
            cleanup_decision = "block" if cleanup_reasons else "complete"
        elif outcome == "degraded":
            residues = cleanup.get("residues")
            warnings = cleanup.get("warnings")
            if merged_count != len(owned):
                cleanup_reasons.append("cleanup degradation is available only after every owned PR merged")
            if cleanup.get("residueSafe") is not True:
                cleanup_reasons.append("cleanup residue safety is not proven")
            if (
                not isinstance(residues, list)
                or not residues
                or not all(
                    isinstance(item, dict)
                    and set(item) == {"repository", "pullRequest", "worktree", "branch", "reasonCode"}
                    and _text(item.get("repository"))
                    and isinstance(item.get("pullRequest"), int)
                    and not isinstance(item.get("pullRequest"), bool)
                    and _text(item.get("worktree"))
                    and _text(item.get("branch"))
                    and item.get("reasonCode") == "removal-denied"
                    for item in residues
                )
            ):
                cleanup_reasons.append("cleanup residue records are missing")
            if warnings != ["cleanup-residue-remains"]:
                cleanup_reasons.append("cleanup degradation warnings are missing")
            cleanup_decision = "block" if cleanup_reasons else "degraded"
        else:
            cleanup_reasons.append("cleanup outcome is invalid")
            cleanup_decision = "block"
    reasons.extend(cleanup_reasons)
    delivery_decision = "unavailable" if delivery_unavailable else ("block" if delivery_reasons else "allow")
    decision = (
        "unavailable"
        if delivery_unavailable or cleanup_unavailable
        else "allow"
        if delivery_decision == "allow" and cleanup_decision in {"complete", "degraded"}
        else "block"
    )
    return {
        "schemaVersion": 1, "kind": "delivery-status",
        "repository": execution_repository, "headOid": execution_head,
        "observedAt": datetime.now(UTC).isoformat(),
        "decision": decision,
        "deliveryDecision": delivery_decision,
        "cleanupDecision": cleanup_decision,
        "reasons": reasons, "mergedCount": merged_count, "pullRequests": results,
        "legacyCommitProofs": legacy_commit_proofs,
        "cleanup": cleanup,
    }


def collect_delivery(manifest: dict, *, default_root: Path) -> list[dict]:
    if not isinstance(manifest, dict):
        raise ValueError("delivery manifest must be an object")
    statuses = []
    for owned in manifest.get("ownedPullRequests", []):
        if not isinstance(owned, dict):
            raise ValueError("each owned pull request must be an object")
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


def _git_read(runner, git: str, root: Path, *arguments: str) -> str:
    result = runner(
        [git, *arguments], cwd=root, capture_output=True, text=True, timeout=10, check=False
    )
    if result.returncode:
        raise ValueError(result.stderr.strip() or f"git {' '.join(arguments)} failed")
    return result.stdout


def _worktree_records(output: str) -> dict[str, dict[str, object]]:
    records: dict[str, dict[str, object]] = {}
    for block in output.strip().split("\n\n"):
        record: dict[str, object] = {"branch": None, "locked": False}
        record_path = None
        for line in block.splitlines():
            if line.startswith("worktree "):
                record_path = _normalized_path(line.removeprefix("worktree "))
            elif line.startswith("branch "):
                record["branch"] = line.removeprefix("branch refs/heads/")
            elif line.startswith("locked"):
                record["locked"] = True
        if record_path:
            records[record_path] = record
    return records


def _dirty_worktrees(runner, git: str, worktrees: set[str]) -> set[str]:
    dirty_worktrees = set()
    for value in worktrees:
        path = Path(value)
        result = runner(
            [git, "status", "--porcelain"],
            cwd=path,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        if result.returncode:
            raise ValueError(result.stderr.strip() or f"cannot inspect unrelated worktree {path}")
        if result.stdout.strip():
            dirty_worktrees.add(value)
    return dirty_worktrees


def _cleanup_snapshot(
    runner,
    git: str,
    root: Path,
    branch: str,
    named_worktrees: list[str],
) -> dict[str, object]:
    worktree_output = _git_read(runner, git, root, "worktree", "list", "--porcelain")
    records = _worktree_records(worktree_output)
    present = set(records)
    unrelated = present - {_normalized_path(root)} - set(named_worktrees)
    return {
        "local": _git_read(runner, git, root, "rev-parse", branch).strip(),
        "remoteHead": _git_read(runner, git, root, "rev-parse", f"origin/{branch}").strip(),
        "repository": _github_repository(
            _git_read(runner, git, root, "remote", "get-url", "origin")
        ),
        "records": records,
        "branches": _git_read(
            runner, git, root, "branch", "--format=%(refname:short)"
        ).splitlines(),
        "dirtyUnrelated": _dirty_worktrees(runner, git, unrelated),
    }


def _residue_head_is_safe(
    runner,
    git: str,
    root: Path,
    worktree: Path,
    default_branch: str,
    residue_branch: str,
    expected_head: str,
) -> bool:
    dirty = runner([git, "status", "--porcelain"], cwd=worktree, capture_output=True, text=True, timeout=10, check=False)
    cherry = runner([git, "cherry", f"origin/{default_branch}", residue_branch], cwd=root, capture_output=True, text=True, timeout=10, check=False)
    branch_head = runner([git, "rev-parse", residue_branch], cwd=root, capture_output=True, text=True, timeout=10, check=False)
    worktree_head = runner([git, "rev-parse", "HEAD"], cwd=worktree, capture_output=True, text=True, timeout=10, check=False)
    return all((
        dirty.returncode == 0,
        not dirty.stdout.strip(),
        cherry.returncode == 0,
        not any(line.startswith("+") for line in cherry.stdout.splitlines()),
        branch_head.returncode == 0,
        branch_head.stdout.strip() == expected_head,
        worktree_head.returncode == 0,
        worktree_head.stdout.strip() == expected_head,
    ))


def _policy_denied(result) -> bool:
    lines = [
        line.strip().casefold()
        for line in f"{result.stderr}\n{result.stdout}".splitlines()
        if line.strip()
    ]
    return result.returncode != 0 and lines in [[value] for value in (
        "policy denied",
        "denied by policy",
        "blocked by policy",
        "blocked by host policy",
        "host policy denied",
    )]


def _residue_scope_is_safe(
    *,
    delivery_authorized: bool,
    normalized_worktree: str,
    named_worktrees: list[str],
    residue_branch: object,
    named_branches: list[str],
    root: Path,
    default_branch: str,
    residue_repository: object,
    residue_pull_request: object,
    live_repository: object,
    expected_head: object,
    record: object,
) -> bool:
    return all((
        delivery_authorized,
        normalized_worktree in named_worktrees,
        residue_branch in named_branches,
        normalized_worktree != _normalized_path(root),
        residue_branch != default_branch,
        isinstance(residue_pull_request, int),
        not isinstance(residue_pull_request, bool),
        _text(residue_repository),
        isinstance(live_repository, str),
        str(live_repository).casefold() == str(residue_repository).casefold(),
        _text(expected_head),
        isinstance(record, dict),
        record.get("branch") == residue_branch if isinstance(record, dict) else False,
        record.get("locked") is False if isinstance(record, dict) else False,
    ))


def _post_denial_is_safe(
    runner,
    git: str,
    root: Path,
    default_branch: str,
    named_worktrees: list[str],
    unrelated_expected: set[str],
    residue_repository: object,
    residue_branch: str,
    worktree: Path,
    normalized_worktree: str,
    expected_head: str,
) -> bool:
    snapshot = _cleanup_snapshot(runner, git, root, default_branch, named_worktrees)
    record = snapshot["records"].get(normalized_worktree)
    return all((
        snapshot["local"] == snapshot["remoteHead"],
        isinstance(snapshot["repository"], str),
        str(snapshot["repository"]).casefold() == str(residue_repository).casefold(),
        residue_branch in snapshot["branches"],
        snapshot["dirtyUnrelated"] == unrelated_expected,
        isinstance(record, dict),
        record.get("branch") == residue_branch if isinstance(record, dict) else False,
        record.get("locked") is False if isinstance(record, dict) else False,
        _residue_head_is_safe(
            runner, git, root, worktree, default_branch, residue_branch, expected_head
        ) if isinstance(record, dict) else False,
    ))


def _inspect_degraded_residue(
    residue: object,
    *,
    manifest: dict,
    delivery_authorized: bool,
    snapshot: dict[str, object],
    runner,
    git: str,
    root: Path,
    default_branch: str,
    named_worktrees: list[str],
    named_branches: list[str],
    unrelated_expected: set[str],
) -> dict[str, object] | None:
    if not isinstance(residue, dict):
        return None
    worktree_value = residue.get("worktree")
    residue_branch = residue.get("branch")
    residue_repository = residue.get("repository")
    residue_pull_request = residue.get("pullRequest")
    if not _text(worktree_value) or not _text(residue_branch):
        return None
    normalized_worktree = _normalized_path(worktree_value)
    owner = next((
        item for item in manifest.get("ownedPullRequests", [])
        if isinstance(item, dict)
        and item.get("repository") == residue_repository
        and item.get("number") == residue_pull_request
    ), None)
    expected_head = owner.get("headOid") if isinstance(owner, dict) else None
    record = snapshot["records"].get(normalized_worktree)
    if not _residue_scope_is_safe(
        delivery_authorized=delivery_authorized,
        normalized_worktree=normalized_worktree,
        named_worktrees=named_worktrees,
        residue_branch=residue_branch,
        named_branches=named_branches,
        root=root,
        default_branch=default_branch,
        residue_repository=residue_repository,
        residue_pull_request=residue_pull_request,
        live_repository=snapshot["repository"],
        expected_head=expected_head,
        record=record,
    ):
        return None
    resolved_worktree = Path(worktree_value).resolve()
    if not _residue_head_is_safe(
        runner, git, root, resolved_worktree, default_branch, residue_branch, expected_head
    ):
        return None
    removal = runner(
        [git, "worktree", "remove", "--", str(resolved_worktree)],
        cwd=root, capture_output=True, text=True, timeout=10, check=False,
    )
    if not _policy_denied(removal) or not _post_denial_is_safe(
        runner, git, root, default_branch, named_worktrees, unrelated_expected,
        residue_repository, residue_branch, resolved_worktree, normalized_worktree,
        expected_head,
    ):
        return None
    return {
        "repository": residue_repository,
        "pullRequest": residue_pull_request,
        "worktree": str(resolved_worktree),
        "branch": residue_branch,
        "reasonCode": "removal-denied",
    }


def inspect_cleanup(
    manifest: dict,
    statuses: object = None,
    *,
    runner=None,
    executable: str | None = None,
) -> dict:
    """Inspect named cleanup targets and make at most one safe removal attempt in total."""
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
    degraded_requested = False
    all_residue_safe = True
    residues: list[dict[str, object]] = []
    warnings: list[str] = []
    delivery_authorized = evaluate_delivery(
        manifest,
        statuses,
        {
            "outcome": "complete",
            "primarySynced": True,
            "unrelatedDirtyPreserved": True,
            "taskWorktreesAbsent": True,
            "taskBranchesAbsent": True,
        },
    )["deliveryDecision"] == "allow"
    requested_residue_total = sum(
        len(value)
        for target in repositories if isinstance(target, dict)
        for value in [target.get("degradedResidues")]
        if isinstance(value, list)
    )
    for target in repositories:
        if not isinstance(target, dict) or not _text(target.get("root")) or not _text(target.get("defaultBranch")):
            raise ValueError("invalid cleanup repository target")
        root = Path(target["root"]).resolve()
        named_worktree_values = target.get("taskWorktrees")
        named_branches = target.get("taskBranches")
        unrelated_values = target.get("unrelatedDirtyWorktrees")
        if (
            not isinstance(named_worktree_values, list) or not named_worktree_values
            or not all(_text(value) for value in named_worktree_values)
            or not isinstance(named_branches, list) or not named_branches
            or not all(_text(value) for value in named_branches)
            or not isinstance(unrelated_values, list)
            or not all(_text(value) for value in unrelated_values)
        ):
            raise ValueError("cleanup scope must explicitly name task worktrees and branches")
        branch = target["defaultBranch"]
        named_worktrees = [_normalized_path(value) for value in named_worktree_values]
        snapshot = _cleanup_snapshot(runner, git, root, branch, named_worktrees)
        present_worktrees = set(snapshot["records"])
        synced = snapshot["local"] == snapshot["remoteHead"]
        worktrees_absent = not any(value in present_worktrees for value in named_worktrees)
        branches_absent = not any(value in snapshot["branches"] for value in named_branches)
        unrelated_expected = {_normalized_path(value) for value in unrelated_values}
        dirty_preserved = snapshot["dirtyUnrelated"] == unrelated_expected

        requested_residues = target.get("degradedResidues")
        if requested_residues is not None:
            degraded_requested = True
            if (
                not isinstance(requested_residues, list)
                or len(requested_residues) != 1
                or requested_residue_total != 1
            ):
                all_residue_safe = False
                warnings.append("cleanup-residue-receipt-missing")
            else:
                residue_record = _inspect_degraded_residue(
                    requested_residues[0], manifest=manifest,
                    delivery_authorized=delivery_authorized, snapshot=snapshot,
                    runner=runner, git=git, root=root, default_branch=branch,
                    named_worktrees=named_worktrees, named_branches=named_branches,
                    unrelated_expected=unrelated_expected,
                )
                safe = residue_record is not None
                all_residue_safe &= safe
                if residue_record:
                    residues.append(residue_record)
                    warnings.append("cleanup-residue-remains")
        all_synced &= synced
        all_worktrees_absent &= worktrees_absent
        all_branches_absent &= branches_absent
        all_dirty_preserved &= dirty_preserved
        observations.append({"root": str(root), "primarySynced": synced, "taskWorktreesAbsent": worktrees_absent, "taskBranchesAbsent": branches_absent, "unrelatedDirtyPreserved": dirty_preserved})
    return {
        "outcome": "degraded" if degraded_requested else "complete",
        "primarySynced": all_synced,
        "taskWorktreesAbsent": all_worktrees_absent,
        "taskBranchesAbsent": all_branches_absent,
        "unrelatedDirtyPreserved": all_dirty_preserved,
        "residueSafe": all_residue_safe if degraded_requested else True,
        "residues": residues,
        "warnings": warnings,
        "repositories": observations,
    }
