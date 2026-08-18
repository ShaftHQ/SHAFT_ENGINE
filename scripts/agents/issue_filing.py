"""Validate template-compliant issue plans and produce creation receipts."""

from __future__ import annotations

from datetime import UTC, datetime
from contextlib import contextmanager
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess  # nosec B404 - fixed gh list/create arguments, no shell.
import tempfile
import time
if os.name == "nt":
    import msvcrt
else:
    import fcntl
from urllib.parse import urlparse


REQUIRED_HEADINGS = {
    "bug": (
        "Describe the Bug",
        "Steps to Reproduce",
        "Expected Behavior",
        "Actual Behavior",
        "User Scenarios & Testing",
        "Edge Cases",
        "Functional Requirements",
        "Success Criteria",
    ),
    "enhancement": (
        "Problem Statement",
        "Proposed Solution",
        "Alternatives Considered",
        "Use Case & Impact",
        "User Scenarios & Testing",
        "Edge Cases",
        "Functional Requirements",
        "Success Criteria",
    ),
}
SCOPE_HEADINGS = ("Assumptions", "Out of scope")


def _text(value) -> bool:
    return isinstance(value, str) and bool(value.strip())


def _texts(value) -> bool:
    return isinstance(value, list) and bool(value) and all(_text(item) for item in value)


def _issue_url(value) -> bool:
    if not _text(value):
        return False
    parsed = urlparse(value)
    if parsed.scheme != "https" or not parsed.netloc:
        return False
    host = parsed.netloc.lower()
    path = parsed.path or ""
    if host == "github.com" and "/issues/" in path:
        return True
    if "/-/issues/" in path:
        return True
    if ("dev.azure.com" in host or host.endswith("visualstudio.com")) and "/_workitems/" in path:
        return True
    return False


def build_glab_issue_create_argv(plan: dict, repository: str, *, executable: str = "glab") -> list[str]:
    return [
        executable, "issue", "create", "--repo", repository, "--title", plan["title"],
        "--description", plan["body"], "--label", ",".join(plan.get("labels") or []),
    ]


def build_az_boards_create_argv(
    plan: dict, *, organization: str, project: str, executable: str = "az", work_item_type: str = "Issue",
) -> list[str]:
    return [
        executable, "boards", "work-item", "create", "--organization", organization,
        "--project", project, "--title", plan["title"], "--type", work_item_type,
        "--description", plan["body"],
    ]


def validate_issue_plan(plan: object, taxonomy: object) -> dict:  # noqa: MC0001
    reasons: list[str] = []
    if not isinstance(plan, dict) or not isinstance(taxonomy, dict):
        return {"schemaVersion": 1, "kind": "issue-plan", "decision": "unavailable", "reasons": ["invalid plan or taxonomy"]}
    labels = plan.get("labels")
    labels = labels if isinstance(labels, list) and all(_text(item) for item in labels) else []
    primary = [item for item in labels if item in taxonomy.get("primaryTypes", [])]
    lifecycle = [item for item in labels if item in taxonomy.get("lifecycle", [])]
    subsystems = [item for item in labels if item in taxonomy.get("subsystems", [])]
    issue_type = primary[0] if len(primary) == 1 else None
    state = lifecycle[0] if len(lifecycle) == 1 else None
    if not _text(plan.get("title")) or len(plan.get("title", "").strip()) < 10:
        reasons.append("meaningful title is required")
    if len(primary) != 1:
        reasons.append("exactly one primary type is required")
    if len(lifecycle) != 1:
        reasons.append("exactly one lifecycle label is required")
    if not subsystems:
        reasons.append("at least one proven subsystem or module is required")
    module_labels = [label for label in subsystems if label.startswith("module:")]
    if len(module_labels) > 1 and "cross-cutting" not in labels:
        reasons.append("multiple modules require explicit cross-cutting classification")
    if issue_type and plan.get("template") != taxonomy.get("templates", {}).get(issue_type):
        reasons.append("template does not match primary type")
    body = plan.get("body")
    if not _text(body):
        reasons.append("rendered template body is required")
    elif issue_type:
        for heading in REQUIRED_HEADINGS[issue_type]:
            if heading not in body:
                reasons.append(f"template field is missing: {heading}")
        if not any(heading in body for heading in SCOPE_HEADINGS):
            reasons.append("template field is missing: Assumptions or Out of scope")
    if not _texts(plan.get("acceptanceCriteria")):
        reasons.append("acceptance criteria are required")
    if state == "ready" and not _texts(plan.get("proofPlan")):
        reasons.append("ready issues require a proof plan")
    if state == "blocked" and (
        not _texts(plan.get("dependencies")) or not all(_issue_url(item) for item in plan["dependencies"])
    ):
        reasons.append("blocked issues require dependency links")
    search = plan.get("duplicateSearch")
    if not isinstance(search, dict) or not _text(search.get("query")) or not isinstance(search.get("open"), list) or not isinstance(search.get("closed"), list):
        reasons.append("open and closed duplicate search receipt is required")
        matches = []
    else:
        matches = [*search["open"], *search["closed"]]
        if any(not _issue_url(item) for item in matches):
            reasons.append("duplicate matches must be issue URLs")
    related = plan.get("related")
    if not isinstance(related, list) or any(not _text(item) for item in related):
        reasons.append("related references must be a list")
    return {
        "schemaVersion": 1, "kind": "issue-plan", "observedAt": datetime.now(UTC).isoformat(),
        "decision": "block" if reasons else "allow", "reasons": reasons,
        "template": plan.get("template"), "type": issue_type, "subsystems": subsystems,
        "lifecycle": state, "labels": labels, "dependencyLinks": plan.get("dependencies", []),
        "duplicatePolicy": taxonomy.get("duplicatePolicy"), "duplicateMatches": matches,
    }


def receipt_digest(receipt: dict) -> str:
    stable = {key: value for key, value in receipt.items() if key not in {"observedAt", "sha256"}}
    return hashlib.sha256(json.dumps(stable, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def confirmation_digest(plan: dict, taxonomy: dict, repository: str) -> str:
    validation = validate_issue_plan(plan, taxonomy)
    stable_validation = {key: value for key, value in validation.items() if key != "observedAt"}
    payload = {"repository": repository, "plan": plan, "validation": stable_validation}
    return receipt_digest(payload)


def search_duplicates(repository: str, query: str, *, runner=None, executable: str | None = None) -> dict:
    runner = subprocess.run if runner is None else runner
    gh = executable or shutil.which("gh")
    if not gh or not _text(repository) or "/" not in repository or not _text(query):
        raise ValueError("duplicate search requires gh, owner/repo, and query")
    result = {"query": query, "open": [], "closed": []}
    for state in ("open", "closed"):
        completed = runner(
            [gh, "issue", "list", "--repo", repository, "--state", state, "--search", query,
             "--limit", "1000", "--json", "url"],
            capture_output=True, text=True, timeout=30, check=False,
        )
        if completed.returncode:
            raise ValueError(completed.stderr.strip() or "duplicate search failed")
        try:
            values = json.loads(completed.stdout or "[]")
        except json.JSONDecodeError as error:
            raise ValueError("duplicate search returned invalid JSON") from error
        if not isinstance(values, list) or any(not isinstance(item, dict) or not _text(item.get("url")) for item in values):
            raise ValueError("duplicate search returned an invalid shape")
        result[state] = [item["url"] for item in values]
    return result


def prepare_issue_plan(plan: dict, taxonomy: dict, repository: str, **transport) -> dict:
    candidate = dict(plan)
    query = plan.get("duplicateSearch", {}).get("query") if isinstance(plan.get("duplicateSearch"), dict) else plan.get("title")
    candidate["duplicateSearch"] = search_duplicates(repository, query, **transport)
    receipt = validate_issue_plan(candidate, taxonomy)
    receipt["normalizedPlan"] = candidate
    return receipt


@contextmanager
def _creation_lock(repository: str, confirmation: str):
    """Serialize same-host retries; GitHub's issue-create API has no idempotency key."""
    key = hashlib.sha256(f"{repository}:{confirmation}".encode()).hexdigest()
    path = Path(tempfile.gettempdir()) / f"act-as-mohab-issue-{key}.lock"
    handle = path.open("a+b")
    acquired = False
    for _ in range(4000):
        try:
            if os.name == "nt":
                handle.seek(0)
                if handle.read(1) == b"":
                    handle.seek(0); handle.write(b"0"); handle.flush()
                handle.seek(0)
                msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
            else:
                fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            acquired = True
            break
        except OSError:
            time.sleep(0.05)
    if not acquired:
        handle.close()
        raise ValueError("timed out waiting for concurrent issue creation")
    try:
        yield
    finally:
        try:
            if os.name == "nt":
                handle.seek(0); msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
        finally:
            handle.close()


def create_issue(plan: dict, taxonomy: dict, repository: str, confirmation: str, *, runner=None, executable: str | None = None) -> dict:
    with _creation_lock(repository, confirmation):
        return _create_issue_locked(plan, taxonomy, repository, confirmation, runner=runner, executable=executable)


def _create_issue_locked(plan: dict, taxonomy: dict, repository: str, confirmation: str, *, runner=None, executable: str | None = None) -> dict:
    runner = subprocess.run if runner is None else runner
    gh = executable or shutil.which("gh")
    dry_receipt = validate_issue_plan(plan, taxonomy)
    if dry_receipt["decision"] != "allow" or confirmation_digest(plan, taxonomy, repository) != confirmation:
        raise ValueError("issue creation confirmation does not match the validated dry-run receipt")
    marker = f"<!-- act-as-mohab:{confirmation} -->"
    marker_search = search_duplicates(
        repository, f'"act-as-mohab:{confirmation}" in:body', runner=runner, executable=gh
    )
    marker_matches = [*marker_search["open"], *marker_search["closed"]]
    if marker_matches:
        return {**dry_receipt, "issueUrl": marker_matches[0], "idempotencyMarker": marker, "reused": True}
    live_search = search_duplicates(repository, plan["duplicateSearch"]["query"], runner=runner, executable=gh)
    if live_search != plan["duplicateSearch"]:
        raise ValueError("duplicate search changed; rerun dry-run before creation")
    duplicate_links = dry_receipt["duplicateMatches"]
    duplicate_note = "\n\nRelated duplicate search matches:\n" + "\n".join(
        f"- {url}" for url in duplicate_links
    ) if duplicate_links else ""
    body = plan["body"] + duplicate_note + "\n\n" + marker
    completed = runner(
        [gh, "issue", "create", "--repo", repository, "--title", plan["title"], "--body", body,
         "--label", ",".join(dry_receipt["labels"])],
        capture_output=True, text=True, timeout=30, check=False,
    )
    if completed.returncode or not completed.stdout.strip().startswith("https://github.com/"):
        raise ValueError(completed.stderr.strip() or "issue creation returned no URL")
    return {**dry_receipt, "issueUrl": completed.stdout.strip(), "idempotencyMarker": marker}


def reconcile_labels(repository: str, taxonomy: dict, *, apply: bool = False, runner=None, executable: str | None = None) -> dict:  # noqa: MC0001
    runner = subprocess.run if runner is None else runner
    gh = executable or shutil.which("gh")
    if not gh:
        raise ValueError("GitHub CLI is required for label reconciliation")
    completed = runner([gh, "label", "list", "--repo", repository, "--limit", "1000", "--json", "name"], capture_output=True, text=True, timeout=30, check=False)
    if completed.returncode:
        raise ValueError(completed.stderr.strip() or "cannot list labels")
    try:
        existing = [item["name"] for item in json.loads(completed.stdout)]
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise ValueError("label list returned invalid JSON") from error
    canonical = [*taxonomy.get("primaryTypes", []), *taxonomy.get("supplemental", []), *taxonomy.get("lifecycle", []), *taxonomy.get("subsystems", [])]
    case_drift = [{"canonical": label, "existing": found} for label in canonical for found in existing if label.lower() == found.lower() and label != found]
    missing = [label for label in canonical if label.lower() not in {value.lower() for value in existing}]
    aliases = [{"existing": old, "canonical": new} for old, new in taxonomy.get("aliases", {}).items() if old in existing]
    if apply:
        exact = set(existing)
        for drift in [*case_drift, *aliases]:
            old = drift["existing"]
            new = drift["canonical"]
            if new not in exact:
                result = runner([gh, "label", "edit", old, "--repo", repository, "--name", new], capture_output=True, text=True, timeout=30, check=False)
                if result.returncode:
                    raise ValueError(result.stderr.strip() or f"cannot reconcile label {old}")
                exact.add(new)
        for label in missing:
            if label in exact:
                continue
            result = runner([gh, "label", "create", label, "--repo", repository, "--color", "ededed", "--description", "Canonical automated taxonomy"], capture_output=True, text=True, timeout=30, check=False)
            if result.returncode:
                raise ValueError(result.stderr.strip() or f"cannot create label {label}")
    issues = runner([gh, "issue", "list", "--repo", repository, "--state", "open", "--limit", "1000", "--json", "number,url,labels"], capture_output=True, text=True, timeout=30, check=False)
    if issues.returncode:
        raise ValueError(issues.stderr.strip() or "cannot audit open issue labels")
    try:
        open_issues = json.loads(issues.stdout or "[]")
        migration = []
        applied_migrations = []
        for issue in open_issues:
            names = [item["name"] for item in issue.get("labels", [])]
            if apply:
                for drift in [*case_drift, *aliases]:
                    if drift["existing"] not in names:
                        continue
                    result = runner(
                        [gh, "issue", "edit", str(issue["number"]), "--repo", repository,
                         "--add-label", drift["canonical"], "--remove-label", drift["existing"]],
                        capture_output=True, text=True, timeout=30, check=False,
                    )
                    if result.returncode:
                        raise ValueError(result.stderr.strip() or f"cannot migrate issue {issue['number']} label")
                    applied_migrations.append({"number": issue["number"], **drift})
            if sum(name in taxonomy["primaryTypes"] for name in names) != 1 or sum(name in taxonomy["lifecycle"] for name in names) != 1 or not any(name in taxonomy["subsystems"] for name in names):
                migration.append({"number": issue["number"], "url": issue["url"], "labels": names})
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise ValueError("open issue audit returned invalid JSON") from error
    return {"schemaVersion": 1, "kind": "label-reconciliation", "repository": repository, "applied": apply, "missing": missing, "caseDrift": case_drift, "aliases": aliases, "migrationAudit": migration, "appliedMigrations": applied_migrations}


def transition_issue(repository: str, number: int, plan: dict, taxonomy: dict, *, runner=None, executable: str | None = None) -> dict:
    """Apply one validated lifecycle transition, removing every prior lifecycle label."""
    receipt = validate_issue_plan(plan, taxonomy)
    if receipt["decision"] != "allow" or not isinstance(number, int) or number < 1:
        raise ValueError("lifecycle transition requires a valid updated issue plan and number")
    runner = subprocess.run if runner is None else runner
    gh = executable or shutil.which("gh")
    current = runner([gh, "issue", "view", str(number), "--repo", repository, "--json", "labels"], capture_output=True, text=True, timeout=30, check=False)
    if current.returncode:
        raise ValueError(current.stderr.strip() or "cannot read issue lifecycle")
    try:
        names = [item["name"] for item in json.loads(current.stdout)["labels"]]
    except (json.JSONDecodeError, KeyError, TypeError) as error:
        raise ValueError("issue lifecycle response is invalid") from error
    aliases = {key.lower(): value for key, value in taxonomy.get("aliases", {}).items()}
    lifecycle = {label.lower(): label for label in taxonomy["lifecycle"]}
    old = []
    for label in names:
        canonical = aliases.get(label.lower(), lifecycle.get(label.lower()))
        if canonical in taxonomy["lifecycle"] and label != receipt["lifecycle"]:
            old.append(label)
    command = [gh, "issue", "edit", str(number), "--repo", repository, "--add-label", receipt["lifecycle"]]
    for label in old:
        command.extend(("--remove-label", label))
    updated = runner(command, capture_output=True, text=True, timeout=30, check=False)
    if updated.returncode:
        raise ValueError(updated.stderr.strip() or "cannot update issue lifecycle")
    return {"schemaVersion": 1, "kind": "issue-lifecycle-transition", "repository": repository, "issue": number, "removed": old, "lifecycle": receipt["lifecycle"], "validation": receipt}
