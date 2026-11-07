#!/usr/bin/env python3
"""Fail-closed merge-queue diagnostic for Dependabot (#7704).

Scheduled and workflow_dispatch runs inventory real queue membership, the
synthetic merge-group SHA, and both required checks. They do not enqueue,
dequeue, or re-arm. Dependabot opened, reopened, and synchronize events
enqueue once with BOT_TOKEN when dependabot_enqueue_decision says so.
GITHUB_TOKEN is never that credential: events it creates start no
merge_group workflows. A dependabot-hold label is authoritative.

Empty check lists and API errors are different. Abandoned or cancelled
installer results are not success. Looking up the pull-request head SHA does
not prove anything about the merge-group commit.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Mapping, Sequence

REQUIRED_MERGE_GROUP_WORKFLOWS = frozenset({
    "PR Gate",
    "Release-note governance",
})
REQUIRED_QUEUE_CHECKS = frozenset({
    "PR Gate Summary",
    "Release-note governance",
})
GREEN_CONCLUSIONS = frozenset({"success", "skipped"})
RETRY_BUDGET = 2
GRACE_SECONDS = 20 * 60


@dataclass(frozen=True)
class Diagnosis:
    code: str
    mutate: bool
    exit_code: int
    detail: str


def conclusion_is_green(value: object) -> bool:
    """True only for check conclusions the PR Gate summary already accepts."""
    return str(value or "").strip().lower() in GREEN_CONCLUSIONS


def installer_result_is_green(value: object) -> bool:
    """Abandoned and cancelled installer runs stay non-green."""
    return conclusion_is_green(value)


def required_workflows_declare_merge_group(workflows: Mapping[str, Any]) -> list[str]:
    """Return workflow file names that lack an ``on.merge_group`` trigger."""
    missing: list[str] = []
    for name, document in workflows.items():
        triggers = document.get("on") or document.get(True) or {}
        if not isinstance(triggers, Mapping) or "merge_group" not in triggers:
            missing.append(name)
    return sorted(missing)


def _diag(code: str, exit_code: int, detail: str) -> Diagnosis:
    return Diagnosis(code, False, exit_code, detail)


def _holds(entry: Mapping[str, Any]) -> set[str]:
    return {str(item) for item in (entry.get("hold_heads") or []) if str(item)}


def _checks_for(entry: Mapping[str, Any], sha: str) -> list[Mapping[str, Any]]:
    checks = entry.get("checks") or []
    if not sha:
        return []
    return [
        check for check in checks
        if isinstance(check, Mapping) and str(check.get("sha") or "") == sha
    ]


def _matching_checks(entry: Mapping[str, Any]) -> list[Mapping[str, Any]]:
    return _checks_for(entry, str(entry.get("queue_sha") or ""))


def _required_map(checks: Sequence[Mapping[str, Any]]) -> dict[str, str]:
    found: dict[str, str] = {}
    for check in checks:
        name = str(check.get("name") or "")
        if name in REQUIRED_QUEUE_CHECKS:
            found[name] = str(check.get("conclusion") or "").strip().lower()
    return found


def _named_checks_green(checks: Sequence[Mapping[str, Any]]) -> bool:
    found = _required_map(checks)
    if set(found) != set(REQUIRED_QUEUE_CHECKS):
        return False
    return all(conclusion_is_green(value) for value in found.values())


def queue_checks_green(entry: Mapping[str, Any]) -> bool:
    """Both required checks are present on the synthetic queue SHA and green."""
    return _named_checks_green(_matching_checks(entry))


def head_checks_green(entry: Mapping[str, Any]) -> bool:
    """Both required checks are green on the pull-request head SHA."""
    return _named_checks_green(_checks_for(entry, str(entry.get("head_sha") or "")))


def missing_either_required_check(entry: Mapping[str, Any]) -> bool:
    """True when either required check is absent or not green on the queue SHA."""
    return not queue_checks_green(entry)


def wrong_sha_evidence(entry: Mapping[str, Any]) -> bool:
    """PR-head check runs do not count as the merge-group commit."""
    head = str(entry.get("head_sha") or "")
    queue = str(entry.get("queue_sha") or "")
    checks = [item for item in (entry.get("checks") or []) if isinstance(item, Mapping)]
    if entry.get("queued") and not queue:
        return True
    if not queue or not head or head == queue or not checks:
        return False
    on_queue = [item for item in checks if str(item.get("sha") or "") == queue]
    on_head = [item for item in checks if str(item.get("sha") or "") == head]
    return bool(on_head) and not on_queue


def _installer_checks_failed(entry: Mapping[str, Any], checks: Sequence[Mapping[str, Any]]) -> bool:
    explicit = entry.get("installer_result")
    if explicit is not None and not installer_result_is_green(explicit):
        return True
    for check in checks:
        if "installer" not in str(check.get("name") or "").lower():
            continue
        conclusion = str(check.get("conclusion") or "").strip().lower()
        if conclusion and not conclusion_is_green(conclusion):
            return True
    return False


def _installer_failed(entry: Mapping[str, Any]) -> bool:
    return _installer_checks_failed(entry, _matching_checks(entry))


def _is_young(entry: Mapping[str, Any]) -> bool:
    now = entry.get("now_epoch")
    started = entry.get("enqueued_at_epoch")
    if now is None or started is None:
        return False
    age = int(now) - int(started)
    return 0 <= age < GRACE_SECONDS


def _policy_block(entry: Mapping[str, Any]) -> Diagnosis | None:
    if entry.get("api_error") or entry.get("checks_api_error"):
        return _diag("api_error", 1, "queue or check lookup failed; not an empty run list")
    head = str(entry.get("head_sha") or "")
    if head and head in _holds(entry):
        return _diag("explicit_hold", 0, "manual hold is authoritative for this head")
    if wrong_sha_evidence(entry):
        return _diag("wrong_sha", 1, "synthetic merge-group SHA was not the lookup key")
    if _installer_failed(entry):
        return _diag("installer_not_green", 1, "abandoned or cancelled installer is not success")
    return None


def _healthy_code(entry: Mapping[str, Any]) -> Diagnosis:
    queued = bool(entry.get("queued"))
    auto_merge = entry.get("auto_merge_request")
    if queued and auto_merge is None:
        return _diag("already_queued", 0, "null autoMergeRequest while queued is not a re-arm")
    return _diag("healthy", 0, "both required checks are green on the queue SHA")


def classify(entry: Mapping[str, Any]) -> Diagnosis:
    """Read-only verdict. ``mutate`` is always false."""
    if not isinstance(entry, Mapping):
        return _diag("api_error", 1, "entry is not an object")
    blocked = _policy_block(entry)
    if blocked is not None:
        return blocked
    if queue_checks_green(entry):
        return _healthy_code(entry)
    if int(entry.get("timeouts") or 0) >= RETRY_BUDGET:
        return _diag("retry_exhausted", 1, "retry budget spent; no dequeue and no re-arm")
    if _is_young(entry):
        return _diag("young_queue_entry", 0, "queue entry is inside the age grace")
    return _diag("missing_required_check", 1, "a required queue check is missing or not green")


def _enqueue_refused(entry: Mapping[str, Any]) -> bool:
    head = str(entry.get("head_sha") or "")
    if not head or head in _holds(entry) or entry.get("queued"):
        return True
    if entry.get("opt_in_head") != head or not entry.get("opt_in_verified"):
        return True
    if int(entry.get("timeouts") or 0) >= RETRY_BUDGET:
        return True
    return bool(entry.get("api_error") or entry.get("checks_api_error"))


def may_enqueue(entry: Mapping[str, Any]) -> bool:
    """Maintainer opt-in bound to this head. The default is no.

    A hold wins over opt-in. Synchronize does not clear a hold for that head.
    Already-queued PRs are not re-armed, even when autoMergeRequest is null.
    The scheduled workflow does not call this. Dependabot pull_request_target
    uses dependabot_enqueue_decision and BOT_TOKEN instead.
    """
    if not isinstance(entry, Mapping) or _enqueue_refused(entry):
        return False
    head = str(entry.get("head_sha") or "")
    if _installer_checks_failed(entry, _checks_for(entry, head)):
        return False
    return head_checks_green(entry)


DEPENDABOT_LOGINS = frozenset({"dependabot[bot]", "app/dependabot"})
HOLD_LABEL = "dependabot-hold"


def dependabot_enqueue_decision(pull: Mapping[str, Any]) -> str:
    """Return ``enqueue`` or ``skip:<reason>`` for one Dependabot pull request.

    Callers are opened, reopened, and synchronize only. A schedule must not
    use a skip as a reason to try again. Null autoMergeRequest plus a
    merge-queue entry means the pull request is already queued.
    """
    if not isinstance(pull, Mapping):
        return "skip:not-an-object"
    author = pull.get("author")
    if isinstance(author, Mapping):
        login = str(author.get("login") or "")
    elif isinstance(author, str):
        login = author
    else:
        login = ""
    if login not in DEPENDABOT_LOGINS:
        return "skip:not-dependabot"
    if pull.get("isDraft") is True:
        return "skip:draft"
    if pull.get("mergeable") == "CONFLICTING" or pull.get("mergeStateStatus") == "DIRTY":
        return "skip:conflict"
    labels = pull.get("labels") or []
    if isinstance(labels, list):
        for label in labels:
            name = label.get("name") if isinstance(label, Mapping) else label
            if str(name or "") == HOLD_LABEL:
                return "skip:hold"
    entry = pull.get("mergeQueueEntry")
    if isinstance(entry, Mapping) and entry:
        return "skip:already-queued"
    auto = pull.get("autoMergeRequest")
    if isinstance(auto, Mapping) and auto:
        return "skip:already-armed"
    return "enqueue"


def _epoch(value: object) -> int | None:
    if not isinstance(value, str) or not value:
        return None
    try:
        moment = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None
    if moment.tzinfo is None:
        moment = moment.replace(tzinfo=timezone.utc)
    return int(moment.timestamp())


def _load_checks(directory: Path | None) -> dict[str, list[dict[str, Any]] | None]:
    found: dict[str, list[dict[str, Any]] | None] = {}
    if directory is None or not directory.is_dir():
        return found
    for path in sorted(directory.glob("*.json")):
        payload = json.loads(path.read_text(encoding="utf-8"))
        sha = str(payload.get("sha") or path.stem)
        if payload.get("api_error"):
            found[sha] = None
            continue
        runs = payload.get("runs") if "runs" in payload else payload.get("checks")
        found[sha] = list(runs or [])
    return found


def _entry_from_node(
    node: Mapping[str, Any],
    checks_by_sha: Mapping[str, list[dict[str, Any]] | None],
    *,
    now_epoch: int,
) -> dict[str, Any]:
    pull = node.get("pullRequest") if isinstance(node.get("pullRequest"), Mapping) else {}
    commit = node.get("headCommit") if isinstance(node.get("headCommit"), Mapping) else {}
    queue_sha = str(commit.get("oid") or "")
    auto_merge = pull.get("autoMergeRequest") if isinstance(pull, Mapping) else None
    enabled = auto_merge.get("enabledAt") if isinstance(auto_merge, Mapping) else auto_merge
    known = queue_sha in checks_by_sha
    runs = checks_by_sha.get(queue_sha) if known else None
    return {
        "state": node.get("state"),
        "queue_sha": queue_sha,
        "head_sha": str(pull.get("headRefOid") or "") if isinstance(pull, Mapping) else "",
        "queued": True,
        "auto_merge_request": enabled,
        "number": pull.get("number") if isinstance(pull, Mapping) else None,
        "enqueued_at_epoch": _epoch(node.get("enqueuedAt")),
        "now_epoch": now_epoch,
        "checks": list(runs or []),
        "checks_api_error": bool(queue_sha) and (not known or runs is None),
        "timeouts": 0,
        "event": "schedule",
    }


def diagnoses_from_document(
    document: Mapping[str, Any],
    checks_by_sha: Mapping[str, list[dict[str, Any]] | None] | None = None,
    *,
    now_epoch: int | None = None,
) -> list[Diagnosis]:
    """Classify a GraphQL inventory or a normalized ``entries`` payload."""
    if document.get("errors") or document.get("api_error"):
        return [_diag("api_error", 1, "inventory failed")]
    if "entries" in document and "data" not in document:
        entries = document.get("entries") or []
        if not isinstance(entries, list):
            return [_diag("api_error", 1, "entries payload is not a list")]
        return [classify(entry) if isinstance(entry, Mapping) else _diag("api_error", 1, "bad entry")
                for entry in entries]
    repository = (document.get("data") or {}).get("repository") if isinstance(document.get("data"), Mapping) else None
    queue = repository.get("mergeQueue") if isinstance(repository, Mapping) else None
    if not isinstance(queue, Mapping):
        return [_diag("api_error", 1, "mergeQueue missing")]
    nodes = (queue.get("entries") or {}).get("nodes") if isinstance(queue.get("entries"), Mapping) else None
    if not isinstance(nodes, list):
        return [_diag("api_error", 1, "mergeQueue entries missing")]
    moment = int(datetime.now(timezone.utc).timestamp()) if now_epoch is None else now_epoch
    attached = checks_by_sha or {}
    return [_diag("api_error", 1, "malformed queue node") if not isinstance(node, Mapping)
            else classify(_entry_from_node(node, attached, now_epoch=moment))
            for node in nodes]


def queue_shas(document: Mapping[str, Any]) -> list[str]:
    """Synthetic merge-group SHAs. Raises ValueError when the inventory is unusable."""
    if document.get("errors") or document.get("api_error"):
        raise ValueError("graphql inventory failed")
    repository = (document.get("data") or {}).get("repository") if isinstance(document.get("data"), Mapping) else None
    queue = repository.get("mergeQueue") if isinstance(repository, Mapping) else None
    if not isinstance(queue, Mapping):
        raise ValueError("mergeQueue missing")
    nodes = (queue.get("entries") or {}).get("nodes") if isinstance(queue.get("entries"), Mapping) else None
    if not isinstance(nodes, list):
        raise ValueError("mergeQueue entries missing")
    shas: list[str] = []
    for node in nodes:
        if not isinstance(node, Mapping):
            raise ValueError("malformed queue node")
        commit = node.get("headCommit") if isinstance(node.get("headCommit"), Mapping) else {}
        sha = str(commit.get("oid") or "")
        if not sha:
            raise ValueError("queue entry has no synthetic headCommit.oid")
        shas.append(sha)
    return shas


def _print_diagnoses(rows: Sequence[Diagnosis]) -> int:
    if not rows:
        print("queue inventory: empty")
        return 0
    exit_code = 0
    for row in rows:
        print(f"{row.code}\texit={row.exit_code}\tmutate={str(row.mutate).lower()}\t{row.detail}")
        exit_code = max(exit_code, row.exit_code)
    return exit_code


def _read_json(path: str) -> Any:
    return json.loads(Path(path).read_text(encoding="utf-8"))


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    shas = sub.add_parser("shas", help="print synthetic queue SHAs, or fail closed")
    shas.add_argument("--queue-json", required=True)

    diagnose = sub.add_parser("diagnose", help="read-only verdict; non-zero on diagnostic failure")
    diagnose.add_argument("--queue-json", required=True)
    diagnose.add_argument("--checks-dir", default="")

    decide = sub.add_parser("enqueue-decision", help="print enqueue or skip:<reason>")
    decide.add_argument("--pr-json", required=True)

    args = parser.parse_args(argv)
    document = _read_json(args.queue_json if args.command != "enqueue-decision" else args.pr_json)
    if not isinstance(document, Mapping):
        print("api_error\tinventory JSON is not an object", file=sys.stderr)
        return 1
    if args.command == "enqueue-decision":
        print(dependabot_enqueue_decision(document))
        return 0
    if args.command == "shas":
        try:
            found = queue_shas(document)
        except ValueError as exc:
            print(f"api_error\t{exc}", file=sys.stderr)
            return 1
        for sha in found:
            print(sha)
        return 0
    directory = Path(args.checks_dir) if args.checks_dir else None
    rows = diagnoses_from_document(document, _load_checks(directory))
    return _print_diagnoses(rows)


if __name__ == "__main__":
    sys.exit(main())
