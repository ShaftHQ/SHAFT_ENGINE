"""Build fail-closed, head-bound pull-request feedback audit receipts."""

from __future__ import annotations

from datetime import UTC, datetime
import hashlib
import json
from typing import Any
from urllib.parse import urlparse
import re


SURFACES = ("threads", "reviews", "conversationComments", "annotations")
DISPOSITION_MARKER = re.compile(r"<!-- act-as-mohab-disposition:([^\s>]+) -->\s*$")
THREAD_QUERY = """
query($owner:String!,$name:String!,$pr:Int!,$endCursor:String) {
  repository(owner:$owner,name:$name) { pullRequest(number:$pr) {
    reviewThreads(first:100,after:$endCursor) {
      nodes { id isResolved comments(last:1) { nodes { url body } } }
      pageInfo { hasNextPage endCursor }
    }
  } }
}
"""


def _text(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip())


def _finding(surface: str, item: dict) -> bool:
    if surface == "threads":
        return _text(item.get("body"))
    if surface == "reviews":
        return item.get("state") != "DISMISSED" and _text(item.get("body"))
    if surface == "annotations":
        return str(item.get("level", "")).lower() in {"warning", "failure", "error"}
    return _text(item.get("body"))


def _github_url(value: object) -> bool:
    if not _text(value):
        return False
    parsed = urlparse(value)
    return parsed.scheme == "https" and parsed.netloc == "github.com"


def _github_issue_url(value: object) -> bool:
    return _github_url(value) and "/issues/" in urlparse(value).path


def _valid_disposition(value: object, finding: dict, live_replies: set[tuple[str, str]]) -> bool:
    if not isinstance(value, dict) or value.get("resolved") is not True or not _github_url(value.get("replyUrl")):
        return False
    if (value["replyUrl"], finding["id"]) not in live_replies:
        return False
    if finding["surface"] == "threads" and finding.get("liveResolved") is not True:
        return False
    kind = value.get("disposition")
    if kind == "valid":
        return True
    if kind == "false-positive":
        return _text(value.get("justification"))
    if kind == "approved-follow-up":
        return _github_issue_url(value.get("issueUrl")) and _text(value.get("approvalEvidence"))
    return False


def collect_pr_snapshot(client, number: int) -> dict:  # noqa: C901
    """Collect every relevant GitHub surface with explicit pagination."""
    if not isinstance(number, int) or isinstance(number, bool) or number < 1:
        raise ValueError("pull request number must be positive")
    pull = client.get(f"pulls/{number}")
    pull_author = pull.get("user", {}).get("login") if isinstance(pull.get("user"), dict) else None
    head = pull.get("head", {}).get("sha") if isinstance(pull.get("head"), dict) else None
    if not _text(head):
        raise ValueError("pull request response has no head SHA")
    checks_page = client.rest_page_result(f"commits/{head}/check-runs", jq=".check_runs")
    reviews_page = client.rest_page_result(f"pulls/{number}/reviews")
    comments_page = client.rest_page_result(f"issues/{number}/comments")
    owner, name = client.repository.split("/", 1)
    thread_pages = client.graphql_pages(
        THREAD_QUERY, {"owner": owner, "name": name, "pr": number}
    )
    threads: list[dict] = []
    cursors: set[str] = set()
    pages = thread_pages["pages"]
    for page_index, page in enumerate(pages):
        if page.get("errors"):
            raise ValueError("review-thread GraphQL response contains errors")
        try:
            connection = page["data"]["repository"]["pullRequest"]["reviewThreads"]
            nodes = connection["nodes"]
            page_info = connection["pageInfo"]
        except (KeyError, TypeError) as error:
            raise ValueError("invalid review-thread GraphQL response") from error
        if not isinstance(nodes, list) or not isinstance(page_info, dict):
            raise ValueError("invalid review-thread GraphQL page")
        has_next = page_info.get("hasNextPage")
        cursor = page_info.get("endCursor")
        if not isinstance(has_next, bool):
            raise ValueError("invalid review-thread pagination state")
        if page_index < len(pages) - 1 and (not has_next or not _text(cursor) or cursor in cursors):
            raise ValueError("inconsistent review-thread pagination")
        if page_index == len(pages) - 1 and has_next:
            raise ValueError("incomplete review-thread pagination")
        if _text(cursor):
            cursors.add(cursor)
        for node in nodes:
            comments = node.get("comments", {}).get("nodes", []) if isinstance(node, dict) else []
            last = comments[-1] if comments and isinstance(comments[-1], dict) else {}
            threads.append({
                "id": f"thread:{node.get('id')}", "url": last.get("url"),
                "body": last.get("body", ""), "resolved": node.get("isResolved"),
            })
    checks = []
    annotations: dict[str, dict] = {}
    annotation_pages = 0
    for check in checks_page["items"]:
        checks.append({
            "name": check.get("name"), "status": str(check.get("status", "")).upper(),
            "conclusion": str(check.get("conclusion", "")).upper(),
        })
        check_id = check.get("id")
        if not isinstance(check_id, int) or isinstance(check_id, bool):
            raise ValueError("invalid check-run ID")
        page = client.rest_page_result(f"check-runs/{check_id}/annotations")
        annotation_pages += page["pageCount"]
        for annotation in page["items"]:
            path = annotation.get("path")
            start = annotation.get("start_line")
            end = annotation.get("end_line")
            fingerprint = hashlib.sha256(json.dumps(
                {
                    "title": annotation.get("title"), "message": annotation.get("message"),
                    "raw_details": annotation.get("raw_details"),
                    "level": annotation.get("annotation_level"),
                }, sort_keys=True,
            ).encode("utf-8")).hexdigest()[:12]
            annotation_id = f"annotation:{path}:{start}:{end}:{fingerprint}"
            annotations[annotation_id] = {
                "id": annotation_id,
                "url": annotation.get("blob_href") or pull.get("html_url"),
                "message": annotation.get("message", ""),
                "level": annotation.get("annotation_level", ""),
            }
    reviews = [{
        "id": f"review:{item.get('id')}", "url": item.get("html_url") or pull.get("html_url"),
        "body": item.get("body", ""), "state": str(item.get("state", "")).upper(),
    } for item in reviews_page["items"]]
    conversation = [{
        "id": f"comment:{item.get('id')}", "url": item.get("html_url"), "body": item.get("body", ""),
        "author": item.get("user", {}).get("login") if isinstance(item.get("user"), dict) else None,
    } for item in comments_page["items"]]
    return {
        "repository": client.repository, "number": number, "url": pull.get("html_url"),
        "author": pull_author,
        "headOid": head, "state": str(pull.get("state", "")).upper(),
        "isDraft": pull.get("draft"), "mergeStateStatus": str(pull.get("mergeable_state", "")).upper(),
        "mergedAt": pull.get("merged_at"), "autoMergeRequest": pull.get("auto_merge"),
        "checks": checks, "threads": threads, "reviews": reviews,
        "conversationComments": conversation, "annotations": list(annotations.values()),
        "pagination": {
            "threads": {"complete": thread_pages["complete"], "pageCount": thread_pages["pageCount"]},
            "reviews": {"complete": reviews_page["complete"], "pageCount": reviews_page["pageCount"]},
            "conversationComments": {"complete": comments_page["complete"], "pageCount": comments_page["pageCount"]},
            "annotations": {"complete": True, "pageCount": max(1, annotation_pages)},
        },
    }


def audit_snapshot(
    snapshot: object,
    dispositions: object,
    *,
    expected_head: str | None = None,
    observed_at: str | None = None,
) -> dict:  # noqa: C901
    """Validate a fully paginated GitHub snapshot and classify every finding."""
    reasons: list[str] = []
    findings: list[dict] = []
    repository = snapshot.get("repository") if isinstance(snapshot, dict) else None
    number = snapshot.get("number") if isinstance(snapshot, dict) else None
    head = snapshot.get("headOid") if isinstance(snapshot, dict) else None
    envelope = {
        "schemaVersion": 1,
        "kind": "pull-request-audit",
        "repository": repository,
        "pullRequest": number,
        "headOid": head,
        "observedAt": observed_at or datetime.now(UTC).isoformat(),
    }
    if not isinstance(snapshot, dict):
        return {**envelope, "decision": "unavailable", "reasons": ["invalid snapshot"], "findings": [], "openFindingCount": 0}
    if not _text(repository) or not isinstance(number, int) or isinstance(number, bool) or number < 1 or not _text(head):
        reasons.append("invalid repository, pull request, or head identity")
    if expected_head is not None and head != expected_head:
        reasons.append(f"head changed: expected {expected_head}, observed {head}")
    pagination = snapshot.get("pagination")
    if not isinstance(pagination, dict):
        reasons.append("invalid pagination receipt")
    for surface in SURFACES:
        values = snapshot.get(surface)
        page = pagination.get(surface) if isinstance(pagination, dict) else None
        if not isinstance(values, list):
            reasons.append(f"invalid {surface} collection")
            continue
        if (
            not isinstance(page, dict)
            or page.get("complete") is not True
            or not isinstance(page.get("pageCount"), int)
            or isinstance(page.get("pageCount"), bool)
            or page["pageCount"] < 1
        ):
            reasons.append(f"pagination incomplete for {surface}")
        for item in values:
            if not isinstance(item, dict) or not _text(item.get("id")) or not _text(item.get("url")):
                reasons.append(f"invalid {surface} finding")
                continue
            if _finding(surface, item):
                findings.append({
                    "id": item["id"], "url": item["url"], "surface": surface,
                    "liveResolved": item.get("resolved") if surface == "threads" else None,
                })
    checks = snapshot.get("checks")
    if not isinstance(checks, list):
        reasons.append("invalid checks collection")
    else:
        for check in checks:
            if not isinstance(check, dict) or not _text(check.get("name")):
                reasons.append("invalid check result")
                continue
            if check.get("status") != "COMPLETED" or check.get("conclusion") not in {"SUCCESS", "NEUTRAL", "SKIPPED"}:
                reasons.append(f"check not green: {check.get('name')}")
    dispositions = dispositions if isinstance(dispositions, dict) else {}
    live_replies = {
        (item["url"], match.group(1))
        for item in snapshot.get("conversationComments", [])
        if isinstance(item, dict) and _github_url(item.get("url")) and _text(item.get("body"))
        and item.get("author") == snapshot.get("author")
        for match in [DISPOSITION_MARKER.search(item["body"])]
        if match and len(item["body"][:match.start()].strip()) >= 12
    }
    finding_ids = {finding["id"] for finding in findings}
    evidenced_reply_urls = {url for url, finding_id in live_replies if finding_id in finding_ids}
    findings = [
        finding for finding in findings
        if not (
            finding["surface"] == "conversationComments"
            and finding["url"] in evidenced_reply_urls
        )
    ]
    open_findings: list[dict] = []
    for finding in findings:
        disposition = dispositions.get(finding["id"])
        finding["disposition"] = disposition
        if not _valid_disposition(disposition, finding, live_replies):
            open_findings.append(finding)
    if open_findings:
        reasons.append(f"{len(open_findings)} feedback finding(s) remain unexamined, unanswered, or unresolved")
    unavailable = any(reason.startswith("invalid") or "pagination incomplete" in reason for reason in reasons)
    decision = "unavailable" if unavailable else ("block" if reasons else "allow")
    return {
        **envelope,
        "decision": decision,
        "reasons": reasons,
        "findings": findings,
        "openFindingCount": len(open_findings),
        "pagination": pagination,
    }
