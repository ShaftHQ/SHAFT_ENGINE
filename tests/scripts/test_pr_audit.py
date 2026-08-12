"""Pull-request audit receipt contract tests (#4769)."""

import unittest

from scripts.agents.pr_audit import audit_snapshot, collect_pr_snapshot


def clean_snapshot() -> dict:
    return {
        "repository": "consumer/project",
        "number": 17,
        "url": "https://github.com/consumer/project/pull/17",
        "author": "owner",
        "headOid": "abc123",
        "state": "OPEN",
        "isDraft": False,
        "mergeStateStatus": "CLEAN",
        "mergedAt": None,
        "autoMergeRequest": None,
        "checks": [{"name": "unit", "status": "COMPLETED", "conclusion": "SUCCESS"}],
        "threads": [], "reviews": [], "conversationComments": [], "annotations": [],
        "pagination": {
            name: {"complete": True, "pageCount": 1}
            for name in ("threads", "reviews", "conversationComments", "annotations")
        },
    }


def add_live_reply(snapshot: dict, finding_id: str, suffix: str = "1") -> str:
    url = f"https://github.com/consumer/project/pull/17#issuecomment-{suffix}"
    snapshot["conversationComments"].append({
        "id": f"comment:reply-{suffix}", "url": url, "author": "owner",
        "body": f"Addressed with verified evidence. <!-- act-as-mohab-disposition:{finding_id} -->",
    })
    return url


class PullRequestAuditTest(unittest.TestCase):
    def test_collector_queries_every_paginated_surface_and_annotations(self):
        class Client:
            repository = "consumer/project"

            def __init__(self):
                self.endpoints = []

            def get(self, endpoint):
                self.endpoints.append(endpoint)
                return {
                    "number": 17, "html_url": "https://example/pr/17", "state": "open",
                    "draft": False, "mergeable_state": "clean", "merged_at": None,
                    "auto_merge": None, "head": {"sha": "abc123"},
                }

            def rest_page_result(self, endpoint, *, jq=None):
                self.endpoints.append((endpoint, jq))
                if endpoint.endswith("check-runs"):
                    return {"items": [
                        {"id": 9, "name": "unit", "status": "completed", "conclusion": "success"},
                        {"id": 10, "name": "integration", "status": "completed", "conclusion": "success"},
                        {"id": 11, "name": "lint", "status": "completed", "conclusion": "success"},
                    ], "pageCount": 2, "complete": True}
                if endpoint.endswith("annotations"):
                    level = "notice" if endpoint == "check-runs/11/annotations" else "warning"
                    return {"items": [{"path": "a.py", "start_line": 2, "end_line": 2, "annotation_level": level, "message": "warn", "blob_href": "https://example/a"}], "pageCount": 3, "complete": True}
                return {"items": [], "pageCount": 1, "complete": True}

            def graphql_pages(self, query, variables):
                self.endpoints.append("graphql")
                return {"pages": [{"data": {"repository": {"pullRequest": {"reviewThreads": {"nodes": [], "pageInfo": {"hasNextPage": False, "endCursor": None}}}}}}], "pageCount": 1, "complete": True}

        client = Client()
        snapshot = collect_pr_snapshot(client, 17)
        self.assertEqual("abc123", snapshot["headOid"])
        self.assertEqual(9, snapshot["pagination"]["annotations"]["pageCount"])
        self.assertEqual(2, len(snapshot["annotations"]), "only identical matrix findings deduplicate")
        self.assertTrue(snapshot["annotations"][0]["id"].startswith("annotation:a.py:2:2:"))
        self.assertIn(("check-runs/9/annotations", None), client.endpoints)
        self.assertIn(("check-runs/10/annotations", None), client.endpoints)
        self.assertIn(("check-runs/11/annotations", None), client.endpoints)
        self.assertIn("graphql", client.endpoints)

    def test_collector_rejects_graphql_errors_and_unfinished_last_page(self):
        class Client:
            repository = "consumer/project"
            def get(self, _endpoint):
                return {"number": 17, "head": {"sha": "abc"}}
            def rest_page_result(self, endpoint, *, jq=None):
                return {"items": [], "pageCount": 1, "complete": True}
            def graphql_pages(self, query, variables):
                return {"pages": self.pages, "pageCount": 1, "complete": True}

        client = Client()
        client.pages = [{"errors": [{"message": "denied"}]}]
        with self.assertRaises(ValueError):
            collect_pr_snapshot(client, 17)
        client.pages = [{"data": {"repository": {"pullRequest": {"reviewThreads": {
            "nodes": [], "pageInfo": {"hasNextPage": True, "endCursor": "cursor"}
        }}}}}]
        with self.assertRaises(ValueError):
            collect_pr_snapshot(client, 17)

    def test_clean_green_head_produces_zero_open_receipt(self):
        receipt = audit_snapshot(clean_snapshot(), {}, expected_head="abc123")
        self.assertEqual("allow", receipt["decision"])
        self.assertEqual(0, receipt["openFindingCount"])
        self.assertEqual("abc123", receipt["headOid"])
        self.assertEqual(1, receipt["schemaVersion"])

    def test_each_review_surface_blocks_until_disposed_with_evidence(self):
        surfaces = {
            "threads": {"id": "thread:T1", "url": "https://example/t1", "body": "fix race", "resolved": False},
            "reviews": {"id": "review:R1", "url": "https://example/r1", "body": "needs work", "state": "CHANGES_REQUESTED"},
            "conversationComments": {"id": "comment:C1", "url": "https://example/c1", "body": "please prove this"},
            "annotations": {"id": "annotation:A1", "url": "https://example/a1", "message": "unsafe", "level": "failure"},
        }
        for surface, finding in surfaces.items():
            with self.subTest(surface=surface):
                snapshot = clean_snapshot()
                snapshot[surface] = [finding]
                blocked = audit_snapshot(snapshot, {}, expected_head="abc123")
                self.assertEqual("block", blocked["decision"])
                self.assertEqual(1, blocked["openFindingCount"])
                disposition = {
                    finding["id"]: {
                        "disposition": "valid",
                        "replyUrl": add_live_reply(snapshot, finding["id"]),
                        "resolved": True,
                    },
                }
                if surface == "threads":
                    snapshot[surface][0]["resolved"] = True
                self.assertEqual(
                    "allow", audit_snapshot(snapshot, disposition, expected_head="abc123")["decision"]
                )

    def test_approved_review_body_is_still_feedback(self):
        snapshot = clean_snapshot()
        snapshot["reviews"] = [{
            "id": "review:R2", "url": "https://example/r2",
            "body": "Please rename this later.", "state": "APPROVED",
        }]
        self.assertEqual(1, audit_snapshot(snapshot, {})["openFindingCount"])

    def test_false_positive_and_follow_up_require_specific_evidence(self):
        snapshot = clean_snapshot()
        snapshot["annotations"] = [{
            "id": "annotation:A1", "url": "https://example/a1", "message": "warning", "level": "warning"
        }]
        false_positive = {"annotation:A1": {"disposition": "false-positive", "resolved": True}}
        self.assertEqual("block", audit_snapshot(snapshot, false_positive)["decision"])
        false_positive["annotation:A1"].update(replyUrl=add_live_reply(snapshot, "annotation:A1"), justification="Tool analyzed generated fixture, not runtime code.")
        self.assertEqual("allow", audit_snapshot(snapshot, false_positive)["decision"])

        follow_up = {"annotation:A1": {
            "disposition": "approved-follow-up", "resolved": True,
            "replyUrl": add_live_reply(snapshot, "annotation:A1", "2"), "issueUrl": "https://github.com/consumer/project/issues/22",
        }}
        self.assertEqual("block", audit_snapshot(snapshot, follow_up)["decision"])
        follow_up["annotation:A1"]["approvalEvidence"] = "user instruction 2026-08-12"
        self.assertEqual("allow", audit_snapshot(snapshot, follow_up)["decision"])
        follow_up["annotation:A1"]["issueUrl"] = "not-an-issue"
        self.assertEqual("block", audit_snapshot(snapshot, follow_up)["decision"])

    def test_stale_head_incomplete_pagination_red_checks_and_malformed_data_fail_closed(self):
        for mutate, expected in (
            (lambda item: item.update(headOid="new"), "head"),
            (lambda item: item["pagination"]["threads"].update(complete=False), "pagination"),
            (lambda item: item["checks"][0].update(conclusion="FAILURE"), "check"),
            (lambda item: item.update(threads="not-a-list"), "invalid"),
        ):
            snapshot = clean_snapshot()
            mutate(snapshot)
            receipt = audit_snapshot(snapshot, {}, expected_head="abc123")
            self.assertNotEqual("allow", receipt["decision"])
            self.assertTrue(any(expected in reason.lower() for reason in receipt["reasons"]), receipt)

    def test_disposition_for_old_finding_does_not_hide_a_new_finding(self):
        snapshot = clean_snapshot()
        snapshot["threads"] = [
            {"id": "thread:new", "url": "https://example/new", "body": "new", "resolved": False}
        ]
        old = {"thread:old": {"disposition": "valid", "replyUrl": "https://example/reply", "resolved": True}}
        self.assertEqual(1, audit_snapshot(snapshot, old)["openFindingCount"])

    def test_local_disposition_cannot_resolve_a_live_unresolved_thread(self):
        snapshot = clean_snapshot()
        snapshot["threads"] = [{"id": "thread:T", "url": "https://github.com/x/y/pull/1#discussion_r1", "body": "fix", "resolved": False}]
        disposition = {"thread:T": {"disposition": "valid", "replyUrl": "https://github.com/x/y/pull/1#discussion_r2", "resolved": True}}
        self.assertEqual("block", audit_snapshot(snapshot, disposition)["decision"])

    def test_resolved_thread_still_requires_a_live_evidenced_disposition(self):
        snapshot = clean_snapshot()
        snapshot["threads"] = [{"id": "thread:T", "url": "https://github.com/x/y/pull/1#discussion_r1", "body": "fix this race", "resolved": True}]
        self.assertEqual("block", audit_snapshot(snapshot, {})["decision"])
        reply = add_live_reply(snapshot, "thread:T")
        disposition = {"thread:T": {"disposition": "valid", "replyUrl": reply, "resolved": True}}
        self.assertEqual("allow", audit_snapshot(snapshot, disposition)["decision"])

    def test_marker_text_does_not_hide_a_substantive_comment_or_empty_reply(self):
        snapshot = clean_snapshot()
        snapshot["conversationComments"] = [{"id": "comment:C", "url": "https://github.com/x/y/pull/1#issuecomment-1", "body": "Unfixed security bug <!-- act-as-mohab-disposition:anything -->"}]
        self.assertEqual("block", audit_snapshot(snapshot, {})["decision"])
        snapshot = clean_snapshot()
        snapshot["reviews"] = [{"id": "review:R", "url": "https://github.com/x/y/pull/1#review-1", "body": "fix", "state": "CHANGES_REQUESTED"}]
        snapshot["conversationComments"] = [{"id": "comment:C", "url": "https://github.com/x/y/pull/1#issuecomment-2", "body": "<!-- act-as-mohab-disposition:review:R -->"}]
        disposition = {"review:R": {"disposition": "valid", "replyUrl": snapshot["conversationComments"][0]["url"], "resolved": True}}
        self.assertEqual("block", audit_snapshot(snapshot, disposition)["decision"])

    def test_reply_marker_from_non_author_cannot_clear_finding(self):
        snapshot = clean_snapshot()
        snapshot["reviews"] = [{"id": "review:R", "url": "https://github.com/x/y/pull/1#review-1", "body": "fix", "state": "CHANGES_REQUESTED"}]
        reply = add_live_reply(snapshot, "review:R")
        snapshot["conversationComments"][-1]["author"] = "attacker"
        disposition = {"review:R": {"disposition": "valid", "replyUrl": reply, "resolved": True}}
        self.assertEqual("block", audit_snapshot(snapshot, disposition)["decision"])

    def test_disposition_reply_must_exist_live_and_name_the_exact_finding(self):
        snapshot = clean_snapshot()
        snapshot["reviews"] = [{"id": "review:R", "url": "https://github.com/x/y/pull/1#pullrequestreview-1", "body": "fix", "state": "CHANGES_REQUESTED"}]
        disposition = {"review:R": {"disposition": "valid", "replyUrl": "https://github.com/x/y/pull/1#issuecomment-99", "resolved": True}}
        self.assertEqual("block", audit_snapshot(snapshot, disposition)["decision"])
        disposition["review:R"]["replyUrl"] = add_live_reply(snapshot, "review:other")
        self.assertEqual("block", audit_snapshot(snapshot, disposition)["decision"])
        snapshot["conversationComments"] = []
        disposition["review:R"]["replyUrl"] = add_live_reply(snapshot, "review:R", "2")
        self.assertEqual("allow", audit_snapshot(snapshot, disposition)["decision"])


if __name__ == "__main__":
    unittest.main()
