"""Unit tests for CE program epic auto-close (#5585)."""

from __future__ import annotations

import json
import os
import subprocess  # nosec B404 - fixed list-args CLI invocations in tests only
import unittest
from unittest import mock

from scripts.ci.epic_autoclose import (
    EPIC_LABEL,
    KNOWN_EPIC_NUMBERS,
    Decision,
    IssueRef,
    close_comment,
    decide,
    evaluate_closed_issue,
    is_eligible_epic,
    issue_from_graphql_node,
    main,
    parse_sub_issues,
    split_repository,
)

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CLI = os.path.join(REPO_ROOT, "scripts", "ci", "epic_autoclose.py")


def _issue(number: int, title: str, state: str, labels: tuple[str, ...] = ()) -> IssueRef:
    return IssueRef(number=number, title=title, state=state, labels=labels)


class EligibilityTests(unittest.TestCase):
    def test_known_epic_5569_is_eligible_without_label(self):
        self.assertIn(5569, KNOWN_EPIC_NUMBERS)
        self.assertTrue(is_eligible_epic(_issue(5569, "Epic: CE", "OPEN")))

    def test_label_makes_other_issue_eligible(self):
        self.assertTrue(
            is_eligible_epic(_issue(9999, "Epic: other", "OPEN", (EPIC_LABEL,)))
        )

    def test_unrelated_issue_is_not_eligible(self):
        self.assertFalse(is_eligible_epic(_issue(42, "random", "OPEN", ("enhancement",))))


class DecideTests(unittest.TestCase):
    def test_noop_when_no_parent(self):
        decision = decide(parent=None, children=())
        self.assertEqual("noop_no_parent", decision.action)
        self.assertFalse(decision.should_close)

    def test_noop_when_parent_ineligible(self):
        parent = _issue(100, "not an epic", "OPEN")
        decision = decide(parent=parent, children=(_issue(101, "child", "CLOSED"),))
        self.assertEqual("noop_ineligible", decision.action)
        self.assertFalse(decision.should_close)

    def test_noop_when_epic_already_closed(self):
        parent = _issue(5569, "Epic: CE", "CLOSED")
        decision = decide(parent=parent, children=(_issue(5585, "autoclose", "CLOSED"),))
        self.assertEqual("noop_already_closed", decision.action)

    def test_noop_when_epic_has_no_children(self):
        parent = _issue(5569, "Epic: CE", "OPEN")
        decision = decide(parent=parent, children=())
        self.assertEqual("noop_no_children", decision.action)

    def test_noop_while_any_child_open_including_follow_ons(self):
        parent = _issue(5569, "Epic: CE", "OPEN", (EPIC_LABEL,))
        children = (
            _issue(5584, "eval", "CLOSED"),
            _issue(5585, "autoclose", "CLOSED"),
            _issue(5613, "headroom", "OPEN"),  # must block autoclose if tracked
        )
        decision = decide(parent=parent, children=children)
        self.assertEqual("noop_open_children", decision.action)
        self.assertEqual((5613,), tuple(c.number for c in decision.open_children))
        self.assertFalse(decision.should_close)

    def test_close_when_all_tracked_children_closed(self):
        parent = _issue(5569, "Epic: CE", "OPEN")
        children = (
            _issue(5584, "eval", "CLOSED"),
            _issue(5585, "autoclose", "CLOSED"),
        )
        decision = decide(parent=parent, children=children)
        self.assertEqual("close_epic", decision.action)
        self.assertTrue(decision.should_close)
        self.assertIn("2 sub-issues", decision.reason)

    def test_labeled_epic_closes_same_as_known_number(self):
        parent = _issue(7000, "Epic: next wave", "OPEN", (EPIC_LABEL,))
        children = (_issue(7001, "child", "CLOSED"),)
        decision = decide(parent=parent, children=children)
        self.assertEqual("close_epic", decision.action)


class ParseTests(unittest.TestCase):
    def test_issue_from_graphql_node_reads_labels(self):
        node = {
            "number": 5569,
            "title": "Epic: CE",
            "state": "OPEN",
            "labels": {"nodes": [{"name": EPIC_LABEL}, {"name": "enhancement"}]},
        }
        parsed = issue_from_graphql_node(node)
        assert parsed is not None
        self.assertEqual((EPIC_LABEL, "enhancement"), parsed.labels)

    def test_parse_sub_issues_skips_malformed(self):
        nodes = [
            {"number": 1, "title": "ok", "state": "CLOSED"},
            {"number": "bad", "title": "x", "state": "OPEN"},
            None,
        ]
        parsed = parse_sub_issues(nodes)
        self.assertEqual((1,), tuple(item.number for item in parsed))

    def test_split_repository(self):
        self.assertEqual(("ShaftHQ", "SHAFT_ENGINE"), split_repository("ShaftHQ/SHAFT_ENGINE"))
        with self.assertRaises(ValueError):
            split_repository("not-a-slug")


class CommentTests(unittest.TestCase):
    def test_close_comment_lists_children_and_trigger(self):
        epic = _issue(5569, "Epic: CE", "OPEN")
        children = (_issue(5584, "a", "CLOSED"), _issue(5585, "b", "CLOSED"))
        body = close_comment(epic=epic, children=children, trigger=5585)
        self.assertIn("#5585", body)
        self.assertIn("#5584", body)
        self.assertIn("GitHub sub-issues", body)
        self.assertIn("work-github-planning.md", body)


class EvaluateIntegrationTests(unittest.TestCase):
    def test_evaluate_closed_issue_closes_only_when_children_closed(self):
        parent_payload = {
            "data": {
                "repository": {
                    "issue": {
                        "number": 5585,
                        "title": "autoclose",
                        "state": "CLOSED",
                        "labels": {"nodes": []},
                        "parent": {
                            "number": 5569,
                            "title": "Epic: CE",
                            "state": "OPEN",
                            "labels": {"nodes": [{"name": "enhancement"}]},
                        },
                    }
                }
            }
        }
        children_payload = {
            "data": {
                "repository": {
                    "issue": {
                        "number": 5569,
                        "title": "Epic: CE",
                        "state": "OPEN",
                        "labels": {"nodes": []},
                        "subIssues": {
                            "nodes": [
                                {"number": 5584, "title": "eval", "state": "CLOSED"},
                                {"number": 5585, "title": "autoclose", "state": "CLOSED"},
                            ]
                        },
                    }
                }
            }
        }
        open_child_payload = {
            "data": {
                "repository": {
                    "issue": {
                        "number": 5569,
                        "title": "Epic: CE",
                        "state": "OPEN",
                        "labels": {"nodes": []},
                        "subIssues": {
                            "nodes": [
                                {"number": 5585, "title": "autoclose", "state": "CLOSED"},
                                {"number": 5613, "title": "headroom", "state": "OPEN"},
                            ]
                        },
                    }
                }
            }
        }

        def runner_factory(second_payload):
            calls = {"n": 0}

            def runner(args, **_kwargs):
                calls["n"] += 1
                if calls["n"] == 1:
                    return subprocess.CompletedProcess(args, 0, json.dumps(parent_payload), "")
                return subprocess.CompletedProcess(args, 0, json.dumps(second_payload), "")

            return runner

        close_decision = evaluate_closed_issue(
            "ShaftHQ", "SHAFT_ENGINE", 5585, runner=runner_factory(children_payload)
        )
        self.assertEqual("close_epic", close_decision.action)

        blocked = evaluate_closed_issue(
            "ShaftHQ", "SHAFT_ENGINE", 5585, runner=runner_factory(open_child_payload)
        )
        self.assertEqual("noop_open_children", blocked.action)
        self.assertEqual((5613,), tuple(c.number for c in blocked.open_children))

    def test_main_dry_run_does_not_close(self):
        decision_payload = Decision(
            action="close_epic",
            epic=_issue(5569, "Epic: CE", "OPEN"),
            open_children=(),
            closed_children=(_issue(5585, "autoclose", "CLOSED"),),
            reason="all closed",
        )
        with mock.patch(
            "scripts.ci.epic_autoclose.evaluate_closed_issue", return_value=decision_payload
        ), mock.patch("scripts.ci.epic_autoclose.close_issue") as close_mock:
            code = main(
                ["--repository", "ShaftHQ/SHAFT_ENGINE", "--issue", "5585", "--dry-run"]
            )
        self.assertEqual(0, code)
        close_mock.assert_not_called()


class CliSmokeTests(unittest.TestCase):
    def test_cli_help(self):
        completed = subprocess.run(  # nosec B603
            ["python3", CLI, "--help"],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        self.assertEqual(0, completed.returncode)
        self.assertIn("--dry-run", completed.stdout)


if __name__ == "__main__":
    unittest.main()
