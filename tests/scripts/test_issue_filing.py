"""Canonical issue-filing contract tests (#4770)."""

import copy
import json
import subprocess
import unittest
from pathlib import Path

from scripts.agents.issue_filing import create_issue, prepare_issue_plan, receipt_digest, transition_issue, validate_issue_plan


ROOT = Path(__file__).resolve().parents[2]
TAXONOMY = json.loads((ROOT / ".github/issue-taxonomy.json").read_text(encoding="utf-8"))


def planned() -> dict:
    return {
        "title": "Enforce detailed harness plans", "template": "feature_request.md",
        "body": "## 🎯 Problem Statement\nPlans miss intent.\n## 💡 Proposed Solution\nValidate evidence.\n## 🔄 Alternatives Considered\nProse only.\n## 🗂️ Area of the Framework\nOther: harness\n## 📈 Use Case & Impact\nMaintainers.",
        "labels": ["enhancement", "subsystem:agent-harness", "ready"],
        "acceptanceCriteria": ["Validator rejects incomplete plans."],
        "proofPlan": ["Run focused tests."], "dependencies": [], "related": ["#4774"],
        "duplicateSearch": {"query": "detailed harness plans", "open": [], "closed": []},
    }


class IssueFilingTest(unittest.TestCase):
    def test_planned_harness_improvement_is_ready(self):
        receipt = validate_issue_plan(planned(), TAXONOMY)
        self.assertEqual("allow", receipt["decision"])
        self.assertEqual("enhancement", receipt["type"])
        self.assertEqual("ready", receipt["lifecycle"])

    def test_missing_template_type_subsystem_or_lifecycle_fails(self):
        mutations = (
            lambda item: item.update(template="bug_report.md"),
            lambda item: item.update(labels=[label for label in item["labels"] if label != "enhancement"]),
            lambda item: item.update(labels=[label for label in item["labels"] if not label.startswith("subsystem:")]),
            lambda item: item["labels"].append("triage"),
            lambda item: item.update(body="missing required sections"),
        )
        for mutate in mutations:
            item = planned(); mutate(item)
            self.assertNotEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])

    def test_ready_needs_proof_blocked_needs_dependency_and_ambiguous_is_triage(self):
        item = planned(); item["proofPlan"] = []
        self.assertEqual("block", validate_issue_plan(item, TAXONOMY)["decision"])
        item = planned(); item["labels"][-1] = "blocked"; item["dependencies"] = []
        self.assertEqual("block", validate_issue_plan(item, TAXONOMY)["decision"])
        item = planned(); item["labels"] = ["enhancement", "subsystem:unclassified", "triage"]
        self.assertEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])

    def test_duplicate_is_linked_under_create_linked_policy(self):
        item = planned(); item["duplicateSearch"]["closed"] = ["https://github.com/x/y/issues/1"]
        receipt = validate_issue_plan(item, TAXONOMY)
        self.assertEqual("allow", receipt["decision"])
        self.assertEqual("create-linked", receipt["duplicatePolicy"])
        self.assertEqual(item["duplicateSearch"]["closed"], receipt["duplicateMatches"])
        later = dict(receipt); later["observedAt"] = "later"
        self.assertEqual(receipt_digest(receipt), receipt_digest(later))

    def test_documentation_does_not_require_an_engine_module_and_cross_cutting_is_explicit(self):
        item = planned(); item["labels"] = ["enhancement", "documentation", "subsystem:documentation", "ready"]
        self.assertEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])

    def test_confirmed_creation_rechecks_duplicates_and_marker(self):
        item = planned()
        receipt = validate_issue_plan(item, TAXONOMY)
        confirmation = receipt_digest(receipt)
        calls = []
        def runner(command, **kwargs):
            calls.append(command)
            if command[1:3] == ["issue", "list"]:
                return subprocess.CompletedProcess(command, 0, "[]", "")
            return subprocess.CompletedProcess(
                command, 0, "https://github.com/consumer/project/issues/9\n", ""
            )
        created = create_issue(
            item, TAXONOMY, "consumer/project", confirmation, runner=runner, executable="gh"
        )
        self.assertEqual("https://github.com/consumer/project/issues/9", created["issueUrl"])
        self.assertTrue(any("in:body" in token for command in calls for token in command))
        create_command = next(command for command in calls if command[1:3] == ["issue", "create"])
        self.assertIn("act-as-mohab:", create_command[create_command.index("--body") + 1])

    def test_dry_run_digest_creates_then_reuses_marker_before_changed_duplicates(self):
        def empty_search(command, **kwargs):
            return subprocess.CompletedProcess(command, 0, "[]", "")
        prepared = prepare_issue_plan(
            planned(), TAXONOMY, "consumer/project", runner=empty_search, executable="gh"
        )
        normalized = prepared["normalizedPlan"]
        confirmation = receipt_digest(validate_issue_plan(normalized, TAXONOMY))
        def existing_marker(command, **kwargs):
            query = command[command.index("--search") + 1]
            if "act-as-mohab:" in query:
                return subprocess.CompletedProcess(
                    command, 0, '[{"url":"https://github.com/consumer/project/issues/9"}]', ""
                )
            raise AssertionError("ordinary duplicate search must not run after marker match")
        reused = create_issue(
            normalized, TAXONOMY, "consumer/project", confirmation,
            runner=existing_marker, executable="gh",
        )
        self.assertTrue(reused["reused"])

    def test_blocked_to_ready_transition_removes_old_lifecycle(self):
        item = planned()
        calls = []
        def runner(command, **kwargs):
            calls.append(command)
            if command[1:3] == ["issue", "view"]:
                return subprocess.CompletedProcess(command, 0, '{"labels":[{"name":"blocked"},{"name":"enhancement"}]}', "")
            return subprocess.CompletedProcess(command, 0, "https://example", "")
        receipt = transition_issue(
            "consumer/project", 7, item, TAXONOMY, runner=runner, executable="gh"
        )
        self.assertEqual("ready", receipt["lifecycle"])
        edit = calls[-1]
        self.assertIn("blocked", edit)
        self.assertIn("ready", edit)
        item = planned(); item["labels"] = ["enhancement", "cross-cutting", "module:shaft-engine", "module:shaft-mcp", "ready"]
        self.assertEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])


if __name__ == "__main__":
    unittest.main()
