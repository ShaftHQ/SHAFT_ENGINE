"""Canonical issue-filing contract tests (#4770)."""

import copy
import json
import subprocess  # nosec B404 - subprocess objects are test fixtures only.
import threading
import unittest
from pathlib import Path

from scripts.agents.issue_filing import (
    build_az_boards_create_argv,
    build_glab_issue_create_argv,
    confirmation_digest,
    create_issue,
    prepare_issue_plan,
    receipt_digest,
    reconcile_labels,
    transition_issue,
    validate_issue_plan,
)


ROOT = Path(__file__).resolve().parents[2]
TAXONOMY = json.loads((ROOT / ".github/issue-taxonomy.json").read_text(encoding="utf-8"))
SPEC_KIT_SECTIONS = (
    "User Scenarios & Testing",
    "Edge Cases",
    "Functional Requirements",
    "Success Criteria",
    "Assumptions",
)


def planned() -> dict:
    return {
        "title": "Enforce detailed harness plans", "template": "feature_request.md",
        "body": (
            "## 🎯 Problem Statement\nPlans miss intent.\n"
            "## 💡 Proposed Solution\nValidate evidence.\n"
            "## 🔄 Alternatives Considered\nProse only.\n"
            "## 🗂️ Area of the Framework\nOther: harness\n"
            "## 📈 Use Case & Impact\nMaintainers.\n"
            "## User Scenarios & Testing\n### User Story 1\n"
            "**Acceptance Scenarios**:\n1. **Given** a plan, **When** validated, **Then** allow.\n"
            "## Edge Cases\n- Missing taxonomy fails closed.\n"
            "## Functional Requirements\n- **FR-001**: Plans require Spec Kit sections.\n"
            "## Success Criteria\n- **SC-001**: Incomplete plans are rejected.\n"
            "## Assumptions\n- GitHub remains the live CLI."
        ),
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
        confirmation = confirmation_digest(item, TAXONOMY, "consumer/project")
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
        confirmation = confirmation_digest(normalized, TAXONOMY, "consumer/project")
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

    def test_confirmation_binds_repository_and_full_issue_content(self):
        item = planned()
        digest = confirmation_digest(item, TAXONOMY, "consumer/project")
        changed = copy.deepcopy(item); changed["body"] += "\nChanged"
        self.assertNotEqual(digest, confirmation_digest(changed, TAXONOMY, "consumer/project"))
        self.assertNotEqual(digest, confirmation_digest(item, TAXONOMY, "consumer/other"))

    def test_concurrent_same_host_creation_is_serialized_and_reused(self):
        item = planned()
        confirmation = confirmation_digest(item, TAXONOMY, "consumer/project")
        created = []
        lock = threading.Lock()
        def runner(command, **kwargs):
            if command[1:3] == ["issue", "create"]:
                with lock:
                    created.append(1)
                return subprocess.CompletedProcess(command, 0, "https://github.com/consumer/project/issues/9\n", "")
            query = command[command.index("--search") + 1]
            with lock:
                exists = bool(created)
            payload = '[{"url":"https://github.com/consumer/project/issues/9"}]' if "act-as-mohab:" in query and exists else "[]"
            return subprocess.CompletedProcess(command, 0, payload, "")
        results = []
        threads = [threading.Thread(target=lambda: results.append(create_issue(item, TAXONOMY, "consumer/project", confirmation, runner=runner, executable="gh"))) for _ in range(2)]
        for thread in threads: thread.start()
        for thread in threads: thread.join()
        self.assertEqual(1, len(created))
        self.assertEqual(2, len(results))

    def test_multiple_modules_require_cross_cutting(self):
        item = planned(); item["labels"] = ["enhancement", "module:shaft-engine", "module:shaft-mcp", "ready"]
        self.assertEqual("block", validate_issue_plan(item, TAXONOMY)["decision"])

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

    def test_transition_removes_lifecycle_alias_and_case_drift(self):
        item = planned()
        for existing in ("deferred", "BLOCKED"):
            calls = []
            def runner(command, **kwargs):
                calls.append(command)
                if command[1:3] == ["issue", "view"]:
                    return subprocess.CompletedProcess(command, 0, json.dumps({"labels": [{"name": existing}]}), "")
                return subprocess.CompletedProcess(command, 0, "", "")
            transition_issue("consumer/project", 7, item, TAXONOMY, runner=runner, executable="gh")
            self.assertIn(existing, calls[-1])

    def test_reconciliation_migrates_alias_when_canonical_label_already_exists(self):
        calls = []
        def runner(command, **kwargs):
            calls.append(command)
            if command[1:3] == ["label", "list"]:
                labels = [{"name": name} for name in [*TAXONOMY["primaryTypes"], *TAXONOMY["supplemental"], *TAXONOMY["lifecycle"], *TAXONOMY["subsystems"], "deferred"]]
                return subprocess.CompletedProcess(command, 0, json.dumps(labels), "")
            if command[1:3] == ["issue", "list"]:
                return subprocess.CompletedProcess(command, 0, '[{"number":7,"url":"https://github.com/x/y/issues/7","labels":[{"name":"deferred"}]}]', "")
            return subprocess.CompletedProcess(command, 0, "", "")
        receipt = reconcile_labels("consumer/project", TAXONOMY, apply=True, runner=runner, executable="gh")
        self.assertEqual("deferred", receipt["appliedMigrations"][0]["existing"])
        edit = next(command for command in calls if command[1:3] == ["issue", "edit"])
        self.assertIn("blocked", edit)
        self.assertIn("deferred", edit)

    def test_missing_success_criteria_blocks(self):
        item = planned()
        item["body"] = item["body"].replace("## Success Criteria\n- **SC-001**: Incomplete plans are rejected.\n", "")
        receipt = validate_issue_plan(item, TAXONOMY)
        self.assertEqual("block", receipt["decision"])
        self.assertTrue(any("Success Criteria" in reason for reason in receipt["reasons"]))

    def test_gitlab_dependency_url_allowed_for_blocked(self):
        item = planned()
        item["labels"][-1] = "blocked"
        item["dependencies"] = ["https://gitlab.com/group/proj/-/issues/12"]
        self.assertEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])

    def test_garbage_dependency_url_rejected(self):
        item = planned()
        item["labels"][-1] = "blocked"
        for bad in ("javascript:alert(1)", "http://github.com/x/y/issues/1", "not-a-url", "ftp://example.com/x"):
            item["dependencies"] = [bad]
            receipt = validate_issue_plan(item, TAXONOMY)
            self.assertEqual("block", receipt["decision"], bad)

    def test_azure_boards_dependency_url_allowed_for_blocked(self):
        item = planned()
        item["labels"][-1] = "blocked"
        item["dependencies"] = ["https://dev.azure.com/org/project/_workitems/edit/42"]
        self.assertEqual("allow", validate_issue_plan(item, TAXONOMY)["decision"])

    def test_fake_glab_and_az_boards_argv(self):
        item = planned()
        glab = build_glab_issue_create_argv(item, "group/proj", executable="glab")
        self.assertEqual(
            ["glab", "issue", "create", "--repo", "group/proj", "--title", item["title"],
             "--description", item["body"], "--label", ",".join(item["labels"])],
            glab,
        )
        az = build_az_boards_create_argv(item, organization="https://dev.azure.com/org", project="project", executable="az")
        self.assertEqual(
            ["az", "boards", "work-item", "create", "--organization", "https://dev.azure.com/org",
             "--project", "project", "--title", item["title"], "--type", "Issue",
             "--description", item["body"]],
            az,
        )

    def test_ready_plan_still_allows_with_spec_kit_sections(self):
        receipt = validate_issue_plan(planned(), TAXONOMY)
        self.assertEqual("allow", receipt["decision"])

    def test_work_item_contract_names_spec_kit_sections(self):
        skill = (ROOT / "chaos-engine/skills/work-item/SKILL.md").read_text(encoding="utf-8")
        contract = (ROOT / "chaos-engine/references/work-item.md").read_text(encoding="utf-8")
        combined = skill + "\n" + contract
        for section in SPEC_KIT_SECTIONS:
            self.assertIn(section, combined)


if __name__ == "__main__":
    unittest.main()
