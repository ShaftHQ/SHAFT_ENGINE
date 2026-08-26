from __future__ import annotations

import json
import re
import shutil
import subprocess  # nosec B404 - fixed test fixture commands only.
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from typing import cast
from unittest.mock import patch

import yaml

from scripts.ci.harness_pr_gate import (
    GateError,
    GatePlan,
    WaiverReceipt,
    changed_paths,
    classify_paths,
    event_waiver,
    parse_waiver,
    render_json,
    render_text,
    run_plan,
)
from scripts.ci.validate_agent_setup import validate_host_parity


ROOT = Path(__file__).resolve().parents[2]
HEAD = "a" * 40
NOW = datetime(2026, 8, 22, 12, 0, tzinfo=timezone.utc)


def waiver_body(**overrides: object) -> str:
    payload: dict[str, object] = {
        "schema": 1,
        "allowed_check_ids": ["guidance-contract"],
        "expires_at": "2026-08-29T12:00:00Z",
        "rationale": "Legacy router shape is replaced by the lifecycle kernel contract.",
        "replacement_proof": "py -3 -m unittest tests.scripts.test_chaos_engine_kernel -v",
    }
    payload.update(overrides)
    return f"PR prose\n\n```chaos-engine-waiver\n{json.dumps(payload)}\n```\n"


def review_record(**overrides: object) -> dict[str, object]:
    review: dict[str, object] = {
        "id": 42,
        "user": {"login": "MohabMohie"},
        "body": waiver_body(),
        "state": "COMMENTED",
        "submitted_at": "2026-08-22T11:00:00Z",
        "last_edited_at": None,
        "commit_id": HEAD,
    }
    review.update(overrides)
    return review


def write_reviews(directory: str, reviews: list[dict[str, object]]) -> Path:
    path = Path(directory) / "reviews.json"
    path.write_text(json.dumps(reviews), encoding="utf-8")
    return path


class ClassifierTest(unittest.TestCase):
    def test_readme_and_promotion_changes_select_only_focused_contracts(self) -> None:
        documentation = classify_paths(["chaos-engine/README.md"])
        promotion = classify_paths(["scripts/ci/chaos_engine_promotion.py"])
        promotion_runner = classify_paths(
            ["scripts/ci/chaos_engine_promotion_trials.py"]
        )

        self.assertEqual(("documentation",), documentation.surfaces)
        self.assertEqual(
            {
                "documentation-inventory-contract",
                "protected-ownership",
                "protected-secret-safety",
            },
            {check.id for check in documentation.checks},
        )
        self.assertEqual(("promotion",), promotion.surfaces)
        self.assertIn("promotion-contract", {check.id for check in promotion.checks})
        self.assertEqual(("promotion",), promotion_runner.surfaces)

    def test_kernel_change_selects_only_focused_and_protected_checks(self) -> None:
        plan = classify_paths(["chaos-engine/hooks/kernel.py"])

        self.assertEqual(("kernel",), plan.surfaces)
        self.assertEqual(
            ("kernel-contract", "protected-ownership", "protected-secret-safety"),
            tuple(check.id for check in plan.checks),
        )
        self.assertEqual((), plan.unknown_paths)
        self.assertNotIn(
            "tests.scripts.test_agent_plugin_client_smoke",
            plan.test_modules,
        )

    def test_copilot_repository_hook_selects_host_contract(self) -> None:
        plan = classify_paths([".github/hooks/chaos-engine.json"])

        self.assertEqual(("hosts",), plan.surfaces)
        self.assertEqual(
            ("host-contract", "protected-ownership", "protected-secret-safety"),
            tuple(check.id for check in plan.checks),
        )

    def test_installer_change_keeps_5299_acceptance_and_rollback_protected(self) -> None:
        plan = classify_paths(["chaos-engine/dependencies.py"])

        self.assertEqual(("installer",), plan.surfaces)
        protected = {check.id for check in plan.checks if check.protected}
        self.assertIn("protected-installer-acceptance", protected)
        self.assertIn("protected-rollback", protected)
        self.assertIn("tests.scripts.test_chaos_engine_bootstrap", plan.test_modules)
        self.assertIn("tests.scripts.test_chaos_engine_dependencies", plan.test_modules)

    def test_every_installer_manifest_uses_protected_installer_checks(self) -> None:
        manifests = (
            "chaos-engine/dependencies.json",
            "chaos-engine/distributions.json",
            "chaos-engine/profiles/portable/profile.json",
            "chaos-engine/profiles/shaft/profile.json",
            "chaos-engine/vendor/caveman/PIN.json",
            "chaos-engine/vendor/ponytail/PIN.json",
            "chaos-engine/vendor/caveman/src/hooks/package.json",
            "chaos-engine/vendor/ponytail/hooks/claude-codex-hooks.json",
        )
        for path in manifests:
            with self.subTest(path=path):
                plan = classify_paths([path])
                self.assertIn("installer", plan.surfaces)
                protected = {check.id for check in plan.checks if check.protected}
                self.assertIn("protected-installer-acceptance", protected)
                self.assertIn("protected-rollback", protected)

    def test_guard_owner_change_runs_non_waivable_security_check(self) -> None:
        plan = classify_paths(["scripts/agents/guard.py"])

        checks = {check.id: check for check in plan.checks}
        self.assertIn("protected-security", checks)
        security = checks["protected-security"]
        self.assertTrue(security.protected)
        self.assertTrue(
            all(module.startswith("tests.scripts.test_guard_memory_worktree.") for module in security.modules)
        )
        self.assertTrue(
            any("without_a_target_is_denied" in module for module in security.modules)
        )
        self.assertTrue(
            any(
                "test_failure_classifications_never_persist_secret_or_user_path" in module
                for module in checks["protected-secret-safety"].modules
            )
        )

    def test_unknown_harness_path_falls_back_instead_of_skipping(self) -> None:
        plan = classify_paths(["scripts/agents/new_runtime_surface.py"])

        self.assertEqual(("fallback",), plan.surfaces)
        self.assertEqual(("scripts/agents/new_runtime_surface.py",), plan.unknown_paths)
        self.assertIn("fallback-contract", {check.id for check in plan.checks})
        fallback = next(check for check in plan.checks if check.id == "fallback-contract")
        self.assertEqual(("tests.scripts.test_validate_agent_setup",), fallback.modules)

    def test_every_registered_but_unmapped_harness_path_uses_fallback(self) -> None:
        for path in (
            ".github/copilot-instructions.md",
            "scripts/ci/build_retry.sh",
            "scripts/ci/extract_allure_failures.py",
            "tests/scripts/test_repository_context.py",
            "tests/scripts/test_watch_pr_checks.py",
            "tests/scripts/test_worktree_hygiene.py",
            "tests/scripts/test_sync_user_harness.py",
            "tests/scripts/test_graphify_maintenance.py",
            "tests/scripts/test_shaft_knowledge_refresh.py",
            "tools/intellij-plugin-recording/install.ps1",
        ):
            with self.subTest(path=path):
                self.assertIn("fallback", classify_paths([path]).surfaces)

    def test_edited_harness_test_uses_its_mapped_final_batch_contract(self) -> None:
        plan = classify_paths(["tests/scripts/test_agent_router_contract.py"])

        self.assertEqual(("guidance",), plan.surfaces)
        self.assertFalse(any(check.surface == "changed-test" for check in plan.checks))

    def test_setup_aggregator_change_selects_its_direct_contract(self) -> None:
        plan = classify_paths(["scripts/ci/validate_agent_setup.py"])

        self.assertIn("tests.scripts.test_validate_agent_setup", plan.test_modules)

    def test_git_diff_includes_deletes_renames_and_type_changes(self) -> None:
        completed = subprocess.CompletedProcess(
            [],
            0,
            "D\0tests/scripts/test_agent_old.py\0"
            "R100\0tests/scripts/test_guard_old.py\0tests/scripts/test_guard_new.py\0"
            "T\0chaos-engine/hooks/kernel.py\0",
        )
        with patch("scripts.ci.harness_pr_gate.subprocess.run", return_value=completed) as run:
            paths = changed_paths(ROOT, "a" * 40, "b" * 40)

        self.assertIn("--diff-filter=ACDMRT", run.call_args.args[0])
        self.assertIn("--name-status", run.call_args.args[0])
        self.assertIn("--merge-base", run.call_args.args[0])
        self.assertEqual(
            [False, False, True, True],
            [path.executable for path in paths],
        )
        deleted_plan = classify_paths([paths[0]])
        renamed_plan = classify_paths([paths[2]])
        self.assertIn("fallback", deleted_plan.surfaces)
        self.assertIn("lifecycle", renamed_plan.surfaces)
        self.assertFalse(any(check.surface == "changed-test" for check in renamed_plan.checks))

    def test_git_diff_excludes_changes_unique_to_diverged_base_tip(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            git_executable = shutil.which("git")
            self.assertIsNotNone(git_executable)

            def git(*args: str) -> str:
                completed = subprocess.run(  # nosec B603 - resolved git on a temporary fixture.
                    [cast(str, git_executable), *args],
                    cwd=root,
                    capture_output=True,
                    text=True,
                    check=True,
                )
                return completed.stdout.strip()

            git("init", "--initial-branch=main")
            git("config", "user.name", "Harness Test")
            git("config", "user.email", "harness@example.invalid")
            (root / "shared.txt").write_text("shared\n", encoding="utf-8")
            git("add", "shared.txt")
            git("commit", "-m", "shared")
            git("switch", "-c", "candidate")
            (root / "candidate-only.txt").write_text("candidate\n", encoding="utf-8")
            git("add", "candidate-only.txt")
            git("commit", "-m", "candidate")
            head = git("rev-parse", "HEAD")
            git("switch", "main")
            (root / "base-only.txt").write_text("base\n", encoding="utf-8")
            git("add", "base-only.txt")
            git("commit", "-m", "base")
            base = git("rev-parse", "HEAD")

            paths = changed_paths(root, base, head)

        self.assertEqual(["candidate-only.txt"], list(paths))

    def test_force_rewrite_uses_only_the_rewritten_head_tree(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            git_executable = shutil.which("git")
            self.assertIsNotNone(git_executable)

            def git(*args: str) -> str:
                completed = subprocess.run(  # nosec B603 - resolved git on a temporary fixture.
                    [cast(str, git_executable), *args],
                    cwd=root,
                    capture_output=True,
                    text=True,
                    check=True,
                )
                return completed.stdout.strip()

            git("init", "--initial-branch=main")
            git("config", "user.name", "Harness Test")
            git("config", "user.email", "harness@example.invalid")
            (root / "shared.txt").write_text("shared\n", encoding="utf-8")
            git("add", "shared.txt")
            git("commit", "-m", "shared")
            git("switch", "-c", "candidate")
            old_path = root / "scripts/agents/old_only.py"
            old_path.parent.mkdir(parents=True)
            old_path.write_text("old\n", encoding="utf-8")
            git("add", old_path.relative_to(root).as_posix())
            git("commit", "-m", "old candidate")
            old_head = git("rev-parse", "HEAD")

            git("switch", "-C", "candidate", "main")
            new_path = root / ".github/workflows/pr-gate.yml"
            new_path.parent.mkdir(parents=True)
            new_path.write_text("name: rewritten\n", encoding="utf-8")
            git("add", new_path.relative_to(root).as_posix())
            git("commit", "-m", "rewritten candidate")
            rewritten_head = git("rev-parse", "HEAD")

            paths = changed_paths(root, old_head, rewritten_head)

        self.assertEqual([".github/workflows/pr-gate.yml"], list(paths))

    def test_generation_and_live_installer_contracts_never_use_full_fallback(self) -> None:
        for path in (
            "tests/scripts/test_chaos_engine_generation_runtime.py",
            "tests/scripts/test_chaos_engine_live_installer_acceptance.py",
        ):
            with self.subTest(path=path):
                plan = classify_paths([path])
                check_ids = {check.id for check in plan.checks}
                self.assertEqual(("installer",), plan.surfaces)
                self.assertNotIn("fallback-contract", check_ids)
                self.assertIn("protected-installer-acceptance", check_ids)
                self.assertIn("protected-rollback", check_ids)

    def test_reachability_contract_uses_focused_guidance_surface(self) -> None:
        plan = classify_paths(["tests/scripts/test_agent_harness_reachability.py"])

        self.assertEqual(("guidance",), plan.surfaces)
        self.assertNotIn("fallback-contract", {check.id for check in plan.checks})
        self.assertIn("tests.scripts.test_agent_harness_reachability", plan.test_modules)

    def test_non_harness_path_selects_no_harness_checks(self) -> None:
        plan = classify_paths(["shaft-engine/src/main/java/example/Thing.java"])

        self.assertEqual((), plan.surfaces)
        self.assertEqual((), plan.checks)

    def test_dotted_host_adapter_is_not_lost_during_path_normalization(self) -> None:
        plan = classify_paths([".claude/settings.json"])

        self.assertIn("hosts", plan.surfaces)

    def test_path_traversal_input_fails_closed(self) -> None:
        with self.assertRaises(GateError):
            classify_paths(["../chaos-engine/hooks/kernel.py"])


class WaiverTest(unittest.TestCase):
    def test_valid_receipt_is_exact_head_check_specific_and_expiring(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            receipt = event_waiver(
                write_reviews(directory, [review_record()]), HEAD, now=NOW
            )

        self.assertIsNotNone(receipt)
        receipt = cast(WaiverReceipt, receipt)
        self.assertEqual(("guidance-contract",), receipt.check_ids)
        self.assertEqual(HEAD, receipt.head_sha)

    def test_stale_malformed_blanket_and_blank_receipts_fail_closed(self) -> None:
        cases = (
            waiver_body(allowed_check_ids=["*"]),
            waiver_body(expires_at="2026-08-21T12:00:00Z"),
            waiver_body(rationale=" "),
            waiver_body(replacement_proof=""),
            "```chaos-engine-waiver\n{broken\n```",
            waiver_body() + waiver_body(),
        )
        for body in cases:
            with self.subTest(body=body[:80]):
                with self.assertRaises(GateError):
                    parse_waiver(body, now=NOW)

    def test_protected_categories_can_never_be_waived(self) -> None:
        protected = (
            "protected-security",
            "protected-ownership",
            "protected-corruption",
            "protected-rollback",
            "protected-secret-safety",
            "protected-installer-acceptance",
            "protected-confirmed-correctness",
        )
        for check_id in protected:
            with self.subTest(check_id=check_id):
                with self.assertRaises(GateError):
                    parse_waiver(
                        waiver_body(allowed_check_ids=[check_id]),
                        now=NOW,
                    )

    def test_behavioral_kernel_lifecycle_and_host_contracts_cannot_be_waived(self) -> None:
        for check_id in ("kernel-contract", "lifecycle-contract", "host-contract"):
            with self.subTest(check_id=check_id):
                self.assertTrue(
                    next(
                        check
                        for check in classify_paths(
                            {
                                "kernel-contract": ["chaos-engine/hooks/kernel.py"],
                                "lifecycle-contract": ["chaos-engine/hooks/guard.py"],
                                "host-contract": ["chaos-engine/hosts.py"],
                            }[check_id]
                        ).checks
                        if check.id == check_id
                    ).protected
                )
                with self.assertRaises(GateError):
                    parse_waiver(
                        waiver_body(allowed_check_ids=[check_id]),
                        now=NOW,
                    )

    def test_only_owner_review_on_exact_head_can_authorize(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            non_owner = review_record(user={"login": "outside-contributor"})
            stale_owner = review_record(commit_id="b" * 40)
            path = write_reviews(directory, [non_owner, stale_owner])
            self.assertIsNone(event_waiver(path, HEAD, now=NOW))

    def test_pr_author_or_editor_body_cannot_authorize(self) -> None:
        forgeable_event = {
            "pull_request": {
                "body": waiver_body(),
                "head": {"sha": HEAD},
                "user": {"login": "MohabMohie"},
                "author_association": "OWNER",
            }
        }
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "event.json"
            path.write_text(json.dumps(forgeable_event), encoding="utf-8")
            with self.assertRaises(GateError):
                event_waiver(path, HEAD, now=NOW)

    def test_owner_review_must_be_submitted_and_unambiguous(self) -> None:
        invalid = (
            review_record(state="PENDING", submitted_at=None),
            review_record(state="DISMISSED"),
            review_record(
                body=waiver_body(allowed_check_ids=["host-contract"]),
                last_edited_at="2026-08-22T11:30:00Z",
            ),
        )
        with tempfile.TemporaryDirectory() as directory:
            path = write_reviews(directory, list(invalid))
            self.assertIsNone(event_waiver(path, HEAD, now=NOW))

            path = write_reviews(directory, [review_record(), review_record(id=43)])
            with self.assertRaises(GateError):
                event_waiver(path, HEAD, now=NOW)


class OutputAndWorkflowTest(unittest.TestCase):
    def test_plan_only_text_renders_checks_as_planned(self):
        payload = {
            "valid": True,
            "surfaces": ["hosts"],
            "timing": {"elapsed_seconds": 0.0, "budget_seconds": 240},
            "checks": [{
                "id": "host-contract", "protected": True,
                "tests": ["tests.scripts.test_chaos_engine_hosts"],
                "reproduction_command": "python -m unittest hosts",
            }],
        }
        self.assertIn("status=planned", render_text(payload))

    def workflow(self) -> dict[str, object]:
        return yaml.safe_load(
            (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        )

    def filters(self) -> dict[str, list[str]]:
        workflow = self.workflow()
        steps = workflow["jobs"]["changes"]["steps"]
        filter_step = next(step for step in steps if step.get("id") == "filter")
        return yaml.safe_load(filter_step["with"]["filters"])

    def test_every_referenced_changes_output_is_declared_and_filtered(self) -> None:
        workflow = self.workflow()
        jobs = workflow["jobs"]
        outputs = set(jobs["changes"]["outputs"])
        filters = set(self.filters())
        referenced = {
            match.group(1)
            for job in jobs.values()
            for match in re.finditer(
                r"needs\.changes\.outputs\.([a-z_]+)", str(job.get("if", ""))
            )
        }

        self.assertEqual(set(), referenced - outputs)
        self.assertEqual(set(), referenced - filters)

    def test_pr_gate_definition_and_shared_actions_have_distinct_fanout(self) -> None:
        workflow = self.workflow()
        jobs = workflow["jobs"]
        filters = self.filters()
        product_jobs = (
            "installer-verify",
            "intellij-build",
            "cli",
            "capture-e2e",
            "unit-tests",
            "template-coupling",
            "module-boundary",
        )

        self.assertIn("pr_gate", filters)
        self.assertEqual([".github/workflows/pr-gate.yml"], filters["pr_gate"])
        self.assertNotIn(".github/workflows/pr-gate.yml", filters["infra"])
        self.assertIn(".github/actions/**", filters["infra"])
        self.assertIn("needs.changes.outputs.pr_gate == 'true'", jobs["agent-guidance"]["if"])
        for name in product_jobs:
            with self.subTest(job=name):
                condition = jobs[name]["if"]
                self.assertNotIn("outputs.pr_gate", condition)
                self.assertIn("outputs.infra", condition)

    def test_json_plan_is_concise_reproducible_and_contains_no_pr_body(self) -> None:
        plan = classify_paths(["chaos-engine/hooks/kernel.py"])
        payload = json.loads(render_json(plan, head_sha=HEAD, budget_seconds=240))

        self.assertEqual(1, payload["schema"])
        self.assertEqual(["kernel"], payload["surfaces"])
        self.assertEqual(240, payload["timing"]["budget_seconds"])
        self.assertEqual(600, payload["timing"]["recorded_baseline_median_seconds"])
        self.assertEqual(0.6, payload["timing"]["maximum_budget_reduction"])
        self.assertEqual(
            ["scheduled-exhaustive", "release-promotion"], payload["deferred_classes"]
        )
        self.assertEqual("blocking-protected-invariant", payload["checks"][0]["class"])
        self.assertIn("-m unittest", payload["checks"][0]["reproduction_command"])
        self.assertNotIn("pr_body", payload)
        self.assertNotIn("replacement_proof", payload)
        self.assertEqual(
            "git push --force-with-lease origin HEAD",
            payload["safe_history_update_command"],
        )

    def test_runner_never_applies_a_waiver_to_a_protected_failed_check(self) -> None:
        plan = classify_paths(["chaos-engine/hooks/kernel.py"])
        receipt = WaiverReceipt(
            HEAD,
            ("kernel-contract",),
            datetime(2026, 8, 29, tzinfo=timezone.utc),
        )

        def result_for(command: list[str], *_: object, **__: object) -> tuple[str, int | None]:
            module = command[3]
            return ("failed", 1) if module.endswith("kernel") else ("passed", 0)

        with patch("scripts.ci.harness_pr_gate._run_check", side_effect=result_for):
            payload, exit_code = run_plan(
                ROOT,
                plan,
                head_sha=HEAD,
                budget_seconds=240,
                waiver=receipt,
            )

        self.assertEqual(1, exit_code)
        self.assertEqual("failed", payload["checks"][0]["status"])
        self.assertEqual([], payload["waiver"]["applied_check_ids"])

    def test_timeout_is_never_waived(self) -> None:
        plan = GatePlan(("kernel",), (classify_paths(["chaos-engine/hooks/kernel.py"]).checks[0],))
        receipt = WaiverReceipt(HEAD, ("kernel-contract",), NOW)

        with patch("scripts.ci.harness_pr_gate._run_check", return_value=("timeout", None)):
            payload, exit_code = run_plan(
                ROOT,
                plan,
                head_sha=HEAD,
                budget_seconds=1,
                waiver=receipt,
            )

        self.assertEqual(1, exit_code)
        self.assertEqual("timeout", payload["checks"][0]["status"])

    def test_pr_workflow_is_bounded_and_full_suites_are_scheduled(self) -> None:
        pr_gate = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        agent_job = pr_gate.split("  agent-guidance:", 1)[1].split(
            "  installer-verify:", 1
        )[0]
        scheduled = (ROOT / ".github/workflows/agent-plugin-acceptance.yml").read_text(
            encoding="utf-8"
        )

        self.assertIn("scripts/ci/harness_pr_gate.py", agent_job)
        self.assertIn("--budget-seconds 240", agent_job)
        self.assertNotIn("npm install --global", agent_job)
        self.assertNotIn("tests.scripts.test_agent_plugin_client_smoke", agent_job)
        self.assertNotIn("matrix:", agent_job)
        self.assertIn("pull-requests: read", agent_job)
        self.assertIn("set -euo pipefail", agent_job)
        self.assertIn("gh api graphql", agent_job)
        self.assertIn("lastEditedAt", agent_job)
        self.assertIn("--reviews", agent_job)
        self.assertNotIn("--event", agent_job)
        self.assertIn("'scripts/ci/harness_pr_gate.py'", pr_gate)
        self.assertIn("'tests/scripts/test_harness_pr_gate.py'", pr_gate)
        self.assertIn("schedule:", scheduled)
        self.assertIn("workflow_dispatch:", scheduled)
        self.assertIn("deterministic-harness-full:", scheduled)
        self.assertIn("chaos-engine-cross-platform:", scheduled)
        self.assertIn("harness-platform-contracts:", scheduled)
        self.assertIn("tests.scripts.test_build_retry", scheduled)
        self.assertIn("tests.scripts.test_chaos_engine_bootstrap", scheduled)
        self.assertIn("tests.scripts.test_chaos_engine_dependencies", scheduled)

    def test_scheduled_suite_retains_exhaustive_harness_coverage(self) -> None:
        scheduled = (ROOT / ".github/workflows/agent-plugin-acceptance.yml").read_text(
            encoding="utf-8"
        )
        deterministic = scheduled.split("  deterministic-harness-full:", 1)[1].split(
            "  chaos-engine-cross-platform:", 1
        )[0]
        self.assertIn("tests.scripts.test_agent_harness_portability", deterministic)
        self.assertIn("tests.scripts.test_guard_lifecycle", deterministic)
        self.assertIn("tests.scripts.test_chaos_engine_generation_runtime", deterministic)

    def test_host_parity_evidence_can_live_in_scheduled_exhaustive_suite(self) -> None:
        errors = validate_host_parity(ROOT)

        self.assertEqual(
            [],
            [error for error in errors if error["code"] == "host-parity-ci"],
        )


if __name__ == "__main__":
    unittest.main()
