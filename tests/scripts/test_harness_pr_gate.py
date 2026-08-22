from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

from scripts.ci.harness_pr_gate import (
    GateError,
    GatePlan,
    WaiverReceipt,
    changed_paths,
    classify_paths,
    event_waiver,
    parse_waiver,
    render_json,
    run_plan,
)
from scripts.ci.validate_agent_setup import validate_host_parity


ROOT = Path(__file__).resolve().parents[2]
HEAD = "a" * 40
NOW = datetime(2026, 8, 22, 12, 0, tzinfo=timezone.utc)


def waiver_body(**overrides: object) -> str:
    payload: dict[str, object] = {
        "schema": 1,
        "head_sha": HEAD,
        "check_ids": ["kernel-contract"],
        "expires_at": "2026-08-29T12:00:00Z",
        "owner_authorization": "@MohabMohie approved",
        "rationale": "Legacy router shape is replaced by the lifecycle kernel contract.",
        "replacement_proof": "py -3 -m unittest tests.scripts.test_chaos_engine_kernel -v",
    }
    payload.update(overrides)
    return f"PR prose\n\n```chaos-engine-waiver\n{json.dumps(payload)}\n```\n"


class ClassifierTest(unittest.TestCase):
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

    def test_installer_change_keeps_5299_acceptance_and_rollback_protected(self) -> None:
        plan = classify_paths(["chaos-engine/dependencies.py"])

        self.assertEqual(("installer",), plan.surfaces)
        protected = {check.id for check in plan.checks if check.protected}
        self.assertIn("protected-installer-acceptance", protected)
        self.assertIn("protected-rollback", protected)
        self.assertIn("tests.scripts.test_chaos_engine_bootstrap", plan.test_modules)
        self.assertIn("tests.scripts.test_chaos_engine_dependencies", plan.test_modules)

    def test_guard_owner_change_runs_non_waivable_security_check(self) -> None:
        plan = classify_paths(["scripts/agents/guard.py"])

        checks = {check.id: check for check in plan.checks}
        self.assertIn("protected-security", checks)
        security = checks["protected-security"]
        self.assertTrue(security.protected)
        self.assertEqual(("tests.scripts.test_guard_memory_worktree",), security.modules)
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
        self.assertIn("tests.scripts.test_validate_agent_setup", plan.test_modules)

    def test_every_registered_but_unmapped_harness_path_uses_fallback(self) -> None:
        for path in (
            ".github/copilot-instructions.md",
            "scripts/ci/validate_red_before_green.py",
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

    def test_edited_harness_test_executes_itself_as_protected(self) -> None:
        plan = classify_paths(["tests/scripts/test_agent_router_contract.py"])

        changed = [check for check in plan.checks if check.surface == "changed-test"]
        self.assertEqual(1, len(changed))
        self.assertTrue(changed[0].protected)
        self.assertEqual(("tests.scripts.test_agent_router_contract",), changed[0].modules)

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
        self.assertEqual(
            [False, False, True, True],
            [path.executable for path in paths],
        )
        deleted_plan = classify_paths([paths[0]])
        self.assertFalse(any(check.surface == "changed-test" for check in deleted_plan.checks))
        renamed_plan = classify_paths([paths[2]])
        self.assertTrue(any(check.surface == "changed-test" for check in renamed_plan.checks))

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
        receipt = parse_waiver(waiver_body(), expected_head=HEAD, now=NOW)

        self.assertIsNotNone(receipt)
        assert receipt is not None
        self.assertEqual(("kernel-contract",), receipt.check_ids)
        self.assertEqual(HEAD, receipt.head_sha)

    def test_stale_malformed_blanket_and_blank_receipts_fail_closed(self) -> None:
        cases = (
            waiver_body(head_sha="b" * 40),
            waiver_body(check_ids=["*"]),
            waiver_body(expires_at="2026-08-21T12:00:00Z"),
            waiver_body(rationale=" "),
            waiver_body(replacement_proof=""),
            waiver_body(owner_authorization="approved"),
            "```chaos-engine-waiver\n{broken\n```",
            waiver_body() + waiver_body(),
        )
        for body in cases:
            with self.subTest(body=body[:80]):
                with self.assertRaises(GateError):
                    parse_waiver(body, expected_head=HEAD, now=NOW)

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
                        waiver_body(check_ids=[check_id]),
                        expected_head=HEAD,
                        now=NOW,
                    )

    def test_event_rejects_forgeable_marker_from_non_owner_pr_author(self) -> None:
        event = {
            "pull_request": {
                "body": waiver_body(),
                "head": {"sha": HEAD},
                "user": {"login": "outside-contributor"},
                "author_association": "CONTRIBUTOR",
            }
        }
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "event.json"
            path.write_text(json.dumps(event), encoding="utf-8")
            with self.assertRaises(GateError):
                event_waiver(path, HEAD)


class OutputAndWorkflowTest(unittest.TestCase):
    def test_json_plan_is_concise_reproducible_and_contains_no_pr_body(self) -> None:
        plan = classify_paths(["chaos-engine/hooks/kernel.py"])
        payload = json.loads(render_json(plan, head_sha=HEAD, budget_seconds=240))

        self.assertEqual(1, payload["schema"])
        self.assertEqual(["kernel"], payload["surfaces"])
        self.assertEqual(240, payload["timing"]["budget_seconds"])
        self.assertIn("-m unittest", payload["checks"][0]["reproduction_command"])
        self.assertNotIn("pr_body", payload)
        self.assertNotIn("replacement_proof", payload)
        self.assertEqual(
            "git push --force-with-lease origin HEAD",
            payload["safe_history_update_command"],
        )

    def test_runner_applies_only_exact_non_protected_failed_check_waiver(self) -> None:
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

        self.assertEqual(0, exit_code)
        self.assertEqual("waived", payload["checks"][0]["status"])
        self.assertEqual(["kernel-contract"], payload["waiver"]["applied_check_ids"])

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

    def test_host_parity_evidence_can_live_in_scheduled_exhaustive_suite(self) -> None:
        errors = validate_host_parity(ROOT)

        self.assertEqual(
            [],
            [error for error in errors if error["code"] == "host-parity-ci"],
        )


if __name__ == "__main__":
    unittest.main()
