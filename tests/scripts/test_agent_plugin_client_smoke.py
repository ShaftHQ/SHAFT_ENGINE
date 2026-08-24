"""Native Claude/Codex compatibility evidence tests (#4641)."""

import json
import subprocess  # nosec B404 - test double and fixed local commands only.
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

try:
    from scripts.ci.agent_plugin_client_smoke import (
        CLIENTS,
        EVIDENCE_LEVELS,
        LOAD_PROMPT,
        LOAD_PROOF_TERMS,
        _run,
        collect_evidence,
    )
except ImportError:
    CLIENTS = None
    EVIDENCE_LEVELS = None
    LOAD_PROMPT = None
    LOAD_PROOF_TERMS = None
    collect_evidence = None
try:
    from scripts.ci.agent_plugin_client_smoke import collect_runtime_launch_evidence
except ImportError:
    collect_runtime_launch_evidence = None


class AgentPluginClientSmokeTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.package_root = Path(self.temporary_directory.name) / "shaft-skills"
        self.package_root.mkdir()
        self.runner = FakeRunner()
        self.routing_corpus = {
            "schema_version": 1,
            "package": "shaft-skills",
            "thresholds": {"case_pass_rate": 1.0, "positive_skill_coverage": 1.0},  # nosec B105 - routing thresholds.
            "cases": [
                {
                    "id": "requirements",
                    "prompt": "Analyze checkout requirements for testability and acceptance gaps.",
                    "expected_skill": "skill-a",
                    "rejected_skills": ["skill-b"],
                },
                {
                    "id": "planning",
                    "prompt": "Create a risk-based scope, schedule, and entry and exit criteria.",
                    "expected_skill": "skill-b",
                    "rejected_skills": ["skill-a"],
                },
            ],
        }
        self.runner.routing_choices = {
            case["prompt"]: case["expected_skill"] for case in self.routing_corpus["cases"]
        }

    def test_client_evidence_api_is_available(self):
        self.assertIsInstance(CLIENTS, dict)
        self.assertEqual(
            EVIDENCE_LEVELS,
            (
                "package_validation",
                "marketplace_discovery",
                "install_enable",
                "real_load",
            ),
        )
        self.assertTrue(callable(collect_evidence))

    def test_native_runner_resolves_npm_shims_before_process_start(self):
        resolved = r"C:\npm\claude.CMD"
        environment = {"PATH": "fixture-path"}
        with (
            patch(
                "scripts.ci.agent_plugin_client_smoke.shutil.which",
                return_value=resolved,
            ) as which,
            patch("scripts.ci.agent_plugin_client_smoke.subprocess.run") as native_runner,
        ):
            native_runner.return_value = subprocess.CompletedProcess(
                [resolved, "--version"], 0, stdout="claude 2.1.223", stderr=""
            )
            result = _run(
                native_runner,
                ["claude", "--version"],
                self.package_root,
                environment,
            )

        self.assertEqual(0, result.returncode)
        which.assert_called_once_with("claude", path="fixture-path")
        self.assertEqual(resolved, native_runner.call_args.args[0][0])

    def test_smoke_keeps_four_evidence_levels_distinct_for_both_clients(self):
        evidence = collect_evidence(self.package_root, runner=self.runner)

        self.assertEqual(evidence["schema_version"], 1)
        self.assertEqual(evidence["package"], "shaft-skills")
        self.assertEqual(
            {(row["client"], row["evidence_level"]) for row in evidence["results"]},
            {(client, level) for client in CLIENTS for level in EVIDENCE_LEVELS},
        )
        for row in evidence["results"]:
            expected = "not_run" if row["evidence_level"] == "real_load" else "pass"
            self.assertEqual(row["verdict"], expected, row)
            self.assertIsInstance(row["commands"], list)
            self.assertIsInstance(row["context_warnings"], list)

    def test_native_commands_and_versions_are_exact_and_pinned(self):
        collect_evidence(self.package_root, runner=self.runner)
        commands = self.runner.commands
        resolved_package_root = str(self.package_root.resolve())

        self.assertIn(("claude", "--version"), commands)
        self.assertIn(("codex", "--version"), commands)
        self.assertIn(
            ("claude", "plugin", "validate", "--strict", resolved_package_root),
            commands,
        )
        self.assertIn(
            ("claude", "plugin", "marketplace", "add", resolved_package_root, "--scope", "project"),
            commands,
        )
        self.assertIn(("claude", "plugin", "list", "--available", "--json"), commands)
        self.assertIn(
            ("claude", "plugin", "install", "shaft-skills@shaft-skills", "--scope", "project"),
            commands,
        )
        self.assertIn(
            ("codex", "plugin", "marketplace", "add", resolved_package_root, "--json"),
            commands,
        )
        self.assertIn(("codex", "plugin", "list", "--available", "--json"), commands)
        self.assertIn(("codex", "plugin", "add", "shaft-skills@shaft-skills", "--json"), commands)

    def test_client_state_is_cleaned_after_success(self):
        evidence = collect_evidence(self.package_root, runner=self.runner)

        self.assertIn(
            ("claude", "plugin", "uninstall", "shaft-skills@shaft-skills", "--scope", "project", "--yes"),
            self.runner.commands,
        )
        self.assertIn(
            ("claude", "plugin", "marketplace", "remove", "shaft-skills", "--scope", "project"),
            self.runner.commands,
        )
        self.assertIn(
            ("codex", "plugin", "remove", "shaft-skills@shaft-skills", "--json"),
            self.runner.commands,
        )
        self.assertIn(
            ("codex", "plugin", "marketplace", "remove", "shaft-skills", "--json"),
            self.runner.commands,
        )
        installs = [row for row in evidence["results"] if row["evidence_level"] == "install_enable"]
        recorded = {tuple(command) for row in installs for command in row["commands"]}
        self.assertIn(
            ("claude", "plugin", "uninstall", "shaft-skills@shaft-skills", "--scope", "project", "--yes"),
            recorded,
        )
        self.assertIn(("codex", "plugin", "remove", "shaft-skills@shaft-skills", "--json"), recorded)

    def test_preexisting_client_state_is_never_mutated(self):
        self.runner.preexisting = True

        evidence = collect_evidence(self.package_root, runner=self.runner)

        validations = [row for row in evidence["results"] if row["evidence_level"] == "package_validation"]
        self.assertEqual({row["verdict"] for row in validations}, {"fail"})
        self.assertTrue(all("pre-existing" in row["detail"] for row in validations))
        self.assertFalse(any(command[1:4] == ("plugin", "marketplace", "add") for command in self.runner.commands))
        self.assertFalse(any("remove" in command or "uninstall" in command for command in self.runner.commands))

    def test_failed_state_preflight_never_mutates_client_state(self):
        self.runner.fail_when = lambda command: command[1:4] == ("plugin", "marketplace", "list")

        evidence = collect_evidence(self.package_root, runner=self.runner)

        validations = [row for row in evidence["results"] if row["evidence_level"] == "package_validation"]
        self.assertEqual({row["verdict"] for row in validations}, {"fail"})
        self.assertTrue(all("preflight" in row["detail"].lower() for row in validations))
        self.assertFalse(any(command[1:4] == ("plugin", "marketplace", "add") for command in self.runner.commands))

    def test_missing_client_process_is_structured_as_failure(self):
        self.runner.raise_when = lambda command: command[0] == "claude"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        claude = [row for row in evidence["results"] if row["client"] == "claude"]
        self.assertEqual(len(claude), 4)
        self.assertEqual(claude[0]["verdict"], "fail")
        self.assertIn("not found", claude[0]["detail"].lower())

    def test_client_state_is_cleaned_after_failure(self):
        self.runner.fail_when = lambda command: "install" in command

        evidence = collect_evidence(self.package_root, runner=self.runner)

        self.assertTrue(any(row["verdict"] == "fail" for row in evidence["results"]))
        self.assertIn(
            ("claude", "plugin", "marketplace", "remove", "shaft-skills", "--scope", "project"),
            self.runner.commands,
        )

    def test_cleanup_failure_invalidates_install_evidence(self):
        self.runner.fail_when = lambda command: "uninstall" in command or "remove" in command

        evidence = collect_evidence(self.package_root, runner=self.runner)

        installs = [row for row in evidence["results"] if row["evidence_level"] == "install_enable"]
        self.assertEqual({row["verdict"] for row in installs}, {"fail"})
        self.assertTrue(all("cleanup" in row["detail"].lower() for row in installs))
        self.assertIn(
            ("codex", "plugin", "marketplace", "remove", "shaft-skills", "--json"),
            self.runner.commands,
        )

    def test_partial_marketplace_add_failure_is_cleaned_and_verified(self):
        self.runner.partial_fail_when = lambda command: command[1:4] == (
            "plugin",
            "marketplace",
            "add",
        )

        evidence = collect_evidence(self.package_root, runner=self.runner)

        affected_rows = {
            (row["client"], row["evidence_level"]): row
            for row in evidence["results"]
        }
        self.assertEqual(
            affected_rows[("claude", "marketplace_discovery")]["verdict"], "fail"
        )
        self.assertEqual(
            affected_rows[("codex", "package_validation")]["verdict"], "fail"
        )
        self.assertFalse(any(self.runner.marketplace_added.values()))
        self.assertTrue(any(command[1:4] == ("plugin", "marketplace", "remove") for command in self.runner.commands))

    def test_missing_live_credentials_are_external_blockers_only(self):
        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={},
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"external_blocker"})
        self.assertTrue(all("credential" in row["detail"].lower() for row in loads))
        self.assertTrue(
            all(
                row["verdict"] == "pass"
                for row in evidence["results"]
                if row["evidence_level"] != "real_load"
            )
        )

    def test_live_credentials_run_real_load_commands(self):
        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "test", "OPENAI_API_KEY": "test"},
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"pass"})
        self.assertTrue(any(command[:2] == ("claude", "-p") for command in self.runner.commands))
        self.assertTrue(any(command[:2] == ("codex", "exec") for command in self.runner.commands))

    def test_live_routing_corpus_runs_every_case_with_native_structured_output(self):
        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"pass"})
        self.assertTrue(all(len(row["case_results"]) == 2 for row in loads))
        self.assertTrue(
            all(case["verdict"] == "pass" for row in loads for case in row["case_results"])
        )
        self.assertTrue(any("--json-schema" in command for command in self.runner.commands))
        self.assertTrue(any("--output-schema" in command for command in self.runner.commands))
        self.assertEqual(evidence["package_decision"]["decision"], "retain-single-package")

    def test_live_routing_corpus_fails_when_the_selected_specialist_changes(self):
        first = self.routing_corpus["cases"][0]
        self.runner.routing_choices[first["prompt"]] = "skill-b"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"fail"})
        self.assertEqual(
            {case["verdict"] for row in loads for case in row["case_results"]},
            {"pass", "fail"},
        )
        self.assertEqual(
            evidence["package_decision"]["decision"], "investigate-split-or-profile"
        )

    def test_missing_credentials_block_every_routing_case_without_fabricating_results(self):
        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={},
            routing_corpus=self.routing_corpus,
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"external_blocker"})
        self.assertTrue(
            all(
                case["verdict"] == "external_blocker"
                for row in loads
                for case in row["case_results"]
            )
        )
        self.assertEqual(evidence["package_decision"]["decision"], "insufficient-evidence")

    def test_late_auth_failure_preserves_completed_routing_case(self):
        second = self.routing_corpus["cases"][1]
        self.runner.fail_when = lambda command: (
            command[:2] == ("claude", "-p") and second["prompt"] in command[2]
        )
        self.runner.failure_message = "401 Unauthorized"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual(
            [case["verdict"] for case in claude["case_results"]],
            ["pass", "external_blocker"],
        )

    def test_late_rate_limit_preserves_completed_routing_case(self):
        second = self.routing_corpus["cases"][1]
        self.runner.fail_when = lambda command: (
            command[:2] == ("claude", "-p") and second["prompt"] in command[2]
        )
        self.runner.failure_message = "429 Too Many Requests"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual(
            [case["verdict"] for case in claude["case_results"]],
            ["pass", "external_blocker"],
        )
        self.assertEqual(evidence["package_decision"]["decision"], "insufficient-evidence")

    def test_timeout_stops_the_client_loop_and_returns_partial_evidence(self):
        self.runner.timeout_when = lambda command: command[:2] == ("claude", "-p")

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual({case["verdict"] for case in claude["case_results"]}, {"external_blocker"})
        self.assertEqual(
            sum(command[:2] == ("claude", "-p") for command in self.runner.commands),
            1,
        )
        self.assertEqual(evidence["package_decision"]["decision"], "insufficient-evidence")

    def test_aggregate_deadline_stops_slow_successful_routing_and_returns_partial_evidence(self):
        clock = FakeClock()
        self.runner.clock = clock
        self.runner.routing_duration_seconds = 3

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
            clock=clock,
            routing_budget_seconds=5,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual(
            [case["verdict"] for case in claude["case_results"]],
            ["pass", "external_blocker"],
        )
        self.assertLessEqual(clock.value, 5)
        self.assertEqual(evidence["package_decision"]["decision"], "insufficient-evidence")

    def test_execution_deadline_bounds_setup_and_cleanup_before_artifact_reserve(self):
        clock = FakeClock()
        self.runner.clock = clock
        self.runner.command_duration_seconds = 100

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
            clock=clock,
            execution_budget_seconds=900,
            routing_budget_seconds=600,
            cleanup_reserve_seconds=120,
            artifact_reserve_seconds=60,
        )

        self.assertLessEqual(clock.value, 840)
        self.assertFalse(
            any(command[:2] in (("claude", "-p"), ("codex", "exec")) for command in self.runner.commands)
        )
        self.assertEqual(len(evidence["results"]), len(CLIENTS) * len(EVIDENCE_LEVELS))
        self.assertTrue(all(timeout <= 180 for timeout in self.runner.timeouts))

    def test_unknown_nonzero_is_a_client_failure_not_a_routing_failure(self):
        second = self.routing_corpus["cases"][1]
        self.runner.fail_when = lambda command: (
            command[:2] == ("claude", "-p") and second["prompt"] in command[2]
        )
        self.runner.failure_message = "unexpected native client crash"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual(claude["verdict"], "client_failure")
        self.assertEqual(
            [case["verdict"] for case in claude["case_results"]],
            ["pass", "client_failure"],
        )
        self.assertEqual(evidence["package_decision"]["decision"], "insufficient-evidence")

    def test_dns_failure_is_an_external_blocker_and_stops_the_client_loop(self):
        self.runner.fail_when = lambda command: command[:2] == ("claude", "-p")
        self.runner.failure_message = "getaddrinfo ENOTFOUND api.anthropic.com"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        claude = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual({case["verdict"] for case in claude["case_results"]}, {"external_blocker"})
        self.assertEqual(
            sum(command[:2] == ("claude", "-p") for command in self.runner.commands),
            1,
        )

    def test_validation_context_warning_changes_the_package_decision(self):
        self.runner.warning = "Warning: skill descriptions shortened to fit context budget"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
            routing_corpus=self.routing_corpus,
        )

        self.assertEqual(
            evidence["package_decision"]["decision"], "investigate-split-or-profile"
        )
        self.assertIn(self.runner.warning, evidence["package_decision"]["context_budget_warnings"])

    def test_each_live_client_receives_only_its_own_credential(self):
        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "anthropic-secret", "OPENAI_API_KEY": "openai-secret"},
        )

        self.assertEqual(
            {row["verdict"] for row in evidence["results"] if row["evidence_level"] == "real_load"},
            {"pass"},
        )
        for command, environment in self.runner.environments:
            if command[:2] == ("claude", "-p"):
                self.assertEqual(environment.get("ANTHROPIC_API_KEY"), "anthropic-secret")
                self.assertNotIn("OPENAI_API_KEY", environment)
            elif command[:2] == ("codex", "exec"):
                self.assertEqual(environment.get("OPENAI_API_KEY"), "openai-secret")
                self.assertNotIn("ANTHROPIC_API_KEY", environment)
            else:
                self.assertNotIn("ANTHROPIC_API_KEY", environment)
                self.assertNotIn("OPENAI_API_KEY", environment)

    def test_disabled_configured_credential_is_an_external_blocker(self):
        self.runner.fail_when = lambda command: command[:2] == ("claude", "-p")
        self.runner.failure_message = "organization access is disabled for this API key"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
        )

        claude_load = next(
            row
            for row in evidence["results"]
            if row["client"] == "claude" and row["evidence_level"] == "real_load"
        )
        self.assertEqual(claude_load["verdict"], "external_blocker")

    def test_common_rejected_credential_messages_are_external_blockers(self):
        for message in ("401 Unauthorized", "invalid API key"):
            with self.subTest(message=message):
                runner = FakeRunner()
                runner.fail_when = lambda command: command[:2] == ("claude", "-p")
                runner.failure_message = message

                evidence = collect_evidence(
                    self.package_root,
                    mode="live",
                    runner=runner,
                    environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
                )

                claude_load = next(
                    row
                    for row in evidence["results"]
                    if row["client"] == "claude" and row["evidence_level"] == "real_load"
                )
                self.assertEqual(claude_load["verdict"], "external_blocker")

    def test_live_proof_is_not_supplied_in_the_prompt(self):
        self.assertTrue(LOAD_PROOF_TERMS)
        self.assertTrue(all(term.lower() not in LOAD_PROMPT.lower() for term in LOAD_PROOF_TERMS))

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": "configured", "OPENAI_API_KEY": "configured"},
        )

        loads = [row for row in evidence["results"] if row["evidence_level"] == "real_load"]
        self.assertEqual({row["verdict"] for row in loads}, {"pass"})

    def test_context_budget_warnings_are_preserved_in_structured_results(self):
        self.runner.warning = "Warning: skill description exceeds the context token budget"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        warnings = [warning for row in evidence["results"] for warning in row["context_warnings"]]
        self.assertIn(self.runner.warning, warnings)

    def test_context_budget_notice_without_warning_prefix_is_preserved(self):
        self.runner.warning = "Skill descriptions were shortened to fit the context budget"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        warnings = [warning for row in evidence["results"] for warning in row["context_warnings"]]
        self.assertIn(self.runner.warning, warnings)

    def test_runtime_credentials_are_redacted_from_structured_evidence(self):
        credential_value = "DUMMY_SECRET_SENTINEL"
        self.runner.warning = f"Warning: context token {credential_value}"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": credential_value, "OPENAI_API_KEY": credential_value},
        )

        serialized = json.dumps(evidence)
        self.assertNotIn(credential_value, serialized)
        self.assertIn("[REDACTED]", serialized)

    def test_short_runtime_credentials_are_also_redacted(self):
        credential_value = "short"
        self.runner.warning = f"Warning: context token {credential_value}"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": credential_value, "OPENAI_API_KEY": credential_value},
        )

        self.assertNotIn(credential_value, json.dumps(evidence))

    def test_collect_evidence_rejects_an_invalid_redaction_result(self):
        with patch(
            "scripts.ci.agent_plugin_client_smoke._redact",
            return_value="not-an-evidence-object",
        ):
            with self.assertRaisesRegex(TypeError, "redacted evidence must be an object"):
                collect_evidence(self.package_root, runner=self.runner)

    def test_install_evidence_requires_the_native_plugin_id_to_be_enabled(self):
        self.runner.installed_plugin = {"id": "shaft-skills@shaft-skills", "enabled": False}

        evidence = collect_evidence(self.package_root, runner=self.runner)

        discovery = [row for row in evidence["results"] if row["evidence_level"] == "marketplace_discovery"]
        installs = [row for row in evidence["results"] if row["evidence_level"] == "install_enable"]
        self.assertEqual({row["verdict"] for row in discovery}, {"pass"})
        self.assertEqual({row["verdict"] for row in installs}, {"fail"})

    def test_install_evidence_rejects_a_bare_package_name(self):
        self.runner.installed_plugin = "shaft-skills"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        installs = [row for row in evidence["results"] if row["evidence_level"] == "install_enable"]
        self.assertEqual({row["verdict"] for row in installs}, {"fail"})

    def test_client_version_must_match_the_exact_pinned_token(self):
        self.runner.versions["claude"] = "claude 2.1.2230"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        claude = [row for row in evidence["results"] if row["client"] == "claude"]
        self.assertEqual(claude[0]["verdict"], "fail")
        self.assertTrue(all(row["verdict"] == "not_run" for row in claude[1:]))

    def test_client_version_rejects_contradictory_diagnostic_output(self):
        self.runner.versions["claude"] = "actual 2.1.224; expected 2.1.223"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        claude = [row for row in evidence["results"] if row["client"] == "claude"]
        self.assertEqual(claude[0]["verdict"], "fail")

    def test_client_version_rejects_a_different_build_suffix(self):
        self.runner.versions["claude"] = "claude 2.1.223+different-build"

        evidence = collect_evidence(self.package_root, runner=self.runner)

        claude = [row for row in evidence["results"] if row["client"] == "claude"]
        self.assertEqual(claude[0]["verdict"], "fail")

    def test_discovery_rejects_marketplace_name_as_plugin_identity(self):
        self.runner.available_plugin = {
            "pluginId": "other@shaft-skills",
            "marketplaceName": "shaft-skills",
        }

        evidence = collect_evidence(self.package_root, runner=self.runner)

        discovery = [row for row in evidence["results"] if row["evidence_level"] == "marketplace_discovery"]
        self.assertEqual({row["verdict"] for row in discovery}, {"fail"})

    def test_repository_docs_and_workflows_pin_the_contract(self):
        root = Path(__file__).resolve().parents[2]
        compatibility = (root / "agent-plugins/shaft-skills/COMPATIBILITY.md").read_text(encoding="utf-8")
        live = (root / ".github/workflows/agent-plugin-acceptance.yml").read_text(encoding="utf-8")
        workflow_readme = (root / ".github/workflows/README.md").read_text(encoding="utf-8")

        for heading in ("Package validation", "Marketplace discovery", "Install / enable", "Real load"):
            self.assertIn(heading, compatibility)
        self.assertIn("Claude Code 2.1.223", compatibility)
        self.assertIn("Codex CLI 0.146.0", compatibility)
        self.assertIn("tests.scripts.test_agent_plugin_client_smoke", live)
        self.assertIn("python scripts/ci/agent_plugin_client_smoke.py --mode live", live)
        self.assertIn("tests.scripts.test_shaft_skill_routing_eval", live)

    def test_chaos_engine_is_discovered_installed_and_launched_from_package_config(self):
        from scripts.ci.assemble_chaos_engine_plugin import assemble as assemble_chaos_engine

        package_root = Path(self.temporary_directory.name) / "chaos-engine"
        assemble_chaos_engine(Path(__file__).resolve().parents[2], package_root)
        runner = FakeRunner()
        runner.package_name = "chaos-engine"
        runner.installed_plugin = {"id": "chaos-engine@chaos-engine", "enabled": True}
        runner.available_plugin = {"name": "chaos-engine"}

        evidence = collect_evidence(package_root, runner=runner)
        self.assertEqual("chaos-engine", evidence["package"])
        self.assertEqual(
            {"pass"},
            {
                row["verdict"]
                for row in evidence["results"]
                if row["evidence_level"] in {"marketplace_discovery", "install_enable"}
            },
        )
        selectors = {part for command in runner.commands for part in command}
        self.assertIn("chaos-engine@chaos-engine", selectors)

        self.assertTrue(callable(collect_runtime_launch_evidence))
        launch = collect_runtime_launch_evidence(package_root)
        self.assertEqual("pass", launch["verdict"])
        self.assertEqual("runtime_launch", launch["evidence_level"])
        root = Path(__file__).resolve().parents[2]
        pr_gate = (root / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        live = (root / ".github/workflows/agent-plugin-acceptance.yml").read_text(encoding="utf-8")
        workflow_readme = (root / ".github/workflows/README.md").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_shaft_skill_routing_eval", live)
        guidance_filter = pr_gate.split("            agent_guidance:\n", 1)[1].split(
            "              # Reachability elements:", 1
        )[0]
        self.assertIn("'.github/workflows/agent-plugin-acceptance.yml'", guidance_filter)
        self.assertIn("'.github/workflows/README.md'", guidance_filter)
        self.assertIn("schedule:", live)
        self.assertIn("workflow_dispatch:", live)
        self.assertIn("ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}", live)
        self.assertIn("OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}", live)
        self.assertIn("--routing-corpus agent-plugins/shaft-skills/evals/cases.json", live)
        self.assertIn("--routing-budget-seconds 600", live)
        self.assertIn("--execution-budget-seconds 900", live)
        self.assertIn("timeout-minutes: 20", live)
        self.assertLessEqual(900, (20 * 60) - 300)
        self.assertIn("routing stops after 600 seconds", workflow_readme)
        self.assertIn("60 seconds for artifact writing", workflow_readme)
        before_live_step, live_step = live.split("      - name: Collect live compatibility evidence", 1)
        self.assertNotIn("ANTHROPIC_API_KEY", before_live_step)
        self.assertNotIn("OPENAI_API_KEY", before_live_step)
        self.assertIn("        env:\n          ANTHROPIC_API_KEY", live_step)
        self.assertIn("          OPENAI_API_KEY", live_step)
        self.assertIn("if: always()", live)
        self.assertIn("actions/upload-artifact@v7", live)
        agent_evidence_upload = live.split(
            "      - name: Upload structured compatibility evidence", 1
        )[1].split("\n      - name:", 1)[0]
        self.assertIn("retention-days: 4", agent_evidence_upload)
        for client in CLIENTS.values():
            self.assertIn(client["npm_package"], live)


class FakeClock:
    def __init__(self):
        """Start a deterministic monotonic clock at zero."""
        self.value = 0.0

    def __call__(self):
        return self.value


class FakeRunner:
    def __init__(self):
        """Initialize deterministic native-client state for smoke tests."""
        self.commands = []
        self.environments = []
        self.fail_when = lambda _command: False
        self.raise_when = lambda _command: False
        self.partial_fail_when = lambda _command: False
        self.timeout_when = lambda _command: False
        self.failure_message = "simulated failure"
        self.warning = ""
        self.installed_plugin = {"id": "shaft-skills@shaft-skills", "enabled": True}
        self.available_plugin = {"name": "shaft-skills"}
        self.package_name = "shaft-skills"
        self.versions = {"claude": "claude 2.1.223", "codex": "codex-cli 0.146.0"}
        self.preexisting = False
        self.marketplace_added = {"claude": False, "codex": False}
        self.installed = {"claude": False, "codex": False}
        self.routing_choices = {}
        self.clock = None
        self.command_duration_seconds = 0
        self.routing_duration_seconds = 0
        self.timeouts = []

    def __call__(self, command, **kwargs):  # noqa: MC0001  # Command fixture mirrors the supported client matrix.
        command = tuple(str(part) for part in command)
        self.commands.append(command)
        self.environments.append((command, kwargs.get("env", {})))
        self.timeouts.append(kwargs["timeout"])
        client = command[0]
        if self.raise_when(command):
            raise FileNotFoundError("simulated client not found")
        if self.timeout_when(command):
            raise subprocess.TimeoutExpired(command, 180)
        if self.clock:
            timeout = kwargs["timeout"]
            duration = (
                self.routing_duration_seconds
                if command[:2] in (("claude", "-p"), ("codex", "exec"))
                else self.command_duration_seconds
            )
            elapsed = min(duration, timeout)
            self.clock.value += elapsed
            if duration > timeout:
                raise subprocess.TimeoutExpired(command, timeout)
        if self.fail_when(command):
            return subprocess.CompletedProcess(command, 1, stdout="", stderr=self.failure_message)
        if command[-1:] == ("--version",):
            return subprocess.CompletedProcess(command, 0, stdout=self.versions[client], stderr="")
        if command[1:4] == ("plugin", "marketplace", "list"):
            entries = [{"name": self.package_name}] if self.preexisting or self.marketplace_added[client] else []
            output = json.dumps({"marketplaces": entries})
        elif command[1:3] == ("plugin", "list") and "--json" in command:
            if "--available" in command:
                entries = [self.available_plugin] if self.marketplace_added[client] else []
            else:
                entries = [self.installed_plugin] if self.preexisting or self.installed[client] else []
            output = json.dumps({"plugins": entries})
        elif command[1:4] == ("plugin", "marketplace", "add"):
            self.marketplace_added[client] = True
            if self.partial_fail_when(command):
                return subprocess.CompletedProcess(command, 1, stdout="", stderr="failed after write")
            output = "{}"
        elif command[1:3] in (("plugin", "install"), ("plugin", "add")):
            self.installed[client] = True
            output = "{}"
        elif command[1:3] in (("plugin", "uninstall"), ("plugin", "remove")):
            self.installed[client] = False
            output = "{}"
        elif command[1:4] == ("plugin", "marketplace", "remove"):
            self.marketplace_added[client] = False
            output = "{}"
        elif command[:2] in (("claude", "-p"), ("codex", "exec")):
            prompt = command[2] if command[:2] == ("claude", "-p") else kwargs.get("input", "")
            choice = next(
                (skill for marker, skill in self.routing_choices.items() if marker in prompt),
                None,
            )
            if choice and "--json-schema" in command:
                output = json.dumps({"structured_output": {"chosen_skill": choice}})
            elif choice and "--output-schema" in command:
                output = json.dumps(
                    {
                        "type": "item.completed",
                        "item": {
                            "type": "agent_message",
                            "text": json.dumps({"chosen_skill": choice}),
                        },
                    }
                )
            else:
                output = "shaft-requirements-analysis | Load no adjacent skill preemptively."
        else:
            output = "{}"
        stderr = self.warning if "validate" in command else ""
        return subprocess.CompletedProcess(command, 0, stdout=output, stderr=stderr)


if __name__ == "__main__":
    unittest.main()
