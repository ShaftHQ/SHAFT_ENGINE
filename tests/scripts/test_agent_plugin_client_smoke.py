"""Native Claude/Codex compatibility evidence tests (#4641)."""

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

try:
    from scripts.ci.agent_plugin_client_smoke import (
        CLIENTS,
        EVIDENCE_LEVELS,
        LOAD_PROMPT,
        LOAD_PROOF_TERMS,
        collect_evidence,
    )
except ImportError:
    CLIENTS = None
    EVIDENCE_LEVELS = None
    LOAD_PROMPT = None
    LOAD_PROOF_TERMS = None
    collect_evidence = None


class AgentPluginClientSmokeTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.package_root = Path(self.temporary_directory.name) / "shaft-skills"
        self.package_root.mkdir()
        self.runner = FakeRunner()

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

        self.assertIn(("claude", "--version"), commands)
        self.assertIn(("codex", "--version"), commands)
        self.assertIn(
            ("claude", "plugin", "validate", "--strict", str(self.package_root)),
            commands,
        )
        self.assertIn(
            ("claude", "plugin", "marketplace", "add", str(self.package_root), "--scope", "project"),
            commands,
        )
        self.assertIn(("claude", "plugin", "list", "--available", "--json"), commands)
        self.assertIn(
            ("claude", "plugin", "install", "shaft-skills@shaft-skills", "--scope", "project"),
            commands,
        )
        self.assertIn(
            ("codex", "plugin", "marketplace", "add", str(self.package_root), "--json"),
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
        secret = "DUMMY_SECRET_SENTINEL"
        self.runner.warning = f"Warning: context token {secret}"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": secret, "OPENAI_API_KEY": secret},
        )

        serialized = json.dumps(evidence)
        self.assertNotIn(secret, serialized)
        self.assertIn("[REDACTED]", serialized)

    def test_short_runtime_credentials_are_also_redacted(self):
        secret = "short"
        self.runner.warning = f"Warning: context token {secret}"

        evidence = collect_evidence(
            self.package_root,
            mode="live",
            runner=self.runner,
            environ={"ANTHROPIC_API_KEY": secret, "OPENAI_API_KEY": secret},
        )

        self.assertNotIn(secret, json.dumps(evidence))

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
        pr_gate = (root / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        live = (root / ".github/workflows/agent-plugin-acceptance.yml").read_text(encoding="utf-8")

        for heading in ("Package validation", "Marketplace discovery", "Install / enable", "Real load"):
            self.assertIn(heading, compatibility)
        self.assertIn("Claude Code 2.1.223", compatibility)
        self.assertIn("Codex CLI 0.146.0", compatibility)
        self.assertIn("tests.scripts.test_agent_plugin_client_smoke", pr_gate)
        self.assertIn("python scripts/ci/agent_plugin_client_smoke.py --mode smoke", pr_gate)
        self.assertIn("schedule:", live)
        self.assertIn("workflow_dispatch:", live)
        self.assertIn("ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}", live)
        self.assertIn("OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}", live)
        before_live_step, live_step = live.split("      - name: Collect live compatibility evidence", 1)
        self.assertNotIn("ANTHROPIC_API_KEY", before_live_step)
        self.assertNotIn("OPENAI_API_KEY", before_live_step)
        self.assertIn("        env:\n          ANTHROPIC_API_KEY", live_step)
        self.assertIn("          OPENAI_API_KEY", live_step)
        self.assertIn("if: always()", live)
        self.assertIn("actions/upload-artifact@v7", live)
        for client in CLIENTS.values():
            self.assertIn(client["npm_package"], pr_gate)
            self.assertIn(client["npm_package"], live)


class FakeRunner:
    def __init__(self):
        self.commands = []
        self.environments = []
        self.fail_when = lambda _command: False
        self.raise_when = lambda _command: False
        self.partial_fail_when = lambda _command: False
        self.failure_message = "simulated failure"
        self.warning = ""
        self.installed_plugin = {"id": "shaft-skills@shaft-skills", "enabled": True}
        self.available_plugin = {"name": "shaft-skills"}
        self.versions = {"claude": "claude 2.1.223", "codex": "codex-cli 0.146.0"}
        self.preexisting = False
        self.marketplace_added = {"claude": False, "codex": False}
        self.installed = {"claude": False, "codex": False}

    def __call__(self, command, **kwargs):
        command = tuple(str(part) for part in command)
        self.commands.append(command)
        self.environments.append((command, kwargs.get("env", {})))
        client = command[0]
        if self.raise_when(command):
            raise FileNotFoundError("simulated client not found")
        if self.fail_when(command):
            return subprocess.CompletedProcess(command, 1, stdout="", stderr=self.failure_message)
        if command[-1:] == ("--version",):
            return subprocess.CompletedProcess(command, 0, stdout=self.versions[client], stderr="")
        if command[1:4] == ("plugin", "marketplace", "list"):
            entries = [{"name": "shaft-skills"}] if self.preexisting or self.marketplace_added[client] else []
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
            output = "shaft-requirements-analysis | Load no adjacent skill preemptively."
        else:
            output = "{}"
        stderr = self.warning if "validate" in command else ""
        return subprocess.CompletedProcess(command, 0, stdout=output, stderr=stderr)


if __name__ == "__main__":
    unittest.main()
