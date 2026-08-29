"""Contracts for scheduled real ChaosEngine installer acceptance (#5299)."""

from __future__ import annotations

import ast
import importlib.util
import json
import os
import sys
import tempfile
from pathlib import Path, PurePosixPath
from subprocess import CompletedProcess  # nosec B404 - test fixture type only.
from unittest import TestCase, main, mock

import yaml


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/chaos_engine_live_installer_acceptance.py"
WORKFLOW = ROOT / ".github/workflows/agent-plugin-acceptance.yml"
PROTECTED_GATE = ROOT / "scripts/ci/harness_pr_gate.py"


def load_acceptance():
    if not SCRIPT.is_file():
        return None
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_live_installer_acceptance", SCRIPT
    )
    if specification is None or specification.loader is None:
        return None
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class ChaosEngineLiveInstallerAcceptanceTest(TestCase):
    def test_wrapper_failure_keeps_installer_phase_and_component(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        diagnostic = "\n".join((
            "long progress output",
            "CE-INSTALL-FAILED: ChaosEngine doctor did not report a healthy installation",
            "https://github.com/ShaftHQ/SHAFT_ENGINE/issues/new?"
            "failed_phase=Verify+installation&unhealthy=hooks&cause=doctor+failed",
            "PowerShell invocation error",
        ))

        self.assertEqual(
            "CE-INSTALL-FAILED: ChaosEngine doctor did not report a healthy installation; "
            "failed phase: Verify installation; unhealthy: hooks",
            module.installer_failure_detail(diagnostic),
        )

    def test_managed_python_version_comes_from_installed_contract(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            installed = Path(temporary) / ".chaos-engine"
            installed.mkdir()
            installed.joinpath("dependencies.json").write_text(
                json.dumps({"runtimes": {"python": {"version": "3.11"}}}),
                encoding="utf-8",
            )

            self.assertEqual("3.11", module.managed_python_version(installed))

    def test_runner_contract_is_bounded_and_standard_library_only(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        self.assertEqual(
            module.TOOLS,
            ("uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp"),
        )
        self.assertGreater(module.PHASE_TIMEOUT_SECONDS, 0)
        self.assertLessEqual(module.PHASE_TIMEOUT_SECONDS, 900)
        imported = set()
        for node in ast.walk(ast.parse(SCRIPT.read_text(encoding="utf-8"))):
            if isinstance(node, ast.Import):
                imported.update(alias.name.split(".", 1)[0] for alias in node.names)
            elif isinstance(node, ast.ImportFrom) and node.module:
                imported.add(node.module.split(".", 1)[0])
        self.assertTrue(imported <= sys.stdlib_module_names, imported - sys.stdlib_module_names)

    def test_mcp_probe_requires_successful_initialize_response(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        process = mock.Mock()
        process.communicate.return_value = (
            '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18",'
            '"capabilities":{"tools":{}},"serverInfo":{"name":"fixture",'
            '"version":"1"}}}\n'
            '{"jsonrpc":"2.0","id":2,"result":{"tools":[]}}\n',
            "",
        )
        process.returncode = 0

        module.probe_mcp(["fixture-mcp"], ROOT, popen=lambda *_args, **_kwargs: process)

        requests = [
            json.loads(line) for line in process.communicate.call_args.args[0].splitlines()
        ]
        self.assertEqual("initialize", requests[0]["method"])
        self.assertEqual(1, requests[0]["id"])
        self.assertEqual("notifications/initialized", requests[1]["method"])
        self.assertEqual("tools/list", requests[2]["method"])
        self.assertEqual(2, requests[2]["id"])

    def test_mcp_probe_rejects_closed_initialize_handshake(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        process = mock.Mock()
        process.communicate.return_value = ("", "server exited")
        process.returncode = 1

        with self.assertRaisesRegex(RuntimeError, "closed during initialize"):
            module.probe_mcp(
                ["fixture-mcp"], ROOT, popen=lambda *_args, **_kwargs: process
            )

    def test_mcp_probe_rejects_incomplete_initialize_result(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        process = mock.Mock()
        process.communicate.return_value = (
            '{"jsonrpc":"2.0","id":1,"result":{"serverInfo":{}}}\n',
            "",
        )
        process.returncode = 0

        with self.assertRaisesRegex(RuntimeError, "initialize failed"):
            module.probe_mcp(
                ["fixture-mcp"], ROOT, popen=lambda *_args, **_kwargs: process
            )

    def test_mcp_probe_rejects_wrong_protocol_and_boolean_id(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        responses = (
            '{"jsonrpc":"2.0","id":true,"result":{"protocolVersion":"2025-06-18",'
            '"capabilities":{},"serverInfo":{"name":"fixture","version":"1"}}}\n',
            '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"garbage",'
            '"capabilities":{},"serverInfo":{"name":"fixture","version":"1"}}}\n',
        )
        for response in responses:
            with self.subTest(response=response):
                process = mock.Mock()
                process.communicate.return_value = (response, "")
                process.returncode = 0
                with self.assertRaisesRegex(RuntimeError, "initialize failed"):
                    module.probe_mcp(
                        ["fixture-mcp"],
                        ROOT,
                        popen=lambda *_args, **_kwargs: process,
                    )

    def test_project_mcp_probe_covers_memory_and_mempalace(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        tool = ROOT / ".chaos-engine/tool.py"
        with mock.patch.object(module, "probe_mcp") as probe:
            module.probe_project_mcps(tool, ROOT)

        commands = {call.args[0][2] for call in probe.call_args_list}
        self.assertEqual({"memory-mcp", "mempalace-mcp"}, commands)

    def test_project_mcp_probe_never_supplies_mempalace_storage_arguments(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        tool = ROOT / ".chaos-engine/tool.py"
        with mock.patch.object(module, "probe_mcp") as probe:
            module.probe_project_mcps(tool, ROOT)

        mempalace = next(
            call.args[0] for call in probe.call_args_list if call.args[0][2] == "mempalace-mcp"
        )
        self.assertEqual([sys.executable, str(tool), "mempalace-mcp"], mempalace)

    def test_generated_mcp_commands_use_platform_fields_with_common_fallback(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".mcp.json").write_text(json.dumps({
                "mcpServers": {
                    "chaosengine-memory": {
                        "command": "memory-mcp", "args": [], "cwd": ".",
                    },
                    "chaosengine-mempalace": {
                        "command": "python3", "args": [".chaos-engine/tool.py", "mempalace-mcp"],
                        "commandWindows": "py",
                        "argsWindows": ["-3", ".chaos-engine/tool.py", "mempalace-mcp"],
                        "cwd": ".", "env": {"MEMPALACE_BACKEND": "sqlite_exact"},
                    },
                },
            }), encoding="utf-8")

            commands = {
                name: command
                for name, command, _cwd, _environment in module.generated_mcp_commands(
                    project, windows=True
                )
            }

        self.assertEqual(["memory-mcp"], commands["chaosengine-memory"])
        self.assertEqual(
            ["py", "-3", ".chaos-engine/tool.py", "mempalace-mcp"],
            commands["chaosengine-mempalace"],
        )

    def test_offline_environment_blocks_package_network_and_hides_secrets(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        environment = module.offline_environment(
            {
                "PATH": "trusted-tools",
                "OPENAI_API_KEY": "secret",
                "HTTP_PROXY": "http://remote.invalid",
            },
            block_path=True,
        )
        self.assertEqual("", environment["PATH"])
        self.assertEqual("1", environment["PIP_NO_INDEX"])
        self.assertEqual("true", environment["NPM_CONFIG_OFFLINE"])
        self.assertEqual("http://127.0.0.1:9", environment["HTTPS_PROXY"])
        self.assertNotIn("OPENAI_API_KEY", environment)

    def test_default_child_environment_never_receives_runner_secrets(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        with mock.patch.dict(os.environ, {"OPENAI_API_KEY": "secret"}), mock.patch.object(
            module.subprocess,
            "run",
            return_value=CompletedProcess(["probe"], 0, stdout="", stderr=""),
        ) as runner:
            module.run_checked(["probe"], cwd=ROOT)
        child_environment = runner.call_args.kwargs["env"]
        self.assertIsInstance(child_environment, dict)
        if isinstance(child_environment, dict):
            self.assertNotIn("OPENAI_API_KEY", child_environment)

    def test_download_environment_allows_only_scoped_github_token(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        allowed = os.pathsep.join(("scoped", "fixture"))
        blocked = os.pathsep.join(("blocked", "fixture"))
        environment = module.download_environment(
            {
                "PATH": "trusted-tools",
                "GITHUB_TOKEN": allowed,
                "OPENAI_API_KEY": blocked,
                "PRIVATE_KEY": blocked,
                "UNRELATED_SECRET": blocked,
            }
        )
        self.assertEqual("trusted-tools", environment["PATH"])
        self.assertEqual(allowed, environment["GITHUB_TOKEN"])
        self.assertNotIn("OPENAI_API_KEY", environment)
        self.assertNotIn("PRIVATE_KEY", environment)
        self.assertNotIn("UNRELATED_SECRET", environment)

    def test_public_wrapper_child_receives_only_scoped_github_token(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        project = ROOT
        installed = project / ".chaos-engine" / "install.py"
        allowed = os.pathsep.join(("scoped", "fixture"))
        blocked = os.pathsep.join(("blocked", "fixture"))
        with mock.patch.dict(
            os.environ,
            {"GITHUB_TOKEN": allowed, "OPENAI_API_KEY": blocked},
        ), mock.patch.object(module, "run_checked") as runner, mock.patch.object(
            module.Path, "is_file", return_value=True
        ):
            runner.return_value = CompletedProcess(
                ["wrapper"], 0,
                stdout='{"status":"installed","clients":{}}',
                stderr="Installing ChaosEngine\nSTART Resolve source\nElapsed 00:00",
            )
            module.run_public_wrapper("a" * 40, project)
        self.assertTrue(installed.is_absolute())
        child_environment = runner.call_args.kwargs["environment"]
        self.assertEqual(allowed, child_environment["GITHUB_TOKEN"])
        self.assertNotIn("OPENAI_API_KEY", child_environment)
        self.assertEqual(os.defpath, child_environment["PATH"])

    def test_account_verification_uses_the_isolated_wrapper_environment(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".chaos-engine").mkdir()
            project.joinpath(".chaos-engine-dependencies.json").write_text(
                json.dumps({"schemaVersion": 2, "components": {}, "commands": {}}),
                encoding="utf-8",
            )
            healthy = json.dumps({"status": "healthy", "commit": "a" * 40})
            with mock.patch.object(
                module, "run_checked", return_value=CompletedProcess([], 0, healthy, "")
            ) as runner:
                module.verify_account_phase(project, "a" * 40, probe_generated=False)

        for call in runner.call_args_list:
            self.assertEqual(os.defpath, call.kwargs["environment"]["PATH"])

    def test_failure_still_writes_sanitized_json_evidence(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "evidence.json"
            leaked = str(Path(temporary) / "consumer")
            with mock.patch.object(
                module, "run_acceptance", side_effect=RuntimeError(f"failed at {leaked}")
            ):
                exit_code = module.main(["--output", str(output)])
            evidence = json.loads(output.read_text(encoding="utf-8"))
        self.assertEqual(1, exit_code)
        self.assertFalse(evidence["accepted"])
        self.assertNotIn(leaked, json.dumps(evidence))
        self.assertEqual("RuntimeError", evidence["failure"]["type"])

    def test_sanitize_redacts_platform_paths_without_hiding_relative_diagnostics(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        cases = {
            "/var/folders/ab/cd/consumer/state.json": "failed at <path>",
            "/Users/runner/work/project with spaces/state.json": 'failed at "<path>": denied',
            str(PurePosixPath("/", "tmp", "chaos-engine-live", "consumer", "state.json")): "failed at <path>",
            "/private/var/folders/ab/cd/consumer/state.json": "failed at <path>",
            "/home/runner/work/SHAFT_ENGINE/consumer/state.json": "failed at <path>",
            r"C:\Users\runner\work\SHAFT_ENGINE\consumer\state.json": "failed at <path>",
            r"\\server\share\runner\work\SHAFT_ENGINE\consumer\state.json": 'failed at "<path>": denied',
        }
        for leaked, expected in cases.items():
            diagnostic = (
                f'failed at "{leaked}": denied'
                if "spaces" in leaked or leaked.startswith("\\\\")
                else f"failed at {leaked}"
            )
            with self.subTest(path=leaked):
                sanitized = module.sanitize(diagnostic)
                self.assertEqual(expected, sanitized)
                self.assertNotIn(leaked, sanitized)
        relative = "distribution policy rejected forbidden content: hooks/kernel.py"
        self.assertEqual(relative, module.sanitize(relative))

    def test_sanitize_bounds_paths_and_url_credentials_without_losing_suffix(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        diagnostics = {
            "failed at /Users/alice/work/state.json; fallback hooks/kernel.py": (
                "failed at <path>; fallback hooks/kernel.py"
            ),
            "failed at /Users/alice/Library/Application Support/Chaos/state.json; "
            "fallback hooks/kernel.py": (
                "failed at <path>; fallback hooks/kernel.py"
            ),
            "failed at /Users/alice/; fallback hooks/kernel.py": (
                "failed at <path>; fallback hooks/kernel.py"
            ),
            'failed at "/Users/build agent/Library/Application Support/Chaos/state.json", '
            "fallback hooks/kernel.py": (
                'failed at "<path>", fallback hooks/kernel.py'
            ),
            "proxy https://alice:s3cr3t@proxy.example.com:8443/simple failed": (
                "proxy https://<redacted>@proxy.example.com:8443/simple failed"
            ),
            "index http://token@packages.example.test/simple unavailable": (
                "index http://<redacted>@packages.example.test/simple unavailable"
            ),
            "proxy https://o'connor:secret@proxy.example.com/simple failed": (
                "proxy https://<redacted>@proxy.example.com/simple failed"
            ),
            "proxy https://alice:secret@[2001:db8::1]:8443/Users/alice/simple"
            "?next=/tmp/x failed": (
                "proxy https://<redacted>@[2001:db8::1]:8443/Users/alice/simple"
                "?next=/tmp/x failed"
            ),
        }
        for diagnostic, expected in diagnostics.items():
            with self.subTest(diagnostic=diagnostic):
                self.assertEqual(expected, module.sanitize(diagnostic))
        safe = "index https://packages.example.test/simple; fallback hooks/kernel.py"
        self.assertEqual(safe, module.sanitize(safe))

    def test_staged_source_preserves_actual_checked_out_payload(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            source.joinpath("hooks").mkdir(parents=True)
            source.joinpath("hooks/kernel.py").write_text("owned = True\n", encoding="utf-8")
            staged = module.stage_source(source, root / "staged")
            self.assertTrue(staged.joinpath("hooks/kernel.py").is_file())
            self.assertEqual(
                "owned = True\n",
                staged.joinpath("hooks/kernel.py").read_text(encoding="utf-8"),
            )

    def test_candidate_install_uses_public_wrapper_not_install_py(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        sha = "1" * 40
        posix = module.public_wrapper_command(sha, windows=False)
        windows = module.public_wrapper_command(sha, windows=True)
        self.assertIn("install.sh", " ".join(posix))
        self.assertEqual(2, " ".join(posix).count(module.raw_wrapper_url(sha, windows=False)))
        self.assertIn("install.ps1", " ".join(windows))
        self.assertIn("irm", " ".join(windows))
        self.assertNotIn("install.py", " ".join(posix + windows))

    def test_acceptance_source_has_no_ambient_node_or_direct_installer_shortcut(self):
        source = SCRIPT.read_text(encoding="utf-8")
        self.assertNotIn('shutil.which("node")', source)
        self.assertNotIn('shutil.which("npm")', source)
        self.assertNotIn("def install_command(", source)
        self.assertIn("--candidate-sha", source)
        self.assertIn("source_record=manifest['source']", source)
        self.assertIn("offline_environment(block_path=True)", source)

    def test_acceptance_uses_preseeded_base_and_blank_candidate_projects(self):
        source = SCRIPT.read_text(encoding="utf-8")
        self.assertIn('base_project = root / "base consumer with spaces Ω"', source)
        self.assertIn('blank_project = root / "blank consumer with spaces Ω"', source)
        self.assertIn("seed_exact_mempalace(source, base_project)", source)
        self.assertIn("rollback", source)

    def test_weekly_manual_three_os_job_is_bounded_and_uploads_evidence(self):
        workflow = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))
        self.assertIn("schedule", workflow[True])
        self.assertIn("workflow_dispatch", workflow[True])
        self.assertIn("chaos-engine-live-installer", workflow["jobs"])
        job = workflow["jobs"]["chaos-engine-live-installer"]
        self.assertEqual(45, job["timeout-minutes"])
        self.assertFalse(job["strategy"]["fail-fast"])
        self.assertEqual(
            ["ubuntu-22.04", "macos-15", "windows-2025"],
            job["strategy"]["matrix"]["os"],
        )
        commands = "\n".join(str(step.get("run", "")) for step in job["steps"])
        self.assertIn("scripts/ci/chaos_engine_live_installer_acceptance.py", commands)
        self.assertIn("--candidate-sha", commands)
        self.assertIn("--base-sha", commands)
        acceptance = next(
            step
            for step in job["steps"]
            if "chaos_engine_live_installer_acceptance.py" in str(step.get("run", ""))
        )
        self.assertEqual("${{ github.token }}", acceptance["env"]["GITHUB_TOKEN"])
        checkout = next(step for step in job["steps"] if step.get("uses") == "actions/checkout@v7")
        self.assertEqual(2, checkout["with"]["fetch-depth"])
        uploads = [step for step in job["steps"] if step.get("uses") == "actions/upload-artifact@v7"]
        self.assertEqual(1, len(uploads))
        self.assertEqual("always()", uploads[0]["if"])
        self.assertEqual(4, uploads[0]["with"]["retention-days"])
        self.assertEqual("error", uploads[0]["with"]["if-no-files-found"])

    def test_generation_runtime_is_reachable_from_protected_and_scheduled_suites(self):
        protected = PROTECTED_GATE.read_text(encoding="utf-8")
        scheduled = WORKFLOW.read_text(encoding="utf-8")
        for module in (
            "tests.scripts.test_chaos_engine_generation_runtime",
            "tests.scripts.test_chaos_engine_live_installer_acceptance",
        ):
            with self.subTest(module=module):
                self.assertIn(module, protected)
                self.assertIn(module, scheduled)


if __name__ == "__main__":
    main()
