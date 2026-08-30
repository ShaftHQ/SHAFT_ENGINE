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

    def test_disposable_account_root_exposes_only_the_installer_python(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            owned_python = root / "python3.14"
            owned_python.write_text("fixture", encoding="utf-8")
            account_root = module.prepare_account_command_root(
                root / "account-commands", {"python3": str(owned_python)}
            )

            alias = account_root / ("python3.cmd" if os.name == "nt" else "python3")
            if os.name == "nt":
                self.assertIn(str(owned_python), alias.read_text(encoding="utf-8"))
            else:
                self.assertTrue(alias.is_symlink())
                self.assertEqual(owned_python, alias.resolve())
            environment = module.wrapper_environment(account_command_root=account_root)

        self.assertEqual(
            os.pathsep.join((str(account_root), os.defpath)), environment["PATH"]
        )

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

    def test_account_doctor_failure_carries_component_statuses_and_command(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".chaos-engine").mkdir()
            healthy = json.dumps({"status": "healthy", "commit": "a" * 40})
            unhealthy = json.dumps({
                "status": "unhealthy", "commit": "a" * 40,
                "kernel": {"status": "healthy"},
                "hosts": {"status": "unhealthy"},
                "dependencies": {
                    "status": "unhealthy",
                    "components": {"memory": {"status": "unhealthy"}},
                },
                "components": {"hooks": {"status": "unhealthy"}},
            })
            with mock.patch.object(module, "run_checked", side_effect=(
                CompletedProcess([], 0, healthy, ""),
                CompletedProcess([], 0, unhealthy, ""),
            )):
                with self.assertRaisesRegex(RuntimeError, "doctor did not report") as raised:
                    module.verify_account_phase(project, "a" * 40, probe_generated=False)

        error = raised.exception
        self.assertEqual("doctor", error.command[2])
        self.assertEqual(
            "unhealthy",
            error.component_statuses["doctor"]["components"]["hooks"]["status"],
        )
        self.assertEqual(
            "unhealthy",
            error.component_statuses["doctor"]["dependencyComponents"]["memory"]["status"],
        )

    def test_wrapper_failure_collects_read_only_doctor_component_details(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath(".chaos-engine").mkdir()
            project.joinpath(".chaos-engine/install.py").write_text("# fixture\n")
            error = RuntimeError("wrapper failed")
            error.command = ["wrapper", "--fixture"]
            healthy = json.dumps({"status": "healthy", "commit": "a" * 40})
            unhealthy = json.dumps({
                "status": "recovery-required", "commit": "a" * 40,
                "kernel": {"status": "healthy"},
                "hosts": {"status": "recovery-required"},
                "dependencies": {
                    "status": "recovery-required",
                    "components": {
                        "memory": {
                            "status": "unhealthy", "action": "repaired", "probe": "exit-1",
                        },
                    },
                },
                "components": {
                    "hooks": {"status": "unhealthy", "detail": "missing-managed-hook"},
                },
            })
            isolated_environment = {"PATH": "isolated-account-tools"}
            with mock.patch.object(
                module, "wrapper_environment", return_value=isolated_environment
            ), mock.patch.object(
                module, "run_public_wrapper", side_effect=error
            ) as wrapper, mock.patch.object(
                module, "run_checked", side_effect=(
                    CompletedProcess([], 0, healthy, ""),
                    CompletedProcess([], 0, unhealthy, ""),
                )
            ) as runner:
                with self.assertRaisesRegex(RuntimeError, "wrapper failed") as raised:
                    module.run_public_wrapper_with_diagnostics("a" * 40, project)

        self.assertIs(error, raised.exception)
        self.assertIs(isolated_environment, wrapper.call_args.kwargs["environment"])
        for call in runner.call_args_list:
            self.assertIs(isolated_environment, call.kwargs["environment"])
        doctor = error.component_statuses["doctor"]
        self.assertEqual("missing-managed-hook", doctor["components"]["hooks"]["detail"])
        self.assertEqual("repaired", doctor["dependencyComponents"]["memory"]["action"])
        self.assertEqual("exit-1", doctor["dependencyComponents"]["memory"]["probe"])

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

    def test_failure_evidence_records_sanitized_phase_command_and_components(self):
        module = load_acceptance()
        self.assertIsNotNone(module, "live installer acceptance runner is missing")
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "evidence.json"
            leaked = str(Path(temporary) / "consumer")

            def failing_acceptance(_source, evidence, **_kwargs):
                error = RuntimeError(
                    f"doctor rejected {leaked} https://user:secret@example.invalid"
                )
                error.command = [
                    sys.executable, f"{leaked}/.chaos-engine/install.py", "doctor",
                    "--project", leaked, "--json",
                ]
                error.component_statuses = {
                    "doctor": {
                        "status": "unhealthy",
                        "hosts": "unhealthy",
                        "dependencies": "healthy",
                        "components": {
                            "hooks": {
                                "status": "unhealthy", "detail": "missing-managed-hook",
                            },
                            "mcps": {"status": "unhealthy"},
                        },
                        "dependencyComponents": {
                            "memory": {
                                "status": "healthy", "action": "reused", "probe": "passed",
                            },
                        },
                    }
                }

                def fail():
                    raise error

                module.record_phase(evidence, "preseeded-base-wrapper", fail)

            with mock.patch.object(module, "run_acceptance", side_effect=failing_acceptance):
                exit_code = module.main([
                    "--candidate-sha", "a" * 40,
                    "--base-sha", module.KNOWN_BASE_SHA,
                    "--output", str(output),
                ])
            evidence = json.loads(output.read_text(encoding="utf-8"))

        self.assertEqual(1, exit_code)
        self.assertEqual("preseeded-base-wrapper", evidence["failure"]["phase"])
        self.assertEqual("doctor", evidence["failure"]["command"][2])
        self.assertEqual(
            "unhealthy", evidence["failure"]["componentStatuses"]["doctor"]["hosts"]
        )
        self.assertEqual(
            "missing-managed-hook",
            evidence["failure"]["componentStatuses"]["doctor"]["components"]["hooks"]["detail"],
        )
        self.assertEqual(
            "reused",
            evidence["failure"]["componentStatuses"]["doctor"]["dependencyComponents"]["memory"]["action"],
        )
        self.assertEqual("fail", evidence["phases"][0]["status"])
        serialized = json.dumps(evidence)
        self.assertNotIn(leaked, serialized)
        self.assertNotIn("secret", serialized)

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

    def test_isolated_account_environment_redirects_every_account_root(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve() / "isolated account"
            environment = module.isolated_account_environment(root)

        required = {
            "HOME", "USERPROFILE", "HOMEDRIVE", "HOMEPATH", "APPDATA",
            "LOCALAPPDATA", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
            "XDG_STATE_HOME", "NPM_CONFIG_CACHE", "NPM_CONFIG_PREFIX",
            "NPM_CONFIG_USERCONFIG", "UV_CACHE_DIR", "UV_TOOL_DIR",
            "UV_TOOL_BIN_DIR", "UV_PYTHON_INSTALL_DIR", "UV_PYTHON_DIR",
        }
        self.assertTrue(required <= set(environment))
        for name in required:
            with self.subTest(name=name):
                if name in {"HOMEDRIVE", "HOMEPATH"} and os.name == "nt":
                    continue
                self.assertTrue(Path(environment[name]).is_relative_to(root), environment[name])
        if os.name == "nt":
            self.assertTrue(
                Path(environment["HOMEDRIVE"] + environment["HOMEPATH"]).is_relative_to(root)
            )

    def test_isolated_account_command_check_rejects_optional_outside_root(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve()
            owned = root / "bin/mempalace"
            owned.parent.mkdir()
            owned.write_text("fixture", encoding="utf-8")
            module.assert_account_command_roots(
                {"mempalace": str(owned), "python3": sys.executable}, root
            )
            with self.assertRaisesRegex(RuntimeError, "outside isolated account"):
                module.assert_account_command_roots(
                    {"mempalace": sys.executable}, root
                )
            with self.assertRaisesRegex(RuntimeError, "unknown account command"):
                module.assert_account_command_roots(
                    {"unexpected": str(owned)}, root
                )

    def test_only_exact_platform_base_failure_enters_compatibility_transition(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        base = "1dec809c7c43709a8fcceef5e53690d124012eb3"
        posix = module.AcceptanceCommandFailure(
            module.public_wrapper_command(base, windows=False),
            1,
            "CE-INSTALL-FAILED: ChaosEngine doctor did not report a healthy installation; "
            "failed phase: Verify installation; unhealthy: hooks, mcps",
        )
        posix.component_statuses = module.known_base_component_statuses(base)

        self.assertEqual(
            "posix", module.exact_base_compatibility_transition(posix, base, windows=False)
        )
        for mutation in (
            ("sha", "f" * 40),
            ("command", ["wrong-wrapper"]),
            ("detail", "different failure"),
            ("extra-component", None),
            ("missing-component", None),
        ):
            with self.subTest(mutation=mutation[0]):
                candidate = module.AcceptanceCommandFailure(
                    list(posix.command), posix.returncode, posix.args[0].split(": ", 1)[1]
                )
                candidate.component_statuses = json.loads(json.dumps(posix.component_statuses))
                base_sha = base
                if mutation[0] == "sha":
                    base_sha = mutation[1]
                elif mutation[0] == "command":
                    candidate.command = tuple(mutation[1])
                elif mutation[0] == "detail":
                    candidate = module.AcceptanceCommandFailure(
                        list(posix.command), 1, mutation[1]
                    )
                    candidate.component_statuses = json.loads(json.dumps(posix.component_statuses))
                elif mutation[0] == "extra-component":
                    candidate.component_statuses["doctor"]["components"]["extra"] = {
                        "status": "healthy"
                    }
                else:
                    del candidate.component_statuses["doctor"]["components"]["memory"]
                with self.assertRaises(module.AcceptanceCommandFailure) as raised:
                    module.exact_base_compatibility_transition(
                        candidate, base_sha, windows=False
                    )
                self.assertIs(candidate, raised.exception)

        windows = module.AcceptanceCommandFailure(
            module.public_wrapper_command(base, windows=True),
            1,
            "CE-INSTALL-FAILED: dependency verification failed: memory, context7; "
            "failed phase: Provision dependencies; unhealthy: not reported",
        )
        self.assertEqual(
            "windows", module.exact_base_compatibility_transition(windows, base, windows=True)
        )
        windows.component_statuses = {"status": {}}
        with self.assertRaises(module.AcceptanceCommandFailure):
            module.exact_base_compatibility_transition(windows, base, windows=True)

        alternate_url = module.AcceptanceCommandFailure(
            list(posix.command), 1, module.POSIX_BASE_FAILURE_DETAIL
        )
        alternate_url.command = tuple(
            part.replace(module.raw_wrapper_url(base, windows=False), "https://example.invalid/install.sh")
            for part in alternate_url.command
        )
        alternate_url.component_statuses = module.known_base_component_statuses(base)
        with self.assertRaises(module.AcceptanceCommandFailure):
            module.exact_base_compatibility_transition(alternate_url, base, windows=False)

        for mutation in (
            lambda shape: shape["status"].update({"unexpected": "field"}),
            lambda shape: shape["status"].pop("commit"),
            lambda shape: shape["status"]["dependencyComponents"]["uv"].update({"action": "reused"}),
        ):
            candidate = module.AcceptanceCommandFailure(
                list(posix.command), 1, module.POSIX_BASE_FAILURE_DETAIL
            )
            candidate.component_statuses = module.known_base_component_statuses(base)
            mutation(candidate.component_statuses)
            with self.assertRaises(module.AcceptanceCommandFailure):
                module.exact_base_compatibility_transition(candidate, base, windows=False)

        with self.assertRaises(module.AcceptanceCommandFailure):
            module.exact_base_compatibility_transition(posix, base, windows=True)

    def test_account_phase_requires_project_and_generated_mcp_handshakes(self):
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
            ), mock.patch.object(module, "probe_project_mcps") as project_probe, mock.patch.object(
                module, "probe_generated_mcps"
            ) as generated_probe:
                module.verify_account_phase(project, "a" * 40)

        project_probe.assert_called_once_with(
            project / ".chaos-engine/tool.py", project, base_environment=mock.ANY
        )
        generated_probe.assert_called_once()

    def test_account_phase_can_explicitly_omit_both_mcp_probe_families(self):
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
            ), mock.patch.object(module, "probe_project_mcps") as project_probe, mock.patch.object(
                module, "probe_generated_mcps"
            ) as generated_probe:
                module.verify_account_phase(project, "a" * 40, probe_generated=False)

        project_probe.assert_not_called()
        generated_probe.assert_not_called()

    def test_windows_base_reconstruction_uses_discovered_account_commands(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            project = root / "project"
            project.mkdir()
            account = root / "account"
            owned = account / "bin/mempalace"
            owned.parent.mkdir(parents=True)
            owned.write_text("fixture", encoding="utf-8")
            base_source = root / "base-source"
            base_source.mkdir()
            installer = mock.Mock()
            installer.detect_distribution.return_value = "repository"
            installer.install.return_value = project / ".chaos-engine"
            installer.verify_install.return_value = {"capabilityPolicySha256": "digest"}
            dependencies = mock.Mock()
            dependencies.load_specification.return_value = {"fixture": True}
            discovered = {"python3": sys.executable, "mempalace": str(owned)}
            dependencies.discover_account_commands.return_value = ({}, discovered)
            hosts = mock.Mock()
            with mock.patch.object(
                module, "download_commit_source", return_value=base_source
            ), mock.patch.object(
                module, "load_source_controller", side_effect=(installer, dependencies, hosts)
            ):
                module.reconstruct_windows_base(
                    project, root / "candidate", module.KNOWN_BASE_SHA, account
                )

        installer.install.assert_called_once_with(
            project,
            base_source,
            module.KNOWN_BASE_SHA,
            source_record={
                "kind": "git", "repository": "shafthq/shaft_engine",
                "branch": module.KNOWN_BASE_SHA, "commit": module.KNOWN_BASE_SHA,
            },
            distribution="repository",
        )
        self.assertEqual(discovered, hosts.install.call_args.kwargs["account_commands"])
        hosts.verify.assert_called_once_with(project, core_commit=module.KNOWN_BASE_SHA)

    def test_main_rejects_an_unrecognized_base_commit_before_running_acceptance(self):
        module = load_acceptance()
        self.assertIsNotNone(module)
        if module is None:
            return
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "evidence.json"
            with mock.patch.object(module, "run_acceptance") as acceptance:
                result = module.main([
                    "--candidate-sha", "a" * 40, "--base-sha", "b" * 40,
                    "--output", str(output),
                ])
            evidence = json.loads(output.read_text(encoding="utf-8"))

        self.assertEqual(1, result)
        acceptance.assert_not_called()
        self.assertEqual("RuntimeError", evidence["failure"]["type"])

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
        rollback_body = source.split("def rollback_base()", 1)[1].split(
            'record_phase(evidence, "rollback-base-account-and-hosts"', 1
        )[0]
        self.assertNotIn('"--json"', rollback_body)

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
