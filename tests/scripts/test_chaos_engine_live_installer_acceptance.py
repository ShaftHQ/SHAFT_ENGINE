"""Contracts for scheduled real ChaosEngine installer acceptance (#5299)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from subprocess import CompletedProcess
from unittest import mock

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


class ChaosEngineLiveInstallerAcceptanceTest(unittest.TestCase):
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
        for node in module.ast.walk(
            module.ast.parse(SCRIPT.read_text(encoding="utf-8"))
        ):
            if isinstance(node, module.ast.Import):
                imported.update(alias.name.split(".", 1)[0] for alias in node.names)
            elif isinstance(node, module.ast.ImportFrom) and node.module:
                imported.add(node.module.split(".", 1)[0])
        self.assertTrue(imported <= sys.stdlib_module_names, imported - sys.stdlib_module_names)

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
        uploads = [step for step in job["steps"] if step.get("uses") == "actions/upload-artifact@v7"]
        self.assertEqual(1, len(uploads))
        self.assertEqual("always()", uploads[0]["if"])
        self.assertEqual(30, uploads[0]["with"]["retention-days"])
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
    unittest.main()
