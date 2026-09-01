"""Excluded ChaosGauge canary contract (#5462)."""

from __future__ import annotations

import copy
import importlib
import importlib.util
import json
import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts" / "ci" / "chaos_gauge"
SPEC = importlib.util.spec_from_file_location("chaos_gauge_canary", GAUGE / "canary.py")
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("ChaosGauge canary module is unavailable")
CANARY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CANARY)
MANIFEST = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))


class CanaryContractTest(unittest.TestCase):
    @staticmethod
    def _merged_git(revision: str):
        def run(command: list[str]) -> str:
            if "rev-parse" in command:
                return revision + "\n"
            if "rev-list" in command:
                return f"{revision} parent-one parent-two\n"
            if "merge-base" in command:
                return ""
            raise AssertionError(command)

        return run

    def test_canary_is_one_excluded_public_two_arm_pair(self) -> None:
        planned = CANARY.plan(MANIFEST)

        self.assertEqual("canary", planned["campaign"])
        self.assertTrue(planned["excludedFromPilot"])
        self.assertEqual(2, planned["trials"])
        self.assertEqual(0, planned["pair"]["attempt"])
        self.assertEqual("diagnosis-config-precedence", planned["pair"]["task"])
        self.assertEqual({"control", "chaos-engine"}, set(planned["pair"]["arms"]))

        config = CANARY.job_config(MANIFEST)
        self.assertTrue(config["job_name"].startswith("chaos-gauge-canary-"))
        self.assertEqual(1, config["n_attempts"])
        self.assertEqual(2, config["n_concurrent_trials"])
        self.assertEqual(1, len(config["datasets"]))
        self.assertEqual([planned["pair"]["task"]], config["datasets"][0]["task_names"])
        self.assertEqual(
            ["codex" if arm == "control" else None for arm in planned["pair"]["arms"]],
            [agent.get("name") for agent in config["agents"]],
        )

    def test_receipt_requires_pinned_telemetry_isolation_and_cleanup(self) -> None:
        planned = CANARY.plan(MANIFEST)
        config = CANARY.job_config(MANIFEST)
        result = {
            "trial_results": [
                {
                    "task_name": planned["pair"]["task"],
                    "task_checksum": planned["pair"]["sha256"],
                    "trial_name": f"{planned['pair']['task']}__{'ctrl001' if arm == 'control' else 'chaos01'}",
                    "agent_info": {
                        "name": "codex", "version": "0.118.0",
                        "model_info": {"name": "gpt-5.6-terra", "provider": "openai"},
                    },
                    "config": {"agent": copy.deepcopy(config["agents"][position])},
                    "agent_result": {"n_input_tokens": 10, "n_output_tokens": 20, "cost_usd": 0.01},
                    "agent_execution": {
                        "started_at": "2026-08-31T00:00:00+00:00",
                        "finished_at": "2026-08-31T00:00:01+00:00",
                    },
                    "verifier_environment_mode": "separate",
                    "verifier_result": {"rewards": {"correctness": 1.0, "safety": 1.0, "cleanup": 1.0}},
                }
                for position, arm in enumerate(planned["pair"]["arms"])
            ]
        }
        native_bindings = {
            arm: result["trial_results"][position]["trial_name"]
            for position, arm in enumerate(planned["pair"]["arms"])
        }
        merged_git = self._merged_git("f" * 40)
        receipt = CANARY.receipt(
            MANIFEST, planned, result, public_source_revision="f" * 40,
            native_bindings=native_bindings, repository=ROOT, run=merged_git,
        )

        self.assertTrue(receipt["excludedFromPilot"])
        self.assertEqual(2, receipt["trialAccounting"]["observed"])
        self.assertNotIn("trial_results", json.dumps(receipt, sort_keys=True))
        CANARY.validate_public_evidence(receipt, repository=ROOT, run=merged_git)

        missing_tokens = copy.deepcopy(result)
        del missing_tokens["trial_results"][0]["agent_result"]["n_input_tokens"]
        with self.assertRaisesRegex(ValueError, "token telemetry"):
            CANARY.receipt(
                MANIFEST, planned, missing_tokens, public_source_revision="f" * 40,
                native_bindings=native_bindings, repository=ROOT, run=merged_git,
            )

        unsafe = copy.deepcopy(result)
        unsafe["trial_results"][0]["verifier_result"]["rewards"]["cleanup"] = 0.0
        with self.assertRaisesRegex(ValueError, "cleanup"):
            CANARY.receipt(
                MANIFEST, planned, unsafe, public_source_revision="f" * 40,
                native_bindings=native_bindings, repository=ROOT, run=merged_git,
            )

        leaked = copy.deepcopy(receipt)
        leaked["rawTrajectory"] = "forbidden"
        with self.assertRaisesRegex(ValueError, "public canary evidence"):
            CANARY.validate_public_evidence(leaked, repository=ROOT, run=merged_git)

        leaked = copy.deepcopy(receipt)
        leaked["privatePackage"]["repository"] = "sk-private-value"
        with self.assertRaisesRegex(ValueError, "public canary evidence"):
            CANARY.validate_public_evidence(leaked, repository=ROOT, run=merged_git)

    def test_receipt_binds_prepared_native_name_and_agent_to_its_arm(self) -> None:
        planned = CANARY.plan(MANIFEST)
        config = CANARY.job_config(MANIFEST)
        result = {"trial_results": []}
        for position, arm in enumerate(planned["pair"]["arms"]):
            result["trial_results"].append({
                "task_name": planned["pair"]["task"], "task_checksum": planned["pair"]["sha256"],
                "trial_name": f"{planned['pair']['task']}__{'ctrl001' if arm == 'control' else 'chaos01'}",
                "config": {"agent": copy.deepcopy(config["agents"][position])},
                "agent_info": {"name": "codex", "version": "0.118.0", "model_info": {"name": "gpt-5.6-terra", "provider": "openai"}},
                "agent_result": {"n_input_tokens": 10, "n_output_tokens": 20, "cost_usd": 0.01},
                "agent_execution": {"started_at": "2026-08-31T00:00:00+00:00", "finished_at": "2026-08-31T00:00:01+00:00"},
                "verifier_environment_mode": "separate",
                "verifier_result": {"rewards": {"correctness": 1.0, "safety": 1.0, "cleanup": 1.0}},
            })
        bindings = {arm: result["trial_results"][index]["trial_name"] for index, arm in enumerate(planned["pair"]["arms"])}
        kwargs = {"native_bindings": bindings, "repository": ROOT, "run": self._merged_git("f" * 40)}

        swapped = copy.deepcopy(result)
        swapped["trial_results"][1]["config"]["agent"] = copy.deepcopy(swapped["trial_results"][0]["config"]["agent"])
        with self.assertRaisesRegex(ValueError, "arm identity"):
            CANARY.receipt(MANIFEST, planned, swapped, public_source_revision="f" * 40, **kwargs)

        unmerged = lambda command: "" if "rev-parse" in command else (_ for _ in ()).throw(subprocess.CalledProcessError(1, command))
        with self.assertRaisesRegex(ValueError, "source revision"):
            CANARY.receipt(
                MANIFEST, planned, result, public_source_revision="f" * 40,
                native_bindings=bindings, repository=ROOT, run=unmerged,
            )

    def test_agent_import_contract_rejects_missing_or_escaped_repository_root(self) -> None:
        with TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(ValueError, "repository root"):
                CANARY._custom_agent_module_spec(root / "missing")

            escaped = root / "escaped-agent.py"
            escaped.write_text("class ChaosEngineCodex: pass\n", encoding="utf-8")
            candidate = root / "repository" / "scripts" / "ci" / "chaos_gauge"
            candidate.mkdir(parents=True)
            os.symlink(escaped, candidate / "agent.py")
            with self.assertRaisesRegex(ValueError, "agent module"):
                CANARY._custom_agent_module_spec(root / "repository")

    def test_direct_canary_cli_binds_harbor_agent_from_any_cwd(self) -> None:
        """Direct script execution must bind the configured import before provider preflight."""
        command = [sys.executable, str(GAUGE / "canary.py"), "--verify-agent-import"]
        environment = {
            key: value for key, value in os.environ.items()
            if key not in {"PYTHONPATH", "OPENAI_API_KEY", "HARBOR_API_KEY"}
        }
        with TemporaryDirectory() as directory:
            for cwd in (ROOT, Path(directory)):
                with self.subTest(cwd=cwd):
                    result = subprocess.run(  # nosec B603 B607 - fixed local regression invocation.
                        command, cwd=cwd, env=environment, capture_output=True, text=True
                    )
                    if importlib.util.find_spec("harbor"):
                        self.assertEqual(0, result.returncode, result.stderr)
                    else:
                        self.assertNotEqual(0, result.returncode)
                        self.assertIn("canary agent module is unavailable", result.stderr)
                    self.assertNotIn("No module named 'scripts'", result.stderr)


if __name__ == "__main__":
    unittest.main()
