"""Scheduled ChaosEngine promotion evaluator contracts (#5301)."""

from __future__ import annotations

import importlib.util
import json
import os
import sys
import tempfile
from pathlib import Path
from unittest import TestCase, main, mock


ROOT = Path(__file__).resolve().parents[2]
PROMOTION = ROOT / "scripts/ci/chaos_engine_promotion.py"
PROMOTION_TRIALS = ROOT / "scripts/ci/chaos_engine_promotion_trials.py"
SPEC = importlib.util.spec_from_file_location("chaos_engine_promotion", PROMOTION)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("promotion evaluator could not be loaded")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)
TRIAL_SPEC = importlib.util.spec_from_file_location(
    "chaos_engine_promotion_trials", PROMOTION_TRIALS
)
if TRIAL_SPEC is None or TRIAL_SPEC.loader is None:
    raise RuntimeError("promotion trial runner could not be loaded")
TRIAL_MODULE = importlib.util.module_from_spec(TRIAL_SPEC)
TRIAL_SPEC.loader.exec_module(TRIAL_MODULE)


def receipt(host: str, scenario: str, trial: int, variant: str) -> dict[str, object]:
    candidate = variant == "candidate"
    return {
        "schemaVersion": 1,
        "host": host,
        "scenario": scenario,
        "trial": trial,
        "variant": variant,
        "client": host,
        "clientVersion": "pinned-client-1",
        "revision": ("b" if candidate else "a") * 40,
        "driverSha256": ("d" if candidate else "c") * 64,
        "commandSha256": ("f" if candidate else "e") * 64,
        "completed": True,
        "safe": True,
        "tokens": 40 if candidate else 100,
        "latencyMs": 70 if candidate else 100,
        "retries": 0,
        "denials": 0,
        "repeatedStates": 0,
        "terminalReason": "Complete",
    }


def complete_matrix() -> list[dict[str, object]]:
    return [
        receipt(host, scenario, trial, variant)
        for host in MODULE.HOSTS
        for scenario in MODULE.SCENARIOS
        for trial in range(1, MODULE.TRIALS + 1)
        for variant in MODULE.VARIANTS
    ]


def credentials() -> dict[str, str]:
    return {
        **{name: "present-but-never-rendered" for name in MODULE.CREDENTIALS.values()},
        MODULE.REVISION_VARIABLES["baseline"]: "a" * 40,
        MODULE.REVISION_VARIABLES["candidate"]: "b" * 40,
    }


class ChaosEnginePromotionTest(TestCase):
    def test_manifest_resolves_the_issue_arithmetic_without_dropping_trials(self):
        manifest = MODULE.case_manifest()

        self.assertEqual(16, len(manifest["scenarios"]))
        self.assertEqual(5, len(manifest["hosts"]))
        self.assertEqual(5, manifest["trialsPerScenario"])
        self.assertEqual(400, manifest["pairedTrials"])
        self.assertEqual(800, manifest["individualRuns"])
        self.assertEqual(160, manifest["issueDeclaredRuns"])
        self.assertIn("omits its five-trial requirement", manifest["arithmeticResolution"])

    def test_complete_matrix_meets_all_host_and_global_thresholds(self):
        report = MODULE.evaluate(complete_matrix(), credentials())

        self.assertEqual(("Promoted", "complete"), (report["status"], report["terminalReason"]))
        self.assertEqual([], report["failures"])
        self.assertRegex(report["receiptSetSha256"], r"^[0-9a-f]{64}$")
        for metrics in [*report["metrics"]["hosts"].values(), report["metrics"]["global"]]:
            self.assertGreaterEqual(metrics["tokenReduction"], 0.5)
            self.assertGreaterEqual(metrics["medianLatencyImprovement"], 0.2)
            self.assertGreaterEqual(metrics["p95LatencyImprovement"], 0.2)

    def test_missing_credentials_block_without_rendering_secret_values(self):
        environment = credentials()
        environment.pop(MODULE.CREDENTIALS["gemini"])
        report = MODULE.evaluate([], environment)
        rendered = json.dumps(report, sort_keys=True)

        self.assertEqual("Blocked", report["status"])
        self.assertEqual(["gemini"], report["missingCredentialHosts"])
        self.assertNotIn("present-but-never-rendered", rendered)

    def test_completion_safety_and_loop_regressions_block_promotion(self):
        records = complete_matrix()
        candidate = next(item for item in records if item["variant"] == "candidate")
        candidate.update(
            completed=False,
            safe=False,
            repeatedStates=1,
            terminalReason="Blocked",
        )

        report = MODULE.evaluate(records, credentials())

        self.assertEqual("Blocked", report["status"])
        self.assertLessEqual(
            {"completion-regression", "safety-regression", "candidate-safety", "loop-or-deadlock"},
            set(report["failures"]),
        )

    def test_receipt_schema_rejects_transcripts_secrets_and_duplicate_runs(self):
        value = receipt("codex", MODULE.SCENARIOS[0], 1, "baseline")
        value["transcript"] = "do not persist"
        with self.assertRaisesRegex(ValueError, "fields"):
            MODULE.validate_receipt(value)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            valid = receipt("codex", MODULE.SCENARIOS[0], 1, "baseline")
            for name in ("one.json", "two.json"):
                (root / name).write_text(json.dumps(valid), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "duplicate"):
                MODULE.load_receipts(root)

    def test_blocked_cli_report_returns_nonzero(self):
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "report.json"
            with mock.patch.object(
                sys,
                "argv",
                ["chaos_engine_promotion.py", "--output", str(output)],
            ), mock.patch.dict(os.environ, {}, clear=True):
                self.assertEqual(1, MODULE.main())
            self.assertEqual("Blocked", json.loads(output.read_text())["status"])

    def test_unbound_python_stub_cannot_pose_as_a_native_driver(self):
        environment = {
            **credentials(),
            TRIAL_MODULE.command_variable("codex", "candidate"): json.dumps(
                [sys.executable, "-c", "print('{}')"]
            ),
        }
        with self.assertRaisesRegex(
            TRIAL_MODULE.TrialCollectionError, "command-invalid"
        ):
            TRIAL_MODULE._driver_spec("codex", "candidate", environment)

    def test_trial_runner_binds_driver_and_revision_and_isolates_credentials(self):
        summary = {
            "completed": True,
            "safe": True,
            "tokens": 10,
            "retries": 0,
            "denials": 0,
            "repeatedStates": 0,
            "terminalReason": "Complete",
        }
        environment = {
            **os.environ,
            **credentials(),
        }
        driver = TRIAL_MODULE.DriverSpec(
            host="codex",
            variant="candidate",
            argv=("codex", "exec"),
            version_argv=("codex", "--version"),
            client_version="codex-cli 1",
            revision="b" * 40,
            driver_sha256="c" * 64,
            command_sha256="d" * 64,
        )
        observed: dict[str, object] = {}

        def bounded(command, request, child_environment, **options):
            payload = json.loads(request)
            observed.update(
                command=command,
                request=payload,
                environment=dict(child_environment),
                options=options,
            )
            return 0, json.dumps({**summary, "binding": payload["binding"]}).encode(), b""

        with mock.patch.object(TRIAL_MODULE, "_run_bounded", side_effect=bounded):
            value = TRIAL_MODULE.run_trial(
                "codex",
                MODULE.SCENARIOS[0],
                1,
                "candidate",
                environment,
                timeout=30,
                spec=driver,
            )

        self.assertEqual(set(MODULE.RECEIPT_FIELDS), set(value))
        self.assertNotIn("transcript", json.dumps(value).casefold())
        self.assertGreaterEqual(value["latencyMs"], 0)
        self.assertEqual("b" * 40, value["revision"])
        self.assertEqual("present-but-never-rendered", observed["environment"]["OPENAI_API_KEY"])
        self.assertNotIn("ANTHROPIC_API_KEY", observed["environment"])
        self.assertNotIn("GITHUB_TOKEN", observed["environment"])

    def test_trial_process_output_is_bounded_while_the_process_runs(self):
        command = (
            sys.executable,
            "-c",
            f"import os; os.write(1, b'x' * {TRIAL_MODULE.MAX_OUTPUT_BYTES + 1})",
        )
        with self.assertRaisesRegex(
            TRIAL_MODULE.TrialCollectionError, "output-oversized"
        ):
            TRIAL_MODULE._run_bounded(
                command,
                b"",
                TRIAL_MODULE._child_environment("codex", credentials(), include_credential=True),
                timeout=30,
                code="test-trial",
            )

    def test_scheduled_workflow_collects_live_receipts_before_enforcing_report(self):
        workflow = (ROOT / ".github/workflows/agent-plugin-acceptance.yml").read_text(
            encoding="utf-8"
        )

        self.assertIn("chaos_engine_promotion_trials.py", workflow)
        self.assertIn("--receipts chaos-engine-promotion-receipts", workflow)
        self.assertIn("continue-on-error: true", workflow)
        self.assertIn("CHAOS_ENGINE_BASELINE_REVISION", workflow)
        self.assertIn("CHAOS_ENGINE_CANDIDATE_REVISION", workflow)


if __name__ == "__main__":
    main()
