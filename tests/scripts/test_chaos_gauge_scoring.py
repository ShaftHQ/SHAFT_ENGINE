"""ChaosGauge Harbor metrics and paired statistical inference (#5461)."""

from __future__ import annotations

import csv
import json
import runpy
import tempfile
import types
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import TestCase, main


ROOT = Path(__file__).resolve().parents[2]
GAUGE = ROOT / "scripts/ci/chaos_gauge"
MANIFEST = json.loads((GAUGE / "experiment.json").read_text(encoding="utf-8"))


METRIC = types.SimpleNamespace(**runpy.run_path(str(GAUGE / "dataset/metric.py")))
REPORTER = types.SimpleNamespace(**runpy.run_path(str(GAUGE / "compare_results.py")))


def compare(control, candidate, **kwargs):
    private_tasks = [task for task in MANIFEST["tasks"] if task["visibility"] == "private-reference"]
    resolution = {
        "name": MANIFEST["privatePackage"]["name"],
        "ref": MANIFEST["privatePackage"]["ref"],
        "tasks": [{"name": task["name"], "sha256": task["sha256"]} for task in private_tasks],
    }
    return REPORTER.compare(
        MANIFEST, control, candidate, campaign="full-pilot",
        private_resolution=resolution, **kwargs,
    )


def job(
    arm: str,
    *,
    successes: int = 5,
    tokens: int | None = 100,
    seconds: int = 10,
    cost: float | None = 0.10,
    unsafe: bool = False,
    visibility: set[str] | None = None,
) -> dict[str, object]:
    trials = []
    start = datetime(2026, 8, 28, tzinfo=timezone.utc)
    selected_tasks = MANIFEST["tasks"] if visibility is None else [
        task for task in MANIFEST["tasks"] if task["visibility"] in visibility
    ]
    for task in selected_tasks:
        for attempt in range(1, 6):
            agent_result = {
                "n_input_tokens": None if tokens is None else tokens - 10,
                "n_output_tokens": None if tokens is None else 10,
                "cost_usd": cost,
            }
            trials.append(
                {
                    "task_name": task["name"],
                    "trial_name": f'{task["name"]}__{attempt}',
                    "task_checksum": task["sha256"],
                    "agent_info": {
                        "name": "codex",
                        "version": "0.152.0",
                        "model_info": {"name": "gpt-5.6-terra", "provider": "openai"},
                    },
                    "agent_result": agent_result,
                    "verifier_result": {
                        "rewards": {
                            "correctness": int(attempt <= successes),
                            "safety": int(not (unsafe and attempt == 1 and task == MANIFEST["tasks"][0])),
                            "cleanup": 1,
                        }
                    },
                    "verifier_environment_mode": "separate",
                    "exception_info": None,
                    "agent_execution": {
                        "started_at": start.isoformat(),
                        "finished_at": (start + timedelta(seconds=seconds)).isoformat(),
                    },
                }
            )
    return {
        "stats": {"n_retries": 2 if arm == "chaos-engine" else 1},
        "trial_results": trials,
    }


class ChaosGaugeScoringTest(TestCase):
    def test_campaign_selection_requires_exact_public_or_resolved_full_matrix(self):
        calibration = REPORTER.compare(
            MANIFEST,
            job("control", visibility={"public"}),
            job("chaos-engine", visibility={"public"}),
            campaign="calibration",
        )
        self.assertEqual("calibration", calibration["campaign"])
        self.assertEqual(60, calibration["arms"]["control"]["sampleSize"])
        with self.assertRaisesRegex(ValueError, "private package is unresolved"):
            REPORTER.compare(
                MANIFEST, job("control"), job("chaos-engine"), campaign="full-pilot"
            )

    def test_native_metric_keeps_reward_dimensions_separate(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "rewards.jsonl"
            output = root / "metrics.json"
            source.write_text(
                '\n'.join([
                    '{"correctness": 1, "safety": 1, "cleanup": 1}',
                    '{"correctness": 0, "safety": 1, "cleanup": 1}',
                ]) + '\n',
                encoding="utf-8",
            )

            METRIC.aggregate(source, output)

            self.assertEqual(
                {"correctness": 0.5, "safety": 1.0, "cleanup": 1.0, "trials": 2},
                json.loads(output.read_text(encoding="utf-8")),
            )

    def test_native_metric_rejects_missing_or_non_numeric_rewards(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for payload in ('{"correctness": 1, "safety": 1}\n', '{"correctness": true, "safety": 1, "cleanup": 1}\n', 'null\n'):
                (root / "input.jsonl").write_text(payload, encoding="utf-8")
                with self.subTest(payload=payload), self.assertRaises(ValueError):
                    METRIC.aggregate(root / "input.jsonl", root / "output.json")

    def test_clear_candidate_win_has_components_and_paired_interval(self):
        report = compare(
            job("control", successes=3, tokens=150, seconds=15, cost=0.15),
            job("chaos-engine", successes=5, tokens=100, seconds=10, cost=0.10),
        )

        self.assertEqual("winner", report["verdict"]["state"])
        self.assertEqual("chaos-engine", report["verdict"]["winner"])
        self.assertGreater(report["confidenceInterval95"]["lower"], 0)
        self.assertEqual(10000, report["bootstrapIterations"])
        self.assertEqual(0.6, report["arms"]["control"]["effectiveness"])
        self.assertEqual(1.0, report["arms"]["chaos-engine"]["effectiveness"])
        self.assertEqual("reported", report["arms"]["control"]["tokenProvenance"])
        self.assertEqual({"control": 1, "chaos-engine": 2}, report["retries"])

    def test_tie_missing_tokens_safety_and_zero_success_fail_closed(self):
        tie = compare(job("control"), job("chaos-engine"))
        self.assertEqual("no significant difference", tie["verdict"]["state"])

        missing = compare(
            job("control", tokens=None), job("chaos-engine", tokens=None)
        )
        self.assertEqual("insufficient evidence", missing["verdict"]["state"])
        self.assertIsNone(missing["arms"]["control"]["efficiency"])
        self.assertIsNone(missing["arms"]["control"]["overallScore"])
        self.assertEqual("unavailable", missing["arms"]["control"]["tokenProvenance"])

        unsafe = compare(job("control"), job("chaos-engine", unsafe=True))
        self.assertEqual("ineligible", unsafe["verdict"]["state"])

        zero = compare(
            job("control", successes=0), job("chaos-engine", successes=0)
        )
        self.assertEqual(0.0, zero["arms"]["control"]["effectiveness"])
        self.assertEqual(0.0, zero["arms"]["control"]["overallScore"])

    def test_mismatch_and_unexplained_missing_trial_are_rejected(self):
        candidate = job("chaos-engine")
        candidate["trial_results"][0]["agent_info"]["model_info"]["name"] = "different"
        with self.assertRaisesRegex(ValueError, "model"):
            compare(job("control"), candidate)

        candidate = job("chaos-engine")
        candidate["trial_results"].pop()
        with self.assertRaisesRegex(ValueError, "trial matrix"):
            compare(job("control"), candidate)

        candidate = job("chaos-engine")
        candidate["trial_results"][0]["agent_info"]["version"] = "different"
        with self.assertRaisesRegex(ValueError, "agent version"):
            compare(job("control"), candidate)

    def test_task_resampling_recomputes_efficiency_from_selected_tasks(self):
        candidate = job("chaos-engine")
        first_task = MANIFEST["tasks"][0]["name"]
        second_task = MANIFEST["tasks"][1]["name"]
        for trial in candidate["trial_results"]:
            if trial["task_name"] == first_task:
                trial["agent_result"]["n_input_tokens"] = 990
        private_tasks = [task for task in MANIFEST["tasks"] if task["visibility"] == "private-reference"]
        resolution = {"name": MANIFEST["privatePackage"]["name"], "ref": MANIFEST["privatePackage"]["ref"], "tasks": [{"name": task["name"], "sha256": task["sha256"]} for task in private_tasks]}
        arms, tasks, attempts = REPORTER._experiment(MANIFEST, "full-pilot", resolution)
        records, _ = REPORTER._records(
            candidate, "chaos-engine", arms["chaos-engine"], tasks, attempts, []
        )

        first = REPORTER._base_metrics(records, [first_task])
        second = REPORTER._base_metrics(records, [second_task])

        self.assertEqual(1000.0, first["tokensPerSuccess"])
        self.assertEqual(100.0, second["tokensPerSuccess"])

    def test_explicit_exclusion_is_reported_and_exports_are_stable(self):
        control = job("control", successes=3, tokens=150)
        candidate = job("chaos-engine")
        exclusion = {
            "trialName": control["trial_results"][0]["trial_name"],
            "reasonCode": "provider-outage",
            "provenance": "provider incident receipt 123",
        }
        for selected_job in (control, candidate):
            selected_job["trial_results"][0]["exception_info"] = {"type": "ProviderError"}
            selected_job["trial_results"][0]["verifier_result"] = None
        report = compare(control, candidate, exclusions=[exclusion])

        self.assertEqual([exclusion], report["exclusions"])
        self.assertEqual(
            {"planned": 80, "excluded": 1, "analyzed": 79},
            report["pairAccounting"],
        )
        self.assertEqual(1.0, report["arms"]["control"]["reliability"])
        self.assertEqual(1.0, report["arms"]["chaos-engine"]["reliability"])
        self.assertEqual(79, report["arms"]["control"]["sampleSize"])
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary)
            REPORTER.write_reports(report, output)
            first = {path.name: path.read_bytes() for path in output.iterdir()}
            REPORTER.write_reports(report, output)
            second = {path.name: path.read_bytes() for path in output.iterdir()}
            self.assertEqual(first, second)
            self.assertEqual(
                {"comparison.json", "comparison.csv", "comparison.md"}, set(first)
            )
            with (output / "comparison.csv").open(encoding="utf-8") as stream:
                rows = list(csv.DictReader(stream))
            self.assertIn("effectiveness", {row["metric"] for row in rows})
            self.assertIn("Verdict", (output / "comparison.md").read_text(encoding="utf-8"))

    def test_exclusion_cannot_hide_safety_failure(self):
        candidate = job("chaos-engine", unsafe=True)
        unsafe_trial = candidate["trial_results"][0]
        exclusion = {
            "trialName": unsafe_trial["trial_name"],
            "reasonCode": "harbor-infrastructure",
            "provenance": "Harbor verifier receipt 123",
        }

        report = compare(
            job("control"), candidate, exclusions=[exclusion]
        )

        self.assertEqual("ineligible", report["verdict"]["state"])
        self.assertFalse(report["arms"]["chaos-engine"]["safetyEligible"])

    def test_exception_without_verifier_counts_as_unreliable(self):
        candidate = job("chaos-engine")
        failed = candidate["trial_results"][0]
        failed["exception_info"] = {"type": "AgentTimeoutError"}
        failed["verifier_result"] = None

        report = compare(job("control"), candidate)

        self.assertLess(report["arms"]["chaos-engine"]["reliability"], 1.0)
        self.assertEqual(79, report["arms"]["chaos-engine"]["sampleSize"])
        self.assertEqual("insufficient evidence", report["verdict"]["state"])


if __name__ == "__main__":
    main()
