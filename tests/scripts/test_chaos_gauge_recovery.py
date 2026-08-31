"""Regression guard for restoration of merged ChaosGauge source."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]
CHAOS_GAUGE = ROOT / "scripts" / "ci" / "chaos_gauge"


class ChaosGaugeRecoveryTest(unittest.TestCase):
    def test_recovery_restores_the_complete_public_contract(self):
        required_paths = (
            "experiment.json",
            "validate_experiment.py",
            "compare_results.py",
            "corpus.json",
            "dataset/dataset.toml",
            "dataset/metric.py",
            "job-configs/control.yaml",
            "job-configs/chaos-engine.yaml",
        )

        for relative_path in required_paths:
            self.assertTrue(
                (CHAOS_GAUGE / relative_path).is_file(),
                f"missing restored ChaosGauge contract: {relative_path}",
            )


if __name__ == "__main__":
    unittest.main()
