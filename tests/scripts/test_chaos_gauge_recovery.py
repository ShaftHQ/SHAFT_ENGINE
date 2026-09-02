"""Regression guard for restoration of merged ChaosGauge source."""

import hashlib
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]
CHAOS_GAUGE = ROOT / "scripts" / "ci" / "chaos_gauge"

# `e7d329bd3b` is the recovery-source baseline; deliberate public corpus
# additions update this complete inventory together with their task digests.
# This covers the full public ChaosGauge tree, unlike representative anchors
# that can remain after an executable or dataset member is lost.
RECOVERED_PUBLIC_FILE_COUNT = 156
RECOVERED_PUBLIC_PATHS_SHA256 = (
    "94ae0e6906f7b51072dfda1ab868ade10020c420e4899d38e969538ac1d0d05c"
)
GENERATED_PUBLIC_PARTS = frozenset({"jobs", "reports", "private", "__pycache__"})


def public_recovery_inventory() -> tuple[str, ...]:
    return tuple(
        sorted(
            path.relative_to(CHAOS_GAUGE).as_posix()
            for path in CHAOS_GAUGE.rglob("*")
            if path.is_file()
            and GENERATED_PUBLIC_PARTS.isdisjoint(path.relative_to(CHAOS_GAUGE).parts)
        )
    )


def inventory_sha256(paths: tuple[str, ...]) -> str:
    return hashlib.sha256("\n".join(paths).encode("utf-8")).hexdigest()


class ChaosGaugeRecoveryTest(unittest.TestCase):
    def test_recovery_restores_the_complete_public_contract(self):
        inventory = public_recovery_inventory()

        self.assertEqual(RECOVERED_PUBLIC_FILE_COUNT, len(inventory))
        self.assertEqual(RECOVERED_PUBLIC_PATHS_SHA256, inventory_sha256(inventory))

    def test_pr_gate_runs_recovery_guard_for_chaos_gauge_changes(self):
        workflow = (ROOT / ".github" / "workflows" / "pr-gate.yml").read_text(
            encoding="utf-8"
        )

        self.assertIn("chaos_gauge:", workflow)
        self.assertIn("'scripts/ci/chaos_gauge/**'", workflow)
        self.assertIn("'tests/scripts/test_chaos_gauge_*.py'", workflow)
        self.assertIn("needs.changes.outputs.chaos_gauge == 'true'", workflow)
        self.assertIn("tests.scripts.test_chaos_gauge_canary", workflow)
        self.assertIn("tests.scripts.test_chaos_gauge_campaign", workflow)
        self.assertIn("tests.scripts.test_chaos_gauge_recovery", workflow)
        scheduled = (
            ROOT / ".github" / "workflows" / "agent-plugin-acceptance.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_gauge_recovery", scheduled)


if __name__ == "__main__":
    unittest.main()
