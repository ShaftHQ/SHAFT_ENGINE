"""
Always-loaded instruction budget (top-20 #7, #12).

Fails when the bundle a bot host loads every task (entry --full, bot-entry,
AGENTS.md) grows more than tolerance_pct over the recorded baseline, when it
shrinks far below it without lowering the baseline, or when entry output is
not a stable, timestamp-free prefix (prompt-cache friendly).
"""
from __future__ import annotations

import json
import re
import subprocess  # nosec B404 - runs the repository's own tool.py
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
BUDGET = CE / "always-loaded-budget.json"
TIMESTAMP = re.compile(r"\b20\d\d-\d\d-\d\d(?:[T ]\d\d:\d\d)?")


def entry_full() -> str:
    result = subprocess.run([sys.executable, str(CE / "tool.py"), "entry", "--full"],  # nosec B603 - fixed argv
                            cwd=ROOT, capture_output=True, text=True, timeout=60, check=True)
    return result.stdout


def bundle_bytes(entry: str) -> int:
    return (len(entry.encode("utf-8"))
            + len((CE / "references/bot-entry.md").read_bytes())
            + len((ROOT / "AGENTS.md").read_bytes()))


def verdict(current: int, budget: dict) -> str:
    """Return "" when within budget, else the failure reason."""
    base = budget["bytes"]
    if current > base * (1 + budget["tolerance_pct"] / 100):
        return (f"always-loaded bundle {current} B exceeds baseline {base} B "
                f"+{budget['tolerance_pct']}%: move detail to an on-demand reference")
    if current < base * (1 - budget["shrink_ratchet_pct"] / 100):
        return f"bundle shrank to {current} B; lower bytes in {BUDGET.name} to lock the gain"
    return ""


class AlwaysLoadedBudgetTest(unittest.TestCase):
    def setUp(self) -> None:
        self.budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        self.entry = entry_full()

    def test_bundle_is_within_budget(self) -> None:
        self.assertEqual("", verdict(bundle_bytes(self.entry), self.budget))

    def test_entry_is_a_stable_timestamp_free_prefix(self) -> None:
        self.assertEqual(self.entry, entry_full())
        self.assertIsNone(TIMESTAMP.search(self.entry), "entry output must not carry dates")
        self.assertLess(self.entry.index("# ChaosEngine identity"), self.entry.index("## Iron laws"))

    def test_verdict_pass_and_fail_cases(self) -> None:
        budget = {"bytes": 1000, "tolerance_pct": 5, "shrink_ratchet_pct": 10}
        self.assertEqual("", verdict(1050, budget))
        self.assertIn("exceeds baseline", verdict(1051, budget))
        self.assertIn("lower bytes", verdict(899, budget))
        self.assertEqual("", verdict(900, budget))


if __name__ == "__main__":
    unittest.main()
