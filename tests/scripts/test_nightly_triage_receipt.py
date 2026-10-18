"""A fresh nightly receipt skips a repeat triage that cannot change the decision."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agents/nightly_triage_receipt.py"
sys.path.insert(0, str(ROOT))

from scripts.agents.nightly_triage_receipt import freshness  # noqa: E402

RUNS = {"live": 39110443294, "local": 39100748568, "e2e": 39100562649}
RECEIPT = {
    "run_ids": RUNS,
    "decision": "NO_PRODUCT_FIX",
    "next_cron": "2026-10-19T01:00:00Z",
}


class NightlyTriageReceiptTest(unittest.TestCase):
    def test_matching_ids_before_cron_skip(self):
        self.assertEqual("SKIP", freshness(RECEIPT, RUNS, "2026-10-18T18:08:10Z"))

    def test_changed_run_id_triages(self):
        current = dict(RUNS)
        current["local"] = 999
        self.assertEqual("TRIAGE", freshness(RECEIPT, current, "2026-10-18T18:08:10Z"))

    def test_clock_at_cron_triages(self):
        self.assertEqual("TRIAGE", freshness(RECEIPT, RUNS, "2026-10-19T01:00:00Z"))

    def test_missing_receipt_triages(self):
        self.assertEqual("TRIAGE", freshness(None, RUNS, "2026-10-18T18:08:10Z"))

    def test_cli_prints_skip(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            receipt = root / "receipt.json"
            runs = root / "runs.json"
            receipt.write_text(json.dumps(RECEIPT), encoding="utf-8")
            runs.write_text(json.dumps({"run_ids": RUNS}), encoding="utf-8")
            completed = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--receipt",
                    str(receipt),
                    "--runs",
                    str(runs),
                    "--now",
                    "2026-10-18T18:08:10Z",
                ],
                check=True,
                capture_output=True,
                text=True,
            )
        self.assertEqual("SKIP\n", completed.stdout)


class WasteNoteTest(unittest.TestCase):
    def test_session_countermeasures_are_written(self):
        refs = ROOT / "chaos-engine/references"
        waste = (refs / "eliminate-waste.md").read_text(encoding="utf-8")
        economy = (refs / "ci-status-economy.md").read_text(encoding="utf-8")
        playbook = (refs / "work-github-playbook.md").read_text(encoding="utf-8")
        self.assertIn("nightly_triage_receipt.py", playbook)
        self.assertIn("fresh SKIP", playbook)
        self.assertIn("codecov/patch", economy)
        self.assertIn("end the turn", waste)
        self.assertIn("400 lines", waste)
