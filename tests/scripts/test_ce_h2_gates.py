"""PR-H2 (#6411 #6412 #6413 #6414): decision-quality gate, dod, latency budgets, usage table."""

from __future__ import annotations

import copy
import json
import os
import subprocess  # nosec B404 - fixed argv only.
import sys
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
FIXTURES = ROOT / "tests/fixtures/ce_dod"
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(CE))

from scripts.ci import decision_quality_gate as dq  # noqa: E402
import dod  # noqa: E402
import session_token_usage as stu  # noqa: E402


class DecisionQualityGateTest(unittest.TestCase):
    def setUp(self):
        self.parity = json.loads((CE / "evals/parity-fixtures.json").read_text(encoding="utf-8"))
        self.aggregate = json.loads(
            (CE / "decision-quality-calibration.aggregate.json").read_text(encoding="utf-8")
        )
        self.baseline = json.loads(dq.BASELINE.read_text(encoding="utf-8"))

    def test_committed_tree_meets_baseline(self):
        labels = json.loads(os.environ.get("PR_LABELS") or "[]")
        self.assertEqual(0, dq.verdict(dq.score(self.parity, self.aggregate), self.baseline, labels))

    def test_lowering_a_fixture_expected_outcome_fails(self):
        lowered = copy.deepcopy(self.parity)
        deny = next(f for f in lowered["fixtures"] if f["expect"].get("decision") == "deny")
        deny["expect"] = {"exit_code": 0, "decision": "allow"}
        found = dq.regressions(dq.score(lowered, self.aggregate), self.baseline)
        self.assertTrue(any("protective_fixtures" in item for item in found), found)

    def test_lower_correctness_beyond_tolerance_fails(self):
        worse = copy.deepcopy(self.aggregate)
        worse["metrics"]["chaos-engine"]["correctness"] = -1.0
        self.assertTrue(dq.regressions(dq.score(self.parity, worse), self.baseline))

    def test_override_label_skips_the_gate(self):
        lowered = copy.deepcopy(self.parity)
        lowered["fixtures"] = []
        self.assertEqual(0, dq.verdict(dq.score(lowered, self.aggregate), self.baseline, [dq.OVERRIDE_LABEL]))
        self.assertEqual(1, dq.verdict(dq.score(lowered, self.aggregate), self.baseline, []))

    def test_override_label_is_documented(self):
        text = (CE / "decision-quality-calibration.md").read_text(encoding="utf-8")
        self.assertIn(dq.OVERRIDE_LABEL, text)


class DodTest(unittest.TestCase):
    def load(self, name):
        return json.loads((FIXTURES / name).read_text(encoding="utf-8"))

    def test_complete_pr_passes(self):
        rows = dod.evaluate(self.load("pr_done.json"))
        self.assertTrue(all(row[1] == "ok" for row in rows), rows)

    def test_missing_release_note_label_exits_one_and_names_gap(self):
        data = self.load("pr_done.json")
        data["pr"]["labels"] = []
        rows = dod.evaluate(data)
        self.assertIn(("release-note label", "gap"), [(r[0], r[1]) for r in rows])
        self.assertEqual(1, dod.exit_code(rows))
        self.assertIn("release-note label", dod.render(rows))

    def test_source_without_tests_and_red_checks_and_no_issue_are_gaps(self):
        rows = dict((r[0], r[1]) for r in dod.evaluate(self.load("pr_gaps.json")))
        self.assertEqual("gap", rows["linked issue"])
        self.assertEqual("gap", rows["tests changed"])
        self.assertEqual("gap", rows["required checks"])

    def test_kanban_skill_links_the_command(self):
        self.assertIn("tool.py dod", (CE / "skills/kanban/SKILL.md").read_text(encoding="utf-8"))


class LatencyBudgetTest(unittest.TestCase):
    """Budgets are documented in chaos-engine/INSTALL.md."""

    BUDGETS = {"entry": 1.0, "retrieve": 5.0, "doctor": 10.0}

    def elapsed(self, argv):
        started = time.monotonic()
        subprocess.run(argv, cwd=ROOT, capture_output=True, timeout=60, check=False)  # nosec B603
        return time.monotonic() - started

    def assert_within(self, name, argv):
        spent = self.elapsed(argv)
        self.assertLess(spent, self.BUDGETS[name], f"{name} took {spent:.2f}s")

    def test_entry_budget(self):
        self.assert_within("entry", [sys.executable, str(CE / "tool.py"), "entry"])

    def test_retrieve_budget(self):
        with tempfile.TemporaryDirectory() as project:
            self.assert_within(
                "retrieve",
                [sys.executable, str(CE / "retrieve.py"), "--store", "graphify", "--project", project, "q"],
            )

    def test_doctor_budget(self):
        with tempfile.TemporaryDirectory() as project:
            self.assert_within(
                "doctor", [sys.executable, str(CE / "install.py"), "doctor", "--project", project, "--json"]
            )

    def test_injected_delay_breaks_the_budget(self):
        with self.assertRaises(AssertionError):
            self.BUDGETS["delay"] = 0.2
            self.assert_within("delay", [sys.executable, "-c", "import time; time.sleep(0.5)"])

    def test_budgets_are_documented(self):
        text = (CE / "INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("Latency budgets", text)


class UsageTableTest(unittest.TestCase):
    def test_fixture_prints_markdown_table(self):
        out = subprocess.run(  # nosec B603
            [sys.executable, str(CE / "tool.py"), "usage", "--fixture", str(FIXTURES / "usage")],
            capture_output=True, text=True, check=False, cwd=ROOT,
        )
        self.assertEqual(0, out.returncode, out.stderr)
        self.assertIn("| Task | Tokens in | Tokens out | Est. cost (USD) |", out.stdout)
        self.assertIn("| fix-6411 | 1500 | 500 | 0.0120 |", out.stdout)

    def test_json_passthrough(self):
        rows = json.loads(stu.usage_json(FIXTURES / "usage"))
        self.assertEqual("fix-6411", rows[0]["task"])

    def test_core_card_mentions_usage(self):
        self.assertIn("tool.py usage", (CE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
