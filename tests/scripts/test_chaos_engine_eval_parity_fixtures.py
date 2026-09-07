"""Eval / parity fixture suite across hosts (#5584)."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

from scripts.ci.chaos_engine_eval_parity import (
    evaluate_suite,
    load_fixtures,
    validate_fixtures,
)

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "chaos-engine/references/eval-parity-fixtures.md"
CORPUS = ROOT / "chaos-engine/evals/parity-fixtures.json"
MATRIX = ROOT / "chaos-engine/references/host-parity-matrix.md"
CATALOG = ROOT / "chaos-engine/references/zero-llm-catalog.md"


class EvalParityFixturesTests(unittest.TestCase):
    def test_corpus_schema_and_five_hosts(self):
        document = load_fixtures(CORPUS)
        defects = validate_fixtures(document)
        self.assertEqual([], defects, msg=defects)
        self.assertEqual(1, document["schema_version"])
        self.assertGreaterEqual(len(document["fixtures"]), 4)
        ids = {row["id"] for row in document["fixtures"]}
        for required in (
            "deny-catastrophic-rm-rf-root",
            "allow-read-tool",
            "sessionstart-locator-budget",
            "research-before-mutation-enforced",
            "research-triage-public-contract-hard",
            "research-triage-one-file-soft",
            "sessionstart-no-memory-prose",
            "learning-session-portable-finalize-allow",
        ):
            self.assertIn(required, ids)

    def test_documented_runner_and_ratchet_guidance(self):
        text = DOC.read_text(encoding="utf-8")
        self.assertIn("scripts/ci/chaos_engine_eval_parity.py", text)
        self.assertIn("Failure ratchet", text)
        self.assertIn("ratchet: hooks", text)
        self.assertIn("ratchet: skills", text)
        self.assertIn("ratchet: matrix", text)
        self.assertIn("ethical-conduct", text)
        catalog = CATALOG.read_text(encoding="utf-8")
        self.assertIn("chaos_engine_eval_parity.py", catalog)
        matrix = MATRIX.read_text(encoding="utf-8")
        self.assertIn("eval-parity-fixtures", matrix)

    def test_suite_passes_under_simulated_hook_runners(self):
        report = evaluate_suite(load_fixtures(CORPUS))
        self.assertEqual([], report["defects"], msg=report["defects"])
        failed = [row for row in report["results"] if not row["passed"]]
        self.assertEqual(
            [],
            failed,
            msg=json.dumps(failed, indent=2),
        )
        self.assertEqual(1.0, report["case_pass_rate"])
        self.assertTrue(report["passed"])

    def test_suite_covers_all_five_hosts_per_fixture(self):
        report = evaluate_suite(load_fixtures(CORPUS))
        for row in report["results"]:
            hosts = {item["host"] for item in row["hosts"]}
            self.assertEqual(
                {"claude", "codex", "gemini", "grok", "copilot"},
                hosts,
                msg=row["id"],
            )


if __name__ == "__main__":
    unittest.main()
