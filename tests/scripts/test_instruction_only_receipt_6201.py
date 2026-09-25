"""Instruction-only hosts: a missing `retrieve:` receipt is flagged, never blocking (#6201)."""

from __future__ import annotations

import importlib.util
import json
import os
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load():
    path = ROOT / "chaos-engine/learning_session.py"
    spec = importlib.util.spec_from_file_location("learning_session_6201", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


SESSION = _load()


class ReceiptFlagTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name)
        previous = Path.cwd()
        os.chdir(self.project)
        self.addCleanup(os.chdir, previous)

    def finalize(self, host):
        return SESSION.finalize("s-6201", extract_heuristics=False, host=host)

    def test_missing_receipt_is_flagged_not_blocking(self):
        receipt = self.finalize("cursor")
        self.assertEqual("issues-first", receipt["disposition"])
        self.assertEqual("missing", receipt["retrieveReceipt"]["status"])
        self.assertIn("missing-retrieve-receipt", receipt["flags"])

    def test_receipt_file_field_satisfies_the_check(self):
        sink = self.project / SESSION.RESEARCH_RECEIPT_SINK
        sink.parent.mkdir(parents=True)
        sink.write_text("Research receipt\n- retrieve: skipped(no-project-index)\n", encoding="utf-8")
        receipt = self.finalize("opencode")
        self.assertEqual("present", receipt["retrieveReceipt"]["status"])
        self.assertEqual("skipped(no-project-index)", receipt["retrieveReceipt"]["field"])
        self.assertNotIn("missing-retrieve-receipt", receipt.get("flags", []))

    def test_retrieve_ledger_counts_as_a_machine_readable_receipt(self):
        ledger = self.project / ".chaos-engine-state/retrieve-justification.json"
        ledger.parent.mkdir(parents=True)
        ledger.write_text(
            json.dumps({"citations": ["a/b.md"], "outcomes": []}), encoding="utf-8"
        )
        receipt = self.finalize("grok-bot")
        self.assertEqual("present", receipt["retrieveReceipt"]["status"])
        self.assertEqual("ledger", receipt["retrieveReceipt"]["source"])

    def test_hook_hosts_are_not_checked_here(self):
        receipt = self.finalize("claude")
        self.assertNotIn("retrieveReceipt", receipt)

    def test_cli_accepts_host(self):
        self.assertEqual(0, SESSION.main(["finalize", "--session-id", "s", "--host", "cursor", "--silent", "--no-heuristics"]))


class ShimEvaluationDocTest(unittest.TestCase):
    def test_reference_names_the_sink_and_the_shim_evaluation(self):
        text = (ROOT / "chaos-engine/references/research-receipt.md").read_text(encoding="utf-8")
        self.assertIn(".chaos-engine-state/research-receipt.md", text)
        self.assertIn("## Instruction-only hosts", text)
        for host in ("OpenCode", "Cursor", "Grok Bot", "Copilot cloud"):
            self.assertIn(host, text)


if __name__ == "__main__":
    unittest.main()
