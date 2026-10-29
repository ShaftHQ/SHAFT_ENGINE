"""Parallel-streams protocol: ownership, disk gate, live review, no force-push."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REFS = ROOT / "chaos-engine/references"
DOC = REFS / "parallel-streams.md"


class ParallelStreamsProtocolTest(unittest.TestCase):
    def test_doc_states_ownership_disk_review_and_no_force_push(self) -> None:
        text = DOC.read_text(encoding="utf-8")
        for needle in (
            "Stream-ownership manifest",
            "owned path globs",
            "Conflict detection",
            "Brief template",
            "riskiest assumption",
            "live behaviour",
            "no force-push",
            "Never force-push",
            "20 GiB",
            "filter=blob:none",
            "git worktree add",
            "scratch_pending_cleanup",
            "WIP limit 1",
            "delete-check",
        ):
            self.assertIn(needle, text)

    def test_core_doc_has_no_ecosystem_or_person_names(self) -> None:
        text = DOC.read_text(encoding="utf-8").lower()
        for term in (
            "shaft",
            "grok",
            "selenium",
            "maven",
            "mohab",
            "automatest",
            "carl",
            "coach",
        ):
            self.assertNotIn(term, text)

    def test_linked_from_entry_flow_pdca_delegation_cleanup(self) -> None:
        for name in (
            "bot-entry.md",
            "iterative-flow.md",
            "multi-agent-pdca.md",
            "delegation.md",
            "cleanup-scopes.md",
        ):
            body = (REFS / name).read_text(encoding="utf-8")
            self.assertIn("parallel-streams.md", body, msg=name)

    def test_pdca_requires_live_behaviour_evidence(self) -> None:
        text = (REFS / "multi-agent-pdca.md").read_text(encoding="utf-8")
        self.assertIn("live behaviour", text)
        self.assertIn("offline fixtures", text)

    def test_bot_entry_disk_gate_and_per_stream_wip(self) -> None:
        text = (REFS / "bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("20 GiB", text)
        self.assertIn("WIP limit 1\nper stream", text)
        self.assertIn("(parallel-streams.md)", text)

    def test_harness_map_lists_parallel_streams(self) -> None:
        hmap = (REFS / "harness-map.md").read_text(encoding="utf-8")
        self.assertIn("references/parallel-streams.md", hmap)

    def test_cleanup_scopes_lists_scratch_after_ship(self) -> None:
        text = (REFS / "cleanup-scopes.md").read_text(encoding="utf-8")
        self.assertIn("scratch_pending_cleanup", text)
        self.assertIn("verified-pushed", text)


if __name__ == "__main__":
    unittest.main()
