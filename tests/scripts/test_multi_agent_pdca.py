import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REFS = ROOT / "chaos-engine/references"
DOC = REFS / "multi-agent-pdca.md"


class MultiAgentPdcaTest(unittest.TestCase):
    """#7294: the multi-agent PDCA rule stays present, capped, guarded, and linked."""

    def test_doc_states_loop_cap_and_guard(self):
        text = DOC.read_text(encoding="utf-8")
        for needle in (
            "adversarial self-Check",
            "Reviewer",
            "logical or visual issue",
            "Both sign-offs",
            "final Done",
            "5 rounds without convergence",
            "destructive, external-send, or out-of-scope",
            "lessons pass and a parity",
        ):
            self.assertIn(needle, text)

    def test_linked_from_entry_and_orchestrator(self):
        for name in ("bot-entry.md", "orchestrator-follow-through.md"):
            self.assertIn("(multi-agent-pdca.md)", (REFS / name).read_text(encoding="utf-8"))

    def test_core_doc_has_no_ecosystem_terms(self):
        text = DOC.read_text(encoding="utf-8").lower()
        for term in ("shaft", "grok", "selenium", "maven"):
            self.assertNotIn(term, text)


if __name__ == "__main__":
    unittest.main()
