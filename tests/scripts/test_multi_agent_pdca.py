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
            "names the full head SHA it reviewed",
            "ask for a re-sign-off",
        ):
            self.assertIn(needle, text)

    def test_linked_from_entry_and_orchestrator(self):
        for name in ("bot-entry.md", "orchestrator-follow-through.md"):
            self.assertIn("(multi-agent-pdca.md)", (REFS / name).read_text(encoding="utf-8"))

    def test_core_doc_has_no_ecosystem_terms(self):
        text = DOC.read_text(encoding="utf-8").lower()
        for term in ("shaft", "grok", "selenium", "maven"):
            self.assertNotIn(term, text)


class IterativeFlowRuleTest(unittest.TestCase):
    """#7318: the always-on iterative flow rule is generic, sourced, and loaded."""

    FLOW = REFS / "iterative-flow.md"

    def test_rule_states_wip_done_and_generic_roles(self):
        text = self.FLOW.read_text(encoding="utf-8")
        for needle in (
            "every task", "WIP limit: 1 item", "both sign-offs AND the item is shipped",
            "failure → root cause → preventive rule", "Design owner",
            "Acceptance reviewer", "Engineering advisor", "(multi-agent-pdca.md#loop)",
            "https://kanbanguides.org/english/", "https://deming.org/explore/pdsa/",
        ):
            self.assertIn(needle, text)
        self.assertIn("(iterative-flow.md)", DOC.read_text(encoding="utf-8"))

    def test_rule_has_no_bot_or_org_names(self):
        text = self.FLOW.read_text(encoding="utf-8").lower()
        for term in ("carl", "coach", "chaos does", "mohab", "automatest", "grok", "shaft"):
            self.assertNotIn(term, text)

    def test_validator_requires_entry_to_load_rule(self):
        import shutil
        import tempfile
        from scripts.ci import validate_agent_setup as setup

        self.assertEqual(setup.validate_iterative_flow_rule(ROOT), [])
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            refs = root / "chaos-engine/references"
            refs.mkdir(parents=True)
            self.assertEqual(setup.validate_iterative_flow_rule(root), [])
            shutil.copy(self.FLOW, refs / "iterative-flow.md")
            (refs / "bot-entry.md").write_text("no link\n", encoding="utf-8")
            codes = [e["code"] for e in setup.validate_iterative_flow_rule(root)]
            self.assertEqual(codes, ["iterative-flow-rule"])
            (refs / "bot-entry.md").write_text("[f](iterative-flow.md)\n", encoding="utf-8")
            self.assertEqual(setup.validate_iterative_flow_rule(root), [])


if __name__ == "__main__":
    unittest.main()
