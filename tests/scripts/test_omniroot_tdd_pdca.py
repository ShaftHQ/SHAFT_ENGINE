import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class OmniRootTddPdcaTest(unittest.TestCase):
    def test_docs_link_workflow_and_phase_boundary(self):
        paths = (
            "chaos-engine/references/tdd.md",
            "chaos-engine/profiles/shaft/references/playbooks/agentic-pdca-loop.md",
        )
        statement = (
            "Focused RED-GREEN-REFACTOR runs occur during PDCA Do; consolidated "
            "Check begins only after the implementation batch and final scope commit."
        )
        for path in paths:
            content = (ROOT / path).read_text(encoding="utf-8")
            self.assertIn("execution-workflows.md", content)
            self.assertIn(statement, " ".join(content.split()))


if __name__ == "__main__":
    unittest.main()
