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
        contents = {
            path: (ROOT / path).read_text(encoding="utf-8") for path in paths
        }
        for content in contents.values():
            self.assertIn("execution-workflows.md", content)
        normalized = {path: " ".join(content.split()) for path, content in contents.items()}
        self.assertIn(statement, normalized[paths[0]])
        self.assertEqual(1, sum(content.count(statement) for content in normalized.values()))
        self.assertIn(
            "../../../../references/tdd.md#workflow",
            contents[paths[1]],
        )


if __name__ == "__main__":
    unittest.main()
