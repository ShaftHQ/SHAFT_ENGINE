import unittest

class TestOrchestratorFollowThrough(unittest.TestCase):
    def test_cadence_and_duties(self):
        path = "chaos-engine/references/orchestrator-follow-through.md"
        with open(path, encoding="utf-8") as f:
            content = f.read()

        self.assertIn("5 minutes", content)
        self.assertIn("10 minutes", content)
        self.assertIn("15 minutes", content)
        self.assertIn("Scrum-master", content)

if __name__ == '__main__':
    unittest.main()
