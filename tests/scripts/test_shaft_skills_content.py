import unittest
from pathlib import Path


PLAYBOOK = (Path(__file__).resolve().parents[2] / "shaft-skills"
            / "shaft-automated-test-authoring" / "references" / "playbook.md")


class AutomatedTestAuthoringContentTest(unittest.TestCase):
    def test_authoring_is_grounded_guarded_and_free_of_raw_driver_shortcuts(self):
        content = PLAYBOOK.read_text(encoding="utf-8")

        self.assertIn("Ground unfamiliar SHAFT syntax with `shaft_guide_search`", content)
        self.assertIn("never add `Thread.sleep`, raw driver calls", content)
        self.assertIn("Run `test_code_guardrails_check`", content)


if __name__ == "__main__":
    unittest.main()
