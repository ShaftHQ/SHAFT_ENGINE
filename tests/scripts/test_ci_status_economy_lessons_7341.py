"""#7341: CI status economy holds the release-prep, Error Prone and link lessons."""
import pathlib
import unittest

DOC = pathlib.Path(__file__).resolve().parents[2] / "chaos-engine" / "references" / "ci-status-economy.md"


class CiStatusEconomyLessonsTest(unittest.TestCase):
    def test_three_rules_present(self):
        text = DOC.read_text(encoding="utf-8")
        for phrase in ("close and reopen it as the owner", "mvn -Perrorprone", "add the host to\n  `lychee.toml` `exclude`", "A pass from the primary checkout does not prove the worktree."):
            self.assertIn(phrase, text)
        self.assertLessEqual(len(text.encode("utf-8")), 16384)


if __name__ == "__main__":
    unittest.main()
