"""#7341: CI status economy holds the release-prep, Error Prone and link lessons."""
import pathlib
import unittest

DOC = pathlib.Path(__file__).resolve().parents[2] / "chaos-engine" / "references" / "ci-status-economy.md"


class CiStatusEconomyLessonsTest(unittest.TestCase):
    def test_three_rules_present(self):
        text = DOC.read_text(encoding="utf-8")
        for phrase in ("close and reopen it as the owner", "mvn -Perrorprone", "add the host to\n  `lychee.toml` `exclude`"):
            self.assertIn(phrase, text)


if __name__ == "__main__":
    unittest.main()
