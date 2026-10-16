import os
import unittest
import unittest.mock

from scripts.ci import pr_title_lint as lint


class PrTitleLintTest(unittest.TestCase):
    def test_conventional_titles_pass(self):
        for title in ("ci: pin actions", "fix(validation): report element-not-found", "feat!: drop Java 21", "harness: budget gate"):
            self.assertEqual([], lint.problems(title), title)

    def test_bot_titles_are_exempt(self):
        self.assertEqual([], lint.problems("build(deps): bump x from 1 to 2"))
        self.assertEqual([], lint.problems("Prepare SHAFT Engine release 10.4.20261013"))

    def test_non_conventional_titles_report_a_reason(self):
        self.assertEqual(["title is not `type(scope): subject`"], lint.problems("Keep scroll gestures on screen"))
        self.assertIn("is not one of", lint.problems("wip: something")[0])

    def test_suggestion_lowercases_a_known_prefix(self):
        self.assertEqual("ci: move jobs", lint.suggestion("CI: move jobs"))
        self.assertEqual("fix: Retry a miss", lint.suggestion("Retry a miss"))

    def test_main_is_advisory_and_emits_a_warning(self):
        with unittest.mock.patch.dict(os.environ, {"PR_TITLE": "Keep it"}), unittest.mock.patch("builtins.print") as printed:
            self.assertEqual(0, lint.main())
        self.assertTrue(printed.call_args[0][0].startswith("::warning"))


if __name__ == "__main__":
    unittest.main()
