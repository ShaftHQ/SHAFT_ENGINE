import re
import unittest
from pathlib import Path


SKILL = Path(__file__).resolve().parents[2] / "shaft-skills" / "writing-shaft-tests" / "SKILL.md"


class WritingShaftTestsContentTest(unittest.TestCase):
    def test_report_failures_and_assertion_apis_have_distinct_exception_contracts(self):
        content = SKILL.read_text(encoding="utf-8")

        self.assertRegex(
            content,
            re.compile(
                r"- `Actions\.report\(\)` failure paths throw `RuntimeException`;\s*"
                r"only validation/accessibility APIs throw `AssertionError`\."
            ),
        )


if __name__ == "__main__":
    unittest.main()
