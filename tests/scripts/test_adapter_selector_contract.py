"""
allure-cucumber8-jvm must not abort a selector aimed at another module.

The module redistributes a jar and has no test sources. A reactor `-am test
-Dtest=<selector>` still configures surefire here. failIfNoSpecifiedTests
defaults to true, so the build dies before the intended module runs. The
contract is the same flag the other reactor modules already set: a foreign
selector is not a failure of this module. A real test in this module is.
"""

import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]


class AdapterSelectorContractTest(unittest.TestCase):
    def test_adapter_module_does_not_abort_a_foreign_selector(self):
        text = (REPO / "allure-cucumber8-jvm/pom.xml").read_text(encoding="utf-8")
        self.assertIn(
            "<surefire.failIfNoSpecifiedTests>false</surefire.failIfNoSpecifiedTests>",
            text,
        )
        self.assertIn(
            "<failIfNoSpecifiedTests>${surefire.failIfNoSpecifiedTests}</failIfNoSpecifiedTests>",
            text,
        )
        tests = list((REPO / "allure-cucumber8-jvm").rglob("src/test/**/*.java"))
        self.assertEqual([], tests)


if __name__ == "__main__":
    unittest.main()
