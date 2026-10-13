"""allure-cucumber8-jvm must not abort a selector aimed at another module.

The module redistributes a jar and has no test sources. A reactor `-am test
-Dtest=<selector>` still configures surefire here. failIfNoSpecifiedTests
defaults to true, so the build dies before the intended module runs. The
contract is the same flag the other reactor modules already set: a foreign
selector is not a failure of this module. A real test in this module is.
"""

import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


class AdapterSelectorContractTest(unittest.TestCase):
    def test_adapter_module_does_not_abort_a_foreign_selector(self):
        pom = ET.parse(REPO / "allure-cucumber8-jvm/pom.xml")
        flag = pom.findtext("m:properties/m:surefire.failIfNoSpecifiedTests", namespaces=NS)
        self.assertEqual("false", flag)
        configured = [
            node.text
            for node in pom.findall(".//m:failIfNoSpecifiedTests", NS)
        ]
        self.assertIn("${surefire.failIfNoSpecifiedTests}", configured)
        tests = list((REPO / "allure-cucumber8-jvm").rglob("src/test/**/*.java"))
        self.assertEqual([], tests)


if __name__ == "__main__":
    unittest.main()
