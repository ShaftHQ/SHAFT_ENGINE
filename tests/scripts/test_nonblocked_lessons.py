"""Shipped lesson statements for the non-blocked harness issues.

The needles below are the acceptance phrases. The text under test is the
file each issue named, read from this checkout, not a fixture copy.
"""

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

def read(relative):
    return " ".join((ROOT / relative).read_text(encoding="utf-8").split())

class NonblockedLessonTests(unittest.TestCase):

    def test_java_tests_record_testng_assert_throws_void(self):
        rules = read("shaft-skills/ce-pack/references/playbooks/java-tests.md")
        self.assertIn("org.testng.Assert.assertThrows` returns void", rules)
        self.assertIn("`expectThrows` returns the throwable", rules)

    def test_cucumber8_adapter_is_the_official_bom_artifact(self):
        pom = read("shaft-engine/pom.xml")
        self.assertIn(
            "<groupId>io.qameta.allure</groupId> <artifactId>allure-cucumber8-jvm</artifactId>",
            pom,
        )
        self.assertNotIn("a1df39ff", pom)
        self.assertNotIn("jitpack.io", read("pom.xml"))
        self.assertFalse((ROOT / "allure-cucumber8-jvm").exists())

if __name__ == "__main__":
    unittest.main()
