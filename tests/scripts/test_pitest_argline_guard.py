"""Weekly PIT must not inherit unresolved Surefire/JaCoCo @{argLine}."""

from __future__ import annotations

import unittest
import xml.etree.ElementTree as ET  # nosec B314 - parses trusted in-repo pom.xml only
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


class PitestArgLineGuardTest(unittest.TestCase):
    def test_pitest_profile_sets_explicit_argline(self) -> None:
        root = ET.parse(ROOT / "pom.xml").getroot()  # nosec B314 - trusted local pom.xml
        profiles = root.find("m:profiles", NS)
        self.assertIsNotNone(profiles)
        pitest = None
        for profile in profiles.findall("m:profile", NS):
            ident = profile.find("m:id", NS)
            if ident is not None and ident.text == "pitest":
                pitest = profile
                break
        self.assertIsNotNone(pitest)
        plugin = None
        for candidate in pitest.findall(".//m:plugin", NS):
            artifact = candidate.find("m:artifactId", NS)
            if artifact is not None and artifact.text == "pitest-maven":
                plugin = candidate
                break
        self.assertIsNotNone(plugin)
        arg_line = plugin.find("m:configuration/m:argLine", NS)
        self.assertIsNotNone(arg_line, "pitest-maven must set argLine explicitly")
        text = (arg_line.text or "").strip()
        self.assertTrue(text, "pitest argLine must be non-empty")
        self.assertNotIn("@{argLine}", text)
        self.assertNotIn("{argLine}", text)


if __name__ == "__main__":
    unittest.main()
