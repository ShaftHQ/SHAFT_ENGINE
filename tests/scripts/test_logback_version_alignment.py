"""logback-classic and logback-core must be the same version in every pin.

classic 1.6.8 calls OptionHelper.toLopsidedBooleanObject, which core 1.6.6 does
not provide. A split pin fails ShaftMcpApplication context load with
NoSuchMethodError, and a newer core beside an older classic prints a version
status line on stdout before MCP JSON-RPC.
"""

from __future__ import annotations

import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
POMS = ("pom.xml", "shaft-mcp/pom.xml")
ARTIFACTS = ("logback-classic", "logback-core")


def managed_version(pom: Path, artifact: str) -> str:
    root = ET.parse(pom).getroot()
    for dependency in root.findall("m:dependencyManagement/m:dependencies/m:dependency", NS):
        group = (dependency.findtext("m:groupId", namespaces=NS) or "").strip()
        name = (dependency.findtext("m:artifactId", namespaces=NS) or "").strip()
        if group == "ch.qos.logback" and name == artifact:
            version = (dependency.findtext("m:version", namespaces=NS) or "").strip()
            if not version:
                raise AssertionError(f"{pom} manages ch.qos.logback:{artifact} without a version")
            return version
    raise AssertionError(f"{pom} does not manage ch.qos.logback:{artifact}")


class LogbackVersionAlignmentTest(unittest.TestCase):
    def test_classic_and_core_versions_match_in_each_pom(self) -> None:
        seen: list[str] = []
        for relative in POMS:
            pom = ROOT / relative
            versions = {artifact: managed_version(pom, artifact) for artifact in ARTIFACTS}
            self.assertEqual(
                versions["logback-classic"],
                versions["logback-core"],
                f"{relative} pins classic {versions['logback-classic']} and core {versions['logback-core']}",
            )
            seen.append(versions["logback-classic"])
        self.assertEqual(seen[0], seen[1], "root and shaft-mcp logback pins diverged")


if __name__ == "__main__":
    unittest.main()
