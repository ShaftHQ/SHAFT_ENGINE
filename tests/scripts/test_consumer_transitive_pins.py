"""Guard the consumer-facing transitive pins in shaft-engine/pom.xml (#7041).

Maven does not apply the reactor's dependencyManagement to a consumer's transitive
resolution, so these artifacts must stay direct dependencies of shaft-engine or
consumers fall back to the vulnerable versions their upstream libraries declare.
"""

import unittest
import xml.etree.ElementTree as ET  # nosec B405 - parses a repository-owned POM
from pathlib import Path

POM = Path(__file__).resolve().parents[2] / "shaft-engine" / "pom.xml"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
REQUIRED = {
    "com.fasterxml.jackson.core:jackson-databind",
    "io.grpc:grpc-netty-shaded",
    "io.netty:netty-codec-http",
    "io.netty:netty-codec-http2",
    "io.netty:netty-handler",
    "io.netty:netty-common",
    "org.apache.httpcomponents.client5:httpclient5",
    "org.apache.httpcomponents.core5:httpcore5",
    "org.apache.httpcomponents.core5:httpcore5-h2",
}


def direct_dependencies(pom: Path) -> dict[str, "ET.Element"]:
    """Map groupId:artifactId to each direct <dependency> outside dependencyManagement."""
    root = ET.parse(pom).getroot()  # nosec B314 - repository-owned POM
    deps = root.find("m:dependencies", NS)
    found = {}
    for dep in deps.findall("m:dependency", NS) if deps is not None else []:
        key = f"{dep.findtext('m:groupId', namespaces=NS)}:{dep.findtext('m:artifactId', namespaces=NS)}"
        found[key] = dep
    return found


class ConsumerTransitivePinsTest(unittest.TestCase):
    def test_vulnerable_transitives_are_direct_dependencies(self):
        missing = sorted(REQUIRED - direct_dependencies(POM).keys())
        self.assertEqual([], missing, "re-add these as direct shaft-engine dependencies (#7041)")

    def test_pins_take_versions_from_the_managed_boms(self):
        deps = direct_dependencies(POM)
        hardcoded = sorted(k for k in REQUIRED if deps[k].findtext("m:version", namespaces=NS))
        self.assertEqual([], hardcoded, "let the BOM/dependencyManagement own these versions")

    def test_pins_are_not_test_or_provided_scoped(self):
        deps = direct_dependencies(POM)
        leaked = sorted(k for k in REQUIRED if deps[k].findtext("m:scope", namespaces=NS) in ("test", "provided"))
        self.assertEqual([], leaked, "test/provided scope does not reach consumers")


if __name__ == "__main__":
    unittest.main()
