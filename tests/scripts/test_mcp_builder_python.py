"""shaft-mcp Maven builder images must have python3 before they run Maven.

allure-cucumber8-jvm stages its adapter with <executable>python3</executable>.
The maven:eclipse-temurin builder does not ship python3, so the image build
fails in that module after unit tests have already passed. That is an
environment failure. Removing the exec step would hide the staging contract.
"""

import re
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
BUILDER_DOCKERFILES = (
    "shaft-mcp/Dockerfile",
    "shaft-mcp/Dockerfile.fly",
    "shaft-mcp/Dockerfile.render",
    "shaft-mcp/Dockerfile.smithery",
    "shaft-mcp/Dockerfile.smithery.build",
)


def builder_python_gap(text: str) -> str | None:
    """Return why a Maven builder stage cannot run the adapter, or None."""
    match = re.search(r"^FROM maven[^\n]*\n(.*?)(?=^FROM |\Z)", text, re.M | re.S)
    if match is None:
        return "no maven builder stage"
    body = match.group(1)
    install = re.search(r"apt-get install[^\n]*\bpython3\b", body)
    maven = re.search(r"^RUN mvn\b", body, re.M)
    if maven is None:
        return "builder does not run mvn"
    if install is None or install.start() > maven.start():
        return "builder runs mvn before python3 is installed"
    return None


class ContainerPythonContractTest(unittest.TestCase):
    def test_every_maven_builder_installs_python3_before_mvn(self):
        for relative in BUILDER_DOCKERFILES:
            text = (REPO / relative).read_text(encoding="utf-8")
            self.assertIsNone(builder_python_gap(text), relative)

    def test_a_builder_without_python3_is_rejected(self):
        text = (
            "FROM maven:3.9.11-eclipse-temurin-25 AS builder\n"
            "WORKDIR /build\n"
            "RUN mvn --batch-mode -pl shaft-mcp -am install -DskipTests\n"
            "FROM eclipse-temurin:25-jre\n"
        )
        self.assertEqual(
            "builder runs mvn before python3 is installed",
            builder_python_gap(text),
        )


if __name__ == "__main__":
    unittest.main()
