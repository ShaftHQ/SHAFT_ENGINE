#!/usr/bin/env python3
"""Validate aggregate coverage and reactor-wide quality-tool configuration."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
JAVA_MODULES = {"shaft-engine", "shaft-browserstack", "shaft-video", "shaft-visual"}
DEPENDABOT_DIRECTORIES = {
    "/",
    "/shaft-engine",
    "/shaft-browserstack",
    "/shaft-video",
    "/shaft-visual",
    "/shaft-bom",
    "/legacy-shaft-engine",
    "/report-aggregate",
}


def text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NS)
    return value.strip() if value else None


def validate_quality_configuration(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    root_pom = ET.parse(root / "pom.xml").getroot()
    modules = {value.text.strip() for value in root_pom.iterfind("m:modules/m:module", NS) if value.text}
    if "report-aggregate" not in modules:
        errors.append("root pom.xml must include report-aggregate")

    aggregate_path = root / "report-aggregate" / "pom.xml"
    if not aggregate_path.is_file():
        errors.append("report-aggregate/pom.xml is missing")
    else:
        aggregate = ET.parse(aggregate_path).getroot()
        dependencies = {
            text(dependency, "m:artifactId")
            for dependency in aggregate.findall("m:dependencies/m:dependency", NS)
        }
        missing = sorted(JAVA_MODULES - dependencies)
        if missing:
            errors.append(f"aggregate report is missing Java modules: {missing}")
        output = text(
            aggregate,
            "m:build/m:plugins/m:plugin[m:artifactId='jacoco-maven-plugin']"
            "/m:executions/m:execution[m:id='aggregate-report']/m:configuration/m:outputDirectory",
        )
        if output != "${maven.multiModuleProjectDirectory}/target/jacoco":
            errors.append("aggregate JaCoCo output must be root target/jacoco")
        deploy_skip = text(
            aggregate,
            "m:build/m:plugins/m:plugin[m:artifactId='maven-deploy-plugin']/m:configuration/m:skip",
        )
        if deploy_skip != "true":
            errors.append("report-aggregate must be excluded from deployment")

    dependabot = (root / ".github" / "dependabot.yml").read_text(encoding="utf-8")
    missing_directories = sorted(
        directory for directory in DEPENDABOT_DIRECTORIES if f'- "{directory}"' not in dependabot
    )
    if missing_directories:
        errors.append(f"Dependabot is missing Maven directories: {missing_directories}")
    if "group-by: dependency-name" not in dependabot:
        errors.append("Dependabot must group aligned Maven dependencies by dependency name")

    workflow_text = "\n".join(
        path.read_text(encoding="utf-8") for path in (root / ".github" / "workflows").glob("*.yml")
    )
    action_text = "\n".join(
        path.read_text(encoding="utf-8") for path in (root / ".github" / "actions").glob("**/*.yml")
    )
    codecov_count = (workflow_text + action_text).count("codecov/codecov-action@")
    if codecov_count != 1:
        errors.append(f"expected one Codecov upload step, found {codecov_count}")
    coverage_workflow = (root / ".github" / "workflows" / "coverage-readiness.yml").read_text(
        encoding="utf-8"
    )
    if "files: ./target/jacoco/jacoco.xml" not in coverage_workflow or "disable_search: true" not in coverage_workflow:
        errors.append("Codecov must upload only root target/jacoco/jacoco.xml with search disabled")

    codeql = (root / ".github" / "workflows" / "codeql-analysis.yml").read_text(encoding="utf-8")
    selector = "-pl shaft-engine,shaft-browserstack,shaft-video,shaft-visual -am"
    if selector not in codeql:
        errors.append("CodeQL build must compile every Java-bearing module")

    engine_pom = (root / "shaft-engine" / "pom.xml").read_text(encoding="utf-8")
    for artifact in (
        "com.browserstack:browserstack-java-sdk",
        "ws.schild:jave-*",
        "com.automation-remarks:video-recorder-*",
        "org.openpnp:opencv",
        "com.applitools:eyes-images-java4",
        "com.assertthat:selenium-shutterbug",
    ):
        if f"<exclude>{artifact}</exclude>" not in engine_pom:
            errors.append(f"shaft-engine dependency boundary does not ban {artifact}")
    return errors


def main() -> int:
    errors = validate_quality_configuration()
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Aggregate coverage, Codecov, CodeQL, Dependabot, and dependency boundaries are configured.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
