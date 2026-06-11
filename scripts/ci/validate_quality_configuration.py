#!/usr/bin/env python3
"""Validate aggregate coverage and reactor-wide quality-tool configuration."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
JAVA_MODULES = {"shaft-engine", "shaft-browserstack", "shaft-video", "shaft-visual", "SHAFT_MCP"}
DEPENDABOT_DIRECTORIES = {
    "/",
    "/shaft-engine",
    "/shaft-browserstack",
    "/shaft-video",
    "/shaft-visual",
    "/shaft-mcp",
    "/shaft-bom",
    "/legacy-shaft-engine",
    "/report-aggregate",
}
JAVA_25_UNSAFE_FLAG = "--sun-misc-unsafe-memory-access=allow"
IGNORE_UNRECOGNIZED_VM_OPTIONS = "-XX:+IgnoreUnrecognizedVMOptions"


def text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NS)
    return value.strip() if value else None


def validate_maven_jvm_configuration(root: Path = ROOT) -> list[str]:
    jvm_config_path = root / ".mvn" / "jvm.config"
    jvm_config = jvm_config_path.read_text(encoding="utf-8") if jvm_config_path.is_file() else ""
    unsafe_flag_index = jvm_config.find(JAVA_25_UNSAFE_FLAG)
    compatibility_guard_index = jvm_config.find(IGNORE_UNRECOGNIZED_VM_OPTIONS)
    if unsafe_flag_index >= 0 and not 0 <= compatibility_guard_index < unsafe_flag_index:
        return [
            "Maven JVM configuration must guard Java 25-only options for the dependency submission Java 21 runtime"
        ]
    return []


def validate_quality_configuration(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    root_pom = ET.parse(root / "pom.xml").getroot()
    root_pom_text = (root / "pom.xml").read_text(encoding="utf-8")
    modules = {value.text.strip() for value in root_pom.iterfind("m:modules/m:module", NS) if value.text}
    if "report-aggregate" not in modules:
        errors.append("root pom.xml must include report-aggregate")
    if JAVA_25_UNSAFE_FLAG not in root_pom_text or "-Xshare:off" not in root_pom_text:
        errors.append("Surefire JVM arguments must suppress Java 25 Unsafe and CDS agent warnings")
    if "<mockitoAgentArgLine>" not in root_pom_text:
        errors.append("root pom.xml must define the Mockito startup agent")
    errors.extend(validate_maven_jvm_configuration(root))

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

    codeql = (root / ".github" / "workflows" / "codeql-analysis.yml").read_text(encoding="utf-8")
    selector = "-pl shaft-engine,shaft-browserstack,shaft-video,shaft-visual,shaft-mcp -am"
    if selector not in codeql:
        errors.append("CodeQL build must compile every Java-bearing module")

    engine_pom = (root / "shaft-engine" / "pom.xml").read_text(encoding="utf-8")
    visual_pom_path = root / "shaft-visual" / "pom.xml"
    visual_pom = visual_pom_path.read_text(encoding="utf-8") if visual_pom_path.is_file() else ""
    if "<artifactId>allure-jupiter</artifactId>" not in engine_pom or "<artifactId>allure-junit5</artifactId>" in engine_pom:
        errors.append("shaft-engine must use the current Allure Jupiter artifact without relocation warnings")
    if "${mockitoAgentArgLine}" not in engine_pom or "${mockitoAgentArgLine}" not in visual_pom:
        errors.append("Mockito-based modules must attach Mockito as a startup agent")
    if "<id>visual-test-runtime</id>" not in engine_pom:
        errors.append("shaft-engine must define the optional visual test runtime profile")
    if "<name>includeVisualTestRuntime</name>" not in engine_pom:
        errors.append("visual test runtime profile must use explicit property activation")
    if "<additionalClasspathDependency>" not in engine_pom or "<artifactId>shaft-visual</artifactId>" not in engine_pom:
        errors.append("visual test runtime profile must add shaft-visual to the Surefire classpath")
    visual_profile = (
        engine_pom.split("<id>visual-test-runtime</id>", 1)[1].split("</profile>", 1)[0]
        if "<id>visual-test-runtime</id>" in engine_pom
        else ""
    )
    if "<artifactId>shaft-engine</artifactId>" not in visual_profile:
        errors.append("visual test runtime profile must exclude shaft-engine's transitive tree")
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

    workflow_expectations = (
        ("e2eLocalTests.yml", 4, 'mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip"'),
        ("e2eTests.yml", 4, 'mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip" "-Dcyclonedx.skip"'),
    )
    for workflow_name, expected_jobs, visual_install_command in workflow_expectations:
        workflow = (root / ".github" / "workflows" / workflow_name).read_text(encoding="utf-8")
        install_count = workflow.count(visual_install_command)
        activation_count = workflow.count('"-DincludeVisualTestRuntime"')
        if install_count != expected_jobs or activation_count != expected_jobs:
            errors.append(
                f"{workflow_name} must prepare and activate the visual test runtime "
                f"for {expected_jobs} broad browser jobs"
            )
    e2e_workflow = (root / ".github" / "workflows" / "e2eTests.yml").read_text(encoding="utf-8")
    for cucumber_argument in (
        '"-Dcucumber.features=src/test/resources/CucumberFeatures,src/test/resources/CustomCucumberFeatures"',
        '"-Dcucumber.glue=customCucumberSteps,com.shaft.cucumber"',
        '"-Dcucumber.plugin=pretty,json:allure-results/cucumber.json,html:allure-results/cucumberReport.html,com.shaft.listeners.CucumberTestRunnerListener"',
    ):
        if cucumber_argument not in e2e_workflow:
            errors.append(f"e2eTests.yml Cucumber job is missing {cucumber_argument}")
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
