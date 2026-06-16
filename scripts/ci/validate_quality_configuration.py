#!/usr/bin/env python3
"""Validate aggregate coverage and reactor-wide quality-tool configuration."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
JAVA_MODULES = {
    "shaft-engine", "shaft-pilot-core", "shaft-capture", "shaft-doctor", "shaft-ai", "shaft-heal",
    "shaft-browserstack", "shaft-video", "shaft-visual", "shaft-mcp",
}
DEPENDABOT_DIRECTORIES = {
    "/",
    "/shaft-engine",
    "/shaft-pilot-core",
    "/shaft-capture",
    "/shaft-doctor",
    "/shaft-ai",
    "/shaft-heal",
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
FORBIDDEN_OPTIONAL_COVERAGE_SETTINGS = (
    "require-coverage: false",
    "allow-missing-coverage: true",
)


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


def validate_surefire_jacoco_arg_lines(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    for module in sorted(JAVA_MODULES):
        pom_path = root / module / "pom.xml"
        if not pom_path.is_file():
            continue
        try:
            project = ET.parse(pom_path).getroot()
        except ET.ParseError as error:
            errors.append(f"{pom_path.relative_to(root).as_posix()} is not valid XML: {error}")
            continue
        plugins = project.findall(".//m:plugin[m:artifactId='maven-surefire-plugin']", NS)
        for plugin in plugins:
            arg_line = plugin.find("m:configuration/m:argLine", NS)
            if arg_line is not None and "@{argLine}" not in (arg_line.text or ""):
                errors.append(
                    f"{module} Surefire argLine must preserve JaCoCo's @{{argLine}} injection"
                )
    return errors


def validate_workflow_coverage_policy(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    workflows = root / ".github" / "workflows"
    if not workflows.is_dir():
        return errors

    for path in sorted(workflows.glob("*.yml")):
        workflow = path.read_text(encoding="utf-8")
        for forbidden in FORBIDDEN_OPTIONAL_COVERAGE_SETTINGS:
            if forbidden in workflow:
                errors.append(
                    f"{path.relative_to(root).as_posix()} must not mark JaCoCo coverage optional with {forbidden!r}"
                )
    return errors


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
    if root_pom.find("m:build/m:plugins/m:plugin[m:artifactId='jacoco-maven-plugin']", NS) is None:
        errors.append("root build plugins must inherit managed JaCoCo execution for module reports")
    errors.extend(validate_maven_jvm_configuration(root))
    errors.extend(validate_surefire_jacoco_arg_lines(root))
    errors.extend(validate_workflow_coverage_policy(root))

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

    codeql = (root / ".github" / "workflows" / "security.yml").read_text(encoding="utf-8")
    selector = "-pl shaft-engine,shaft-pilot-core,shaft-capture,shaft-doctor,shaft-ai,shaft-heal,shaft-browserstack,shaft-video,shaft-visual,shaft-mcp -am"
    if selector not in codeql:
        errors.append("CodeQL build must compile every Java-bearing module")

    engine_pom = (root / "shaft-engine" / "pom.xml").read_text(encoding="utf-8")
    visual_pom_path = root / "shaft-visual" / "pom.xml"
    visual_pom = visual_pom_path.read_text(encoding="utf-8") if visual_pom_path.is_file() else ""
    heal_pom_path = root / "shaft-heal" / "pom.xml"
    heal_pom = heal_pom_path.read_text(encoding="utf-8") if heal_pom_path.is_file() else ""
    if "<artifactId>allure-jupiter</artifactId>" not in engine_pom or "<artifactId>allure-junit5</artifactId>" in engine_pom:
        errors.append("shaft-engine must use the current Allure Jupiter artifact without relocation warnings")
    if any(
        "${mockitoAgentArgLine}" not in pom
        for pom in (engine_pom, visual_pom, heal_pom)
    ):
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
        "io.github.shafthq:shaft-pilot-core",
        "io.github.shafthq:shaft-capture",
        "io.github.shafthq:shaft-doctor",
        "io.github.shafthq:shaft-ai",
        "io.github.shafthq:shaft-heal",
        "com.browserstack:browserstack-java-sdk",
        "ws.schild:jave-*",
        "com.automation-remarks:video-recorder-*",
        "org.openpnp:opencv",
        "com.applitools:eyes-images-java4",
        "com.assertthat:selenium-shutterbug",
    ):
        if f"<exclude>{artifact}</exclude>" not in engine_pom:
            errors.append(f"shaft-engine dependency boundary does not ban {artifact}")

    e2e_workflow = (root / ".github" / "workflows" / "e2eTests.yml").read_text(encoding="utf-8")
    grid_install_count = e2e_workflow.count('mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip" "-Dcyclonedx.skip"')
    local_install_count = (
        e2e_workflow.count('mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip"')
        - grid_install_count
    )
    activation_count = e2e_workflow.count('"-DincludeVisualTestRuntime"')
    if grid_install_count != 4 or local_install_count != 4 or activation_count != 8:
        errors.append(
            "e2eTests.yml must prepare and activate the visual test runtime "
            "for 4 grid/cloud and 4 local broad browser jobs"
        )
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
