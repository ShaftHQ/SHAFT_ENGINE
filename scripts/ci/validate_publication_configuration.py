#!/usr/bin/env python3
"""Validate the atomic Maven Central publication and release-gating configuration."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PUBLIC_MODULES = {
    "shaft-engine",
    "shaft-browserstack",
    "shaft-video",
    "shaft-visual",
    "shaft-bom",
    "legacy-shaft-engine",
}
PUBLIC_JAR_MODULES = {
    "shaft-engine",
    "shaft-browserstack",
    "shaft-video",
    "shaft-visual",
}
PUBLICATION_PLUGINS = {
    "central-publishing-maven-plugin",
    "maven-gpg-plugin",
    "maven-javadoc-plugin",
    "maven-source-plugin",
}


def text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NS)
    return value.strip() if value else None


def validate_publication_configuration(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    root_pom = ET.parse(root / "pom.xml").getroot()
    modules = {
        value.text.strip()
        for value in root_pom.findall("m:modules/m:module", NS)
        if value.text and value.text.strip()
    }
    missing_modules = sorted(PUBLIC_MODULES - modules)
    if missing_modules:
        errors.append(f"root reactor is missing deployable modules: {missing_modules}")

    active_plugins = {
        text(plugin, "m:artifactId"): plugin
        for plugin in root_pom.findall("m:build/m:plugins/m:plugin", NS)
    }
    managed_plugins = {
        text(plugin, "m:artifactId"): plugin
        for plugin in root_pom.findall("m:build/m:pluginManagement/m:plugins/m:plugin", NS)
    }
    missing_plugins = sorted(PUBLICATION_PLUGINS - active_plugins.keys())
    if missing_plugins:
        errors.append(f"root build must activate publication plugins: {missing_plugins}")

    central_plugin = active_plugins.get("central-publishing-maven-plugin")
    if central_plugin is not None:
        central_configuration = managed_plugins.get("central-publishing-maven-plugin", central_plugin)
        if text(central_configuration, "m:extensions") != "true":
            errors.append("Central publishing must be a root reactor build extension")
        if text(central_configuration, "m:configuration/m:autoPublish") != "true":
            errors.append("Central publishing must automatically publish the coherent reactor bundle")
        if text(central_configuration, "m:configuration/m:waitUntil") != "published":
            errors.append("Central publishing must wait until the deployment is published")

    aggregate_path = root / "report-aggregate" / "pom.xml"
    if aggregate_path.is_file():
        aggregate = ET.parse(aggregate_path).getroot()
        deploy_skip = text(
            aggregate,
            "m:build/m:plugins/m:plugin[m:artifactId='maven-deploy-plugin']/m:configuration/m:skip",
        )
        if deploy_skip != "true":
            errors.append("report-aggregate must be excluded from deployment")
        central_skip = text(
            aggregate,
            "m:build/m:plugins/m:plugin[m:artifactId='central-publishing-maven-plugin']"
            "/m:configuration/m:skipPublishing",
        )
        if central_skip != "true":
            errors.append("report-aggregate must be excluded from Central bundle staging")
    else:
        errors.append("report-aggregate/pom.xml is missing")

    bom_path = root / "shaft-bom" / "pom.xml"
    if bom_path.is_file():
        bom = ET.parse(bom_path).getroot()
        managed = {
            text(dependency, "m:artifactId")
            for dependency in bom.findall("m:dependencyManagement/m:dependencies/m:dependency", NS)
            if text(dependency, "m:groupId") in {"io.github.shafthq", "${project.groupId}"}
        }
        missing_bom_entries = sorted(PUBLIC_JAR_MODULES - managed)
        unpublished_bom_entries = sorted(managed - PUBLIC_MODULES)
        if missing_bom_entries:
            errors.append(f"shaft-bom is missing public artifacts: {missing_bom_entries}")
        if unpublished_bom_entries:
            errors.append(f"shaft-bom references unpublished artifacts: {unpublished_bom_entries}")
    else:
        errors.append("shaft-bom/pom.xml is missing")

    workflow_path = root / ".github" / "workflows" / "mavenCentral_cd.yml"
    if workflow_path.is_file():
        workflow = workflow_path.read_text(encoding="utf-8")
        deploy_index = workflow.find("- name: Deploy to Maven Central")
        release_index = workflow.find("- name: Create GitHub Release")
        if deploy_index < 0:
            errors.append("release workflow is missing the Maven Central deployment step")
        elif release_index < 0 or deploy_index > release_index:
            errors.append("Maven Central deployment must complete before GitHub release creation")

        pre_deploy_checks = {
            "publication configuration validation": "python3 scripts/ci/validate_publication_configuration.py",
            "release reactor installation": "mvn --batch-mode clean install -DskipTests -Dgpg.skip",
            "combined-module consumer validation": (
                "mvn --batch-mode --file tools/modularization/publication-fixtures/all-modules/pom.xml verify"
            ),
        }
        for check_name, command in pre_deploy_checks.items():
            check_index = workflow.find(command)
            if check_index < 0 or (deploy_index >= 0 and check_index > deploy_index):
                errors.append(f"release workflow must run {check_name} before Maven Central deployment")

        if "mvn --non-recursive help:evaluate -Dexpression=project.version" not in workflow:
            errors.append("release version must be extracted explicitly from the root parent POM")
        for downstream_step in ("Notify User Guide Repository", "Announce Release on Slack"):
            step_index = workflow.find(f"- name: {downstream_step}")
            if release_index < 0 or step_index < release_index:
                errors.append(f"{downstream_step} must run only after GitHub release creation")
    else:
        errors.append(".github/workflows/mavenCentral_cd.yml is missing")

    fixture_path = root / "tools" / "modularization" / "publication-fixtures" / "all-modules" / "pom.xml"
    if fixture_path.is_file():
        fixture = fixture_path.read_text(encoding="utf-8")
        required_markers = {
            "BOM import": "<artifactId>shaft-bom</artifactId>",
            "engine dependency": "<artifactId>shaft-engine</artifactId>",
            "BrowserStack dependency": "<artifactId>shaft-browserstack</artifactId>",
            "video dependency": "<artifactId>shaft-video</artifactId>",
            "visual dependency": "<artifactId>shaft-visual</artifactId>",
            "dependency convergence": "<dependencyConvergence/>",
            "duplicate-class check": "<banDuplicateClasses>",
            "SBOM generation": "<artifactId>cyclonedx-maven-plugin</artifactId>",
        }
        missing_markers = sorted(label for label, marker in required_markers.items() if marker not in fixture)
        if missing_markers:
            errors.append(f"combined-module fixture is missing validation: {missing_markers}")
    else:
        errors.append("combined-module fixture tools/modularization/publication-fixtures/all-modules/pom.xml is missing")

    return errors


def main() -> int:
    errors = validate_publication_configuration()
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Maven Central publication, deployable artifacts, consumer validation, and release ordering are coherent.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
