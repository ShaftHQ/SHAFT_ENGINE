#!/usr/bin/env python3
"""Validate and optionally package SHAFT's Maven Central publication reactor."""

from __future__ import annotations

import argparse
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PUBLIC_ARTIFACTS = {
    "shaft-parent": (Path("pom.xml"), "pom"),
    "shaft-engine": (Path("shaft-engine/pom.xml"), "jar"),
    "shaft-browserstack": (Path("shaft-browserstack/pom.xml"), "jar"),
    "shaft-video": (Path("shaft-video/pom.xml"), "jar"),
    "shaft-visual": (Path("shaft-visual/pom.xml"), "jar"),
    "shaft-bom": (Path("shaft-bom/pom.xml"), "pom"),
    "SHAFT_ENGINE": (Path("legacy-shaft-engine/pom.xml"), "pom"),
}
JAR_CLASSIFIERS = ("sources", "javadoc")


def _parse(path: Path) -> ET.Element:
    return ET.parse(path).getroot()


def _text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NS)
    return value.strip() if value else None


def _version(root: Path) -> str:
    return _text(_parse(root / "pom.xml"), "m:version") or ""


def validate_publication(root: Path = ROOT, check_build_outputs: bool = False,
                         require_signatures: bool = False) -> list[str]:
    errors: list[str] = []
    parent = _parse(root / "pom.xml")
    modules = {_text(module, ".") for module in parent.findall("m:modules/m:module", NS)}
    expected_modules = {str(path.parent) for path, _ in PUBLIC_ARTIFACTS.values() if path.parent != Path(".")}
    if not expected_modules.issubset(modules):
        errors.append(f"reactor is missing public modules: {sorted(expected_modules - modules)}")
    if "report-aggregate" not in modules:
        errors.append("reactor must include report-aggregate")

    active_plugins = {
        _text(plugin, "m:artifactId")
        for plugin in parent.findall("m:build/m:plugins/m:plugin", NS)
    }
    for plugin in ("central-publishing-maven-plugin", "maven-gpg-plugin", "cyclonedx-maven-plugin"):
        if plugin not in active_plugins:
            errors.append(f"parent build must activate {plugin}")
    sbom_skip = _text(
        parent,
        "m:build/m:plugins/m:plugin[m:artifactId='cyclonedx-maven-plugin']"
        "/m:executions/m:execution[m:id='aggregate-sbom']/m:configuration/m:skipNotDeployed",
    )
    if sbom_skip != "false":
        errors.append("aggregate SBOM must include reactor artifacts regardless of deploy-skip detection")

    aggregate = _parse(root / "report-aggregate" / "pom.xml")
    if _text(aggregate, "m:properties/m:maven.deploy.skip") != "true":
        errors.append("report-aggregate must set maven.deploy.skip=true")
    if _text(aggregate, "m:properties/m:gpg.skip") != "true":
        errors.append("report-aggregate must set gpg.skip=true")

    bom = _parse(root / "shaft-bom" / "pom.xml")
    bom_artifacts = {
        _text(dependency, "m:artifactId")
        for dependency in bom.findall("m:dependencyManagement/m:dependencies/m:dependency", NS)
    }
    unpublished = bom_artifacts - set(PUBLIC_ARTIFACTS)
    if unpublished:
        errors.append(f"BOM references unpublished artifacts: {sorted(unpublished)}")

    for artifact, (pom_path, packaging) in PUBLIC_ARTIFACTS.items():
        pom = _parse(root / pom_path)
        for field in ("m:name", "m:description"):
            if not _text(pom, field):
                errors.append(f"{artifact} is missing {field.removeprefix('m:')}")
        if packaging == "jar":
            plugins = {
                _text(plugin, "m:artifactId")
                for plugin in pom.findall("m:build/m:plugins/m:plugin", NS)
            }
            for plugin in ("maven-source-plugin", "maven-javadoc-plugin"):
                if plugin not in plugins:
                    errors.append(f"{artifact} must activate {plugin}")

    workflow = (root / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8")
    required_steps = [
        "Validate Maven publication",
        "Deploy to Maven Central",
        "Verify published Maven Central coordinates",
        "Create GitHub Release",
        "Notify User Guide Repository",
        "Announce Release on Slack",
    ]
    positions = [workflow.find(step) for step in required_steps]
    if any(position < 0 for position in positions) or positions != sorted(positions):
        errors.append("release workflow must validate and deploy before release creation and announcements")
    if "-f pom.xml help:evaluate" not in workflow:
        errors.append("release workflow must derive the version from the reactor parent pom.xml")
    if "verify_maven_central_release.py" not in workflow:
        errors.append("release workflow must smoke published Maven Central coordinates before release creation")

    javadocs_workflow = (root / ".github/workflows/publishJavaDocs.yml").read_text(encoding="utf-8")
    if "workflow_run.conclusion == 'success'" not in javadocs_workflow:
        errors.append("JavaDocs publication must require a successful Maven Central workflow")
    if "scripts/ci/assemble_javadocs.py" not in javadocs_workflow:
        errors.append("JavaDocs workflow must assemble all Java-bearing module documentation")
    if "javadoc-source-folder: target/javadocs" not in javadocs_workflow:
        errors.append("JavaDocs workflow must publish the assembled root target/javadocs site")

    if check_build_outputs:
        version = _version(root)
        for artifact, (pom_path, packaging) in PUBLIC_ARTIFACTS.items():
            module_dir = (root / pom_path).parent
            target = module_dir / "target"
            pom_output = root / pom_path
            if not pom_output.is_file():
                errors.append(f"missing publication output: {pom_output.relative_to(root)}")
            pom_signature = target / f"{artifact}-{version}.pom.asc"
            if require_signatures and not pom_signature.is_file():
                errors.append(f"missing signature: {pom_signature.relative_to(root)}")
            if packaging == "jar":
                files = [f"{artifact}-{version}.jar"]
                files += [f"{artifact}-{version}-{classifier}.jar" for classifier in JAR_CLASSIFIERS]
                for name in files:
                    output = target / name
                    if not output.is_file():
                        errors.append(f"missing publication output: {output.relative_to(root)}")
                        continue
                    if require_signatures and not output.with_name(output.name + ".asc").is_file():
                        errors.append(f"missing signature: {output.relative_to(root)}.asc")
        if not (root / "target" / "bom.json").is_file():
            errors.append("missing aggregate SBOM: target/bom.json")
    return errors


def create_bundle(destination: Path, root: Path = ROOT, require_signatures: bool = False) -> None:
    errors = validate_publication(root, check_build_outputs=True, require_signatures=require_signatures)
    if errors:
        raise ValueError("\n".join(errors))
    version = _version(root)
    destination.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(destination, "w", zipfile.ZIP_DEFLATED) as archive:
        for artifact, (pom_path, packaging) in sorted(PUBLIC_ARTIFACTS.items()):
            module_dir = (root / pom_path).parent
            group_path = "io/github/shafthq"
            base = f"{group_path}/{artifact}/{version}"
            target = module_dir / "target"
            outputs = [(root / pom_path, f"{artifact}-{version}.pom",
                        target / f"{artifact}-{version}.pom.asc")]
            if packaging == "jar":
                outputs.extend(
                    (target / f"{artifact}-{version}{suffix}.jar",
                     f"{artifact}-{version}{suffix}.jar",
                     target / f"{artifact}-{version}{suffix}.jar.asc")
                    for suffix in ("", "-sources", "-javadoc")
                )
            for source, name, signature in outputs:
                archive.write(source, f"{base}/{name}")
                if signature.is_file():
                    archive.write(signature, f"{base}/{name}.asc")
        archive.write(root / "target" / "bom.json", "sbom/bom.json")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check-build-outputs", action="store_true")
    parser.add_argument("--require-signatures", action="store_true")
    parser.add_argument("--create-bundle", type=Path)
    args = parser.parse_args()
    if args.create_bundle:
        try:
            create_bundle(args.create_bundle, require_signatures=args.require_signatures)
        except ValueError as error:
            print(error, file=sys.stderr)
            return 1
        print(f"Created publication dry-run bundle: {args.create_bundle}")
        return 0
    errors = validate_publication(check_build_outputs=args.check_build_outputs,
                                  require_signatures=args.require_signatures)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Maven Central publication configuration is valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
