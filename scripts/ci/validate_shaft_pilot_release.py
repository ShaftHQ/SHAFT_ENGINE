#!/usr/bin/env python3
"""Validate SHAFT Pilot release contracts, fixtures, evidence, and outputs."""

from __future__ import annotations

import argparse
import configparser
import json
import re
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError:  # Python 3.10 in GitHub Actions.
    tomllib = None

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PILOT_MODULES = (
    "shaft-pilot-core",
    "shaft-capture",
    "shaft-doctor",
    "shaft-ai",
    "shaft-heal",
    "shaft-mcp",
)
PUBLIC_ARTIFACTS = {
    "shaft-pilot-core": "shaft-pilot-core",
    "shaft-capture": "shaft-capture",
    "shaft-doctor": "shaft-doctor",
    "shaft-ai": "shaft-ai",
    "shaft-heal": "shaft-heal",
    "shaft-mcp": "shaft-mcp",
}
SECRET_CANARIES = (
    b"DO-NOT-RETAIN-SECRET",
    b"capture-browser-secret-canary",
    b"capture-secret-canary-value",
    b"sk-secret-canary-123456789",
    b"provider-secret-canary",
    b"advisory-report-secret",
    b"truncated-json-canary",
)
CREDENTIAL_PATTERNS = (
    re.compile(
        rb"-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----"
        rb"\s+[A-Za-z0-9+/=\r\n]{40,}"
    ),
    re.compile(rb"\bsk-(?:proj|ant)-[A-Za-z0-9_-]{16,}\b"),
    re.compile(rb"\bgithub_pat_[A-Za-z0-9_]{16,}\b"),
    re.compile(rb"\bgh[pousr]_[A-Za-z0-9]{20,}\b"),
    re.compile(rb"\bAKIA[0-9A-Z]{16}\b"),
)
INTERNAL_VERSION_DEFAULTS = {
    "shaftEngineVersion": r"\d+\.[1-4]\.\d{8}",
    "allure3Version": r"\d+\.\d+\.\d+",
    "nodeLtsVersion": r"\d+\.\d+\.\d+",
    "appiumServerVersion": r"\d+\.\d+\.\d+",
    "appiumInspectorPluginVersion": r"\d{4}\.\d+\.\d+",
    "appiumUiAutomator2DriverVersion": r"\d+\.\d+\.\d+",
    "appiumXcuitestDriverVersion": r"\d+\.\d+\.\d+",
    "androidCommandLineToolsVersion": r"\d+",
}
UNSTABLE_VERSION = re.compile(r"(?i)(alpha|beta|rc|cr|m[0-9]+|ea|preview|milestone|snapshot)")
STABLE_INTELLIJ_MARKETPLACE_CHANNEL = "default"


def text(element: ET.Element, path: str) -> str:
    return (element.findtext(path, default="", namespaces=NS) or "").strip()


def reactor_version(root: Path = ROOT) -> str:
    return text(ET.parse(root / "pom.xml").getroot(), "m:version")


def java_default_value(source: str, method: str) -> str | None:
    match = re.search(
        r'@DefaultValue\("([^"]+)"\)\s+(?:String|int)\s+' + re.escape(method) + r"\(\);",
        source,
    )
    return match.group(1) if match else None


def read_gradle_properties(path: Path) -> dict[str, str]:
    properties: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or "=" not in stripped:
            continue
        key, value = stripped.split("=", 1)
        properties[key.strip()] = value.strip()
    return properties


def intellij_marketplace_channel_errors(build_source: str) -> list[str]:
    errors = []
    matches = list(re.finditer(r"^\s*channels\s*=\s*listOf\(([^)]*)\)", build_source, re.MULTILINE))
    if not matches:
        return ["shaft-intellij publishing channel must explicitly target the stable Marketplace channel"]
    for match in matches:
        channels = re.findall(r'"([^"]*)"', match.group(1))
        if channels != [STABLE_INTELLIJ_MARKETPLACE_CHANNEL]:
            errors.append("shaft-intellij publishing channel must explicitly target the stable Marketplace channel")
    return errors


def scan_bytes(label: str, content: bytes) -> list[str]:
    errors = []
    for canary in SECRET_CANARIES:
        if canary in content:
            errors.append(f"{label}: contains Pilot secret canary {canary.decode('ascii')}")
    for pattern in CREDENTIAL_PATTERNS:
        if pattern.search(content):
            errors.append(f"{label}: contains a credential-shaped value")
    return errors


def validate_toml_bytes(content: bytes) -> None:
    """Validate TOML with stdlib-only fallback for Python 3.10 CI."""
    text_content = content.decode("utf-8")
    if tomllib is not None:
        tomllib.loads(text_content)
        return

    # ponytail: fixtures use simple TOML. Ceiling: this accepts the fixture subset only.
    # Upgrade trigger: a fixture needs arrays, dotted keys, or another TOML feature.
    parser = configparser.ConfigParser()
    try:
        parser.read_string(text_content if text_content.lstrip().startswith("[") else f"[root]\n{text_content}")
    except configparser.Error as error:
        raise ValueError(str(error)) from error


def validate_static(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    version = reactor_version(root)
    if not re.fullmatch(r"\d+\.[1-4]\.\d{8}", version):
        errors.append(f"reactor version is not a dated SHAFT release: {version!r}")

    parent = ET.parse(root / "pom.xml").getroot()
    modules = {
        text(module, ".")
        for module in parent.findall("m:modules/m:module", NS)
    }
    missing_modules = set(PILOT_MODULES) - modules
    if missing_modules:
        errors.append(f"reactor is missing Pilot modules: {sorted(missing_modules)}")

    bom = ET.parse(root / "shaft-bom/pom.xml").getroot()
    managed = {
        text(dependency, "m:artifactId")
        for dependency in bom.findall(
            "m:dependencyManagement/m:dependencies/m:dependency", NS
        )
    }
    expected_managed = set(PUBLIC_ARTIFACTS.values())
    if not expected_managed.issubset(managed):
        errors.append(
            f"shaft-bom is missing Pilot artifacts: {sorted(expected_managed - managed)}"
        )

    internal = (
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java"
    ).read_text(encoding="utf-8")
    version_match = java_default_value(internal, "shaftEngineVersion")
    if version_match != version:
        errors.append("Internal.shaftEngineVersion must match the reactor version")
    for method, expected_format in INTERNAL_VERSION_DEFAULTS.items():
        default_value = java_default_value(internal, method)
        if default_value is None:
            errors.append(f"Internal.{method} default is missing")
            continue
        if not re.fullmatch(expected_format, default_value):
            errors.append(f"Internal.{method} has unexpected default format: {default_value!r}")
        if method != "shaftEngineVersion" and UNSTABLE_VERSION.search(default_value):
            errors.append(f"Internal.{method} must use a stable release version: {default_value!r}")

    intellij_properties = read_gradle_properties(root / "shaft-intellij/gradle.properties")
    expected_plugin_version = version
    if intellij_properties.get("pluginVersion") != expected_plugin_version:
        errors.append("shaft-intellij pluginVersion must match the reactor version")
    for required_property in ("pluginSinceBuild", "platformVersion"):
        if not intellij_properties.get(required_property):
            errors.append(f"shaft-intellij {required_property} is missing")

    intellij_build = (root / "shaft-intellij/build.gradle.kts").read_text(encoding="utf-8")
    errors.extend(intellij_marketplace_channel_errors(intellij_build))

    intellij_client = (
        root / "shaft-intellij/src/main/java/com/shaft/intellij/mcp/ShaftMcpStdioClient.java"
    ).read_text(encoding="utf-8")
    if re.search(r'clientInfo\.addProperty\("version",\s*"\d+\.\d+\.\d{8}', intellij_client):
        errors.append("shaft-intellij clientInfo version must come from plugin metadata, not a literal")

    pilot_properties = (
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Pilot.java"
    ).read_text(encoding="utf-8")
    for required in (
        '@Key("pilot.ai.enabled")\n    @DefaultValue("false")',
        '@Key("pilot.ai.provider")\n    @DefaultValue("none")',
        '@Key("pilot.ai.consent.local")\n    @DefaultValue("false")',
        '@Key("pilot.ai.consent.remote")\n    @DefaultValue("false")',
        '@Key("pilot.ai.telemetry.enabled")\n    @DefaultValue("false")',
    ):
        if required not in pilot_properties:
            errors.append(f"Pilot safe default is missing: {required.splitlines()[0]}")

    examples = root / "shaft-mcp/src/test/resources/fixtures/shaft-pilot"
    for path in sorted(examples.rglob("*")):
        if not path.is_file():
            continue
        content = path.read_bytes()
        errors.extend(scan_bytes(str(path.relative_to(root)), content))
        if path.suffix == ".json":
            try:
                json.loads(content)
            except json.JSONDecodeError as error:
                errors.append(f"{path.relative_to(root)}: invalid JSON: {error}")
        if path.suffix == ".toml":
            try:
                validate_toml_bytes(content)
            except (UnicodeDecodeError, ValueError) as error:
                errors.append(f"{path.relative_to(root)}: invalid TOML: {error}")

    workflow = (root / ".github/workflows/mavenCentral_cd.yml").read_text(
        encoding="utf-8"
    )
    pilot_release_workflow = (root / ".github/workflows/shaft-pilot-release.yml").read_text(
        encoding="utf-8"
    )
    intellij_command = "bash shaft-intellij/gradlew -p shaft-intellij check buildPlugin verifyPlugin"
    intellij_verify_action = root / ".github/actions/intellij-verify/action.yml"
    if not intellij_verify_action.is_file() or intellij_command not in intellij_verify_action.read_text(
        encoding="utf-8"
    ):
        errors.append("intellij-verify composite action must verify the IntelliJ plugin release candidate")
    intellij_verify_reference = "./.github/actions/intellij-verify"
    for workflow_name, workflow_content in (
        ("mavenCentral_cd.yml", workflow),
        ("shaft-pilot-release.yml", pilot_release_workflow),
    ):
        if intellij_verify_reference not in workflow_content:
            errors.append(f"{workflow_name} must verify the IntelliJ plugin release candidate")
    required_steps = (
        "Verify IntelliJ plugin release candidate",
        "Validate SHAFT Pilot release contract",
        "Run deterministic SHAFT Pilot tests",
        "Run headless SHAFT Capture release journey",
        "Validate Maven publication",
        "Deploy to Maven Central",
        "Verify published Maven Central coordinates",
    )
    positions = [workflow.find(step) for step in required_steps]
    if any(position < 0 for position in positions) or positions != sorted(positions):
        errors.append("Maven Central workflow does not enforce the Pilot release gate")

    errors.extend(validate_release_candidate_isolation(pilot_release_workflow))
    publish_workflow_path = root / ".github/workflows/publish-intellij-plugin.yml"
    if not publish_workflow_path.is_file():
        errors.append("publish-intellij-plugin.yml must publish the IntelliJ plugin to Marketplace")
    else:
        action_text = (
            intellij_verify_action.read_text(encoding="utf-8")
            if intellij_verify_action.is_file()
            else ""
        )
        errors.extend(
            validate_intellij_marketplace_publish(
                publish_workflow_path.read_text(encoding="utf-8"),
                action_text,
            )
        )
    return errors


ISOLATED_RELEASE_CANDIDATE_JOBS = (
    "intellij-verify",
    "release-contracts",
    "pilot-tests",
    "capture-journey",
    "package-and-validate",
    "container-smoke",
)

def _workflow_job_block(workflow_text: str, job_id: str) -> str | None:
    """Return one top-level job block from serialized workflow YAML text."""
    match = re.search(rf"(?m)^  {re.escape(job_id)}:\s*$", workflow_text)
    if not match:
        return None
    rest = workflow_text[match.end() :]
    next_job = re.search(r"(?m)^  [A-Za-z0-9_-]+:\s*$", rest)
    end = match.end() + next_job.start() if next_job else len(workflow_text)
    return workflow_text[match.start() : end]


def validate_release_candidate_isolation(workflow_text: str) -> list[str]:
    """Reject a serialized release-candidate job when the file is a real workflow.

    Stdlib-only: this validator runs on a bare setup-python runner without
    requirements-ci.txt, so it cannot import PyYAML.
    """
    if "jobs:" not in workflow_text:
        return [
            "shaft-pilot-release.yml must declare jobs: for release-candidate isolation"
        ]

    errors: list[str] = []
    slice_changed_if = (
        "if: needs.detect-shaft-version-change.outputs.changed == 'true'"
    )
    for name in ISOLATED_RELEASE_CANDIDATE_JOBS:
        heading = f"  {name}:"
        block = _workflow_job_block(workflow_text, name)
        if block is None:
            errors.append(f"shaft-pilot-release.yml must isolate {name} as its own job")
            continue
        needs_only_detect = (
            f"{heading}\n    needs: detect-shaft-version-change" in block
            or f"{heading}\r\n    needs: detect-shaft-version-change" in block
        )
        if not needs_only_detect:
            errors.append(
                f"shaft-pilot-release.yml job {name} must depend only on detect-shaft-version-change"
            )
        if slice_changed_if not in block:
            errors.append(
                f"shaft-pilot-release.yml job {name} must gate on detect changed output"
            )
    aggregator = _workflow_job_block(workflow_text, "release-candidate")
    if aggregator is None:
        errors.append("shaft-pilot-release.yml must keep a release-candidate aggregator job")
        return errors
    if not re.search(r"(?m)^    if: always\(\)\s*$", aggregator):
        errors.append(
            "shaft-pilot-release.yml release-candidate aggregator must use if: always()"
        )
    for name in ISOLATED_RELEASE_CANDIDATE_JOBS:
        if f"- {name}" not in aggregator:
            errors.append(f"release-candidate aggregator must need isolated job {name}")
    fail_closed_tokens = (
        "DETECT_RESULT",
        "DETECT_CHANGED",
        '!= "success"',
        "success)",
    )
    if not all(token in aggregator for token in fail_closed_tokens):
        errors.append(
            "shaft-pilot-release.yml release-candidate aggregator must stay fail-closed "
            "on DETECT_RESULT/DETECT_CHANGED and isolated slice success"
        )
    return errors


def validate_intellij_marketplace_publish(
    workflow_text: str, action_text: str = ""
) -> list[str]:
    """Reject a publish-on-release workflow that can cancel before Marketplace.

    Stdlib-only: this validator runs on a bare setup-python runner without
    requirements-ci.txt, so it cannot import PyYAML.
    """
    errors: list[str] = []
    if "notify-nightly-failure" in workflow_text:
        errors.append(
            "publish-intellij-plugin.yml must not reuse nightly cancelled=skip for Marketplace publish"
        )
    if re.search(
        r"contains\(needs\.\*\.result,\s*'cancelled'\)\s*&&\s*'skip'",
        workflow_text,
    ):
        errors.append(
            "publish-intellij-plugin.yml must not map a cancelled release publish to skip"
        )
    if "intellij-marketplace-publish-miss" not in workflow_text:
        errors.append(
            "publish-intellij-plugin.yml must file a dedicated intellij-marketplace-publish-miss issue"
        )
    if "issues: write" not in workflow_text:
        errors.append(
            "publish-intellij-plugin.yml must grant issues: write for the dedicated miss tracker"
        )
    if "if: always()" not in workflow_text:
        errors.append(
            "publish-intellij-plugin.yml must fail closed with if: always() when publishPlugin is missed"
        )
    verify_block = _workflow_job_block(workflow_text, "verify")
    publish_block = _workflow_job_block(workflow_text, "publish")
    if verify_block is None:
        errors.append("publish-intellij-plugin.yml must isolate verify as its own job")
    if publish_block is None:
        errors.append("publish-intellij-plugin.yml must isolate publish as its own job")
    if verify_block is not None:
        if "timeout-minutes:" not in verify_block:
            errors.append("publish-intellij-plugin.yml verify job must declare timeout-minutes")
        if re.search(r"publish:\s*'true'", verify_block):
            errors.append("publish-intellij-plugin.yml verify job must not publish to Marketplace")
    if publish_block is not None:
        if "timeout-minutes:" not in publish_block:
            errors.append("publish-intellij-plugin.yml publish job must declare timeout-minutes")
        if "needs: verify" not in publish_block:
            errors.append("publish-intellij-plugin.yml publish job must need the verify job")
        if not re.search(r"verify:\s*'false'", publish_block):
            errors.append(
                "publish-intellij-plugin.yml publish job must skip verifyPlugin so it cannot eat the publish timeout"
            )
        for run_match in re.finditer(
            r"(?m)^\s+run:\s*(?:\|[^\n]*\n((?:[ \t]+.*\n?)+)|(.+))$",
            publish_block,
        ):
            run_text = run_match.group(1) or run_match.group(2) or ""
            if "build_retry.sh" in run_text and "publishPlugin" in run_text:
                errors.append("publishPlugin must not be wrapped in build_retry.sh")
    blob = workflow_text + "\n" + action_text
    if "assert_intellij_marketplace_receipt" not in blob:
        errors.append(
            "publishPlugin must assert a Gradle or Marketplace receipt via assert_intellij_marketplace_receipt"
        )
    if action_text:
        for match in re.finditer(r"run:\s*\|?(.*?)(?=\n    - |\n    [A-Za-z]|\Z)", action_text, re.S):
            run_text = match.group(1)
            if "publishPlugin" in run_text and "build_retry.sh" in run_text:
                errors.append("intellij-verify must not retry publishPlugin")
    return errors


def validate_test_results(root: Path = ROOT) -> list[str]:
    errors = []
    for module in PILOT_MODULES:
        results = root / module / "allure-results"
        populated = list(results.glob("*-result.json")) if results.is_dir() else []
        if not populated:
            errors.append(f"{module}: populated Allure result JSON is missing")
            continue
        for path in results.rglob("*"):
            if path.is_file() and path.stat().st_size <= 10 * 1024 * 1024:
                errors.extend(
                    scan_bytes(str(path.relative_to(root)), path.read_bytes())
                )
        # Check for failed/broken test status in Allure results
        for result_file in populated:
            try:
                content = json.loads(result_file.read_text(encoding="utf-8"))
                status = content.get("status")
                if status in ("failed", "broken"):
                    errors.append(
                        f"{module}: Allure result contains a {status} test status: {result_file.relative_to(root)}"
                    )
            except (json.JSONDecodeError, ValueError):
                # Skip malformed JSON; will be caught by other validation
                pass

    for relative in (
        Path("shaft-capture/target/shaft-capture"),
        Path("shaft-doctor/target/shaft-doctor"),
    ):
        directory = root / relative
        if not directory.is_dir():
            continue
        for path in directory.rglob("*"):
            if path.is_file() and path.stat().st_size <= 10 * 1024 * 1024:
                errors.extend(
                    scan_bytes(str(path.relative_to(root)), path.read_bytes())
                )
    return errors


def validate_build_outputs(root: Path = ROOT) -> list[str]:
    errors = []
    version = reactor_version(root)
    for module, artifact in PUBLIC_ARTIFACTS.items():
        jar = root / module / "target" / f"{artifact}-{version}.jar"
        if not jar.is_file():
            errors.append(f"missing packaged Pilot artifact: {jar.relative_to(root)}")
            continue
        try:
            with zipfile.ZipFile(jar) as archive:
                for entry in archive.infolist():
                    if entry.is_dir() or entry.file_size > 10 * 1024 * 1024:
                        continue
                    errors.extend(
                        scan_bytes(
                            f"{jar.relative_to(root)}!/{entry.filename}",
                            archive.read(entry),
                        )
                    )
        except zipfile.BadZipFile as error:
            errors.append(f"{jar.relative_to(root)}: invalid JAR: {error}")

    if not (root / "target/bom.json").is_file():
        errors.append("missing aggregate CycloneDX output: target/bom.json")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check-test-results", action="store_true")
    parser.add_argument("--check-build-outputs", action="store_true")
    args = parser.parse_args()

    errors = validate_static()
    if args.check_test_results:
        errors.extend(validate_test_results())
    if args.check_build_outputs:
        errors.extend(validate_build_outputs())
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("SHAFT Pilot release contract is valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
