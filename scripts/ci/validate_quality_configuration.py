#!/usr/bin/env python3
"""Validate aggregate coverage and reactor-wide quality-tool configuration."""

from __future__ import annotations

import re
import shlex
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
JAVA_MODULES = {
    "shaft-engine", "shaft-pilot-core", "shaft-capture", "shaft-capture-proxy", "shaft-doctor",
    "shaft-ai", "shaft-heal", "shaft-browserstack", "shaft-video", "shaft-visual", "shaft-sikulix",
    "shaft-mcp",
}
DEPENDABOT_DIRECTORIES = {
    "/",
    "/shaft-engine",
    "/shaft-pilot-core",
    "/shaft-capture",
    "/shaft-capture-proxy",
    "/shaft-doctor",
    "/shaft-ai",
    "/shaft-heal",
    "/shaft-browserstack",
    "/shaft-video",
    "/shaft-visual",
    "/shaft-sikulix",
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
COVERAGE_ACTIONS = (
    "./.github/actions/upload-jacoco-coverage",
    "./.github/actions/post-test-report",
)
MAVEN_EXECUTABLES = {"mvn", "mvn.cmd", "mvnw", "mvnw.cmd", "./mvnw"}
MAVEN_TEST_PHASES = {"test", "package", "verify", "install", "deploy"}
GRADLE_EXECUTABLES = {"gradle", "gradle.bat", "gradlew", "gradlew.bat", "./gradlew"}
GRADLE_TEST_TASKS = {"test", "check", "build"}
COMMAND_WRAPPERS = {"bash", "sh", "pwsh", "powershell", "cmd", "sudo", "env", "timeout"}
SHELL_ASSIGNMENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=.*$")
UNIT_SCOPE_SENTINEL = "!%regex[.*testPackage.unitTests.*]"
GRID_ONLY_SCOPE_EXCLUSIONS = (
    "!%regex[.*playwright.*PlaywrightActionsE2ETest.*]",
    "!%regex[.*LazyLoadingFixtureLiveTest.*]",
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


def _split_shell_commands(run: str) -> list[list[str]]:
    normalized = run.replace("\\\r\n", " ").replace("\\\n", " ").replace("`\r\n", " ")
    logical_lines: list[str] = []
    for line in normalized.splitlines():
        stripped = line.strip()
        if stripped.startswith("-") and logical_lines:
            logical_lines[-1] += " " + stripped
        else:
            logical_lines.append(line)
    normalized = "\n".join(logical_lines)
    commands = re.split(r"(?:\r?\n|&&|\|\||;)", normalized)
    tokenized: list[list[str]] = []
    for command in commands:
        command = command.strip()
        if not command or command.startswith("#"):
            continue
        try:
            tokens = shlex.split(command, posix=True)
        except ValueError:
            tokens = command.split()
        if tokens:
            tokenized.append(tokens)
    return tokenized


def _command_runs_jvm_tests(tokens: list[str]) -> bool:
    executable_index = next(
        (
            index
            for index, token in enumerate(tokens)
            if token in MAVEN_EXECUTABLES or token in GRADLE_EXECUTABLES
        ),
        None,
    )
    if executable_index is None:
        return False
    if executable_index > 0:
        prefix = [token for token in tokens[:executable_index] if not SHELL_ASSIGNMENT.fullmatch(token)]
        if prefix and prefix[0] not in COMMAND_WRAPPERS:
            return False

    executable = tokens[executable_index]
    arguments = tokens[executable_index + 1:]
    if executable in MAVEN_EXECUTABLES:
        skip_tests = any(
            re.fullmatch(r"-D(?:skipTests|maven\.test\.skip)(?:=true)?", argument, re.IGNORECASE)
            for argument in arguments
        )
        if skip_tests:
            return False
        return any(argument.lstrip(":") in MAVEN_TEST_PHASES for argument in arguments)

    return any(
        argument.split(":")[-1] in GRADLE_TEST_TASKS
        for argument in arguments
        if not argument.startswith("-")
    )


def _run_value_runs_jvm_tests(run: object) -> bool:
    return isinstance(run, str) and any(
        _command_runs_jvm_tests(tokens) for tokens in _split_shell_commands(run)
    )


def _local_action_runs_jvm_tests(root: Path, action: str, seen: set[Path] | None = None) -> bool:
    if not action.startswith("./"):
        return False
    action_path = root / action.removeprefix("./")
    metadata_path = action_path / "action.yml" if action_path.is_dir() else action_path
    if not metadata_path.is_file():
        return False
    seen = seen or set()
    resolved = metadata_path.resolve()
    if resolved in seen:
        return False
    seen.add(resolved)
    try:
        document = yaml.safe_load(metadata_path.read_text(encoding="utf-8")) or {}
    except yaml.YAMLError:
        return False
    for step in document.get("runs", {}).get("steps", []):
        if not isinstance(step, dict):
            continue
        if _run_value_runs_jvm_tests(step.get("run")):
            return True
        nested_action = step.get("uses")
        if isinstance(nested_action, str) and _local_action_runs_jvm_tests(
            root, nested_action, seen
        ):
            return True
    return False


def _step_is_disabled(step: dict[object, object]) -> bool:
    condition = step.get("if")
    if condition is False:
        return True
    if not isinstance(condition, str):
        return False
    normalized = condition.strip().lower().replace("${{", "").replace("}}", "").strip()
    return normalized in {"false", "0", "null"}


def _normalize_workflow_condition(condition: str) -> str:
    return condition.strip().lower().replace("${{", "").replace("}}", "").strip()


def _coverage_step_runs_after_failure_on_main(
    step: dict[object, object], test_conditions: set[str]
) -> bool:
    action = step.get("uses")
    if action not in COVERAGE_ACTIONS:
        return False
    condition = step.get("if")
    if not isinstance(condition, str):
        return False
    normalized = _normalize_workflow_condition(condition)
    safe_conditions = (
        r"always\(\)",
        r"always\(\)\s*&&\s*steps\.[a-z0-9_-]+\.outcome\s*!=\s*['\"]skipped['\"]",
        r"always\(\)\s*&&\s*github\.event_name\s*!=\s*['\"]pull_request['\"]",
        r"always\(\)\s*&&\s*github\.ref\s*==\s*['\"]refs/heads/main['\"]",
    )
    if any(re.fullmatch(pattern, normalized) for pattern in safe_conditions):
        return True
    conditional = re.fullmatch(r"always\(\)\s*&&\s*(.+)", normalized)
    return bool(conditional and conditional.group(1).strip() in test_conditions)


def validate_workflow_coverage_policy(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    workflows = root / ".github" / "workflows"
    if not workflows.is_dir():
        return errors

    paths = sorted((*workflows.glob("*.yml"), *workflows.glob("*.yaml")))
    for path in paths:
        workflow = path.read_text(encoding="utf-8")
        for forbidden in FORBIDDEN_OPTIONAL_COVERAGE_SETTINGS:
            if forbidden in workflow:
                errors.append(
                    f"{path.relative_to(root).as_posix()} must not mark JaCoCo coverage optional with {forbidden!r}"
                )
        try:
            document = yaml.safe_load(workflow) or {}
        except yaml.YAMLError as error:
            errors.append(f"{path.relative_to(root).as_posix()} is not valid YAML: {error}")
            continue
        jobs = document.get("jobs", {}) if isinstance(document, dict) else {}
        if not isinstance(jobs, dict):
            continue
        for job, definition in jobs.items():
            if not isinstance(definition, dict):
                continue
            steps = definition.get("steps", [])
            if not isinstance(steps, list):
                continue
            test_step_indexes: list[int] = []
            test_conditions: set[str] = set()
            coverage_steps: list[tuple[int, dict[object, object]]] = []
            for step_index, step in enumerate(steps):
                if not isinstance(step, dict) or _step_is_disabled(step):
                    continue
                action = step.get("uses")
                if isinstance(action, str):
                    if action in COVERAGE_ACTIONS:
                        coverage_steps.append((step_index, step))
                    if _local_action_runs_jvm_tests(root, action):
                        test_step_indexes.append(step_index)
                        if isinstance(step.get("if"), str):
                            test_conditions.add(_normalize_workflow_condition(step["if"]))
                if _run_value_runs_jvm_tests(step.get("run")):
                    test_step_indexes.append(step_index)
                    if isinstance(step.get("if"), str):
                        test_conditions.add(_normalize_workflow_condition(step["if"]))

            coverage_step_indexes = [
                index
                for index, step in coverage_steps
                if _coverage_step_runs_after_failure_on_main(step, test_conditions)
            ]
            has_coverage_after_last_test = bool(
                test_step_indexes
                and any(index > max(test_step_indexes) for index in coverage_step_indexes)
            )
            if test_step_indexes and not has_coverage_after_last_test:
                errors.append(
                    f"{path.relative_to(root).as_posix()} job {job!r} runs JVM tests "
                    "without upload-jacoco-coverage or post-test-report"
                )
    return errors


def _global_testing_scope(workflow: str) -> list[str]:
    match = re.search(r'^\s*GLOBAL_TESTING_SCOPE:\s*"([^"]*)"\s*$', workflow, re.MULTILINE)
    return [token.strip() for token in match.group(1).split(",")] if match else []


def _pr_gate_unit_selectors(workflow: str) -> set[str]:
    job_match = re.search(
        r"^  unit-tests:\s*$\n(?P<body>.*?)(?=^  [A-Za-z0-9_-]+:\s*$|\Z)",
        workflow,
        re.MULTILINE | re.DOTALL,
    )
    if not job_match:
        return set()
    step_match = re.search(
        r"^      - name: Run shaft-engine unit tests\s*$\n(?P<body>.*?)(?=^      - |\Z)",
        job_match.group("body"),
        re.MULTILINE | re.DOTALL,
    )
    if not step_match:
        return set()
    selector_match = re.search(r"^\s*'-Dtest=([^']*)'\s*$", step_match.group("body"), re.MULTILINE)
    if selector_match:
        tokens = {token.strip() for token in selector_match.group(1).split(",") if token.strip()}
        return {token for token in tokens if not token.startswith("!")}
    return set()


def validate_browser_matrix_scope_policy(root: Path = ROOT) -> list[str]:
    workflows = root / ".github" / "workflows"
    paths = {
        name: workflows / name
        for name in ("e2eTests.yml", "e2eLocalTests.yml", "pr-gate.yml")
    }
    missing_paths = [name for name, path in paths.items() if not path.is_file()]
    if missing_paths:
        return ["browser-matrix scope policy is missing workflow files: " + ", ".join(missing_paths)]

    grid_scope = _global_testing_scope(paths["e2eTests.yml"].read_text(encoding="utf-8"))
    if UNIT_SCOPE_SENTINEL not in grid_scope:
        return ["e2eTests.yml must mark the environment-independent unit-test exclusion trailer"]

    invalid_grid_only = [
        exclusion for exclusion in GRID_ONLY_SCOPE_EXCLUSIONS if grid_scope.count(exclusion) != 1
    ]
    unit_exclusions = grid_scope[grid_scope.index(UNIT_SCOPE_SENTINEL):]
    expected_local_scope = [
        exclusion for exclusion in grid_scope if exclusion not in GRID_ONLY_SCOPE_EXCLUSIONS
    ]
    local_scope = _global_testing_scope(paths["e2eLocalTests.yml"].read_text(encoding="utf-8"))
    missing_local = [exclusion for exclusion in expected_local_scope if exclusion not in local_scope]
    unexpected_local = [exclusion for exclusion in local_scope if exclusion not in expected_local_scope]

    pr_gate = paths["pr-gate.yml"].read_text(encoding="utf-8")
    pr_gate_unit_selectors = _pr_gate_unit_selectors(pr_gate)
    missing_pr_gate = []
    for exclusion in unit_exclusions:
        owner_selector = (
            "testPackage/unitTests/*"
            if exclusion == UNIT_SCOPE_SENTINEL
            else exclusion.removeprefix("!%regex[.*").removesuffix(".*]")
        )
        if owner_selector not in pr_gate_unit_selectors:
            missing_pr_gate.append(owner_selector)

    errors = []
    if invalid_grid_only:
        errors.append(
            "e2eTests.yml must contain each grid-only exclusion exactly once: "
            + ", ".join(invalid_grid_only)
        )
    if missing_local:
        errors.append(
            "e2eLocalTests.yml must exclude PR-gate-owned unit tests: "
            + ", ".join(missing_local)
        )
    if unexpected_local:
        errors.append(
            "e2eLocalTests.yml contains exclusions outside the shared local-browser scope: "
            + ", ".join(unexpected_local)
        )
    if not missing_local and not unexpected_local and local_scope != expected_local_scope:
        errors.append("e2eLocalTests.yml must keep the shared browser exclusions in grid order")
    if missing_pr_gate:
        errors.append(
            "pr-gate.yml must retain unit-test ownership for browser-matrix exclusions: "
            + ", ".join(missing_pr_gate)
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
    errors.extend(validate_browser_matrix_scope_policy(root))

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
    selector = (
        "-pl shaft-engine,shaft-pilot-core,shaft-capture,shaft-capture-proxy,shaft-doctor,"
        "shaft-ai,shaft-heal,shaft-browserstack,shaft-video,shaft-visual,shaft-sikulix,"
        "shaft-mcp,report-aggregate -am"
    )
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
        "com.sikulix:sikulixapi",
        "com.applitools:eyes-images-java4",
        "com.assertthat:selenium-shutterbug",
    ):
        if f"<exclude>{artifact}</exclude>" not in engine_pom:
            errors.append(f"shaft-engine dependency boundary does not ban {artifact}")

    workflows = root / ".github" / "workflows"
    e2e_workflow = (workflows / "e2eTests.yml").read_text(encoding="utf-8")
    local_e2e_workflow_path = workflows / "e2eLocalTests.yml"
    local_e2e_workflow = (
        local_e2e_workflow_path.read_text(encoding="utf-8")
        if local_e2e_workflow_path.exists()
        else ""
    )
    grid_install_count = e2e_workflow.count('mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip" "-Dcyclonedx.skip"')
    local_install_count = local_e2e_workflow.count('mvn -pl shaft-visual -am -e install "-DskipTests" "-Dgpg.skip"')
    activation_count = (
        e2e_workflow.count('"-DincludeVisualTestRuntime"')
        + local_e2e_workflow.count('"-DincludeVisualTestRuntime"')
    )
    if grid_install_count != 4 or local_install_count != 4 or activation_count != 8:
        errors.append(
            "e2eTests.yml and e2eLocalTests.yml must prepare and activate the visual test runtime "
            "for 4 grid/cloud and 4 local broad browser jobs"
        )
    for required_local_flow in (
        "Windows_SikuliX_Local",
        'mvn -pl shaft-sikulix -am -e test "-DrunWindowsDesktopE2E=true"',
        "Windows_Appium_Desktop_Local",
        'mvn -pl shaft-engine -am -e test "-DrunWindowsDesktopE2E=true" "-DdefaultElementIdentificationTimeout=5"',
        '"-DtargetBrowserName=WindowsApp"',
        '"-Dmobile_automationName=Windows"',
    ):
        if required_local_flow not in local_e2e_workflow:
            errors.append(f"e2eLocalTests.yml is missing Windows desktop E2E flow token: {required_local_flow}")
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
