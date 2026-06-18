#!/usr/bin/env python3
"""Upgrade Maven test projects to the latest modular SHAFT dependencies.

The upgrader supports:

* legacy ``io.github.shafthq:SHAFT_ENGINE`` projects;
* existing modular SHAFT projects;
* native Selenium, Appium, or REST Assured projects using TestNG or JUnit.

It preserves native test source, imports ``shaft-bom``, adds ``shaft-engine``,
infers optional SHAFT modules for legacy projects, compiles before and after
the migration, and restores every touched file if validation fails.

When ``OPENAI_API_KEY`` is available, an upgrade-induced compilation failure
can trigger up to three constrained repair attempts through the OpenAI
Responses API. Only redacted diagnostics and relevant, secret-free POM/Java
files are sent. The API key is never written to disk or printed.
"""

from __future__ import annotations

import argparse
import copy
import dataclasses
import difflib
import getpass
import json
import os
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path, PurePosixPath
from typing import Callable, Iterable, Mapping, Sequence


SHAFT_GROUP = "io.github.shafthq"
LEGACY_ARTIFACT = "SHAFT_ENGINE"
ENGINE_ARTIFACT = "shaft-engine"
BOM_ARTIFACT = "shaft-bom"
OPTIONAL_MODULES = ("shaft-browserstack", "shaft-video", "shaft-visual")
SHAFT_ARTIFACTS = {LEGACY_ARTIFACT, ENGINE_ARTIFACT, BOM_ARTIFACT, *OPTIONAL_MODULES}
JSONASSERT_GROUP = "org.skyscreamer"
JSONASSERT_ARTIFACT = "jsonassert"
ANDROID_JSON_GROUP = "com.vaadin.external.google"
ANDROID_JSON_ARTIFACT = "android-json"
MAVEN_CENTRAL = "https://repo.maven.apache.org/maven2"
DEFAULT_OPENAI_MODEL = "gpt-5.5"
DEFAULT_OPENAI_KEY_ENV = "OPENAI_API_KEY"
MAX_AI_REPAIR_ATTEMPTS = 3
MAX_AI_CONTEXT_CHARS = 140_000
MAX_AI_FILE_CHARS = 80_000
MAX_AI_CHANGE_CHARS = 500_000
MAVEN_NAMESPACE = "http://maven.apache.org/POM/4.0.0"
XSI_NAMESPACE = "http://www.w3.org/2001/XMLSchema-instance"

IGNORED_DIRECTORIES = {
    ".git",
    ".gradle",
    ".idea",
    ".mvn-repository",
    ".shaft-upgrade",
    ".vscode",
    "allure-report",
    "allure-results",
    "build",
    "node_modules",
    "out",
    "target",
}

TEXT_SCAN_SUFFIXES = {
    ".java",
    ".json",
    ".properties",
    ".xml",
    ".yaml",
    ".yml",
}

SUPPORTED_STACK_MARKERS = {
    "selenium": (
        "org.seleniumhq.selenium:",
        "org.openqa.selenium.",
    ),
    "appium": (
        "io.appium:java-client",
        "io.appium.java_client.",
    ),
    "rest-assured": (
        "io.rest-assured:rest-assured",
        "io.restassured.",
    ),
}

SUPPORTED_RUNNER_MARKERS = {
    "testng": (
        "org.testng:testng",
        "org.testng.",
    ),
    "junit": (
        "org.junit.jupiter:",
        "org.junit:",
        "junit:junit",
        "org.junit.jupiter.",
        "org.junit.",
    ),
}

OPTIONAL_PATTERNS: dict[str, tuple[tuple[str, re.Pattern[str]], ...]] = {
    "shaft-visual": (
        ("reference-image assertion", re.compile(r"\b(?:matches|doesNotMatch)ReferenceImage\s*\(")),
        ("visual validation engine", re.compile(r"\bVisualValidationEngine\b")),
        (
            "visual image-processing API",
            re.compile(
                r"\bImageProcessingActions\s*\.\s*"
                r"(?:findImageWithinCurrentPage|compareAgainstBaseline|loadOpenCV)\s*\("
            ),
        ),
        (
            "image-path touch API",
            re.compile(
                r"\b(?:tap|waitUntilElementIsVisible|swipeElementIntoView)\s*\(\s*"
                r"(?:\"|'|[A-Za-z_$][\w$]*\s*(?:,|\)))"
            ),
        ),
        (
            "explicit visual dependency",
            re.compile(
                r"(?:org\.openpnp:opencv|com\.applitools:eyes-|"
                r"com\.assertthat:selenium-shutterbug|<artifactId>shaft-visual</artifactId>)"
            ),
        ),
    ),
    "shaft-video": (
        (
            "desktop recording enabled",
            re.compile(r"(?:videoParamsRecordVideo|videoParams\.recordVideo)\s*[=:]\s*true", re.IGNORECASE),
        ),
        (
            "desktop recording API",
            re.compile(r"\b(?:RecordManager\s*\.\s*)?startVideoRecording\s*\(\s*\)"),
        ),
        (
            "explicit desktop-video dependency",
            re.compile(
                r"(?:com\.automation-remarks:video-recorder-|ws\.schild:jave-|"
                r"<artifactId>shaft-video</artifactId>)"
            ),
        ),
    ),
    "shaft-browserstack": (
        (
            "BrowserStack SDK property",
            re.compile(
                r"(?:browserStack\.)?(?:platformsList|parallelsPerPlatform|"
                r"browserstackAutomation|customBrowserStackYmlPath)"
            ),
        ),
        (
            "BrowserStack SDK dependency",
            re.compile(
                r"(?:com\.browserstack:browserstack-java-sdk|"
                r"<artifactId>shaft-browserstack</artifactId>)"
            ),
        ),
        (
            "BrowserStack SDK YAML",
            re.compile(r"(?m)^\s*(?:platforms|parallelsPerPlatform)\s*:"),
        ),
    ),
}

SECRET_PATTERNS: tuple[tuple[re.Pattern[str], str], ...] = (
    (
        re.compile(
            r"(?im)\b([A-Z0-9_.-]*(?:TOKEN|KEY|SECRET|PASSWORD|CREDENTIAL)"
            r"[A-Z0-9_.-]*)\s*([=:])\s*([^\s<]+)"
        ),
        r"\1\2<redacted>",
    ),
    (re.compile(r"(?im)(Authorization\s*:\s*Bearer\s+)([^\s]+)"), r"\1<redacted>"),
    (
        re.compile(
            r"(?is)-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----"
        ),
        "<redacted private key>",
    ),
    (re.compile(r"\b(?:sk-[A-Za-z0-9_-]{12,}|gh[pousr]_[A-Za-z0-9_]{12,})\b"), "<redacted>"),
)


class UpgradeError(RuntimeError):
    """Raised for user-actionable upgrade failures."""


@dataclasses.dataclass(frozen=True)
class CommandResult:
    """Captured process result."""

    command: tuple[str, ...]
    returncode: int
    stdout: str
    stderr: str

    @property
    def combined_output(self) -> str:
        """Return stdout and stderr as one diagnostic string."""
        return "\n".join(part for part in (self.stdout, self.stderr) if part)


@dataclasses.dataclass(frozen=True)
class ProjectAnalysis:
    """Detected Maven project shape and requested SHAFT migration."""

    project_root: Path
    all_poms: tuple[Path, ...]
    candidate_poms: tuple[Path, ...]
    legacy_project: bool
    existing_modular_project: bool
    stacks: tuple[str, ...]
    runners: tuple[str, ...]
    optional_evidence: Mapping[str, tuple[str, ...]]

    @property
    def optional_modules(self) -> tuple[str, ...]:
        """Return optional modules supported by scan evidence."""
        return tuple(module for module in OPTIONAL_MODULES if self.optional_evidence.get(module))


@dataclasses.dataclass(frozen=True)
class UpgradeExecution:
    """Final transaction outcome."""

    succeeded: bool
    rolled_back: bool
    compile_attempts: int
    ai_attempts: int
    final_result: CommandResult


@dataclasses.dataclass
class OriginalFile:
    """Original state for one transaction-managed file."""

    content: bytes | None
    mode: int | None


class FileTransaction:
    """Track file writes and restore their original bytes on failure."""

    def __init__(self) -> None:
        self._originals: dict[Path, OriginalFile] = {}
        self._committed = False

    def _remember(self, path: Path) -> None:
        path = path.resolve()
        if path in self._originals:
            return
        if path.exists():
            self._originals[path] = OriginalFile(
                content=path.read_bytes(),
                mode=stat.S_IMODE(path.stat().st_mode),
            )
        else:
            self._originals[path] = OriginalFile(content=None, mode=None)

    def write_bytes(self, path: Path, content: bytes) -> None:
        """Atomically write bytes after recording the original file state."""
        path = path.resolve()
        self._remember(path)
        atomic_write(path, content)
        original = self._originals[path]
        if original.mode is not None:
            path.chmod(original.mode)

    def write_text(self, path: Path, content: str) -> None:
        """Atomically write UTF-8 text after recording the original state."""
        self.write_bytes(path, content.encode("utf-8"))

    def rollback(self) -> None:
        """Restore all tracked paths byte-for-byte."""
        if self._committed:
            return
        for path, original in reversed(tuple(self._originals.items())):
            if original.content is None:
                if path.exists():
                    path.unlink()
                continue
            atomic_write(path, original.content)
            if original.mode is not None:
                path.chmod(original.mode)

    def commit(self) -> None:
        """Mark the transaction successful so rollback becomes a no-op."""
        self._committed = True
        self._originals.clear()


class OpenAIRepairClient:
    """Minimal standard-library client for constrained compile repair."""

    def __init__(self, api_key: str, model: str = DEFAULT_OPENAI_MODEL) -> None:
        if not api_key:
            raise ValueError("An OpenAI API key is required.")
        self._api_key = api_key
        self._model = model

    def request_repair(
        self,
        compile_output: str,
        files: Mapping[str, str],
        shaft_version: str,
        modules: Sequence[str],
    ) -> dict[str, object]:
        """Ask the Responses API for structured full-file replacements."""
        file_context = "\n\n".join(
            f"<<<FILE path={path}\n{content}\nFILE>>>" for path, content in files.items()
        )
        prompt = f"""A Maven Java project was migrated to modular SHAFT {shaft_version}.
The selected dependencies are: {", ".join(modules)}.
Compilation still fails. Make the smallest source or POM correction that restores
compilation while preserving behavior and the modular SHAFT BOM/dependencies.

Rules:
- Change only files included below.
- Return complete replacement content for every changed file.
- Do not disable tests, delete assertions, suppress compiler errors, downgrade SHAFT,
  remove the shaft-bom import, or replace modular artifacts with SHAFT_ENGINE.
- Do not add credentials, repositories, plugins, or unrelated dependencies.
- If no safe fix is possible from this context, return an empty changes array.

Compiler diagnostics:
<<<DIAGNOSTICS
{redact_secrets(compile_output)[-50_000:]}
DIAGNOSTICS>>>

Editable files:
{file_context}
"""
        payload = {
            "model": self._model,
            "store": False,
            "reasoning": {"effort": "low"},
            "instructions": (
                "You repair Java/Maven compilation failures conservatively. "
                "Follow the JSON schema exactly."
            ),
            "input": prompt,
            "text": {
                "format": {
                    "type": "json_schema",
                    "name": "shaft_upgrade_repair",
                    "strict": True,
                    "schema": {
                        "type": "object",
                        "properties": {
                            "summary": {"type": "string"},
                            "changes": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "path": {"type": "string"},
                                        "content": {"type": "string"},
                                        "reason": {"type": "string"},
                                    },
                                    "required": ["path", "content", "reason"],
                                    "additionalProperties": False,
                                },
                            },
                        },
                        "required": ["summary", "changes"],
                        "additionalProperties": False,
                    },
                }
            },
        }
        request = urllib.request.Request(
            "https://api.openai.com/v1/responses",
            data=json.dumps(payload).encode("utf-8"),
            headers={
                "Authorization": f"Bearer {self._api_key}",
                "Content-Type": "application/json",
                "User-Agent": "shaft-modular-upgrader/1",
            },
            method="POST",
        )
        try:
            with urllib.request.urlopen(request, timeout=180) as response:
                response_data = json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as exc:
            details = exc.read().decode("utf-8", errors="replace")
            raise UpgradeError(
                f"OpenAI API request failed with HTTP {exc.code}: "
                f"{redact_secrets(details)[:1_000]}"
            ) from exc
        except (OSError, ValueError) as exc:
            raise UpgradeError(f"OpenAI API request failed: {redact_secrets(str(exc))}") from exc

        output_text = extract_response_output_text(response_data)
        try:
            parsed = json.loads(output_text)
        except json.JSONDecodeError as exc:
            raise UpgradeError("OpenAI returned an invalid structured repair response.") from exc
        validate_repair_response(parsed)
        return parsed


def log(message: str) -> None:
    """Print a secret-redacted progress message."""
    print(redact_secrets(message), flush=True)


def redact_secrets(text: str) -> str:
    """Redact common credentials without exposing their values."""
    redacted = text or ""
    for pattern, replacement in SECRET_PATTERNS:
        redacted = pattern.sub(replacement, redacted)
    return redacted


def atomic_write(path: Path, content: bytes) -> None:
    """Write bytes through a temporary sibling and atomic replace."""
    path.parent.mkdir(parents=True, exist_ok=True)
    handle, temporary_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    temporary_path = Path(temporary_name)
    try:
        with os.fdopen(handle, "wb") as stream:
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary_path, path)
    finally:
        if temporary_path.exists():
            temporary_path.unlink()


def is_ignored(path: Path, root: Path) -> bool:
    """Return whether a path lives under a generated or private directory."""
    try:
        relative = path.resolve().relative_to(root.resolve())
    except ValueError:
        return True
    return any(part in IGNORED_DIRECTORIES for part in relative.parts)


def discover_poms(project_root: Path) -> tuple[Path, ...]:
    """Find Maven POMs while excluding generated directories."""
    return tuple(
        sorted(
            (
                path.resolve()
                for path in project_root.rglob("pom.xml")
                if not is_ignored(path, project_root)
            ),
            key=lambda path: (len(path.relative_to(project_root.resolve()).parts), str(path)),
        )
    )


def iter_scan_files(project_root: Path) -> Iterable[Path]:
    """Yield supported text files without entering generated directories."""
    for path in project_root.rglob("*"):
        if not path.is_file() or is_ignored(path, project_root):
            continue
        if path.name.startswith(".env") or path.suffix.lower() not in TEXT_SCAN_SUFFIXES:
            continue
        yield path


def read_text(path: Path, max_chars: int = 1_000_000) -> str:
    """Read a bounded UTF-8-compatible text file."""
    data = path.read_bytes()
    if len(data) > max_chars * 4:
        return ""
    return data.decode("utf-8", errors="replace")[:max_chars]


def dependency_coordinates(pom: Path) -> set[str]:
    """Return dependency coordinates declared anywhere in one POM."""
    try:
        root = parse_xml(pom.read_bytes())
    except ET.ParseError as exc:
        raise UpgradeError(f"Cannot parse Maven POM {pom}: {exc}") from exc
    coordinates: set[str] = set()
    for dependency in elements_named(root, "dependency"):
        group = child_text(dependency, "groupId")
        artifact = child_text(dependency, "artifactId")
        if group and artifact:
            coordinates.add(f"{group}:{artifact}")
    return coordinates


def coordinates_have_supported_stack(coordinates: set[str]) -> bool:
    """Return whether dependency coordinates contain a supported native stack."""
    return (
        any(coordinate.startswith("org.seleniumhq.selenium:") for coordinate in coordinates)
        or "io.appium:java-client" in coordinates
        or "io.rest-assured:rest-assured" in coordinates
    )


def coordinates_have_supported_runner(coordinates: set[str]) -> bool:
    """Return whether dependency coordinates contain TestNG or JUnit."""
    return any(
        coordinate == "org.testng:testng"
        or coordinate.startswith("org.junit:")
        or coordinate.startswith("org.junit.jupiter:")
        or coordinate == "junit:junit"
        for coordinate in coordinates
    )


def detect_markers(project_root: Path, poms: Sequence[Path]) -> tuple[set[str], set[str], str]:
    """Detect supported native stacks and test runners from POMs and Java imports."""
    pom_text = "\n".join(read_text(pom) for pom in poms)
    coordinate_text = "\n".join(
        coordinate
        for pom in poms
        for coordinate in sorted(dependency_coordinates(pom))
    )
    java_text = "\n".join(
        read_text(path, max_chars=250_000)
        for path in iter_scan_files(project_root)
        if path.suffix.lower() == ".java"
    )
    searchable = f"{pom_text}\n{coordinate_text}\n{java_text}"
    stacks = {
        name
        for name, markers in SUPPORTED_STACK_MARKERS.items()
        if any(marker in searchable for marker in markers)
    }
    runners = {
        name
        for name, markers in SUPPORTED_RUNNER_MARKERS.items()
        if any(marker in searchable for marker in markers)
    }
    return stacks, runners, searchable


def scan_optional_modules(project_root: Path) -> dict[str, tuple[str, ...]]:
    """Infer optional modular SHAFT dependencies and retain auditable evidence."""
    evidence: dict[str, list[str]] = {module: [] for module in OPTIONAL_MODULES}
    for path in iter_scan_files(project_root):
        text = read_text(path)
        if not text:
            continue
        relative = path.relative_to(project_root).as_posix()
        for module, patterns in OPTIONAL_PATTERNS.items():
            for reason, pattern in patterns:
                if pattern.search(text):
                    item = f"{relative}: {reason}"
                    if item not in evidence[module]:
                        evidence[module].append(item)
    return {module: tuple(items) for module, items in evidence.items()}


def analyze_project(project_root: Path) -> ProjectAnalysis:
    """Analyze a Maven project and choose POMs that should receive SHAFT."""
    project_root = project_root.resolve()
    if not project_root.exists() or not project_root.is_dir():
        raise UpgradeError(f"Project directory does not exist: {project_root}")
    all_poms = discover_poms(project_root)
    if not all_poms:
        raise UpgradeError(f"No pom.xml was found under {project_root}.")

    pom_coordinates = {pom: dependency_coordinates(pom) for pom in all_poms}
    legacy_project = any(
        f"{SHAFT_GROUP}:{LEGACY_ARTIFACT}" in coordinates
        for coordinates in pom_coordinates.values()
    )
    existing_modular_project = any(
        any(f"{SHAFT_GROUP}:{artifact}" in coordinates for artifact in {ENGINE_ARTIFACT, *OPTIONAL_MODULES})
        for coordinates in pom_coordinates.values()
    )
    stacks, runners, _ = detect_markers(project_root, all_poms)

    candidate_poms = [
        pom
        for pom, coordinates in pom_coordinates.items()
        if any(
            f"{SHAFT_GROUP}:{artifact}" in coordinates
            for artifact in SHAFT_ARTIFACTS
        )
        or (
            coordinates_have_supported_stack(coordinates)
            and coordinates_have_supported_runner(coordinates)
        )
    ]

    if not candidate_poms and (legacy_project or existing_modular_project):
        candidate_poms = [all_poms[0]]
    if not candidate_poms and stacks and runners:
        root_pom = project_root / "pom.xml"
        candidate_poms = [root_pom.resolve() if root_pom.exists() else all_poms[0]]
    if not candidate_poms:
        raise UpgradeError(
            "This project is not a supported legacy/modular SHAFT project and no "
            "Selenium, Appium, or REST Assured project using TestNG/JUnit was detected."
        )

    optional_evidence = (
        scan_optional_modules(project_root)
        if legacy_project or existing_modular_project
        else {module: () for module in OPTIONAL_MODULES}
    )
    if existing_modular_project and not legacy_project:
        for pom, coordinates in pom_coordinates.items():
            for module in OPTIONAL_MODULES:
                if f"{SHAFT_GROUP}:{module}" in coordinates:
                    existing = list(optional_evidence[module])
                    evidence = f"{pom.relative_to(project_root).as_posix()}: existing module declaration"
                    if evidence not in existing:
                        existing.append(evidence)
                    optional_evidence[module] = tuple(existing)

    return ProjectAnalysis(
        project_root=project_root,
        all_poms=all_poms,
        candidate_poms=tuple(dict.fromkeys(candidate_poms)),
        legacy_project=legacy_project,
        existing_modular_project=existing_modular_project,
        stacks=tuple(sorted(stacks)),
        runners=tuple(sorted(runners)),
        optional_evidence=optional_evidence,
    )


def parse_xml(content: bytes) -> ET.Element:
    """Parse XML while preserving comments."""
    parser = ET.XMLParser(target=ET.TreeBuilder(insert_comments=True))
    return ET.fromstring(content, parser=parser)


def local_name(tag: object) -> str:
    """Return an XML local name, including safe handling for comments."""
    if not isinstance(tag, str):
        return ""
    return tag.rsplit("}", 1)[-1]


def namespace_uri(root: ET.Element) -> str:
    """Return the namespace URI used by an XML root."""
    if isinstance(root.tag, str) and root.tag.startswith("{"):
        return root.tag[1:].split("}", 1)[0]
    return ""


def qualified(namespace: str, name: str) -> str:
    """Build a qualified XML name."""
    return f"{{{namespace}}}{name}" if namespace else name


def direct_child(parent: ET.Element, name: str) -> ET.Element | None:
    """Return a direct child by local name."""
    return next((child for child in parent if local_name(child.tag) == name), None)


def child_text(parent: ET.Element, name: str) -> str:
    """Return stripped text for a direct child."""
    child = direct_child(parent, name)
    return (child.text or "").strip() if child is not None else ""


def elements_named(root: ET.Element, name: str) -> Iterable[ET.Element]:
    """Yield descendants by local name."""
    return (element for element in root.iter() if local_name(element.tag) == name)


def project_insert_index(root: ET.Element, name: str) -> int:
    """Choose a conventional insertion point for a top-level Maven element."""
    order = [
        "modelVersion",
        "parent",
        "groupId",
        "artifactId",
        "version",
        "packaging",
        "name",
        "description",
        "url",
        "properties",
        "dependencyManagement",
        "dependencies",
        "build",
        "profiles",
    ]
    target_order = order.index(name) if name in order else len(order)
    for index, child in enumerate(root):
        child_name = local_name(child.tag)
        child_order = order.index(child_name) if child_name in order else len(order)
        if child_order > target_order:
            return index
    return len(root)


def ensure_project_child(root: ET.Element, namespace: str, name: str) -> ET.Element:
    """Return or create a conventional top-level Maven element."""
    existing = direct_child(root, name)
    if existing is not None:
        return existing
    element = ET.Element(qualified(namespace, name))
    root.insert(project_insert_index(root, name), element)
    return element


def add_text_child(parent: ET.Element, namespace: str, name: str, value: str) -> ET.Element:
    """Append one text child."""
    child = ET.SubElement(parent, qualified(namespace, name))
    child.text = value
    return child


def dependency_coordinate(dependency: ET.Element) -> tuple[str, str]:
    """Return group/artifact for a dependency element."""
    return child_text(dependency, "groupId"), child_text(dependency, "artifactId")


def dependency_template(root: ET.Element) -> ET.Element | None:
    """Find an existing SHAFT dependency whose scope/exclusions should be retained."""
    dependencies = direct_child(root, "dependencies")
    if dependencies is None:
        return None
    for dependency in dependencies:
        if local_name(dependency.tag) != "dependency":
            continue
        group, artifact = dependency_coordinate(dependency)
        if group == SHAFT_GROUP and artifact in SHAFT_ARTIFACTS:
            return dependency
    return None


def remove_shaft_dependencies(container: ET.Element | None) -> None:
    """Remove SHAFT runtime dependencies from a dependency container."""
    if container is None:
        return
    for dependency in list(container):
        if local_name(dependency.tag) != "dependency":
            continue
        group, artifact = dependency_coordinate(dependency)
        if group == SHAFT_GROUP and artifact in SHAFT_ARTIFACTS:
            container.remove(dependency)


def ensure_android_json_exclusion(dependency: ET.Element, namespace: str) -> None:
    """Exclude the legacy Android JSON artifact from a dependency."""
    exclusions = direct_child(dependency, "exclusions")
    if exclusions is not None:
        for exclusion in exclusions:
            if local_name(exclusion.tag) != "exclusion":
                continue
            group, artifact = dependency_coordinate(exclusion)
            if group == ANDROID_JSON_GROUP and artifact == ANDROID_JSON_ARTIFACT:
                return
    else:
        exclusions = ET.SubElement(dependency, qualified(namespace, "exclusions"))

    exclusion = ET.SubElement(exclusions, qualified(namespace, "exclusion"))
    add_text_child(exclusion, namespace, "groupId", ANDROID_JSON_GROUP)
    add_text_child(exclusion, namespace, "artifactId", ANDROID_JSON_ARTIFACT)


def remove_json_runtime_conflicts(container: ET.Element | None, namespace: str) -> None:
    """Remove org.json shadowing dependencies from a dependency container."""
    if container is None:
        return
    for dependency in list(container):
        if local_name(dependency.tag) != "dependency":
            continue
        group, artifact = dependency_coordinate(dependency)
        if group == ANDROID_JSON_GROUP and artifact == ANDROID_JSON_ARTIFACT:
            container.remove(dependency)
        elif group == JSONASSERT_GROUP and artifact == JSONASSERT_ARTIFACT:
            ensure_android_json_exclusion(dependency, namespace)


def copy_dependency_metadata(
    destination: ET.Element,
    template: ET.Element | None,
    namespace: str,
) -> None:
    """Copy scope/optional/exclusions metadata without carrying an old version."""
    if template is None:
        return
    allowed = {"type", "classifier", "scope", "optional", "exclusions"}
    for child in template:
        if local_name(child.tag) in allowed:
            destination.append(copy.deepcopy(child))


def create_dependency(
    namespace: str,
    artifact: str,
    template: ET.Element | None = None,
    version: str | None = None,
    dependency_type: str | None = None,
    scope: str | None = None,
) -> ET.Element:
    """Create one SHAFT dependency."""
    dependency = ET.Element(qualified(namespace, "dependency"))
    add_text_child(dependency, namespace, "groupId", SHAFT_GROUP)
    add_text_child(dependency, namespace, "artifactId", artifact)
    if version:
        add_text_child(dependency, namespace, "version", version)
    if dependency_type:
        add_text_child(dependency, namespace, "type", dependency_type)
    if scope:
        add_text_child(dependency, namespace, "scope", scope)
    if not any((dependency_type, scope)):
        copy_dependency_metadata(dependency, template, namespace)
    return dependency


def set_shaft_version_property(root: ET.Element, namespace: str, version: str) -> None:
    """Create or update the canonical SHAFT version property."""
    properties = ensure_project_child(root, namespace, "properties")
    property_element = direct_child(properties, "shaft.version")
    if property_element is None:
        property_element = ET.SubElement(properties, qualified(namespace, "shaft.version"))
    property_element.text = version


def transform_pom_bytes(
    original: bytes,
    version: str,
    optional_modules: Sequence[str],
) -> bytes:
    """Return a POM with the modular SHAFT BOM and selected dependencies."""
    try:
        root = parse_xml(original)
    except ET.ParseError as exc:
        raise UpgradeError(f"Cannot parse pom.xml: {exc}") from exc
    namespace = namespace_uri(root)
    if namespace:
        ET.register_namespace("", namespace)
        ET.register_namespace("xsi", XSI_NAMESPACE)

    template = dependency_template(root)
    set_shaft_version_property(root, namespace, version)

    dependency_management = ensure_project_child(root, namespace, "dependencyManagement")
    managed_dependencies = direct_child(dependency_management, "dependencies")
    if managed_dependencies is None:
        managed_dependencies = ET.SubElement(
            dependency_management,
            qualified(namespace, "dependencies"),
        )
    remove_shaft_dependencies(managed_dependencies)
    remove_json_runtime_conflicts(managed_dependencies, namespace)
    managed_dependencies.append(
        create_dependency(
            namespace,
            BOM_ARTIFACT,
            version="${shaft.version}",
            dependency_type="pom",
            scope="import",
        )
    )

    dependencies = ensure_project_child(root, namespace, "dependencies")
    remove_shaft_dependencies(dependencies)
    remove_json_runtime_conflicts(dependencies, namespace)
    dependencies.append(create_dependency(namespace, ENGINE_ARTIFACT, template=template))
    for module in OPTIONAL_MODULES:
        if module in optional_modules:
            dependencies.append(create_dependency(namespace, module, template=template))

    ET.indent(root, space="    ")
    xml_declaration = original.lstrip().startswith(b"<?xml")
    rendered = ET.tostring(
        root,
        encoding="utf-8",
        xml_declaration=xml_declaration,
        short_empty_elements=True,
    )
    return rendered.rstrip() + b"\n"


def shaft_dependency_state(pom: Path) -> tuple[str, set[str], str]:
    """Return version property, direct SHAFT dependencies, and managed BOM version."""
    root = parse_xml(pom.read_bytes())
    properties = direct_child(root, "properties")
    version = child_text(properties, "shaft.version") if properties is not None else ""
    dependencies = direct_child(root, "dependencies")
    direct_artifacts: set[str] = set()
    if dependencies is not None:
        for dependency in dependencies:
            group, artifact = dependency_coordinate(dependency)
            if group == SHAFT_GROUP:
                direct_artifacts.add(artifact)
    managed_version = ""
    dependency_management = direct_child(root, "dependencyManagement")
    managed = direct_child(dependency_management, "dependencies") if dependency_management is not None else None
    if managed is not None:
        for dependency in managed:
            group, artifact = dependency_coordinate(dependency)
            if group == SHAFT_GROUP and artifact == BOM_ARTIFACT:
                managed_version = child_text(dependency, "version")
                break
    return version, direct_artifacts, managed_version


def validate_upgraded_poms(
    poms: Sequence[Path],
    version: str,
    optional_modules: Sequence[str],
) -> None:
    """Enforce the modular SHAFT dependency contract after every repair."""
    expected = {ENGINE_ARTIFACT, *optional_modules}
    for pom in poms:
        actual_version, artifacts, managed_version = shaft_dependency_state(pom)
        if actual_version != version:
            raise UpgradeError(f"{pom}: <shaft.version> must remain {version}.")
        if LEGACY_ARTIFACT in artifacts or not expected.issubset(artifacts):
            raise UpgradeError(
                f"{pom}: modular SHAFT dependencies were removed or the legacy coordinate returned."
            )
        if managed_version != "${shaft.version}":
            raise UpgradeError(f"{pom}: shaft-bom import must use ${{shaft.version}}.")


def metadata_release(xml_text: str) -> str:
    """Extract the latest release from Maven metadata."""
    try:
        root = ET.fromstring(xml_text)
    except ET.ParseError as exc:
        raise UpgradeError("Maven Central returned invalid metadata.") from exc
    versioning = direct_child(root, "versioning")
    if versioning is None:
        raise UpgradeError("Maven metadata has no <versioning> section.")
    release = child_text(versioning, "release") or child_text(versioning, "latest")
    if not release:
        versions = direct_child(versioning, "versions")
        available = [
            (child.text or "").strip()
            for child in list(versions or [])
            if local_name(child.tag) == "version" and (child.text or "").strip()
        ]
        release = available[-1] if available else ""
    if not release or release.endswith("-SNAPSHOT"):
        raise UpgradeError("No stable modular SHAFT release was found in Maven metadata.")
    return release


def resolve_latest_shaft_version(
    opener: Callable[..., object] = urllib.request.urlopen,
) -> str:
    """Resolve the latest published modular SHAFT version from Maven Central."""
    path = f"{SHAFT_GROUP.replace('.', '/')}/{ENGINE_ARTIFACT}/maven-metadata.xml"
    request = urllib.request.Request(
        f"{MAVEN_CENTRAL}/{path}",
        headers={"User-Agent": "shaft-modular-upgrader/1"},
    )
    try:
        with opener(request, timeout=30) as response:
            metadata = response.read().decode("utf-8")
    except urllib.error.HTTPError as exc:
        if exc.code == 404:
            raise UpgradeError(
                "No modular shaft-engine release is published on Maven Central yet. "
                "Retry after publication or pass --shaft-version for a controlled local repository."
            ) from exc
        raise UpgradeError(f"Maven Central returned HTTP {exc.code}.") from exc
    except OSError as exc:
        raise UpgradeError(f"Cannot reach Maven Central: {exc}") from exc
    return metadata_release(metadata)


def default_compile_command(project_root: Path) -> list[str]:
    """Choose a Maven wrapper when present, then fall back to Maven on PATH."""
    candidates = (
        project_root / "mvnw.cmd",
        project_root / "mvnw",
    )
    for candidate in candidates:
        if candidate.is_file():
            return [
                str(candidate.resolve()),
                "test-compile",
                "-DskipTests",
                "-Dgpg.skip",
            ]
    executable = shutil.which("mvn.cmd" if os.name == "nt" else "mvn") or shutil.which("mvn")
    if not executable:
        raise UpgradeError("Maven or a Maven wrapper was not found.")
    return [executable, "test-compile", "-DskipTests", "-Dgpg.skip"]


def parse_compile_command(value: str | None, project_root: Path) -> list[str]:
    """Parse a custom command or return the default Maven command."""
    if not value:
        return default_compile_command(project_root)
    command = shlex.split(value, posix=os.name != "nt")
    if os.name == "nt":
        command = [
            argument[1:-1]
            if len(argument) >= 2 and argument[0] == argument[-1] and argument[0] in {'"', "'"}
            else argument
            for argument in command
        ]
    if not command:
        raise UpgradeError("--compile-command cannot be empty.")
    return command


def run_command(command: Sequence[str], cwd: Path, timeout_seconds: int) -> CommandResult:
    """Run a process without a shell and capture diagnostics."""
    try:
        completed = subprocess.run(
            list(command),
            cwd=cwd,
            text=True,
            capture_output=True,
            check=False,
            timeout=timeout_seconds,
        )
    except subprocess.TimeoutExpired as exc:
        return CommandResult(
            command=tuple(command),
            returncode=124,
            stdout=exc.stdout or "",
            stderr=(exc.stderr or "") + f"\nCompilation timed out after {timeout_seconds} seconds.",
        )
    except OSError as exc:
        raise UpgradeError(f"Cannot execute compile command: {exc}") from exc
    return CommandResult(
        command=tuple(command),
        returncode=completed.returncode,
        stdout=completed.stdout,
        stderr=completed.stderr,
    )


def extract_response_output_text(response: Mapping[str, object]) -> str:
    """Extract the assistant output text from a Responses API payload."""
    for item in response.get("output", []):
        if not isinstance(item, Mapping) or item.get("type") != "message":
            continue
        for content in item.get("content", []):
            if isinstance(content, Mapping) and content.get("type") == "output_text":
                text = content.get("text")
                if isinstance(text, str):
                    return text
    raise UpgradeError("OpenAI response did not contain output_text.")


def validate_repair_response(response: object) -> None:
    """Validate the constrained repair object before applying it."""
    if not isinstance(response, dict):
        raise UpgradeError("OpenAI repair response must be an object.")
    if set(response) != {"summary", "changes"}:
        raise UpgradeError("OpenAI repair response contains unexpected fields.")
    if not isinstance(response["summary"], str) or not isinstance(response["changes"], list):
        raise UpgradeError("OpenAI repair response has invalid field types.")
    for change in response["changes"]:
        if not isinstance(change, dict) or set(change) != {"path", "content", "reason"}:
            raise UpgradeError("OpenAI repair change has an invalid shape.")
        if not all(isinstance(change[key], str) for key in ("path", "content", "reason")):
            raise UpgradeError("OpenAI repair change fields must be strings.")
        if len(change["content"]) > MAX_AI_CHANGE_CHARS:
            raise UpgradeError("OpenAI repair change exceeds the size limit.")


def collect_ai_context(
    project_root: Path,
    poms: Sequence[Path],
    compile_output: str,
) -> dict[str, str]:
    """Collect secret-free POMs and compiler-referenced Java files."""
    candidates: list[Path] = list(poms)
    java_files = [
        path for path in iter_scan_files(project_root) if path.suffix.lower() == ".java"
    ]
    normalized_output = compile_output.replace("\\", "/")
    referenced = [
        path
        for path in java_files
        if path.relative_to(project_root).as_posix() in normalized_output
        or path.name in compile_output
    ]
    if not referenced:
        referenced = [
            path
            for path in java_files
            if "com.shaft" in read_text(path, max_chars=MAX_AI_FILE_CHARS)
        ][:8]
    candidates.extend(referenced[:16])

    context: dict[str, str] = {}
    total = 0
    for path in dict.fromkeys(candidates):
        if not path.exists() or path.is_symlink():
            continue
        content = read_text(path, max_chars=MAX_AI_FILE_CHARS)
        if not content or redact_secrets(content) != content:
            continue
        relative = path.resolve().relative_to(project_root.resolve()).as_posix()
        addition = len(relative) + len(content)
        if total + addition > MAX_AI_CONTEXT_CHARS:
            continue
        context[relative] = content
        total += addition
    return context


def resolve_edit_path(project_root: Path, relative_path: str, allowed_paths: set[str]) -> Path:
    """Resolve and validate an AI-proposed editable path."""
    normalized = PurePosixPath(relative_path.replace("\\", "/"))
    if normalized.is_absolute() or ".." in normalized.parts:
        raise UpgradeError(f"OpenAI proposed an unsafe path: {relative_path}")
    canonical = normalized.as_posix()
    if canonical not in allowed_paths:
        raise UpgradeError(f"OpenAI proposed a file outside the supplied context: {relative_path}")
    target = (project_root / Path(*normalized.parts)).resolve()
    try:
        target.relative_to(project_root.resolve())
    except ValueError as exc:
        raise UpgradeError(f"OpenAI proposed a path outside the project: {relative_path}") from exc
    if target.is_symlink() or not target.is_file():
        raise UpgradeError(f"OpenAI can edit only existing non-symlink files: {relative_path}")
    if target.name != "pom.xml" and target.suffix.lower() != ".java":
        raise UpgradeError(f"OpenAI can edit only pom.xml and Java source: {relative_path}")
    return target


def apply_ai_changes(
    transaction: FileTransaction,
    project_root: Path,
    response: Mapping[str, object],
    allowed_paths: set[str],
) -> int:
    """Apply validated full-file replacements through the main transaction."""
    changes = response["changes"]
    assert isinstance(changes, list)
    validated_changes: list[tuple[Path, str]] = []
    for change in changes:
        assert isinstance(change, dict)
        path = resolve_edit_path(project_root, str(change["path"]), allowed_paths)
        content = str(change["content"])
        if path.name == "pom.xml":
            try:
                parse_xml(content.encode("utf-8"))
            except ET.ParseError as exc:
                raise UpgradeError(f"OpenAI produced invalid XML for {path}: {exc}") from exc
        validated_changes.append((path, content))

    for path, content in validated_changes:
        transaction.write_text(path, content)
    return len(validated_changes)


def format_diff(path: Path, before: bytes, after: bytes, project_root: Path) -> str:
    """Return a unified UTF-8 diff for dry-run output."""
    relative = path.relative_to(project_root).as_posix()
    return "".join(
        difflib.unified_diff(
            before.decode("utf-8", errors="replace").splitlines(keepends=True),
            after.decode("utf-8", errors="replace").splitlines(keepends=True),
            fromfile=f"{relative} (before)",
            tofile=f"{relative} (after)",
        )
    )


def execute_upgrade_transaction(
    analysis: ProjectAnalysis,
    version: str,
    compile_command: Sequence[str],
    timeout_seconds: int,
    compile_runner: Callable[[Sequence[str], Path, int], CommandResult] = run_command,
    repair_client: OpenAIRepairClient | None = None,
    skip_baseline_compile: bool = False,
) -> UpgradeExecution:
    """Apply, compile, optionally repair, and commit or rollback the migration."""
    compile_attempts = 0
    ai_attempts = 0
    successful_result = CommandResult(tuple(compile_command), 0, "", "")

    if not skip_baseline_compile:
        log("Compiling the unchanged project to establish a valid baseline...")
        baseline = compile_runner(compile_command, analysis.project_root, timeout_seconds)
        compile_attempts += 1
        if baseline.returncode != 0:
            raise UpgradeError(
                "The project does not compile before migration. No files were changed.\n"
                + redact_secrets(baseline.combined_output)[-4_000:]
            )

    transaction = FileTransaction()
    transformed: dict[Path, bytes] = {
        pom: transform_pom_bytes(
            pom.read_bytes(),
            version,
            analysis.optional_modules,
        )
        for pom in analysis.candidate_poms
    }
    try:
        for pom, content in transformed.items():
            transaction.write_bytes(pom, content)
        validate_upgraded_poms(analysis.candidate_poms, version, analysis.optional_modules)

        log("Compiling the upgraded project...")
        result = compile_runner(compile_command, analysis.project_root, timeout_seconds)
        compile_attempts += 1
        if result.returncode == 0:
            transaction.commit()
            return UpgradeExecution(True, False, compile_attempts, ai_attempts, result)

        if repair_client is None:
            transaction.rollback()
            return UpgradeExecution(False, True, compile_attempts, ai_attempts, result)

        for attempt in range(1, MAX_AI_REPAIR_ATTEMPTS + 1):
            ai_attempts += 1
            log(f"OpenAI repair attempt {attempt}/{MAX_AI_REPAIR_ATTEMPTS}...")
            context = collect_ai_context(
                analysis.project_root,
                analysis.candidate_poms,
                result.combined_output,
            )
            if not context:
                log("No secret-free POM/Java context is available for AI repair.")
            else:
                attempt_snapshot = {
                    (analysis.project_root / Path(*PurePosixPath(relative).parts)).resolve(): content
                    for relative, content in context.items()
                }
                try:
                    response = repair_client.request_repair(
                        result.combined_output,
                        context,
                        version,
                        (ENGINE_ARTIFACT, *analysis.optional_modules),
                    )
                    applied = apply_ai_changes(
                        transaction,
                        analysis.project_root,
                        response,
                        set(context),
                    )
                    log(f"OpenAI proposed {applied} file replacement(s).")
                    validate_upgraded_poms(
                        analysis.candidate_poms,
                        version,
                        analysis.optional_modules,
                    )
                except UpgradeError as exc:
                    for path, content in attempt_snapshot.items():
                        transaction.write_text(path, content)
                    log(f"Repair response was not applied: {exc}")

            log(f"Recompiling after AI repair attempt {attempt}...")
            result = compile_runner(compile_command, analysis.project_root, timeout_seconds)
            compile_attempts += 1
            if result.returncode == 0:
                validate_upgraded_poms(
                    analysis.candidate_poms,
                    version,
                    analysis.optional_modules,
                )
                transaction.commit()
                return UpgradeExecution(True, False, compile_attempts, ai_attempts, result)

        transaction.rollback()
        return UpgradeExecution(False, True, compile_attempts, ai_attempts, result)
    except BaseException:
        transaction.rollback()
        raise


def print_analysis(analysis: ProjectAnalysis, version: str) -> None:
    """Print the migration decision before any writes."""
    project_type = (
        "legacy SHAFT"
        if analysis.legacy_project
        else "modular SHAFT"
        if analysis.existing_modular_project
        else "native Java test automation"
    )
    log(f"Project type: {project_type}")
    log(f"Target SHAFT version: {version}")
    if analysis.stacks:
        log(f"Detected stacks: {', '.join(analysis.stacks)}")
    if analysis.runners:
        log(f"Detected test runners: {', '.join(analysis.runners)}")
    log("POMs to update:")
    for pom in analysis.candidate_poms:
        log(f"  - {pom.relative_to(analysis.project_root)}")
    log("Optional module decision:")
    for module in OPTIONAL_MODULES:
        items = analysis.optional_evidence.get(module, ())
        if items:
            log(f"  - {module}: add")
            for item in items:
                log(f"      {item}")
        else:
            log(f"  - {module}: not detected")


def write_report(
    report_path: Path,
    analysis: ProjectAnalysis,
    version: str,
    execution: UpgradeExecution,
) -> None:
    """Write an optional machine-readable migration report."""
    payload = {
        "schemaVersion": 1,
        "project": str(analysis.project_root),
        "shaftVersion": version,
        "legacyProject": analysis.legacy_project,
        "existingModularProject": analysis.existing_modular_project,
        "stacks": list(analysis.stacks),
        "runners": list(analysis.runners),
        "candidatePoms": [
            str(path.relative_to(analysis.project_root)) for path in analysis.candidate_poms
        ],
        "optionalModules": list(analysis.optional_modules),
        "optionalEvidence": {
            module: list(items) for module, items in analysis.optional_evidence.items()
        },
        "succeeded": execution.succeeded,
        "rolledBack": execution.rolled_back,
        "compileAttempts": execution.compile_attempts,
        "aiAttempts": execution.ai_attempts,
    }
    atomic_write(report_path.resolve(), (json.dumps(payload, indent=2) + "\n").encode("utf-8"))


def confirm_upgrade() -> bool:
    """Ask for confirmation before modifying files."""
    if not sys.stdin.isatty():
        raise UpgradeError("Non-interactive execution requires --yes.")
    answer = input("Apply this migration and compile the project? [y/N] ").strip().lower()
    return answer in {"y", "yes"}


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        description=(
            "Upgrade a Maven Selenium/Appium/REST Assured or legacy SHAFT project "
            "to modular SHAFT with compile validation and automatic rollback."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""Examples:
  python shaft-upgrader/upgrade_to_modular_shaft.py --project .
  python shaft-upgrader/upgrade_to_modular_shaft.py --project . --yes
  python shaft-upgrader/upgrade_to_modular_shaft.py --project . --dry-run
  python shaft-upgrader/upgrade_to_modular_shaft.py --project . --shaft-version 10.2.20260609 --yes

Optional AI repair:
  Set OPENAI_API_KEY in the environment, then run the normal command.
  Use --prompt-for-openai-key to enter it without shell history.
  Use --no-ai to disable API repair even when the environment variable exists.
""",
    )
    parser.add_argument(
        "--project",
        type=Path,
        default=Path.cwd(),
        help="Maven project directory. Defaults to the current directory.",
    )
    parser.add_argument(
        "--shaft-version",
        help="Explicit modular SHAFT version. Defaults to the latest Maven Central release.",
    )
    parser.add_argument(
        "--compile-command",
        help=(
            "Custom compile command. Defaults to './mvnw test-compile -DskipTests "
            "-Dgpg.skip' or Maven on PATH."
        ),
    )
    parser.add_argument(
        "--compile-timeout",
        type=int,
        default=900,
        help="Compilation timeout in seconds. Default: 900.",
    )
    parser.add_argument(
        "--skip-baseline-compile",
        action="store_true",
        help="Skip compilation before editing. Not recommended.",
    )
    parser.add_argument("--dry-run", action="store_true", help="Print POM diffs without writing or compiling.")
    parser.add_argument("--yes", action="store_true", help="Apply without interactive confirmation.")
    parser.add_argument(
        "--report",
        type=Path,
        help="Optional JSON report path written after success or rollback.",
    )
    parser.add_argument(
        "--openai-key-env",
        default=DEFAULT_OPENAI_KEY_ENV,
        help=f"Environment variable containing the API key. Default: {DEFAULT_OPENAI_KEY_ENV}.",
    )
    parser.add_argument(
        "--openai-model",
        default=DEFAULT_OPENAI_MODEL,
        help=f"Responses API model for compile repair. Default: {DEFAULT_OPENAI_MODEL}.",
    )
    parser.add_argument(
        "--prompt-for-openai-key",
        action="store_true",
        help="Securely prompt for an optional OpenAI API key.",
    )
    parser.add_argument(
        "--no-ai",
        action="store_true",
        help="Disable OpenAI repair even when an API key is available.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """CLI entry point."""
    args = build_parser().parse_args(argv)
    try:
        analysis = analyze_project(args.project)
        version = args.shaft_version or resolve_latest_shaft_version()
        print_analysis(analysis, version)

        transformed = {
            pom: transform_pom_bytes(pom.read_bytes(), version, analysis.optional_modules)
            for pom in analysis.candidate_poms
        }
        if args.dry_run:
            for pom, after in transformed.items():
                print(format_diff(pom, pom.read_bytes(), after, analysis.project_root))
            return 0

        if not args.yes and not confirm_upgrade():
            log("Upgrade cancelled. No files were changed.")
            return 0

        compile_command = parse_compile_command(args.compile_command, analysis.project_root)
        log(f"Compile command: {' '.join(compile_command)}")

        api_key = ""
        if not args.no_ai:
            api_key = os.environ.get(args.openai_key_env, "")
            if args.prompt_for_openai_key and not api_key:
                api_key = getpass.getpass("Optional OpenAI API key (leave empty to disable AI repair): ")
        repair_client = OpenAIRepairClient(api_key, args.openai_model) if api_key else None
        if repair_client:
            log(
                f"OpenAI repair is enabled with model {args.openai_model}; "
                f"maximum attempts: {MAX_AI_REPAIR_ATTEMPTS}."
            )
        else:
            log("OpenAI repair is disabled; a failed upgraded compile will roll back immediately.")

        execution = execute_upgrade_transaction(
            analysis,
            version,
            compile_command,
            args.compile_timeout,
            repair_client=repair_client,
            skip_baseline_compile=args.skip_baseline_compile,
        )
        if args.report:
            write_report(args.report, analysis, version, execution)

        if execution.succeeded:
            log(
                f"Upgrade completed. Compilation passed after "
                f"{execution.compile_attempts} compile invocation(s)."
            )
            return 0

        log("Compilation did not pass. All upgrade and AI repair changes were rolled back.")
        diagnostics = redact_secrets(execution.final_result.combined_output)
        if diagnostics:
            log(diagnostics[-4_000:])
        return 1
    except KeyboardInterrupt:
        log("Upgrade interrupted.")
        return 130
    except UpgradeError as exc:
        log(f"Error: {exc}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
