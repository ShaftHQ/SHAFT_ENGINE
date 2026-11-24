#!/usr/bin/env python3
"""Validate installer contract between Python, Java, and shell scripts."""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
JAVA_SETUP = Path("shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java")
JAVA_SERVICE = Path("shaft-mcp/src/main/java/com/shaft/mcp/ShaftProjectService.java")
ASSEMBLER = Path("scripts/ci/assemble_shaft_skills_plugin.py")
HOST_REGISTRY = Path("scripts/mcp/agent_hosts.json")


def parse_targets_from_python(py_file: Path) -> set[str]:
    """Parse TARGETS from a literal tuple or from the host registry the installer loads."""
    content = py_file.read_text(encoding="utf-8")
    literal = re.search(r'TARGETS\s*=\s*\((.*?)\)', content, re.DOTALL)
    if literal:
        return set(re.findall(r'"([^"]+)"', literal.group(1)))
    if re.search(r'TARGETS\s*=\s*tuple\(', content):
        registry = registry_for_installer(py_file)
        return installer_target_ids(registry) if registry else set()
    return set()


def registry_for_installer(py_file: Path) -> dict | None:
    """Load the sibling host registry, or the JSON embedded in a standalone installer."""
    sibling = py_file.with_name("agent_hosts.json")
    if sibling.is_file():
        return json.loads(sibling.read_text(encoding="utf-8"))
    return embedded_registry(py_file.read_text(encoding="utf-8"))


def embedded_registry(content: str) -> dict | None:
    match = re.search(r'EMBEDDED_AGENT_HOSTS_JSON\s*=\s*r"""(.*?)"""', content, re.DOTALL)
    if not match:
        return None
    loaded = json.loads(match.group(1))
    return loaded if isinstance(loaded, dict) else None


def installer_target_ids(registry: dict) -> set[str]:
    return {
        str(host["id"])
        for host in registry.get("hosts", [])
        if isinstance(host, dict) and host.get("installerTarget")
    }


def parse_installer_targets_from_java(java_file: Path) -> dict[str, str]:
    """Parse INSTALLER_TARGETS array and installerArgumentFor mapping from Java file.

    Returns dict: {INSTALLER_TARGET_NAME -> mapped_argument}
    """
    content = java_file.read_text(encoding="utf-8")

    # Parse INSTALLER_TARGETS array
    targets_match = re.search(
        r'static\s+final\s+String\[\]\s+INSTALLER_TARGETS\s*=\s*\{(.*?)\};',
        content,
        re.DOTALL
    )
    if not targets_match:
        return {}

    targets_str = targets_match.group(1)
    installer_targets = re.findall(r'"([^"]+)"', targets_str)

    # Parse installerArgumentFor switch statement
    # Match: public or private static String installerArgumentFor(String target) { return switch (normalize(...)) { ... }; }
    arg_for_match = re.search(
        r'(?:public|private)\s+static\s+String\s+installerArgumentFor\s*\(\s*String\s+target\s*\)\s*\{.*?return\s+switch\s*\(\s*normalize\s*\([^)]*\)\s*\)\s*\{(.*?)\s*\}\s*;',
        content,
        re.DOTALL
    )

    mapping = {}
    if arg_for_match:
        switch_body = arg_for_match.group(1)
        # Parse each case: case "CLAUDE_CODE" -> "claude";
        cases = re.findall(
            r'case\s+"([^"]+)"\s*->\s*"([^"]+)"',
            switch_body
        )
        for case_name, case_value in cases:
            mapping[case_name] = case_value

        # Parse default case: default -> "codex";
        default_match = re.search(r'default\s*->\s*"([^"]+)"', switch_body)
        if default_match:
            # The default case covers entries without explicit cases
            default_value = default_match.group(1)
            # CODEX doesn't have an explicit case, so it maps through default
            for target in installer_targets:
                if target not in mapping:
                    mapping[target] = default_value

    return mapping


def parse_java_string_list(java_text: str, constant: str) -> list[str]:
    """Parse a List.of or Set.of string constant. Order follows the source."""
    match = re.search(
        rf"\b{re.escape(constant)}\s*=\s*(?:List|Set)\.of\((.*?)\)\s*;",
        java_text,
        re.DOTALL,
    )
    if not match:
        return []
    return re.findall(r'"([^"]+)"', match.group(1))


def parse_installer_target_names(java_text: str) -> list[str]:
    match = re.search(
        r'static\s+final\s+String\[\]\s+INSTALLER_TARGETS\s*=\s*\{(.*?)\};',
        java_text,
        re.DOTALL,
    )
    if not match:
        return []
    return re.findall(r'"([^"]+)"', match.group(1))


def python_installer(root: Path) -> Path | None:
    primary = root / "scripts/mcp/install_shaft_agentic_tools.py"
    if primary.is_file():
        return primary
    legacy = root / "scripts/mcp/install_shaft_mcp.py"
    return legacy if legacy.is_file() else None


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    py_installer = python_installer(root)
    if py_installer is None:
        errors.append(f"missing Python installer: {root / 'scripts/mcp/install_shaft_agentic_tools.py'}")
        return errors
    targets = parse_targets_from_python(py_installer)
    java_setup = root / JAVA_SETUP
    errors.extend(validate_target_parity(py_installer, java_setup, targets))
    if java_setup.is_file():
        errors.extend(validate_installer_command(java_setup))
    errors.extend(validate_installer_scripts(root))
    errors.extend(validate_registry_parity(root, py_installer))
    return errors


def validate_target_parity(py_installer: Path, java_setup: Path, targets: set[str]) -> list[str]:
    errors: list[str] = []
    if not targets:
        errors.append(f"unable to parse TARGETS from {py_installer}")
        return errors
    if not java_setup.is_file():
        errors.append(f"missing Java setup panel: {java_setup}")
        return errors
    mapping = parse_installer_targets_from_java(java_setup)
    if not mapping:
        errors.append(f"unable to parse INSTALLER_TARGETS or installerArgumentFor from {java_setup}")
        return errors
    errors.extend(target_set_errors(targets, set(mapping.values()), py_installer, java_setup))
    for installer_target, mapped_value in mapping.items():
        if mapped_value not in targets:
            errors.append(
                f"installerArgumentFor('{installer_target}') maps to '{mapped_value}' "
                f"which is not in TARGETS; check {java_setup} and {py_installer}"
            )
    return errors


def target_set_errors(targets: set[str], java_targets: set[str], py_installer: Path, java_setup: Path) -> list[str]:
    errors: list[str] = []
    for target in sorted(targets - java_targets):
        errors.append(
            f"target '{target}' is in TARGETS ({py_installer}) "
            f"but missing from installerArgumentFor mapping ({java_setup})"
        )
    for target in sorted(java_targets - targets):
        errors.append(
            f"target '{target}' is in installerArgumentFor mapping ({java_setup}) "
            f"but missing from TARGETS ({py_installer})"
        )
    return errors


def validate_installer_command(java_setup: Path) -> list[str]:
    errors: list[str] = []
    java_content = java_setup.read_text(encoding="utf-8")
    if "/scripts/mcp/install-shaft-agentic-tools" not in java_content:
        errors.append(
            f"installerCommandFor must reference '/scripts/mcp/install-shaft-agentic-tools' in {java_setup}"
        )
    if "/scripts/mcp/install-shaft-mcp" in java_content:
        errors.append(
            f"installerCommandFor must not copy install-shaft-mcp as the primary command in {java_setup}"
        )
    return errors


def validate_installer_scripts(root: Path) -> list[str]:
    errors: list[str] = []
    errors.extend(require_text(root / "scripts/mcp/install-shaft-agentic-tools.ps1", None, "Windows installer script"))
    errors.extend(require_text(root / "scripts/mcp/install-shaft-agentic-tools.sh", None, "Unix installer script"))
    errors.extend(require_text(root / "scripts/mcp/install-shaft-mcp.ps1", "deprecat", "Windows deprecation shim"))
    errors.extend(require_text(root / "scripts/mcp/install-shaft-mcp.sh", "deprecat", "Unix deprecation shim"))
    errors.extend(one_liner_errors(root))
    return errors


def require_text(path: Path, needle: str | None, label: str) -> list[str]:
    if not path.is_file():
        return [f"missing {label}: {path}"]
    if needle is not None and needle not in path.read_text(encoding="utf-8").casefold():
        return [f"{label} must mention deprecation: {path}"]
    return []


def one_liner_errors(root: Path) -> list[str]:
    errors: list[str] = []
    windows = root / "scripts/mcp/install.ps1"
    unix = root / "scripts/mcp/install.sh"
    if windows.is_file() and "install-shaft-agentic-tools.ps1" not in windows.read_text(encoding="utf-8"):
        errors.append(f"install.ps1 must download install-shaft-agentic-tools.ps1: {windows}")
    if unix.is_file() and "install-shaft-agentic-tools.sh" not in unix.read_text(encoding="utf-8"):
        errors.append(f"install.sh must download install-shaft-agentic-tools.sh: {unix}")
    return errors


def validate_registry_parity(root: Path, py_installer: Path) -> list[str]:
    """Exact host parity. Skipped when a fixture has no canonical registry."""
    registry_path = root / HOST_REGISTRY
    if not registry_path.is_file():
        return []
    registry = json.loads(registry_path.read_text(encoding="utf-8"))
    errors = embedded_registry_errors(py_installer, registry)
    errors.extend(service_registry_errors(root / JAVA_SERVICE, registry))
    errors.extend(panel_registry_errors(root / JAVA_SETUP, registry))
    errors.extend(assembler_registry_errors(root / ASSEMBLER, registry))
    return errors


def embedded_registry_errors(py_installer: Path, registry: dict) -> list[str]:
    embedded = embedded_registry(py_installer.read_text(encoding="utf-8"))
    if embedded is None:
        return []
    if embedded != registry:
        return [f"embedded host registry in {py_installer} differs from {HOST_REGISTRY}"]
    return []


def service_registry_errors(java_service: Path, registry: dict) -> list[str]:
    if not java_service.is_file():
        return [f"missing ShaftProjectService: {java_service}"]
    text = java_service.read_text(encoding="utf-8")
    hosts = [host for host in registry.get("hosts", []) if isinstance(host, dict)]
    errors = list_parity_errors(
        text, "ALL_AGENT_HOSTS",
        [str(host["id"]) for host in hosts if host.get("initAgentsSupported")],
        java_service,
    )
    errors.extend(list_parity_errors(
        text, "AGENTS_MD_HOSTS",
        [str(host["id"]) for host in hosts if host.get("initAgentsSupported") and host.get("instructionFile") == "AGENTS.md"],
        java_service,
    ))
    errors.extend(list_parity_errors(
        text, "CANONICAL_SKILL_HOSTS",
        [str(host["id"]) for host in hosts if host.get("initAgentsSupported") and host.get("skillRoot") == ".agents/skills"],
        java_service,
    ))
    errors.extend(list_parity_errors(
        text, "RETIRED_SHAFT_SKILL_DIRECTORIES",
        [str(name) for name in registry.get("retiredSkillDirectories", [])],
        java_service,
    ))
    if 'resolve(".github/instructions")' in text:
        errors.append(f"{java_service} must not copy skills into .github/instructions")
    if '".github/copilot-instructions.md"' not in text:
        errors.append(f"{java_service} must write .github/copilot-instructions.md")
    return errors


def list_parity_errors(java_text: str, constant: str, expected: list[str], java_file: Path) -> list[str]:
    actual = parse_java_string_list(java_text, constant)
    if actual == expected:
        return []
    return [f"{java_file} {constant} is {actual}, expected {expected}"]


def panel_registry_errors(java_setup: Path, registry: dict) -> list[str]:
    if not java_setup.is_file():
        return [f"missing Java setup panel: {java_setup}"]
    text = java_setup.read_text(encoding="utf-8")
    hosts = [host for host in registry.get("hosts", []) if isinstance(host, dict) and host.get("installerTarget")]
    expected_names = [str(host["intellijTarget"]) for host in hosts]
    actual_names = parse_installer_target_names(text)
    errors: list[str] = []
    if actual_names != expected_names:
        errors.append(f"{java_setup} INSTALLER_TARGETS is {actual_names}, expected {expected_names}")
    mapping = parse_installer_targets_from_java(java_setup)
    for host in hosts:
        targetName = str(host["intellijTarget"])
        host_id = str(host["id"])
        if mapping.get(targetName) != host_id:
            errors.append(
                f"{java_setup} installerArgumentFor('{targetName}') is {mapping.get(targetName)!r}, expected {host_id!r}"
            )
    return errors


def assembler_registry_errors(assembler: Path, registry: dict) -> list[str]:
    if not assembler.is_file():
        return [f"missing skills assembler: {assembler}"]
    text = assembler.read_text(encoding="utf-8")
    errors: list[str] = []
    if 'CONSUMER_SKILL_PREFIX = "shaft-developer/"' not in text:
        errors.append(f"{assembler} must package only shaft-developer/")
    hosts = [host for host in registry.get("hosts", []) if isinstance(host, dict)]
    for skill_root in sorted({str(host["skillRoot"]) for host in hosts if host.get("skillRoot")}):
        if skill_root not in text:
            errors.append(f"{assembler} is missing skill root {skill_root}")
    for instruction in sorted({str(host["instructionFile"]) for host in hosts if host.get("instructionFile")}):
        if instruction not in text and Path(instruction).name not in text:
            errors.append(f"{assembler} is missing instruction file {instruction}")
    return errors


def main() -> int:
    errors = validate()
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Installer contract between Python TARGETS, Java installerArgumentFor, and shell scripts is valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
