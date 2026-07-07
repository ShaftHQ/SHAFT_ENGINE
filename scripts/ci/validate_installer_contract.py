#!/usr/bin/env python3
"""Validate installer contract between Python, Java, and shell scripts."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def parse_targets_from_python(py_file: Path) -> set[str]:
    """Parse TARGETS tuple from install_shaft_mcp.py using regex."""
    content = py_file.read_text(encoding="utf-8")
    # Match: TARGETS = ("codex", "claude", ...)
    match = re.search(r'TARGETS\s*=\s*\((.*?)\)', content, re.DOTALL)
    if not match:
        return set()
    targets_str = match.group(1)
    # Extract quoted strings
    targets = re.findall(r'"([^"]+)"', targets_str)
    return set(targets)


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
    # Match: private static String installerArgumentFor(String target) { return switch (normalize(target, "CODEX")) { ... }; }
    arg_for_match = re.search(
        r'private\s+static\s+String\s+installerArgumentFor\s*\(\s*String\s+target\s*\)\s*\{.*?return\s+switch\s*\(\s*normalize\s*\([^)]*\)\s*\)\s*\{(.*?)\s*\}\s*;',
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


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []

    # Parse TARGETS from Python installer
    py_installer = root / "scripts/mcp/install_shaft_mcp.py"
    if not py_installer.is_file():
        errors.append(f"missing Python installer: {py_installer}")
        return errors

    targets = parse_targets_from_python(py_installer)
    if not targets:
        errors.append(f"unable to parse TARGETS from {py_installer}")
        return errors

    # Parse installer argument mapping from Java
    java_setup = root / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftMcpSetupPanel.java"
    if not java_setup.is_file():
        errors.append(f"missing Java setup panel: {java_setup}")
        return errors

    installer_targets_mapping = parse_installer_targets_from_java(java_setup)
    if not installer_targets_mapping:
        errors.append(f"unable to parse INSTALLER_TARGETS or installerArgumentFor from {java_setup}")
        return errors

    # Extract the set of mapped values from Java mapping
    java_targets = set(installer_targets_mapping.values())

    # Verify set equality
    if targets != java_targets:
        missing_in_java = targets - java_targets
        extra_in_java = java_targets - targets

        if missing_in_java:
            for target in sorted(missing_in_java):
                errors.append(
                    f"target '{target}' is in TARGETS ({py_installer}) "
                    f"but missing from installerArgumentFor mapping ({java_setup})"
                )

        if extra_in_java:
            for target in sorted(extra_in_java):
                errors.append(
                    f"target '{target}' is in installerArgumentFor mapping ({java_setup}) "
                    f"but missing from TARGETS ({py_installer})"
                )

    # Verify every installer target resolves through the mapping
    for installer_target, mapped_value in installer_targets_mapping.items():
        if mapped_value not in targets:
            errors.append(
                f"installerArgumentFor('{installer_target}') maps to '{mapped_value}' "
                f"which is not in TARGETS; check {java_setup} and {py_installer}"
            )

    # Parse and verify installer command references in Java
    java_content = java_setup.read_text(encoding="utf-8")
    # installerCommandFor builds: /scripts/mcp/install-shaft-mcp + .ps1 or .sh
    if "/scripts/mcp/install-shaft-mcp" not in java_content:
        errors.append(
            f"installerCommandFor must reference '/scripts/mcp/install-shaft-mcp' in {java_setup}"
        )

    # Verify shell scripts exist
    ps1_installer = root / "scripts/mcp/install-shaft-mcp.ps1"
    sh_installer = root / "scripts/mcp/install-shaft-mcp.sh"

    if not ps1_installer.is_file():
        errors.append(
            f"missing Windows installer script referenced by installerCommandFor: {ps1_installer}"
        )

    if not sh_installer.is_file():
        errors.append(
            f"missing Unix installer script referenced by installerCommandFor: {sh_installer}"
        )

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
