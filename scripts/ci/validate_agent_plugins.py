"""Validate the portable Agent Plugin v1.0.0 package contract (#4576)."""

import argparse
import json
import os
import re
from pathlib import Path

import yaml

SCHEMA_URL = "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json"
MANIFEST_PATH = "plugin.json"
ALLOWED_MANIFEST_FIELDS = {
    "$schema",
    "name",
    "version",
    "description",
    "author",
    "homepage",
    "repository",
    "license",
    "keywords",
    "extensions",
}
PLUGIN_NAME = re.compile(r"^(?!.*(?:--|\.\.))[a-z0-9](?:[a-z0-9.-]{0,62}[a-z0-9])?$")
SKILL_NAME = re.compile(r"^(?!.*--)[a-z0-9](?:[a-z0-9-]{0,62}[a-z0-9])?$")


def issue(code: str, path: Path | str, message: str, severity: str = "error") -> dict[str, str]:
    """Return one stable, machine-readable validation finding."""
    return {"code": code, "path": str(path).replace("\\", "/"), "message": message, "severity": severity}


def resolves_inside(root: Path, candidate: Path) -> bool:
    """Whether `candidate`, including links, remains within package `root`."""
    try:
        candidate.resolve(strict=False).relative_to(root.resolve(strict=False))
    except (OSError, ValueError):
        return False
    return True


def has_directory_entry(path: Path) -> bool:
    """Detect a path entry without following a dangling link or junction."""
    return os.path.lexists(path)


def parse_skill_frontmatter(content: str) -> dict | None:
    """Load complete YAML frontmatter without accepting an unclosed marker."""
    content = content.replace("\r\n", "\n").replace("\r", "\n")
    if not content.startswith("---\n"):
        return None
    marker = content.find("\n---", 4)
    if marker < 0:
        return None
    after_marker = content[marker + 4 :]
    if after_marker and not after_marker.startswith("\n"):
        return None
    try:
        frontmatter = yaml.safe_load(content[4:marker])
    except yaml.YAMLError:
        return None
    return frontmatter if isinstance(frontmatter, dict) else None


def manifest_field_errors(manifest: dict) -> list[dict[str, str]]:
    """Validate every schema-governed manifest field that is present."""
    findings: list[dict[str, str]] = []
    string_fields = {"version", "description", "homepage", "repository", "license"}
    for field in sorted(string_fields & set(manifest)):
        if not isinstance(manifest[field], str):
            findings.append(issue("manifest-field", MANIFEST_PATH, f"{field} must be a string"))
    if "keywords" in manifest and (
        not isinstance(manifest["keywords"], list) or not all(isinstance(item, str) for item in manifest["keywords"])
    ):
        findings.append(issue("manifest-field", MANIFEST_PATH, "keywords must be an array of strings"))
    if "author" in manifest:
        author = manifest["author"]
        if (
            not isinstance(author, dict)
            or set(author) - {"name", "email", "url"}
            or not all(isinstance(value, str) for value in author.values())
        ):
            findings.append(issue("manifest-field", MANIFEST_PATH, "author must contain only string name, email, or url fields"))
    if "extensions" in manifest:
        extensions = manifest["extensions"]
        if not isinstance(extensions, dict):
            findings.append(
                issue(
                    "extensions-invalid",
                    MANIFEST_PATH,
                    "extensions must be an object and is ignored by this validator",
                    "warning",
                )
            )
        elif not all(isinstance(value, dict) for value in extensions.values()):
            findings.append(issue("manifest-field", MANIFEST_PATH, "each extensions namespace must contain an object"))
    return findings


def validate_manifest(root: Path) -> tuple[dict | None, list[dict[str, str]]]:
    """Load and validate the required core manifest."""
    manifest_path = root / MANIFEST_PATH
    if not resolves_inside(root, manifest_path):
        return None, [issue("component-escapes-root", MANIFEST_PATH, "plugin.json must remain inside the package")]
    if not manifest_path.is_file():
        return None, [issue("plugin-manifest", MANIFEST_PATH, "plugin.json must be a file")]
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        return None, [issue("plugin-manifest", MANIFEST_PATH, f"plugin.json is not valid JSON: {error}")]
    if not isinstance(manifest, dict):
        return None, [issue("plugin-manifest", MANIFEST_PATH, "plugin.json must contain a JSON object")]

    findings: list[dict[str, str]] = []
    if manifest.get("$schema") != SCHEMA_URL:
        findings.append(issue("plugin-schema", MANIFEST_PATH, f"$schema must be {SCHEMA_URL}"))
    name = manifest.get("name")
    if not isinstance(name, str) or not PLUGIN_NAME.fullmatch(name):
        findings.append(
            issue(
                "plugin-name",
                MANIFEST_PATH,
                "name must be 1-64 lowercase letters, digits, dots, or hyphens without repeated dots or hyphens",
            )
        )
    findings.extend(manifest_field_errors(manifest))
    for field in sorted(set(manifest) - ALLOWED_MANIFEST_FIELDS):
        findings.append(
            issue(
                "manifest-unknown-field",
                MANIFEST_PATH,
                f"unknown manifest field is ignored for forward compatibility: {field}",
                "warning",
            )
        )
    return manifest, findings


def validate_skills(root: Path) -> list[dict[str, str]]:
    """Validate immediate Agent Skills without requiring host-specific policy."""
    skills_path = root / "skills"
    if not has_directory_entry(skills_path):
        return []
    if not resolves_inside(root, skills_path):
        return [issue("component-escapes-root", "skills", "skills must remain inside the package")]
    if not skills_path.is_dir():
        return [issue("component-invalid", "skills", "skills must be a directory when present")]

    findings: list[dict[str, str]] = []
    for child in sorted(skills_path.iterdir(), key=lambda entry: entry.name):
        skill_path = child / "SKILL.md"
        if not child.is_dir() or not has_directory_entry(skill_path):
            continue
        relative_path = skill_path.relative_to(root)
        if not resolves_inside(root, skill_path):
            findings.append(issue("component-escapes-root", relative_path, "SKILL.md must remain inside the package"))
            continue
        try:
            skill = skill_path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as error:
            findings.append(issue("skill-read", relative_path, f"SKILL.md cannot be read: {error}"))
            continue
        frontmatter = parse_skill_frontmatter(skill)
        if frontmatter is None:
            findings.append(issue("skill-frontmatter", relative_path, "SKILL.md must start with YAML frontmatter"))
            continue
        name = frontmatter.get("name")
        if not isinstance(name, str) or not SKILL_NAME.fullmatch(name) or name != child.name:
            findings.append(
                issue("skill-name", relative_path, "frontmatter name must be a valid skill name matching its directory")
            )
        description = frontmatter.get("description")
        if not isinstance(description, str) or not 1 <= len(description) <= 1024:
            findings.append(
                issue("skill-description", relative_path, "frontmatter description must contain 1-1024 characters")
            )
        optional_string_limits = {"license": None, "compatibility": 500, "allowed-tools": None}
        for field, maximum_length in optional_string_limits.items():
            if field not in frontmatter:
                continue
            value = frontmatter[field]
            if not isinstance(value, str) or (maximum_length is not None and not 1 <= len(value) <= maximum_length):
                findings.append(issue("skill-field", relative_path, f"{field} must be a valid string"))
        if "metadata" in frontmatter:
            metadata = frontmatter["metadata"]
            if not isinstance(metadata, dict) or not all(
                isinstance(key, str) and isinstance(value, str) for key, value in metadata.items()
            ):
                findings.append(issue("skill-field", relative_path, "metadata must map string keys to string values"))
    return findings


def validate_package(root: Path) -> list[dict[str, str]]:
    """Return contract findings for one portable Agent Plugin package root."""
    root = Path(root)
    manifest, findings = validate_manifest(root)
    if manifest is None:
        return findings
    if any(finding["severity"] == "error" for finding in findings):
        return findings
    return findings + validate_skills(root)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("package", type=Path, help="Agent Plugin package root")
    arguments = parser.parse_args()
    findings = validate_package(arguments.package)
    for finding in findings:
        print(f"{finding['severity']}: {finding['path']}: {finding['code']}: {finding['message']}")
    return 1 if any(finding["severity"] == "error" for finding in findings) else 0


if __name__ == "__main__":
    raise SystemExit(main())
