#!/usr/bin/env python3
"""Enforce the engine repository's operational Markdown boundary."""

from fnmatch import fnmatch
import os
from pathlib import Path
import sys

try:
    from scripts.ci.readme_contract import (
        destinations,
        validate_readme_contract,
    )
except ModuleNotFoundError:  # Direct script execution adds scripts/ci to sys.path.
    from readme_contract import destinations, validate_readme_contract

ROOT = Path(__file__).resolve().parents[2]
RELEASES_URL = "https://github.com/ShaftHQ/SHAFT_ENGINE/releases"

ALLOWED_EXACT = {
    "README.md",
    "modular-era-feature-catalog.md",
    "AGENTS.md",
    "CLAUDE.md",
    "CODE_OF_CONDUCT.md",
    "CONTRIBUTING.md",
    "SECURITY.md",
    ".github/copilot-instructions.md",
    ".github/pull_request_template.md",
    ".github/RELEASE_BODY_TEMPLATE.md",
    # Internal agent guidance, not a public guide or a module README. Needs an
    # exact entry because ALLOWED_GLOBS uses fnmatch, where "**" is ordinary
    # "*" and so ".agents/skills/**/*.md" requires a second path segment --
    # the same reason ".github/skills/README.md" is listed here.
    ".agents/skills/README.md",
    ".github/skills/README.md",
    ".github/workflows/README.md",
    "shaft-mcp/.github/copilot-instructions.md",
    "tools/repository-map/README.md",
    "chaos-engine/RESEARCH.md",
    "chaos-engine/INSTALL.md",
}
ALLOWED_GLOBS = (
    ".agents/skills/**/*.md",
    ".claude/agents/*.md",
    ".claude/skills/**/*.md",
    ".claude/user-harness/*.md",
    ".codex/**/*.md",
    ".github/codex/prompts/*.md",
    ".github/instructions/*.instructions.md",
    ".github/ISSUE_TEMPLATE/*.md",
    ".github/skills/**/*.md",
    "chaos-engine/**/*.md",
    ".memory/memory/*.md",
    ".memory/memory/**/*.md",
    "skills/*/SKILL.md",
    "shaft-skills/*.md",
    "shaft-skills/**/*.md",
    "agent-plugins/**/*.md",
    "tools/**/*.md",
    "*/src/test/resources/fixtures/**/*.md",
)
FORBIDDEN_LINK_FRAGMENTS = (
    "github.com/ShaftHQ/SHAFT_ENGINE/blob/main/docs/",
    "github.com/ShaftHQ/SHAFT_ENGINE/tree/main/docs/",
)
IGNORED_DIRECTORIES = {
    ".git",
    ".idea",
    "allure-report",
    "allure-results",
    "build",
    "graphify-out",
    "node_modules",
    "target",
}
# Root-relative directories (concurrent agent worktrees and memory recovery areas hold full repo copies).
IGNORED_RELATIVE_DIRECTORIES = {
    ".claude/worktrees",
    ".memory/recovery",
}


def tracked_markdown(root: Path = ROOT) -> list[str]:
    paths: list[str] = []
    for directory, child_directories, files in os.walk(root):
        relative_directory = Path(directory).relative_to(root).as_posix()
        child_directories[:] = [
            child for child in child_directories
            if child not in IGNORED_DIRECTORIES
            and (
                child if relative_directory == "." else f"{relative_directory}/{child}"
            ) not in IGNORED_RELATIVE_DIRECTORIES
        ]
        for filename in files:
            if Path(filename).suffix.lower() not in {".md", ".mdx"}:
                continue
            paths.append(
                (Path(directory) / filename).relative_to(root).as_posix()
            )
    return sorted(paths)


def is_allowed(path: str) -> bool:
    return path in ALLOWED_EXACT or any(fnmatch(path, pattern) for pattern in ALLOWED_GLOBS)


def validate_repository(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    markdown = tracked_markdown(root)

    for path in markdown:
        if not is_allowed(path):
            errors.append(f"public or unapproved Markdown remains: {path}")
        if path != "README.md" and Path(path).name.lower() == "readme.md":
            if not is_allowed(path) or path.startswith("docs/"):
                errors.append(f"non-root README is prohibited: {path}")

    readme = (root / "README.md").read_text(encoding="utf-8")
    readme_links = set(destinations(readme))
    if len(readme.splitlines()) > 160:
        errors.append("README.md exceeds the 160-line landing-page budget")
    errors.extend(validate_readme_contract(readme))
    if "https://github.com/sponsors/MohabMohie" not in readme_links:
        errors.append("README.md is missing the GitHub Sponsors call to action")

    catalog_path = root / "modular-era-feature-catalog.md"
    if catalog_path.is_file():
        catalog_links = set(destinations(catalog_path.read_text(encoding="utf-8")))
        if RELEASES_URL not in catalog_links:
            errors.append(
                "modular-era-feature-catalog.md is missing the canonical release-history link"
            )

    scan_paths = [
        path for path in markdown
        if path != "shaft-doctor/src/test/resources/fixtures/golden/doctor-report.md"
    ]
    for path in scan_paths:
        content = (root / path).read_text(encoding="utf-8")
        for fragment in FORBIDDEN_LINK_FRAGMENTS:
            if fragment in content:
                errors.append(f"{path}: links to deleted repository documentation")

    if (root / "docs").exists():
        errors.append("the local docs/ tree must not exist")

    return errors


def main() -> int:
    errors = validate_repository()
    if errors:
        print("\n".join(f"ERROR: {error}" for error in errors), file=sys.stderr)
        return 1
    print(
        f"Documentation boundary is valid ({len(tracked_markdown())} tracked Markdown files)."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
