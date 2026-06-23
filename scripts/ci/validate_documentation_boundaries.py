#!/usr/bin/env python3
"""Enforce the engine repository's operational Markdown boundary."""

from fnmatch import fnmatch
import os
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
DOCS_BASE = "https://shaftengine.netlify.app/docs/"

ALLOWED_EXACT = {
    "README.md",
    "AGENTS.md",
    "CLAUDE.md",
    "CODE_OF_CONDUCT.md",
    "CONTRIBUTING.md",
    "SECURITY.md",
    ".github/copilot-instructions.md",
    ".github/pull_request_template.md",
    ".github/RELEASE_BODY_TEMPLATE.md",
    ".github/skills/README.md",
    ".github/workflows/README.md",
    "graphify-out/GRAPH_REPORT.md",
    "shaft-mcp/.github/copilot-instructions.md",
}
ALLOWED_GLOBS = (
    ".agents/skills/*/SKILL.md",
    ".github/codex/prompts/*.md",
    ".github/instructions/*.instructions.md",
    ".github/ISSUE_TEMPLATE/*.md",
    ".github/skills/*/SKILL.md",
    ".memory/memory/*.md",
    ".memory/memory/**/*.md",
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
    "node_modules",
    "target",
}


def tracked_markdown(root: Path = ROOT) -> list[str]:
    paths: list[str] = []
    for directory, child_directories, files in os.walk(root):
        child_directories[:] = [
            child for child in child_directories
            if child not in IGNORED_DIRECTORIES
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
            if path not in {".github/skills/README.md", ".github/workflows/README.md"}:
                errors.append(f"non-root README is prohibited: {path}")

    readme = (root / "README.md").read_text(encoding="utf-8")
    if len(readme.splitlines()) > 160:
        errors.append("README.md exceeds the 160-line landing-page budget")
    for route in (
        "start/overview",
        "start/installation",
        "start/upgrade",
        "testing/web",
        "testing/mobile",
        "testing/api",
        "agentic/mcp",
        "agentic/doctor",
        "agentic/heal",
    ):
        if f"{DOCS_BASE}{route}" not in readme:
            errors.append(f"README.md is missing canonical route: {route}")

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
