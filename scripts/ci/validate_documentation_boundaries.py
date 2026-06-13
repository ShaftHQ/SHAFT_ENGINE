#!/usr/bin/env python3
"""Enforce the engine repository's operational Markdown boundary."""

from fnmatch import fnmatch
from pathlib import Path
import subprocess
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
    "shaft-mcp/.github/copilot-instructions.md",
}
ALLOWED_GLOBS = (
    ".agents/skills/*/SKILL.md",
    ".github/codex/prompts/*.md",
    ".github/instructions/*.instructions.md",
    ".github/ISSUE_TEMPLATE/*.md",
    ".github/skills/*/SKILL.md",
    "*/src/test/resources/fixtures/**/*.md",
)
FORBIDDEN_LINK_FRAGMENTS = (
    "github.com/ShaftHQ/SHAFT_ENGINE/blob/main/docs/",
    "github.com/ShaftHQ/SHAFT_ENGINE/tree/main/docs/",
)


def tracked_markdown() -> list[str]:
    result = subprocess.run(
        [
            "git",
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "--",
            "*.md",
            "*.mdx",
        ],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    paths = [line.strip().replace("\\", "/") for line in result.stdout.splitlines()]
    return [path for path in paths if (ROOT / path).is_file()]


def is_allowed(path: str) -> bool:
    return path in ALLOWED_EXACT or any(fnmatch(path, pattern) for pattern in ALLOWED_GLOBS)


def main() -> int:
    errors: list[str] = []
    markdown = tracked_markdown()

    for path in markdown:
        if not is_allowed(path):
            errors.append(f"public or unapproved Markdown remains: {path}")
        if path != "README.md" and Path(path).name.lower() == "readme.md":
            if path != ".github/skills/README.md":
                errors.append(f"non-root README is prohibited: {path}")

    readme = (ROOT / "README.md").read_text(encoding="utf-8")
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
        content = (ROOT / path).read_text(encoding="utf-8")
        for fragment in FORBIDDEN_LINK_FRAGMENTS:
            if fragment in content:
                errors.append(f"{path}: links to deleted repository documentation")

    if (ROOT / "docs").exists():
        errors.append("the local docs/ tree must not exist")

    if errors:
        print("\n".join(f"ERROR: {error}" for error in errors), file=sys.stderr)
        return 1
    print(f"Documentation boundary is valid ({len(markdown)} tracked Markdown files).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
