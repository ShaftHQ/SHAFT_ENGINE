#!/usr/bin/env python3
"""Lint .claude/skills/*/SKILL.md for structural hygiene.

Checks frontmatter presence/well-formedness, description quality (states
what the skill does and when to use it), dead `references/*.md` links
(resolved relative to each SKILL.md's own directory), and the per-skill byte
budget. The byte budget is read from the same `agent_guidance_budget.json`
`validate_agent_setup.py` already uses, not duplicated here — see
`skill_budgets[".claude/skills"].max_skill_md_bytes`.

Adapted (not copied) from bmad-method's `tools/skill-validator.md` rule
catalog (MIT-licensed), trimmed to the rules that fit this repo's skill
authoring conventions: no `bmad-`-prefixed name requirement, no step-file or
workflow-variable rules (SHAFT skills are prose, not the bmad step-machine).
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci.validate_agent_guidance import (  # noqa: E402
    load_budget,
    markdown_body,
    parse_frontmatter,
    relative,
)

SKILLS_ROOT = ".claude/skills"
MIN_DESCRIPTION_CHARS = 20
# Loose, deliberately broad signal set for "states when to use it" -- this is
# a MEDIUM-severity hygiene nudge, not a grammar check; see bmad's own
# SKILL-06 detection note ("absence of a trigger phrase suggests...").
WHEN_TO_USE_SIGNALS = (
    "use when",
    "use for",
    "use on",
    "use if",
    "use during",
    "before ",
    "trigger",
    "invoke",
    "load at",
    "start of",
    "when the",
    "when working",
    "when implementing",
)
REFERENCE_BACKTICK_PATTERN = re.compile(r"`(references/[^`]+\.md)`")
REFERENCE_LINK_PATTERN = re.compile(r"\[[^\]]*\]\((references/[^)#]+\.md)")


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable skill-hygiene issue."""
    return {"code": code, "path": path, "message": message}


def referenced_markdown_paths(content: str) -> list[str]:
    """Return every `references/*.md` path cited by backtick or Markdown link."""
    found = REFERENCE_BACKTICK_PATTERN.findall(content)
    found.extend(REFERENCE_LINK_PATTERN.findall(content))
    return found


def validate_skill(
    root: Path, skill_dir: Path, max_skill_md_bytes: int
) -> list[dict[str, str]]:
    """Validate one skill directory's SKILL.md against the hygiene rules."""
    skill_path = skill_dir / "SKILL.md"
    skill_relative = relative(root, skill_path)
    if not skill_path.is_file():
        return [
            issue("skill-missing", skill_relative, "skill directory must contain SKILL.md")
        ]

    content = skill_path.read_text(encoding="utf-8")
    frontmatter = parse_frontmatter(content)
    if frontmatter is None:
        return [
            issue(
                "frontmatter-malformed",
                skill_relative,
                "SKILL.md must open with a well-formed --- frontmatter block",
            )
        ]

    errors: list[dict[str, str]] = []

    name = frontmatter.get("name")
    if not name:
        errors.append(issue("frontmatter-name", skill_relative, "frontmatter name is required"))
    elif name != skill_dir.name:
        errors.append(
            issue(
                "frontmatter-name",
                skill_relative,
                f"frontmatter name '{name}' must match directory '{skill_dir.name}'",
            )
        )

    description = frontmatter.get("description", "")
    if not description:
        errors.append(
            issue("frontmatter-description", skill_relative, "frontmatter description is required")
        )
    else:
        if len(description) < MIN_DESCRIPTION_CHARS:
            errors.append(
                issue(
                    "description-too-thin",
                    skill_relative,
                    f"description is only {len(description)} characters; state what the skill does",
                )
            )
        lowered = description.lower()
        if not any(signal in lowered for signal in WHEN_TO_USE_SIGNALS):
            errors.append(
                issue(
                    "description-missing-trigger",
                    skill_relative,
                    "description must state when to use the skill "
                    "(e.g. 'Use when ...', 'before ...', a named trigger word/phrase)",
                )
            )

    if not markdown_body(content):
        errors.append(
            issue("body-empty", skill_relative, "SKILL.md must have body content after the frontmatter")
        )

    for raw_target in referenced_markdown_paths(content):
        target = skill_dir / raw_target
        if not target.is_file():
            errors.append(
                issue("dead-reference", skill_relative, f"references a missing file: {raw_target}")
            )

    actual_bytes = len(content.encode("utf-8"))
    if actual_bytes > max_skill_md_bytes:
        errors.append(
            issue(
                "byte-budget",
                skill_relative,
                f"{actual_bytes} bytes exceeds configured maximum {max_skill_md_bytes}",
            )
        )

    return errors


def validate_repository(
    root: Path = ROOT, budget_path: Path | None = None
) -> list[dict[str, str]]:
    """Run every skill-hygiene check across .claude/skills and return sorted issues."""
    budget = load_budget(budget_path or root / "scripts/ci/agent_guidance_budget.json")
    max_skill_md_bytes = budget.get("skill_budgets", {}).get(SKILLS_ROOT, {}).get(
        "max_skill_md_bytes"
    )
    if max_skill_md_bytes is None:
        return [
            issue(
                "budget-config",
                "scripts/ci/agent_guidance_budget.json",
                f"skill_budgets[{SKILLS_ROOT!r}].max_skill_md_bytes is not configured",
            )
        ]

    skills_root = root / SKILLS_ROOT
    if not skills_root.is_dir():
        return [issue("skill-root-missing", SKILLS_ROOT, "skills root is missing")]

    errors: list[dict[str, str]] = []
    for skill_dir in sorted(path for path in skills_root.iterdir() if path.is_dir()):
        errors.extend(validate_skill(root, skill_dir, max_skill_md_bytes))

    return sorted(errors, key=lambda item: (item["path"], item["code"], item["message"]))


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT, help="Repository root to validate.")
    parser.add_argument(
        "--budget",
        type=Path,
        help="Budget JSON path. Defaults to scripts/ci/agent_guidance_budget.json under --root.",
    )
    parser.add_argument("--format", choices=("text", "json"), default="text")
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    root = args.root.resolve()
    budget_path = args.budget.resolve() if args.budget else None
    errors = validate_repository(root, budget_path)
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors}, indent=2))
    elif errors:
        for error in errors:
            print(f"{error['code']}: {error['path']}: {error['message']}", file=sys.stderr)
    else:
        print(
            "Claude skill hygiene is valid: frontmatter, descriptions, "
            "references, and byte budgets all pass."
        )
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
