#!/usr/bin/env python3
"""Validate agent guidance budgets, routing, and cost guardrails."""

from __future__ import annotations

import argparse
import json
import math
import re
import sys
from collections import defaultdict
from pathlib import Path
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[2]


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable validation issue."""
    return {"code": code, "path": path, "message": message}


def load_budget(path: Path) -> dict:
    """Load the JSON guidance budget."""
    return json.loads(path.read_text(encoding="utf-8"))


def relative(root: Path, path: Path) -> str:
    """Return a repository-relative POSIX path."""
    return path.resolve().relative_to(root.resolve()).as_posix()


def expand_globs(root: Path, patterns: list[str]) -> list[Path]:
    """Expand configured repository-relative files and globs."""
    paths: set[Path] = set()
    for pattern in patterns:
        if any(character in pattern for character in "*?["):
            paths.update(path for path in root.glob(pattern) if path.is_file())
        else:
            path = root / pattern
            if path.is_file():
                paths.add(path)
    return sorted(paths)


def validate_file_budgets(root: Path, budget: dict) -> list[dict[str, str]]:
    """Validate byte, character, and line budgets."""
    errors: list[dict[str, str]] = []
    for configured_path, limits in budget.get("file_budgets", {}).items():
        path = root / configured_path
        if not path.is_file():
            errors.append(issue("missing-file", configured_path, "required guidance file is missing"))
            continue
        content = path.read_text(encoding="utf-8")
        checks = (
            ("max_bytes", len(content.encode("utf-8")), "size-budget", "bytes"),
            ("max_chars", len(content), "character-budget", "characters"),
            ("max_lines", len(content.splitlines()), "line-budget", "lines"),
        )
        for key, actual, code, unit in checks:
            maximum = limits.get(key)
            if maximum is not None and actual > maximum:
                errors.append(
                    issue(code, configured_path, f"{actual} {unit} exceeds configured maximum {maximum}")
                )
    return errors


def validate_host_contexts(root: Path, budget: dict) -> list[dict[str, str]]:
    """Estimate auto-loaded host context with the documented character proxy."""
    errors: list[dict[str, str]] = []
    maximum = budget.get("max_estimated_tokens_per_host")
    if maximum is None:
        return errors
    for host, configured_paths in budget.get("host_contexts", {}).items():
        characters = 0
        missing: list[str] = []
        for configured_path in configured_paths:
            path = root / configured_path
            if path.is_file():
                characters += len(path.read_text(encoding="utf-8"))
            else:
                missing.append(configured_path)
        if missing:
            errors.append(
                issue("host-context-missing", host, f"missing context files: {', '.join(missing)}")
            )
            continue
        estimated_tokens = math.ceil(characters / 4)
        if estimated_tokens > maximum:
            errors.append(
                issue(
                    "host-token-budget",
                    host,
                    f"estimated {estimated_tokens} tokens exceeds configured maximum {maximum}",
                )
            )
    return errors


def validate_total_reduction(root: Path, budget: dict) -> list[dict[str, str]]:
    """Keep the consolidated guidance below the approved baseline reduction."""
    baseline = budget.get("reduction_baseline_bytes")
    minimum_reduction = budget.get("minimum_reduction_percent")
    if baseline is None or minimum_reduction is None:
        return []
    paths = expand_globs(root, budget.get("total_guidance_globs", []))
    current = sum(path.stat().st_size for path in paths)
    maximum = math.floor(baseline * (1 - minimum_reduction / 100))
    if current > maximum:
        return [
            issue(
                "total-reduction",
                "agent-guidance",
                f"{current} bytes exceeds {maximum}; minimum reduction is {minimum_reduction}%",
            )
        ]
    return []


def parse_frontmatter(content: str) -> dict[str, str] | None:
    """Parse simple top-level YAML frontmatter without external dependencies."""
    if not content.startswith("---\n"):
        return None
    marker = content.find("\n---\n", 4)
    if marker < 0:
        return None
    values: dict[str, str] = {}
    for raw_line in content[4:marker].splitlines():
        if ":" not in raw_line or raw_line[:1].isspace():
            continue
        key, value = raw_line.split(":", 1)
        values[key.strip()] = value.strip().strip("\"'")
    return values


def validate_skills(root: Path, budget: dict) -> list[dict[str, str]]:
    """Validate discoverable skill directories and required frontmatter."""
    errors: list[dict[str, str]] = []
    skills_root = root / budget.get("skills_root", ".github/skills")
    if not skills_root.is_dir():
        return [issue("skill-root-missing", relative(root, skills_root), "skills root is missing")]
    for directory in sorted(path for path in skills_root.iterdir() if path.is_dir()):
        if not any(path.is_file() for path in directory.rglob("*")):
            continue
        skill_path = directory / "SKILL.md"
        skill_relative = relative(root, skill_path)
        if not skill_path.is_file():
            errors.append(issue("skill-missing", skill_relative, "skill directory must contain SKILL.md"))
            continue
        frontmatter = parse_frontmatter(skill_path.read_text(encoding="utf-8"))
        if frontmatter is None:
            errors.append(issue("skill-frontmatter", skill_relative, "valid YAML frontmatter is required"))
            continue
        if frontmatter.get("name") != directory.name:
            errors.append(
                issue("skill-name", skill_relative, "frontmatter name must match the skill directory")
            )
        if not frontmatter.get("description"):
            errors.append(
                issue("skill-description", skill_relative, "frontmatter description is required")
            )
    return errors


def local_link_targets(path: Path, content: str) -> list[str]:
    """Extract relative Markdown links and Claude-style imports."""
    targets = re.findall(r"\[[^\]]*\]\(([^)]+)\)", content)
    targets.extend(re.findall(r"(?m)^@([^\s]+)\s*$", content))
    return targets


def validate_local_references(root: Path, files: list[Path]) -> list[dict[str, str]]:
    """Validate local Markdown links and imported files."""
    errors: list[dict[str, str]] = []
    for path in files:
        content = path.read_text(encoding="utf-8")
        for raw_target in local_link_targets(path, content):
            target = unquote(raw_target.strip().strip("<>"))
            if not target or target.startswith(("#", "http://", "https://", "mailto:", "app://")):
                continue
            target = target.split("#", 1)[0]
            resolved = (path.parent / target).resolve()
            try:
                resolved.relative_to(root.resolve())
            except ValueError:
                errors.append(
                    issue(
                        "reference-outside-root",
                        relative(root, path),
                        f"local reference leaves the repository: {raw_target}",
                    )
                )
                continue
            if not resolved.exists():
                errors.append(
                    issue(
                        "broken-reference",
                        relative(root, path),
                        f"local reference does not exist: {raw_target}",
                    )
                )
    return errors


def validate_scopes(root: Path, budget: dict) -> list[dict[str, str]]:
    """Ensure every path-scoped instruction matches repository files."""
    errors: list[dict[str, str]] = []
    for configured_path in budget.get("scope_files", []):
        path = root / configured_path
        if not path.is_file():
            continue
        frontmatter = parse_frontmatter(path.read_text(encoding="utf-8"))
        pattern = frontmatter.get("applyTo") if frontmatter else None
        if not pattern:
            errors.append(issue("scope-frontmatter", configured_path, "applyTo frontmatter is required"))
        elif not any(candidate.is_file() for candidate in root.glob(pattern)):
            errors.append(issue("unmatched-scope", configured_path, f"applyTo matches no files: {pattern}"))
    return errors


def validate_forbidden_patterns(
    root: Path, files: list[Path], budget: dict
) -> list[dict[str, str]]:
    """Reject costly mandates from active guidance."""
    errors: list[dict[str, str]] = []
    compiled = [
        (re.compile(entry["pattern"]), entry["message"])
        for entry in budget.get("forbidden_patterns", [])
    ]
    for path in files:
        content = path.read_text(encoding="utf-8")
        for pattern, message in compiled:
            if pattern.search(content):
                errors.append(issue("forbidden-mandate", relative(root, path), message))
    return errors


def validate_stale_references(
    root: Path, files: list[Path], budget: dict
) -> list[dict[str, str]]:
    """Reject references to retired active guidance surfaces."""
    errors: list[dict[str, str]] = []
    stale_references = budget.get("stale_references", [])
    for path in files:
        content = path.read_text(encoding="utf-8")
        for stale_reference in stale_references:
            if stale_reference in content:
                errors.append(
                    issue(
                        "stale-reference",
                        relative(root, path),
                        f"references retired path: {stale_reference}",
                    )
                )
    return errors


def normalized_paragraphs(content: str, minimum: int) -> list[str]:
    """Return normalized long prose paragraphs for duplicate detection."""
    paragraphs: list[str] = []
    for paragraph in re.split(r"\n\s*\n", content):
        normalized = re.sub(r"\s+", " ", paragraph).strip().lower()
        if len(normalized) >= minimum and not normalized.startswith("```"):
            paragraphs.append(normalized)
    return paragraphs


def validate_duplicate_paragraphs(
    root: Path, files: list[Path], budget: dict
) -> list[dict[str, str]]:
    """Reject exact long-paragraph duplication across active guidance."""
    minimum = budget.get("duplicate_paragraph_min_chars", 180)
    locations: dict[str, list[str]] = defaultdict(list)
    for path in files:
        for paragraph in normalized_paragraphs(path.read_text(encoding="utf-8"), minimum):
            locations[paragraph].append(relative(root, path))
    errors: list[dict[str, str]] = []
    for paths in locations.values():
        if len(paths) > 1:
            errors.append(
                issue(
                    "duplicate-paragraph",
                    paths[0],
                    f"long paragraph is duplicated in: {', '.join(paths)}",
                )
            )
    return errors


def validate_refresh_workflow(root: Path, budget: dict) -> list[dict[str, str]]:
    """Ensure paid instruction refresh is manual and audit-gated."""
    configured_path = budget.get("refresh_workflow")
    if not configured_path:
        return []
    path = root / configured_path
    if not path.is_file():
        return [issue("refresh-workflow", configured_path, "refresh workflow is missing")]
    content = path.read_text(encoding="utf-8")
    requirements = {
        "workflow_dispatch:": "manual dispatch trigger is required",
        "reason:": "required reason input is missing",
        "force_ai:": "force_ai input is missing",
        "python3 scripts/ci/validate_agent_guidance.py": "deterministic audit step is missing",
        "if: steps.audit.outputs.needs_ai == 'true'": "paid action is not audit-gated",
    }
    errors: list[dict[str, str]] = []
    if re.search(r"(?m)^\s+schedule:\s*$", content):
        errors.append(issue("refresh-workflow", configured_path, "scheduled paid refresh is prohibited"))
    for required_text, message in requirements.items():
        if required_text not in content:
            errors.append(issue("refresh-workflow", configured_path, message))
    return errors


def validate_repository(root: Path = ROOT, budget_path: Path | None = None) -> list[dict[str, str]]:
    """Run every guidance validation and return sorted issues."""
    selected_budget = budget_path or root / "scripts/ci/agent_guidance_budget.json"
    budget = load_budget(selected_budget)
    active_files = expand_globs(root, budget.get("active_guidance_globs", []))
    reference_files = expand_globs(root, budget.get("reference_scan_globs", []))
    errors = [
        *validate_file_budgets(root, budget),
        *validate_host_contexts(root, budget),
        *validate_total_reduction(root, budget),
        *validate_skills(root, budget),
        *validate_local_references(root, reference_files),
        *validate_scopes(root, budget),
        *validate_forbidden_patterns(root, active_files, budget),
        *validate_stale_references(root, reference_files, budget),
        *validate_duplicate_paragraphs(root, active_files, budget),
        *validate_refresh_workflow(root, budget),
    ]
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
        print("Agent guidance budgets, routing, references, and refresh gates are valid.")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
