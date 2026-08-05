#!/usr/bin/env python3
"""Validate agent guidance budgets, routing, and cost guardrails."""

from __future__ import annotations

import argparse
import ast
import json
import math
import re

# subprocess is used only for fixed, list-args `git show` against this
# repository (never shell=True, no untrusted command construction).
import subprocess  # nosec B404
import sys
from collections import defaultdict
from pathlib import Path
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[2]

SENTENCE_END_RE = re.compile(r"(?<=[.!?])\s+")
CODE_TOKEN_RE = re.compile(r"`[^`\n]+`")
# A claim is pinned to a run of words containing a backticked code token. Three
# words is what separated the real pair -- "a bare `review`" -- from every other
# docstring in a 3,800-line file across 300 commits of main.
CLAIM_PHRASE_WORDS = 3


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


def expand_reported_globs(
    root: Path, patterns: list[str], config_key: str
) -> tuple[list[Path], list[dict[str, str]]]:
    """
    Expand a list-valued guidance glob, reporting any pattern matching nothing.

    ``expand_globs`` merges every pattern's matches into one set, so a pattern
    that resolves to zero files -- the references directory moved or was
    renamed, a typo -- leaves no trace in the result: the merged set just
    ends up a little smaller, and a check that iterates it for
    ``all(...)``-shaped assertions keeps passing having verified nothing for
    that pattern (#4481). ``validate_file_budgets`` already closes this for
    its own glob keys with a ``missing-file`` issue; every other list-valued
    glob key the budget defines -- ``active_guidance_globs``,
    ``total_guidance_globs``, ``reference_scan_globs``, and each host's list
    under ``host_skill_metadata_globs`` -- routes through here instead of
    ``expand_globs`` directly so the same ``empty-glob`` issue fires no
    matter which list it is missing from.

    This guards patterns *within* an already-present list; it cannot detect
    the list itself being absent or empty, because the loop below then runs
    zero times. ``require_glob_list`` closes that one level up, for the three
    budget keys that must always be present.
    """
    errors: list[dict[str, str]] = []
    paths: set[Path] = set()
    for pattern in patterns:
        matched = expand_globs(root, [pattern])
        if not matched:
            errors.append(issue("empty-glob", pattern, f"{config_key} pattern matched no files"))
            continue
        paths.update(matched)
    return sorted(paths), errors


def require_glob_list(budget: dict, config_key: str) -> tuple[list[str], list[dict[str, str]]]:
    """
    Return a required list-valued glob key, or report why it cannot be scanned.

    ``expand_reported_globs`` only iterates the patterns it is given: if the
    configured key is missing, renamed, or present but set to ``[]``, that
    loop runs zero times, no ``empty-glob`` issue fires, and every check
    downstream scans zero files while reporting nothing wrong -- the same
    fail-open shape (#4481) one level up, at the list rather than the
    pattern. The set of required keys is declared here, in the code that
    consumes the budget, rather than inferred from whatever keys the JSON
    happens to contain -- inference cannot notice a key that used to be
    there and is not anymore.
    """
    value = budget.get(config_key)
    if not isinstance(value, list) or not value:
        return [], [issue("empty-glob-list", config_key, "required guidance glob list is missing or empty")]
    return value, []


def validate_file_budgets(root: Path, budget: dict) -> list[dict[str, str]]:
    """Validate byte, character, and line budgets.

    A key may be a literal path or a glob. A glob binds each matched file on
    its own and never their sum, which bounds the worst single file on a
    surface rather than a session total. Summing was the retired global pool
    (#3745): it charged every file for its neighbours, so the only way to fund
    one file's growth was deleting unrelated prose that no session had loaded
    anyway. The claim is incentive locality -- the file that grew is the file
    that pays -- and nothing more; splitting a file relocates load rather than
    removing it, because a split half is usually reached from the other.
    Either form reports ``missing-file`` when it resolves to nothing,
    so a cap on a surface that moved away cannot silently stop enforcing
    anything.
    """
    errors: list[dict[str, str]] = []
    for configured_path, limits in budget.get("file_budgets", {}).items():
        # relative() resolves before it subtracts the root, so anything outside
        # the root raises there rather than reporting a budget. Both branches
        # keep a key that escapes the root out of relative()'s way: the literal
        # one reports the configured string verbatim, and the glob one drops
        # the match, leaving `missing-file` below to report a pattern that
        # resolved to nothing usable.
        if any(character in configured_path for character in "*?["):
            matches = []
            for path in expand_globs(root, [configured_path]):
                try:
                    matches.append((relative(root, path), path))
                except ValueError:
                    continue
        else:
            candidate = root / configured_path
            matches = [(configured_path, candidate)] if candidate.is_file() else []
        if not matches:
            errors.append(issue("missing-file", configured_path, "required guidance file is missing"))
            continue
        for reported, path in matches:
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
                        issue(code, reported, f"{actual} {unit} exceeds configured maximum {maximum}")
                    )
    return errors


def always_loaded_body_chars(root: Path, configured_paths: list[str]) -> tuple[int, list[str]]:
    """Count LF-normalized UTF-8 bytes a host loads before it sees the task."""
    characters = 0
    missing: list[str] = []
    for configured_path in configured_paths:
        path = root / configured_path
        if path.is_file():
            characters += len(path.read_text(encoding="utf-8").encode("utf-8"))
        else:
            missing.append(configured_path)
    return characters, missing


def skill_listing_chars(root: Path, patterns: list[str]) -> int:
    """Count the discovery metadata a host keeps resident for every skill."""
    characters = 0
    for path in expand_globs(root, patterns):
        frontmatter = parse_frontmatter(path.read_text(encoding="utf-8")) or {}
        characters += len(frontmatter.get("name", ""))
        characters += len(frontmatter.get("description", ""))
    return characters


def validate_host_contexts(root: Path, budget: dict) -> list[dict[str, str]]:
    """Check the two surfaces a host pays for, against documented host limits.

    Always-loaded body and skill-listing metadata hit different caps and are
    budgeted separately -- see ``limit_sources`` in the budget file. Both are
    measured in the unit each host documents.
    """
    errors: list[dict[str, str]] = []
    body_maximum = budget.get("max_always_loaded_body_chars")
    listing_maximum = budget.get("max_skill_listing_chars")
    for host, configured_paths in budget.get("host_contexts", {}).items():
        characters, missing = always_loaded_body_chars(root, configured_paths)
        if missing:
            errors.append(
                issue("host-context-missing", host, f"missing context files: {', '.join(missing)}")
            )
            continue
        if body_maximum is not None and characters > body_maximum:
            errors.append(
                issue(
                    "host-body-budget",
                    host,
                    f"{characters} always-loaded UTF-8 bytes exceeds configured maximum {body_maximum}",
                )
            )
    if listing_maximum is None:
        return errors
    for host, patterns in budget.get("host_skill_metadata_globs", {}).items():
        _, pattern_errors = expand_reported_globs(
            root, patterns, f"host_skill_metadata_globs.{host}"
        )
        errors.extend(pattern_errors)
        characters = skill_listing_chars(root, patterns)
        if characters > listing_maximum:
            errors.append(
                issue(
                    "host-listing-budget",
                    host,
                    f"{characters} skill-listing characters exceeds configured maximum {listing_maximum}",
                )
            )
    return errors


# A backticked kebab identifier with no slash and no dot is how the routing
# table names a skill to hand off to. Paths (`src/main/java`), commands (`rg`)
# and filenames (`AGENTS.md`) are excluded by construction.
ROUTED_SKILL_NAME = re.compile(r"`([a-z][a-z0-9]*(?:-[a-z0-9]+)+)`")


def routed_skill_names(content: str) -> list[str]:
    """Return every skill name the routing table hands off to."""
    return sorted(set(ROUTED_SKILL_NAME.findall(content)))


def validate_routing_bridges(root: Path, budget: dict) -> list[dict[str, str]]:
    """Prove every skill the router names resolves to a real SKILL.md.

    A router that points at a skill nobody ships fails silently at run time:
    the agent reads the row, cannot find the file, and improvises. This parses
    the routing table itself rather than a hand-maintained allowlist, so a new
    row naming a skill that does not exist fails without anyone remembering to
    register it. ``required_names`` additionally catches the reverse drift --
    a handoff silently dropped from the table.
    """
    configured = budget.get("routing_bridges")
    if not configured:
        return []
    source_relative = configured.get("source", "")
    source = root / source_relative
    if not source.is_file():
        return [issue("routing-bridge-source", source_relative, "routing source is missing")]
    content = source.read_text(encoding="utf-8")
    roots = configured.get("skill_roots", [])
    errors: list[dict[str, str]] = []

    def resolves(name: str) -> bool:
        return any((root / skills_root / name / "SKILL.md").is_file() for skills_root in roots)

    for name in routed_skill_names(content):
        if not resolves(name):
            errors.append(
                issue(
                    "routing-bridge-missing",
                    source_relative,
                    f"routed skill has no SKILL.md under {', '.join(roots)}: {name}",
                )
            )
    for name in configured.get("required_names", []):
        if name not in content:
            errors.append(
                issue(
                    "routing-bridge-unrouted",
                    source_relative,
                    f"required handoff is not routed: {name}",
                )
            )
    return errors


def validate_total_reduction(root: Path, budget: dict) -> list[dict[str, str]]:
    """Keep the consolidated guidance below the approved baseline reduction."""
    baseline = budget.get("reduction_baseline_bytes")
    minimum_reduction = budget.get("minimum_reduction_percent")
    if baseline is None or minimum_reduction is None:
        return []
    patterns, errors = require_glob_list(budget, "total_guidance_globs")
    if errors:
        return errors
    paths, glob_errors = expand_reported_globs(root, patterns, "total_guidance_globs")
    errors.extend(glob_errors)
    # Count LF-normalized bytes (read_text collapses CRLF) so the metric matches
    # the per-file byte check and the LF blobs CI checks out, not the CRLF a
    # Windows working tree carries.
    current = sum(len(path.read_text(encoding="utf-8").encode("utf-8")) for path in paths)
    maximum = math.floor(baseline * (1 - minimum_reduction / 100))
    if current > maximum:
        errors.append(
            issue(
                "total-reduction",
                "agent-guidance",
                f"{current} bytes exceeds {maximum}; minimum reduction is {minimum_reduction}%",
            )
        )
    return errors


_BLOCK_SCALAR_HEADER = re.compile(r"^[|>][+-]?\d*$")


def parse_frontmatter(content: str) -> dict[str, str] | None:
    """Parse simple top-level YAML frontmatter without external dependencies.

    Handles single-line scalars and multi-line block scalars (`>`, `>-`,
    `|`, `|-`) -- several skill descriptions use folded style, and a value
    of just the block-scalar marker (e.g. `description: >-`) is not the
    real content. This stays a small proxy parser for flat frontmatter, not
    a full YAML implementation: no nested mappings or sequences.
    """
    if not content.startswith("---\n"):
        return None
    marker = content.find("\n---\n", 4)
    if marker < 0:
        return None
    lines = content[4:marker].splitlines()
    values: dict[str, str] = {}
    index = 0
    while index < len(lines):
        raw_line = lines[index]
        index += 1
        if ":" not in raw_line or raw_line[:1].isspace():
            continue
        key, value = raw_line.split(":", 1)
        key = key.strip()
        value = value.strip()
        if _BLOCK_SCALAR_HEADER.match(value):
            folded = value.startswith(">")
            block_lines: list[str] = []
            while index < len(lines) and (lines[index][:1].isspace() or not lines[index].strip()):
                block_lines.append(lines[index].strip())
                index += 1
            if folded:
                paragraphs: list[str] = []
                current: list[str] = []
                for block_line in block_lines:
                    if block_line:
                        current.append(block_line)
                    elif current:
                        paragraphs.append(" ".join(current))
                        current = []
                if current:
                    paragraphs.append(" ".join(current))
                values[key] = "\n\n".join(paragraphs)
            else:
                values[key] = "\n".join(block_lines).strip()
        else:
            values[key] = value.strip("\"'")
    return values


def markdown_body(content: str) -> str:
    """Return Markdown after optional frontmatter."""
    if not content.startswith("---\n"):
        return content.strip()
    marker = content.find("\n---\n", 4)
    return content[marker + 5 :].strip() if marker >= 0 else content.strip()


def quoted_yaml_value(content: str, key: str) -> str | None:
    """Read one quoted scalar from the small agents/openai.yaml interface."""
    match = re.search(rf'(?m)^\s*{re.escape(key)}:\s*"([^"]*)"\s*$', content)
    return match.group(1) if match else None


def validate_skill_interface(
    root: Path, directory: Path, skill_name: str
) -> list[dict[str, str]]:
    """Validate concise Codex UI metadata for a discoverable skill."""
    metadata_path = directory / "agents/openai.yaml"
    metadata_relative = relative(root, metadata_path)
    if not metadata_path.is_file():
        return [issue("skill-metadata", metadata_relative, "agents/openai.yaml is required")]

    content = metadata_path.read_text(encoding="utf-8")
    display_name = quoted_yaml_value(content, "display_name")
    short_description = quoted_yaml_value(content, "short_description")
    default_prompt = quoted_yaml_value(content, "default_prompt")
    errors: list[dict[str, str]] = []
    if not display_name:
        errors.append(issue("skill-metadata", metadata_relative, "display_name is required"))
    if not short_description or not 25 <= len(short_description) <= 64:
        errors.append(
            issue(
                "skill-metadata",
                metadata_relative,
                "short_description must be 25 to 64 characters",
            )
        )
    if not default_prompt or f"${skill_name}" not in default_prompt:
        errors.append(
            issue(
                "skill-metadata",
                metadata_relative,
                f"default_prompt must mention ${skill_name}",
            )
        )
    if not re.search(r"(?m)^\s*allow_implicit_invocation:\s*true\s*$", content):
        errors.append(
            issue(
                "skill-metadata",
                metadata_relative,
                "allow_implicit_invocation must be true",
            )
        )
    return errors


def validate_skills(root: Path, budget: dict) -> list[dict[str, str]]:
    """Validate discoverable skill directories and required frontmatter."""
    errors: list[dict[str, str]] = []
    configured_roots = budget.get("skills_roots")
    if configured_roots is None:
        configured_roots = [budget.get("skills_root", ".github/skills")]
    for configured_root in configured_roots:
        skills_root = root / configured_root
        if not skills_root.is_dir():
            errors.append(
                issue("skill-root-missing", configured_root, "skills root is missing")
            )
            continue
        directories = sorted(path for path in skills_root.iterdir() if path.is_dir())
        actual_names = {
            directory.name
            for directory in directories
            if any(path.is_file() for path in directory.rglob("*"))
        }
        expected_names = set(budget.get("expected_skill_names", {}).get(configured_root, []))
        if expected_names:
            for missing in sorted(expected_names - actual_names):
                errors.append(
                    issue("skill-set", configured_root, f"expected skill is missing: {missing}")
                )
            for unexpected in sorted(actual_names - expected_names):
                errors.append(
                    issue("skill-set", configured_root, f"unexpected skill is present: {unexpected}")
                )
        limits = budget.get("skill_budgets", {}).get(configured_root, {})
        for directory in directories:
            if not any(path.is_file() for path in directory.rglob("*")):
                continue
            skill_path = directory / "SKILL.md"
            skill_relative = relative(root, skill_path)
            if not skill_path.is_file():
                errors.append(
                    issue("skill-missing", skill_relative, "skill directory must contain SKILL.md")
                )
                continue
            content = skill_path.read_text(encoding="utf-8")
            frontmatter = parse_frontmatter(content)
            if frontmatter is None:
                errors.append(
                    issue("skill-frontmatter", skill_relative, "valid YAML frontmatter is required")
                )
                continue
            if frontmatter.get("name") != directory.name:
                errors.append(
                    issue(
                        "skill-name",
                        skill_relative,
                        "frontmatter name must match the skill directory",
                    )
                )
            if not frontmatter.get("description"):
                errors.append(
                    issue("skill-description", skill_relative, "frontmatter description is required")
                )
            maximum_description = limits.get("max_description_chars")
            if (
                maximum_description is not None
                and len(frontmatter.get("description", "")) > maximum_description
            ):
                errors.append(
                    issue(
                        "skill-description-budget",
                        skill_relative,
                        f"description exceeds {maximum_description} characters",
                    )
                )
            maximum_body = limits.get("max_body_chars")
            if maximum_body is not None and len(markdown_body(content)) > maximum_body:
                errors.append(
                    issue(
                        "skill-body-budget",
                        skill_relative,
                        f"body exceeds {maximum_body} characters",
                    )
                )
            maximum_lines = limits.get("max_skill_md_lines")
            if maximum_lines is not None:
                actual_lines = len(markdown_body(content).splitlines())
                if actual_lines > maximum_lines:
                    errors.append(
                        issue(
                            "skill-line-budget",
                            skill_relative,
                            f"{actual_lines} body lines exceeds configured maximum {maximum_lines}",
                        )
                    )
            maximum_skill_bytes = limits.get("max_skill_md_bytes")
            if maximum_skill_bytes is not None:
                # LF-normalized bytes: content came from read_text(), which
                # already collapses CRLF, matching the per-file byte-budget
                # precedent in validate_file_budgets.
                actual_skill_bytes = len(content.encode("utf-8"))
                if actual_skill_bytes > maximum_skill_bytes:
                    errors.append(
                        issue(
                            "skill-md-byte-budget",
                            skill_relative,
                            f"{actual_skill_bytes} bytes exceeds configured maximum {maximum_skill_bytes}",
                        )
                    )
            if limits.get("require_openai_yaml"):
                errors.extend(validate_skill_interface(root, directory, directory.name))
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


def docstring_sentences(source: str) -> list[tuple[str, str]]:
    """(owner, sentence) for every sentence of every docstring in a Python source."""
    try:
        tree = ast.parse(source)
    except SyntaxError:
        return []
    owned: list[tuple[str, str]] = []
    for node in ast.walk(tree):
        if not isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            continue
        text = ast.get_docstring(node, clean=True)
        if not text:
            continue
        owner = getattr(node, "name", "<module>")
        flattened = re.sub(r"\s+", " ", text).strip()
        for sentence in SENTENCE_END_RE.split(flattened):
            if sentence.strip():
                owned.append((owner, sentence.strip()))
    return owned


def claim_phrases(sentence: str) -> set[str]:
    """Word runs of a sentence that carry a backticked code token.  # noqa: D213

    Deliberately not a similarity score. The two docstrings this exists to catch
    score 0.157 on difflib and 0.167 on token Jaccard -- no near-identical-sentence
    threshold reaches them without flagging every pair of English sentences in the
    file. What they genuinely share is a phrase naming a code token, and "does this
    text contain a known phrase" is the one shape that has never lost a review round
    in this repository.
    """
    words = sentence.lower().split()
    carriers = [index for index, word in enumerate(words) if "`" in word]
    phrases: set[str] = set()
    for index in carriers:
        first = max(0, index - CLAIM_PHRASE_WORDS + 1)
        for start in range(first, index + 1):
            run = words[start:start + CLAIM_PHRASE_WORDS]
            if len(run) == CLAIM_PHRASE_WORDS:
                phrases.add(" ".join(run))
    return phrases


def find_orphaned_sibling_claims(
    before_source: str, after_source: str, path: str
) -> list[dict[str, str]]:
    """Report a docstring claim this edit deleted from one owner and left in a sibling.  # noqa: D213

    Issue #4567 section 4.4, recurrence class `sibling-left`. Round two of PR #4554
    corrected `_ledger_records_a_review`, which claimed a bare `review` ledger event
    still counted; the same claim survived in `_reviewer_dispatch_event` and cost a
    round-three finding to re-discover.

    Extends `validate_duplicate_paragraphs`'s idea -- the same prose living in two
    places -- from guidance Markdown to Python docstrings, with one difference that
    the measurement forced: duplication alone is not the signal. Two docstrings may
    legitimately share wording forever. The signal is duplication *at the moment one
    copy is edited*, so this compares two revisions rather than scanning one file.

    An owner missing from `after_source` is skipped: the function was renamed or
    deleted, so no instance is left for a sibling to contradict. That is a
    structural test on the syntax tree, never a judgement about what the prose means.
    """
    before = docstring_sentences(before_source)
    after = docstring_sentences(after_source)
    if not before or not after:
        return []
    surviving_owners = {owner for owner, _ in after}
    surviving_sentences = {sentence.lower() for _, sentence in after}
    findings: list[dict[str, str]] = []
    seen: set[tuple[str, str, str]] = set()
    for owner, sentence in before:
        if sentence.lower() in surviving_sentences or owner not in surviving_owners:
            continue
        if not CODE_TOKEN_RE.search(sentence):
            continue
        for phrase in sorted(claim_phrases(sentence)):
            for sibling, surviving in after:
                if sibling == owner or phrase not in surviving.lower():
                    continue
                key = (owner, sibling, phrase)
                if key in seen:
                    continue
                seen.add(key)
                findings.append(
                    issue(
                        "orphaned-sibling-claim",
                        path,
                        f"this edit rewrote {owner}'s docstring but '{phrase}' still "
                        f"stands in {sibling}: \"{surviving}\" -- confirm the sibling "
                        "is not repeating the claim that was just corrected.",
                    )
                )
    return findings


def validate_repository(root: Path = ROOT, budget_path: Path | None = None) -> list[dict[str, str]]:
    """Run every guidance validation and return sorted issues."""
    selected_budget = budget_path or root / "scripts/ci/agent_guidance_budget.json"
    budget = load_budget(selected_budget)
    active_patterns, active_key_errors = require_glob_list(budget, "active_guidance_globs")
    reference_patterns, reference_key_errors = require_glob_list(budget, "reference_scan_globs")
    active_files, active_glob_errors = expand_reported_globs(
        root, active_patterns, "active_guidance_globs"
    )
    reference_files, reference_glob_errors = expand_reported_globs(
        root, reference_patterns, "reference_scan_globs"
    )
    errors = [
        *validate_file_budgets(root, budget),
        *validate_host_contexts(root, budget),
        *validate_total_reduction(root, budget),
        *validate_skills(root, budget),
        *validate_routing_bridges(root, budget),
        *validate_local_references(root, reference_files),
        *validate_scopes(root, budget),
        *validate_forbidden_patterns(root, active_files, budget),
        *validate_stale_references(root, reference_files, budget),
        *validate_duplicate_paragraphs(root, active_files, budget),
        *active_key_errors,
        *reference_key_errors,
        *active_glob_errors,
        *reference_glob_errors,
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
    parser.add_argument(
        "--docstring-siblings",
        nargs="+",
        metavar="REVISION_THEN_PATHS",
        help=(
            "Advisory scan (#4567 item 8): report a docstring claim REVISION rewrote in "
            "one function and left standing in a sibling. Takes a revision followed by "
            "one or more Python paths. Never changes the exit code."
        ),
    )
    return parser


def source_at_revision(revision: str, path: str, root: Path) -> str | None:
    """File content at a revision, or None when this clone cannot resolve it."""
    try:
        completed = subprocess.run(  # nosec B603 B607
            ["git", "show", f"{revision}:{path}"],
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            cwd=root,
            check=False,
        )
    except OSError:
        return None
    return completed.stdout if completed.returncode == 0 else None


def report_docstring_siblings(arguments: list[str], root: Path) -> int:
    """Print the sibling-claim advisory for one revision. Always returns 0.  # noqa: D213

    Advisory rather than a gate, on the same ground as #4567 item 5: the issue's
    own finding template ranks a docstring as never blocking, and the scan fires
    twice across PR #4554 with one of the two a judgement call a human settles in
    seconds. Failing CI on that is the `fires-on-correct-work` shape. An
    unresolvable revision is said out loud rather than passed over, because a scan
    that cannot read its input is a check that cannot fail.
    """
    revision, paths = arguments[0], arguments[1:]
    if not paths:
        print("--docstring-siblings needs a revision and at least one path", file=sys.stderr)
        return 0
    for path in paths:
        before = source_at_revision(f"{revision}^", path, root)
        after = source_at_revision(revision, path, root)
        if before is None or after is None:
            print(f"{path}: {revision} unavailable in this clone, not scanned", file=sys.stderr)
            continue
        for finding in find_orphaned_sibling_claims(before, after, path):
            print(f"advisory {finding['code']}: {finding['path']}: {finding['message']}")
    return 0


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    root = args.root.resolve()
    if args.docstring_siblings:
        return report_docstring_siblings(args.docstring_siblings, root)
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
