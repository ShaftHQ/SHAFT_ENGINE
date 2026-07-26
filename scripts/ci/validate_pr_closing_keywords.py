#!/usr/bin/env python3
"""Flag PR bodies that would auto-close an issue the author did not intend to close.

Issue #4142: GitHub's closing-keyword scanner (`close`/`closes`/`closed`,
`fix`/`fixes`/`fixed`, `resolve`/`resolves`/`resolved` immediately followed by
`#N` or a full issue URL) has no negation awareness -- it matches the keyword
adjacent to the reference regardless of what precedes it. PR #4009's body
said "Does not fix #3930's original macOS OOM -- that issue is being reopened
separately", written deliberately to explain why #3930 must stay open, and
GitHub still recorded #3930 in `closingIssuesReferences` and auto-closed it 2
seconds after the PR merged -- overriding a human's manual reopen 7 minutes
earlier ("Leaving open."). GitHub's behavior cannot be changed, so this guard
catches the pattern before merge and fails the PR so the author rewords it.

Scope is deliberately the confirmed-adjacent form GitHub's own docs describe
(https://docs.github.com/.../using-keywords-in-issues-and-pull-requests:
keyword, optional colon, then the reference with no other words between) --
not a loose "keyword anywhere near a negation" scan. That keeps false
positives near zero: a body needs both a closing keyword sitting directly
next to `#N` *and* a negation cue in the few words immediately before the
keyword ("does not", "doesn't", "cannot", "won't", "never") to be flagged.
Ordinary intentional closes ("Closes #10"), bare references ("#10"), and
prose that merely discusses an issue number are untouched.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys

CLOSING_KEYWORD_PATTERN = r"(?:close[sd]?|fix(?:e[sd])?|resolve[sd]?)"
ISSUE_REFERENCE_PATTERN = r"(?:#\d+|https://github\.com/[\w.-]+/[\w.-]+/issues/\d+)"
CLOSING_REFERENCE_RE = re.compile(
    rf"\b({CLOSING_KEYWORD_PATTERN})\b\s*:?\s*({ISSUE_REFERENCE_PATTERN})",
    re.IGNORECASE,
)
NEGATION_RE = re.compile(r"\b(?:not|never|cannot)\b|\w+n['’]t\b", re.IGNORECASE)
MARKDOWN_EMPHASIS_RE = re.compile(r"[*_`]")
SENTENCE_BOUNDARY_RE = re.compile(r"[.!?\n]")
NEGATION_WINDOW_TOKENS = 4


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable validation issue."""
    return {"code": code, "path": path, "message": message}


def _reference_label(reference: str) -> str:
    """Render a bare '#N' label from either a '#N' reference or an issue URL."""
    match = re.search(r"(\d+)$", reference)
    return f"#{match.group(1)}" if match else reference


def _is_negated(body: str, keyword_start: int) -> bool:
    """True when a negation cue sits in the few words right before the keyword."""
    preceding = body[:keyword_start]
    boundaries = [match.end() for match in SENTENCE_BOUNDARY_RE.finditer(preceding)]
    clause_start = boundaries[-1] if boundaries else 0
    clause = MARKDOWN_EMPHASIS_RE.sub("", preceding[clause_start:])
    window = " ".join(clause.split()[-NEGATION_WINDOW_TOKENS:])
    return bool(NEGATION_RE.search(window))


def _dewrap_hard_wrapped_text(text: str) -> str:
    """Collapse a hard-wrapped single newline into a space, preserving paragraph breaks.

    Both commit messages (git's ~72-column convention) and PR bodies composed in an
    editor/CLI that hard-wraps at ~72-80 columns can split a negation cue like "Does
    not" onto its own line from the "fix #N" that follows (issue #4146: the real commit
    that squash-merged into PR #4141 did exactly this -- 0 matches unwrapped, 1
    flattened, confirmed against the shipped guard). The clause-boundary logic below
    treats any bare newline as a hard stop by design, so an unrelated bullet's negation
    can't leak into the next bullet in a PR body -- but that same rule silently defeats
    detection on hard-wrapped prose. A blank line (double newline) still marks a real
    paragraph/bullet break and is left alone.
    """
    return re.sub(r"(?<!\n)\n(?!\n)", " ", text)


def find_negated_autocloses(body: str) -> list[dict[str, str]]:
    """Flag every closing-keyword+issue-reference pair written inside a negation."""
    errors: list[dict[str, str]] = []
    if not body:
        return errors
    body = _dewrap_hard_wrapped_text(body)
    for match in CLOSING_REFERENCE_RE.finditer(body):
        if not _is_negated(body, match.start(1)):
            continue
        reference_label = _reference_label(match.group(2))
        errors.append(
            issue(
                "autoclose-negated-reference",
                "pull_request.body",
                f"'{match.group(0)}' would auto-close {reference_label} on merge -- GitHub matches "
                f"the closing keyword '{match.group(1)}' next to the issue reference regardless of "
                f"the preceding negation. Reword to a bare '{reference_label}' (drop the closing "
                "verb) if this PR should not close it.",
            )
        )
    return errors


def find_negated_autocloses_in_commits(commits: list[tuple[str, str]]) -> list[dict[str, str]]:
    """Flag every negated closing-keyword+issue-reference pair in any commit message.

    Reuses find_negated_autocloses unchanged (issue #4146: the detection logic itself
    is surface-agnostic, dewrapping included) and tags the offending commit.
    """
    errors: list[dict[str, str]] = []
    for sha, message in commits:
        for error in find_negated_autocloses(message):
            errors.append(
                issue(
                    error["code"],
                    f"commit:{sha}",
                    f"{sha[:12]}: {error['message']}",
                )
            )
    return errors


def parse_commits_json(raw: str) -> list[tuple[str, str]]:
    """Parse a JSON array of {"sha": ..., "message": ...} objects (e.g. from `gh api .../commits`)."""
    if not raw:
        return []
    return [(entry["sha"], entry["message"]) for entry in json.loads(raw)]


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--body",
        help="PR body text to validate; defaults to the $PR_BODY environment variable",
    )
    parser.add_argument(
        "--commits-json",
        help=(
            "JSON array of {sha, message} commit objects to validate; defaults to the "
            "$PR_COMMITS_JSON environment variable"
        ),
    )
    parser.add_argument("--format", choices=("text", "json"), default="text")
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    body = args.body if args.body is not None else os.environ.get("PR_BODY", "")
    commits_json = (
        args.commits_json if args.commits_json is not None else os.environ.get("PR_COMMITS_JSON", "")
    )
    errors = find_negated_autocloses(body)
    errors.extend(find_negated_autocloses_in_commits(parse_commits_json(commits_json)))
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors}, indent=2))
    elif errors:
        for error in errors:
            print(f"{error['code']}: {error['message']}", file=sys.stderr)
    else:
        print("No negated closing-keyword references found in the PR body.")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
