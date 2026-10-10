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
import subprocess  # nosec B404 -- fixed list-args `git show`, never shell=True
import sys
from typing import Callable

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

LIST_ITEM_RE = re.compile(r"^\s*(?:[-*+]|\d+\.)\s+")
BACKTICKED_RE = re.compile(r"`([^`\n]+)`")
SYMBOL_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


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
    """Collapse a hard-wrapped single newline into a space, preserving paragraph breaks."""
    # Both commit messages (git's ~72-column convention) and PR bodies composed in an
    # editor/CLI that hard-wraps at ~72-80 columns can split a negation cue like "Does
    # not" onto its own line from the "fix #N" that follows (issue #4146: the real commit
    # that squash-merged into PR #4141 did exactly this -- 0 matches unwrapped, 1
    # flattened, confirmed against the shipped guard). The clause-boundary logic below
    # treats any bare newline as a hard stop by design, so an unrelated bullet's negation
    # can't leak into the next bullet in a PR body -- but that same rule silently defeats
    # detection on hard-wrapped prose. A blank line (double newline) still marks a real
    # paragraph/bullet break and is left alone.
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
    """Flag every negated closing-keyword+issue-reference pair in any commit message."""
    # Reuses find_negated_autocloses unchanged (issue #4146: the detection logic itself
    # is surface-agnostic, dewrapping included) and tags the offending commit.
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


def _change_list_items(message: str) -> list[str]:
    """Every markdown list item in the message, with its indented continuation lines.  # noqa: D213

    Judged by the list delimiter alone -- never by what the item says. A commit
    enumerates what it did in its change list, so that is the surface where a
    backticked symbol reads as a credit. Running prose is left alone because it
    legitimately names symbols the commit did not touch: prior art, the check
    that fired, the rule a name comes from.
    """
    items: list[str] = []
    current: str | None = None
    for line in message.splitlines():
        if LIST_ITEM_RE.match(line):
            if current is not None:
                items.append(current)
            current = line
        elif current is not None and line.strip() and line[:1].isspace():
            current += " " + line
        else:
            if current is not None:
                items.append(current)
            current = None
    if current is not None:
        items.append(current)
    return items


def credited_symbols(message: str) -> list[str]:
    """Identifier-shaped backticked tokens a commit's change list credits."""
    symbols: set[str] = set()
    for item in _change_list_items(message):
        for span in BACKTICKED_RE.findall(item):
            token = span.strip()
            if not SYMBOL_RE.match(token):
                continue
            # A token test, never a meaning test. An identifier in this repo
            # carries an underscore or internal capital; a bare lowercase word
            # in backticks is English prose (`review`, `main`, `gh`).
            if token.islower() and "_" not in token:
                continue
            symbols.add(token)
    return sorted(symbols)


def find_credited_symbols_not_in_diff(
    commits: list[tuple[str, str]],
    diff_for_sha: Callable[[str], str | None],
) -> list[dict[str, str]]:
    """Report each symbol a commit's change list credits but its own diff never touches.  # noqa: D213

    Issue #4567 section 4.3, recurrence class `credit-not-in-diff`. `254a830710`
    credited `raw_decode` and `HOOK_BUDGET_SECONDS`, both landed by earlier
    commits on the same branch; catching that consumed a round-two review
    finding and forced PR #4554's body to carry a `## Corrections` section.

    `diff_for_sha` returns the commit's own diff with function context
    (`git show -W`), so a symbol anywhere in a touched function counts as
    credited. Returning None means the diff could not be read -- reported as
    `credit-scan-unavailable` rather than passing silently, because a scan that
    cannot see its input is a check that cannot fail.

    Advisory by design: see `main`.
    """
    findings: list[dict[str, str]] = []
    for sha, message in commits:
        symbols = credited_symbols(message)
        if not symbols:
            continue
        diff = diff_for_sha(sha)
        if diff is None:
            findings.append(
                issue(
                    "credit-scan-unavailable",
                    f"commit:{sha}",
                    f"{sha[:12]}: cannot read this commit's diff, so its "
                    f"{len(symbols)} credited symbol(s) went unchecked. A shallow "
                    "clone cannot resolve a pull request's own commits; the job "
                    "needs the history fetched.",
                )
            )
            continue
        for symbol in symbols:
            if symbol in diff:
                continue
            findings.append(
                issue(
                    "credit-not-in-diff",
                    f"commit:{sha}",
                    f"{sha[:12]}: credits `{symbol}`, absent from this commit's own diff. "
                    "Either the change list names work that landed in a different commit, "
                    "or the symbol is misspelled.",
                )
            )
    return findings


def git_show_diff(sha: str) -> str | None:
    """Return this commit's own diff with function context, or None when unreadable."""
    try:
        completed = subprocess.run(  # nosec B603 B607
            ["git", "show", "-W", "--format=", sha],
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            check=False,
        )
    except OSError:
        return None
    return completed.stdout if completed.returncode == 0 else None


def fetch_issue_labels(numbers: list[str]) -> dict[str, list[str]]:
    """Return {number: label names} for the issues ``gh`` can read; fail open on any lookup problem (#6777)."""
    repository = os.environ.get("GITHUB_REPOSITORY", "")
    labels: dict[str, list[str]] = {}
    for number in numbers:
        command = ["gh", "issue", "view", number, "--json", "labels", "--jq", "[.labels[].name]"]
        if repository:
            command += ["--repo", repository]
        try:
            completed = subprocess.run(  # nosec B603 B607
                command, capture_output=True, text=True, encoding="utf-8", errors="replace", check=False, timeout=30
            )
            if completed.returncode == 0:
                labels[number] = list(json.loads(completed.stdout or "[]"))
        except (OSError, subprocess.SubprocessError, ValueError):
            continue
    return labels


def closing_issue_numbers(body: str) -> list[str]:
    """Issue numbers named by non-negated closing keywords in ``body``, in first-seen order."""
    numbers: list[str] = []
    for match in CLOSING_REFERENCE_RE.finditer(body or ""):
        if _is_negated(body, match.start(1)):
            continue
        number = _reference_label(match.group(2)).lstrip("#")
        if number not in numbers:
            numbers.append(number)
    return numbers


def nightly_tracker_recovered(
    *, conclusion: str, jobs_complete: bool, close_reason: str
) -> bool:
    """#6609: a nightly tracker recovers only from its owning workflow.

    ``conclusion`` is that workflow's conclusion. ``jobs_complete`` is true only
    for a full job set (no skipped matrix). ``close_reason`` is ``workflow`` for
    that proof. ``merge``, ``manual``, ``closing-keyword``, and
    ``partial-dispatch`` are never recovery, even when a later run looks green.
    """
    if close_reason in {"merge", "manual", "closing-keyword", "partial-dispatch"}:
        return False
    return conclusion == "success" and jobs_complete is True and close_reason == "workflow"


def find_nightly_tracker_closes(
    body: str, labels_by_issue: dict[str, list[str]] | None
) -> list[dict[str, str]]:
    """Reject a closing keyword on a nightly-failure tracker (#6308).

    ``labels_by_issue`` maps ``#N`` or the issue number to label names.
    A product pull request may say ``Related to #N`` for that tracker.
    The tracker closes only after ``jobs=all`` succeeds.
    """
    if not body or not labels_by_issue:
        return []
    findings: list[dict[str, str]] = []
    for match in CLOSING_REFERENCE_RE.finditer(body):
        if _is_negated(body, match.start(1)):
            continue
        reference = match.group(2)
        number = _reference_label(reference).lstrip("#")
        labels = labels_by_issue.get(number) or labels_by_issue.get(f"#{number}") or []
        if any(str(label).startswith("nightly-failure:") for label in labels):
            findings.append(
                issue(
                    "nightly-tracker-autoclose",
                    "pr-body",
                    f"{match.group(1)} {reference} would close a nightly-failure tracker. "
                    "Those trackers close only after a successful full-matrix workflow "
                    "(jobs=all). Say Related to "
                    f"#{number}.",
                )
            )
    return findings


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
    commits = parse_commits_json(commits_json)
    errors = find_negated_autocloses(body)
    errors.extend(find_negated_autocloses_in_commits(commits))
    errors.extend(find_nightly_tracker_closes(body, fetch_issue_labels(closing_issue_numbers(body))))
    # Advisory, never a gate. Three independent reasons, all measured (#4567):
    # a commit message is immutable once pushed and this repository blocks the
    # force-push that would amend it, so failing here is a gate the author
    # cannot satisfy; #4567's own finding template ranks commit prose as never
    # blocking; and the scan measured one false positive over 300 commits of
    # main. Printed where review already reads, which is the whole point of
    # moving the finding upstream from a review round.
    advisories = find_credited_symbols_not_in_diff(commits, git_show_diff)
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors, "advisories": advisories}, indent=2))
    else:
        for advisory in advisories:
            print(f"advisory {advisory['code']}: {advisory['message']}")
        if errors:
            for error in errors:
                print(f"{error['code']}: {error['message']}", file=sys.stderr)
        else:
            print("No negated closing-keyword references found in the PR body.")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
