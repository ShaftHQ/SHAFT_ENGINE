"""Advisory PR-title lint (#7010): conventional ``type(scope): subject`` titles.

Release notes are curated from labels (.github/release.yml), so the title does not
decide the changelog category; this check keeps merge-commit history scannable and
suggests a fix. Report-only: it prints a GitHub warning and exits 0.
"""

from __future__ import annotations

import os
import re
import sys

TYPES = ("feat", "fix", "perf", "refactor", "docs", "test", "build", "ci", "chore", "revert",
         "harness", "skills", "lessons", "security", "deps")
_PATTERN = re.compile(r"^(?P<type>[a-z]+)(?:\((?P<scope>[^()\s][^()]*)\))?(?P<bang>!)?: (?P<subject>\S.*)$")
BOT_PREFIXES = ("build(deps", "chore(deps", "Prepare SHAFT Engine release")


def problems(title: str) -> list[str]:
    """Empty when the title is conventional; otherwise one reason per problem."""
    title = title.strip()
    if title.startswith(BOT_PREFIXES):
        return []
    match = _PATTERN.match(title)
    if not match:
        return ["title is not `type(scope): subject`"]
    if match.group("type") not in TYPES:
        return [f"type `{match.group('type')}` is not one of: {', '.join(TYPES)}"]
    return []


def suggestion(title: str) -> str:
    """A conventional rewrite guess, lowercasing a leading `Word:` prefix when it is a known type."""
    head, sep, rest = title.partition(":")
    if sep and head.strip().lower() in TYPES:
        return f"{head.strip().lower()}: {rest.strip()}"
    return f"fix: {title.strip()}" if title.strip().lower().startswith(("fix", "keep", "do not", "retry")) else f"chore: {title.strip()}"


def main() -> int:
    title = os.environ.get("PR_TITLE", "")
    found = problems(title)
    if found:
        print(f"::warning title=PR title lint (#7010)::{'; '.join(found)}. Suggested: {suggestion(title)}")
    else:
        print("PR title is conventional.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
