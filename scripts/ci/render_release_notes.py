#!/usr/bin/env python3
"""Render minimal, user-facing GitHub release notes (issue #6232).

GitHub-native ``.github/release.yml`` categories cannot add a summary, an install
snippet, cleaned titles, or a collapsed internal block, so the release workflow
renders the body here. It lists first-parent pull requests since the previous
release, classifies each one from its labels, title, and changed files, and fills
``.github/RELEASE_BODY_TEMPLATE.md``. When collection fails the body falls back
to the template plus GitHub's generated notes, so a release is never blocked.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
# Fixed git/gh argument lists with resolved executables and no shell.
import subprocess  # nosec B404
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Iterable, Sequence

ROOT = Path(__file__).resolve().parents[2]
TEMPLATE = ROOT / ".github" / "RELEASE_BODY_TEMPLATE.md"
DEFAULT_REPOSITORY = "ShaftHQ/SHAFT_ENGINE"
UPGRADE_GUIDE = "https://shafthq.github.io/docs/start/upgrade"
MAX_TITLE_LENGTH = 100
GRAPHQL_BATCH = 20

# Rendered order; each section appears only when it has entries.
SECTIONS = (
    ("breaking", "Breaking changes and upgrade notes"),
    ("feature", "New features"),
    ("fix", "Fixes"),
    ("performance", "Performance"),
    ("deprecation", "Deprecations"),
    ("dependency", "Dependency upgrades"),
)
SKIP_LABEL = "skip-release-notes"
# Automated "Prepare SHAFT Engine release X" version-bump PRs are the release itself.
RELEASE_PREPARATION = re.compile(r"(?i)^\s*prepare\b.*\brelease\b")
HIGHLIGHT_THRESHOLD = 4
FALLBACK_SUMMARY = "The generated change list follows below."
SLACK_FALLBACK_SUMMARY = "See the release notes for what changed."
# Existing repository labels that mark work SHAFT library users do not consume.
INTERNAL_LABELS = frozenset(
    {
        "subsystem:agent-harness",
        "subsystem:repository-tooling",
        "documentation",
        "tests",
        "github-actions",
        "maintenance",
    }
)
# Supplemental labels (alongside the one required classification) that pick a section.
SUPPLEMENTAL_SECTION_LABELS = (("performance", "performance"), ("deprecation", "deprecation"))
CLASSIFICATION_SECTIONS = (("enhancement", "feature"), ("bug", "fix"), ("regression", "fix"))
TITLE_PREFIX_SECTIONS = (("perf", "performance"), ("fix", "fix"), ("feat", "feature"))
DEPENDENCY_AUTHORS = frozenset(
    {"dependabot", "dependabot[bot]", "app/dependabot", "renovate", "renovate[bot]", "app/renovate"}
)
NOTABLE_DEPENDENCY = re.compile(r"(?i)selenium|appium|playwright")
DEPENDENCY_TITLE = re.compile(
    r"(?i)^(?:\w+\(deps(?:-dev)?\)!?:\s*)?bump\s+(?P<name>\S+)"
    r"(?:.*?\bfrom\s+v?(?P<old>\d+)\S*\s+to\s+v?(?P<new>\d+)\S*)?"
)
INTERNAL_TITLE = re.compile(
    r"(?i)^\s*(?:\[(?:ce|ci|chaos[- ]?engine)\]"
    r"|(?:ci|tests?|docs|chore|build|style|refactor)(?:\([^)]*\))?!?:"
    r"|\w+\((?:ce|ci|chaos-engine|chaos-gauge|agents?|omniroot|omniroute|installer|hooks"
    r"|harness|memory|tests?|docs|deps)\)!?:)"
)
INTERNAL_KEYWORDS = re.compile(
    r"(?i)\b(?:chaos ?engine|chaos ?gauge|mempalace|graphify|omniroot|omniroute|nightly e2e)\b"
)
PRODUCT_PATH = re.compile(
    r"^(?:shaft-[^/]+/src/main/|legacy-shaft-engine/|shaft-skills/|shaft-bom/)"
)
PULL_REFERENCE = re.compile(r"\(#(\d+)\)\s*$|^Merge pull request #(\d+)\b")
TITLE_TAGS = re.compile(r"^\s*(?:\[[^\]]+\]\s*)+")
TITLE_PREFIX = re.compile(r"^(?P<type>\w+)(?:\([^)]*\))?!?:\s*")
TITLE_TRAILING_REFS = re.compile(r"\s*\(#\d+(?:[\s,/]+#?\d+)*\)\s*$")


@dataclass(frozen=True)
class PullRequest:
    """The pull-request facts the renderer needs."""

    number: int
    title: str
    author: str = ""
    labels: frozenset[str] = frozenset()
    files: tuple[str, ...] = ()


@dataclass
class Classified:
    """Pull requests grouped by rendered section."""

    sections: dict[str, list[PullRequest]] = field(default_factory=dict)
    internal: list[PullRequest] = field(default_factory=list)
    dependency_updates: int = 0

    def add(self, section: str, pull: PullRequest) -> None:
        """Append a pull request to a rendered section."""
        self.sections.setdefault(section, []).append(pull)


def clean_title(title: str) -> str:
    """Return a short, user-facing title without tags, prefixes, or trailing refs."""
    cleaned = TITLE_TAGS.sub("", title.strip())
    cleaned = TITLE_PREFIX.sub("", cleaned, count=1)
    previous = None
    while previous != cleaned:
        previous = cleaned
        cleaned = TITLE_TRAILING_REFS.sub("", cleaned)
    cleaned = re.sub(r"\s+in\s+/\S*$", "", cleaned).strip().rstrip(".").strip()
    if cleaned:
        cleaned = cleaned[0].upper() + cleaned[1:]
    if len(cleaned) > MAX_TITLE_LENGTH:
        cleaned = cleaned[: MAX_TITLE_LENGTH - 1].rstrip() + "…"
    return cleaned or title.strip()


def is_dependency_update(pull: PullRequest) -> bool:
    """Return whether the pull request only bumps a dependency."""
    return (
        pull.author.casefold() in DEPENDENCY_AUTHORS
        or "dependencies" in pull.labels
        or DEPENDENCY_TITLE.match(pull.title) is not None
    )


def is_notable_dependency(pull: PullRequest) -> bool:
    """Security bumps and Selenium/Appium/Playwright majors stay visible."""
    if "security" in pull.labels:
        return True
    match = DEPENDENCY_TITLE.match(pull.title)
    name = match.group("name") if match else pull.title
    if not NOTABLE_DEPENDENCY.search(name):
        return False
    if match is None or match.group("old") is None:
        return True
    return match.group("old") != match.group("new")


def is_internal(pull: PullRequest) -> bool:
    """Return whether SHAFT library users would not notice the change."""
    if pull.labels & INTERNAL_LABELS:
        return True
    if INTERNAL_TITLE.match(pull.title) or INTERNAL_KEYWORDS.search(pull.title):
        return True
    return bool(pull.files) and not any(PRODUCT_PATH.match(path) for path in pull.files)


def user_section(pull: PullRequest) -> str:
    """Pick the user-facing section from supplemental labels, classification, then title."""
    for label, section in SUPPLEMENTAL_SECTION_LABELS + CLASSIFICATION_SECTIONS:
        if label in pull.labels:
            return section
    prefix = TITLE_PREFIX.match(TITLE_TAGS.sub("", pull.title))
    kind = prefix.group("type").casefold() if prefix else ""
    for title_type, section in TITLE_PREFIX_SECTIONS:
        if kind == title_type:
            return section
    return "feature"


def classify(pulls: Iterable[PullRequest]) -> Classified:
    """Group pull requests into rendered sections, internal work, and dependency counts."""
    result = Classified()
    for pull in pulls:
        if SKIP_LABEL in pull.labels or RELEASE_PREPARATION.match(pull.title):
            continue
        if "breaking-change" in pull.labels:
            result.add("breaking", pull)
        elif is_dependency_update(pull):
            _classify_dependency(result, pull)
        elif is_internal(pull):
            result.internal.append(pull)
        else:
            result.add(user_section(pull), pull)
    return result


def _classify_dependency(result: Classified, pull: PullRequest) -> None:
    if is_notable_dependency(pull):
        result.add("dependency", pull)
    else:
        result.dependency_updates += 1


def _entry_lines(pulls: Sequence[PullRequest]) -> list[str]:
    """One line per change; identical cleaned titles merge their PR numbers."""
    numbers_by_title: dict[str, list[int]] = {}
    for pull in pulls:
        numbers_by_title.setdefault(clean_title(pull.title), []).append(pull.number)
    return [
        f"- {title} ({', '.join(f'#{number}' for number in numbers)})"
        for title, numbers in numbers_by_title.items()
    ]


def _plural(count: int, noun: str) -> str:
    if count == 1:
        return f"{count} {noun}"
    suffix = "es" if noun.endswith(("x", "s", "ch", "sh")) else "s"
    return f"{count} {noun}{suffix}"


SUMMARY_NOUNS = (
    ("feature", "new feature"),
    ("fix", "fix"),
    ("performance", "performance improvement"),
    ("deprecation", "deprecation"),
)


def _counts(classified: Classified) -> dict[str, int]:
    return {key: len(classified.sections.get(key, ())) for key, _title in SECTIONS}


def _change_line(counts: dict[str, int]) -> str:
    parts = [_plural(counts[key], noun) for key, noun in SUMMARY_NOUNS if counts[key]]
    if not parts:
        return ""
    listed = parts[0] if len(parts) == 1 else ", ".join(parts[:-1]) + " and " + parts[-1]
    return f"This release brings {listed} for SHAFT users."


def render_summary(classified: Classified) -> str:
    """Return the one-to-three line summary for SHAFT users."""
    counts = _counts(classified)
    lines = []
    if counts["breaking"]:
        lines.append(
            f"**Heads-up:** {_plural(counts['breaking'], 'breaking change')}; "
            "read the upgrade notes below before upgrading."
        )
    change_line = _change_line(counts)
    if change_line:
        lines.append(change_line)
    elif not lines:
        lines.append(
            "Maintenance release: no user-facing changes, only internal and dependency updates."
        )
    if counts["feature"] + counts["fix"] > HIGHLIGHT_THRESHOLD:
        lines.append(_highlights(classified))
    return "\n".join(lines)


def _highlights(classified: Classified) -> str:
    """Name the first changes when the list is too long to scan at a glance."""
    highlights = (classified.sections.get("feature") or []) + (classified.sections.get("fix") or [])
    return "Highlights: " + "; ".join(clean_title(pull.title) for pull in highlights[:3]) + "."



def render_changes(classified: Classified) -> str:
    """Render the grouped sections and the collapsed internal block."""
    blocks = []
    for key, title in SECTIONS:
        pulls = classified.sections.get(key)
        if not pulls:
            continue
        lines = [f"## {title}", ""]
        if key == "breaking":
            lines.append(f"Read the [upgrade guide]({UPGRADE_GUIDE}) before upgrading.")
            lines.append("")
        blocks.append("\n".join(lines + _entry_lines(pulls)))
    internal = _internal_block(classified)
    if internal:
        blocks.append(internal)
    return "\n\n".join(blocks)


def _internal_block(classified: Classified) -> str:
    total = len(classified.internal) + classified.dependency_updates
    if not total:
        return ""
    lines = _entry_lines(classified.internal)
    if classified.dependency_updates:
        lines.append(f"- {_plural(classified.dependency_updates, 'dependency update')}")
    return "\n".join(
        [
            "<details>",
            f"<summary>Internal changes ({total}): CI, ChaosEngine and agent harness, tests, "
            "docs, chores, and dependency updates</summary>",
            "",
            *lines,
            "",
            "</details>",
        ]
    )


def render_changelog(repository: str, previous_tag: str | None, version: str) -> str:
    """Return the full-changelog compare line, or nothing without a previous tag."""
    if not previous_tag:
        return ""
    compare = f"https://github.com/{repository}/compare/{previous_tag}...{version}"
    return f"**Full changelog**: {compare}"


def render_body(
    template: str,
    version: str,
    summary: str,
    changes: str,
    changelog: str,
) -> str:
    """Fill the release template and drop empty placeholder lines."""
    body = template
    for placeholder, value in (
        ("$RELEASE_SUMMARY", summary),
        ("$RELEASE_CHANGES", changes),
        ("$RELEASE_CHANGELOG", changelog),
        ("$RELEASE_VERSION", version),
    ):
        body = body.replace(placeholder, value)
    body = re.sub(r"(?m)^[ \t·]+$", "", body)
    return re.sub(r"\n{3,}", "\n\n", body).strip() + "\n"


def render_release(
    version: str,
    pulls: Sequence[PullRequest],
    previous_tag: str | None,
    repository: str = DEFAULT_REPOSITORY,
    template: str | None = None,
) -> str:
    """Render the complete minimal release body for classified pull requests."""
    classified = classify(pulls)
    return render_body(
        template if template is not None else TEMPLATE.read_text(encoding="utf-8"),
        version,
        render_summary(classified),
        render_changes(classified),
        render_changelog(repository, previous_tag, version),
    )


def render_fallback(
    version: str,
    previous_tag: str | None,
    repository: str = DEFAULT_REPOSITORY,
    template: str | None = None,
) -> str:
    """Body used with GitHub's generated notes when pull requests cannot be collected."""
    return render_body(
        template if template is not None else TEMPLATE.read_text(encoding="utf-8"),
        version,
        FALLBACK_SUMMARY,
        "",
        render_changelog(repository, previous_tag, version),
    )


def release_summary(body: str) -> str:
    """Return the rendered summary: the first paragraph after the version heading."""
    paragraphs = body.split("\n\n", 2)
    first = paragraphs[1] if len(paragraphs) > 1 else ""
    summary = " ".join(line.strip() for line in first.splitlines() if line.strip())
    if summary.startswith(("```", "<")):
        return ""
    return "" if summary == FALLBACK_SUMMARY else summary


def slack_payload(version: str, release_url: str, body: str = "") -> dict:
    """Slack announcement (#6241): the release summary line plus a link to the notes."""
    summary = release_summary(body).replace("**", "*") or SLACK_FALLBACK_SUMMARY
    return {
        "text": f"SHAFT_ENGINE {version} released: {release_url}",
        "blocks": [
            {
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": f":tada: *SHAFT_ENGINE {version}* is now available!",
                },
            },
            {
                "type": "section",
                "text": {"type": "mrkdwn", "text": f"{summary} <{release_url}|Release notes>"},
            },
            {
                "type": "actions",
                "elements": [
                    {
                        "type": "button",
                        "text": {"type": "plain_text", "text": "View release notes"},
                        "url": release_url,
                    }
                ],
            },
        ],
    }


Runner = Callable[[Sequence[str]], str]


def run_command(command: Sequence[str]) -> str:
    """Run a fixed git/gh command with a resolved executable and return stdout."""
    executable = shutil.which(command[0])
    if executable is None:
        raise RuntimeError(f"{command[0]} is required to render release notes")
    # Resolved executable and fixed arguments; never a shell.
    result = subprocess.run(  # nosec B603
        [executable, *command[1:]], cwd=ROOT, capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        raise RuntimeError(f"{command[0]} {command[1]} failed: {result.stderr.strip()}")
    return result.stdout


def latest_release_tag(repository: str, runner: Runner = run_command) -> str:
    """Return the latest published release tag, which the new release follows."""
    return runner(
        ["gh", "release", "view", "--repo", repository, "--json", "tagName", "--jq", ".tagName"]
    ).strip()


def pull_numbers(previous_tag: str, head: str, runner: Runner = run_command) -> list[int]:
    """Return first-parent pull-request numbers in ``previous_tag..head``, oldest first."""
    subjects = runner(
        ["git", "log", "--first-parent", "--reverse", "--format=%s", f"{previous_tag}..{head}"]
    )
    numbers: list[int] = []
    for subject in subjects.splitlines():
        match = PULL_REFERENCE.search(subject.strip())
        if match:
            number = int(match.group(1) or match.group(2))
            if number not in numbers:
                numbers.append(number)
    return numbers


def _graphql_query(numbers: Sequence[int]) -> str:
    fields = (
        "number title author { login } labels(first: 50) { nodes { name } } "
        "files(first: 100) { nodes { path } }"
    )
    aliases = " ".join(
        f"pr{number}: pullRequest(number: {number}) {{ {fields} }}" for number in numbers
    )
    return (
        "query($owner: String!, $name: String!) "
        f"{{ repository(owner: $owner, name: $name) {{ {aliases} }} }}"
    )


def _nodes(node: dict, key: str) -> list[dict]:
    return (node.get(key) or {}).get("nodes") or []


def _pull_from_node(node: dict) -> PullRequest:
    return PullRequest(
        number=int(node["number"]),
        title=str(node.get("title") or ""),
        author=str((node.get("author") or {}).get("login") or ""),
        labels=frozenset(label["name"] for label in _nodes(node, "labels")),
        files=tuple(item["path"] for item in _nodes(node, "files")),
    )


def fetch_pulls(
    repository: str, numbers: Sequence[int], runner: Runner = run_command
) -> list[PullRequest]:
    """Fetch pull-request facts in batched GraphQL calls, keeping the input order."""
    owner, name = repository.split("/", 1)
    pulls: list[PullRequest] = []
    for start in range(0, len(numbers), GRAPHQL_BATCH):
        batch = numbers[start : start + GRAPHQL_BATCH]
        output = runner(
            [
                "gh", "api", "graphql",
                "-f", f"query={_graphql_query(batch)}",
                "-f", f"owner={owner}",
                "-f", f"name={name}",
            ]
        )
        nodes = json.loads(output)["data"]["repository"]
        pulls.extend(_pull_from_node(nodes[f"pr{n}"]) for n in batch if nodes.get(f"pr{n}"))
    return pulls


def parse_arguments(argv: Sequence[str] | None = None) -> argparse.Namespace:
    """Parse command-line options."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--version", required=True, help="release version and tag")
    parser.add_argument("--previous-tag", help="previous release tag (default: latest release)")
    parser.add_argument("--head", default="HEAD", help="last commit included in the release")
    parser.add_argument(
        "--repository", default=os.environ.get("GITHUB_REPOSITORY", DEFAULT_REPOSITORY)
    )
    parser.add_argument("--template", type=Path, default=TEMPLATE)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--github-output", type=Path, help="append fallback=true|false")
    return parser.parse_args(argv)


@dataclass(frozen=True)
class ReleaseRequest:
    """Which release to render and from where."""

    version: str
    template_path: Path = TEMPLATE
    repository: str = DEFAULT_REPOSITORY
    previous_tag: str | None = None
    head: str = "HEAD"


def build_release_body(
    request: ReleaseRequest, runner: Runner | None = None
) -> tuple[str, bool]:
    """Return ``(body, fallback)``; fallback asks the caller to append GitHub's generated notes."""
    runner = runner or run_command
    template = request.template_path.read_text(encoding="utf-8")
    previous_tag = request.previous_tag
    try:
        previous_tag = previous_tag or latest_release_tag(request.repository, runner)
        numbers = pull_numbers(previous_tag, request.head, runner)
        pulls = fetch_pulls(request.repository, numbers, runner)
    except (RuntimeError, OSError, KeyError, ValueError, TypeError) as error:
        print(f"release notes: falling back to GitHub generated notes ({error})", file=sys.stderr)
        return render_fallback(request.version, previous_tag, request.repository, template), True
    return render_release(request.version, pulls, previous_tag, request.repository, template), False


def main(argv: Sequence[str] | None = None) -> int:
    """Render the release body file and report whether the fallback was used."""
    arguments = parse_arguments(argv)
    request = ReleaseRequest(
        version=arguments.version,
        template_path=arguments.template,
        repository=arguments.repository,
        previous_tag=arguments.previous_tag,
        head=arguments.head,
    )
    body, fallback = build_release_body(request)
    arguments.output.write_text(body, encoding="utf-8")
    if arguments.github_output is not None:
        with arguments.github_output.open("a", encoding="utf-8") as handle:
            handle.write(f"fallback={'true' if fallback else 'false'}\n")
    print(body)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
