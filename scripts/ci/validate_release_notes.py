#!/usr/bin/env python3
"""Validate release-note configuration and PR classification."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess  # nosec B603 - absolute gh from shutil.which, fixed API argv.
from pathlib import Path


CLASSIFICATION_LABELS = frozenset(
    {"breaking-change", "enhancement", "bug", "skip-release-notes"}
)
# Optional labels that sit beside the one classification and pick a section in
# scripts/ci/render_release_notes.py (issue #6232). They never replace it.
SUPPLEMENTAL_LABELS = frozenset({"regression", "performance", "deprecation"})
TEMPLATE_PLACEHOLDERS = (
    "$RELEASE_VERSION",
    "$RELEASE_SUMMARY",
    "$RELEASE_CHANGES",
    "$RELEASE_CHANGELOG",
)
EXPECTED_CATEGORIES = (
    ("Breaking Changes", ("breaking-change",)),
    ("Enhancements", ("enhancement",)),
    ("Bug Fixes", ("bug", "regression")),
)
REQUIRED_EXCLUDED_AUTHORS = frozenset(
    {
        "dependabot[bot]",
        "dependabot",
        "github-actions[bot]",
        "github-actions",
        "copilot-swe-agent[bot]",
    }
)


def _list_values(block: str, key: str) -> tuple[str, ...]:
    match = re.search(
        rf"(?m)^\s*{re.escape(key)}:\s*$\n(?P<items>(?:\s+-\s+[^\r\n]+\r?\n?)*)",
        block,
    )
    if match is None:
        return ()
    return tuple(
        value.strip().strip("\"'")
        for value in re.findall(r"(?m)^\s+-\s+([^\r\n#]+)", match.group("items"))
    )


def release_config_errors(path: Path) -> list[str]:
    text = path.read_text(encoding="utf-8")
    errors: list[str] = []
    category_matches = list(
        re.finditer(
            r"(?ms)^\s{4}- title:\s*(?P<title>[^\r\n]+)\r?\n"
            r"(?P<body>.*?)(?=^\s{4}- title:|\Z)",
            text,
        )
    )
    categories = tuple(
        (
            match.group("title").strip().strip("\"'"),
            _list_values(match.group("body"), "labels"),
        )
        for match in category_matches
    )
    if categories != EXPECTED_CATEGORIES:
        errors.append(f"release categories must be exactly {EXPECTED_CATEGORIES!r}")
    if "*" in {label for _title, labels in categories for label in labels}:
        errors.append("release categories must not contain a catch-all label")

    exclude = text.split("  categories:", 1)[0]
    excluded_labels = set(_list_values(exclude, "labels"))
    excluded_authors = set(_list_values(exclude, "authors"))
    if "skip-release-notes" not in excluded_labels:
        errors.append("skip-release-notes must be excluded")
    missing_authors = REQUIRED_EXCLUDED_AUTHORS - excluded_authors
    if missing_authors:
        errors.append(f"missing excluded bot authors: {sorted(missing_authors)}")
    return errors


def release_template_errors(path: Path) -> list[str]:
    """The rendered release body needs every placeholder and the install snippet."""
    text = path.read_text(encoding="utf-8")
    errors = [
        f"release template is missing {placeholder}"
        for placeholder in TEMPLATE_PLACEHOLDERS
        if placeholder not in text
    ]
    if "<artifactId>shaft-engine</artifactId>" not in text:
        errors.append("release template must include the shaft-engine Maven snippet")
    return errors


def _is_bot(pull_request: dict) -> bool:
    user = pull_request.get("user")
    user = user if isinstance(user, dict) else {}
    login = str(user.get("login") or "").casefold()
    return str(user.get("type") or "").casefold() == "bot" or login.endswith("[bot]")


def _label_names(pull_request: dict) -> set[str]:
    return {
        str(label.get("name"))
        for label in pull_request.get("labels", [])
        if isinstance(label, dict) and label.get("name")
    }


def fetch_live_labels(pull_request: dict) -> set[str]:
    """Label names currently on the pull request. Empty when GitHub cannot be read."""
    number = pull_request.get("number")
    repository = os.environ.get("GITHUB_REPOSITORY", "").strip()
    executable = shutil.which("gh")
    if not isinstance(number, int) or not repository or executable is None:
        return set()
    completed = subprocess.run(  # nosec B603 - absolute gh, fixed API argv.
        [
            executable,
            "api",
            f"repos/{repository}/pulls/{number}",
            "--jq",
            ".labels[].name",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        return set()
    return {line.strip() for line in completed.stdout.splitlines() if line.strip()}


def event_label_errors(event: dict, *, fetcher=fetch_live_labels) -> list[str]:
    """Use live labels when the event payload recorded none (#6284)."""
    pull_request = event.get("pull_request")
    live = None
    if isinstance(pull_request, dict) and not _is_bot(pull_request) and not _label_names(pull_request):
        live = fetcher(pull_request)
    return pull_request_errors(event, live_labels=live)


def pull_request_errors(event: dict, *, live_labels: set[str] | None = None) -> list[str]:
    """Human pull requests need exactly one release-note classification label."""
    pull_request = event.get("pull_request")
    if not isinstance(pull_request, dict) or _is_bot(pull_request):
        return []
    names = _label_names(pull_request)
    if not names and live_labels is not None:
        names = set(live_labels)
    selected = sorted(names & CLASSIFICATION_LABELS)
    if len(selected) == 1:
        return []
    return [
        "human pull requests require exactly one release-note classification: "
        + ", ".join(sorted(CLASSIFICATION_LABELS))
        + f"; found {selected or 'none'} (optional extras: "
        + ", ".join(sorted(SUPPLEMENTAL_LABELS))
        + ")"
    ]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--event", type=Path)
    parser.add_argument("--config", type=Path, default=Path(".github/release.yml"))
    parser.add_argument(
        "--template", type=Path, default=Path(".github/RELEASE_BODY_TEMPLATE.md")
    )
    arguments = parser.parse_args()
    errors = release_config_errors(arguments.config)
    errors.extend(release_template_errors(arguments.template))
    if arguments.event is not None:
        try:
            event = json.loads(arguments.event.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError, ValueError) as error:
            errors.append(f"cannot read GitHub event: {error}")
        else:
            errors.extend(event_label_errors(event))
    for error in errors:
        print(f"release-note validation failed: {error}")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
