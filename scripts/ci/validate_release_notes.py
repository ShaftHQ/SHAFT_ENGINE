#!/usr/bin/env python3
"""Validate release-note configuration and PR classification."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


CLASSIFICATION_LABELS = frozenset(
    {"breaking-change", "enhancement", "bug", "skip-release-notes"}
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


def pull_request_errors(event: dict) -> list[str]:
    pull_request = event.get("pull_request")
    if not isinstance(pull_request, dict):
        return []
    user = pull_request.get("user")
    user = user if isinstance(user, dict) else {}
    login = str(user.get("login") or "")
    if str(user.get("type") or "").casefold() == "bot" or login.casefold().endswith("[bot]"):
        return []
    labels = {
        str(label.get("name"))
        for label in pull_request.get("labels", [])
        if isinstance(label, dict) and label.get("name")
    }
    selected = sorted(labels & CLASSIFICATION_LABELS)
    if len(selected) == 1:
        return []
    return [
        "human pull requests require exactly one release-note classification: "
        + ", ".join(sorted(CLASSIFICATION_LABELS))
        + f"; found {selected or 'none'}"
    ]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--event", type=Path)
    parser.add_argument("--config", type=Path, default=Path(".github/release.yml"))
    arguments = parser.parse_args()
    errors = release_config_errors(arguments.config)
    if arguments.event is not None:
        try:
            event = json.loads(arguments.event.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError, ValueError) as error:
            errors.append(f"cannot read GitHub event: {error}")
        else:
            errors.extend(pull_request_errors(event))
    for error in errors:
        print(f"release-note validation failed: {error}")
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
