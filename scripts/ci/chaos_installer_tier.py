#!/usr/bin/env python3
"""Resolve the ChaosEngine fresh-installer OS tier for one PR Gate run (#6187).

Pull requests run the fresh installer on Linux and Windows only. macOS (27.8
minute median, zero regressions caught in 22 PR runs) runs on every push to
``main`` (post-merge, never cancelled), nightly, and on a pull request that
carries the ``ci:installer-macos`` opt-in label.
"""

from __future__ import annotations

import argparse
import json
import sys

PR_TIER = ("ubuntu-22.04", "windows-2025")
CONTRACTS_PR_TIER = ("windows-2025",)
MACOS = "macos-15"
OPT_IN_LABEL = "ci:installer-macos"


def parse_labels(raw: str) -> set[str]:
    """Parse a JSON array of label names; ``null``/empty/invalid means none."""
    try:
        value = json.loads(raw or "[]")
    except json.JSONDecodeError:
        return set()
    if not isinstance(value, list):
        return set()
    return {str(item) for item in value if isinstance(item, str)}


def includes_macos(event_name: str, labels: set[str]) -> bool:
    return event_name != "pull_request" or OPT_IN_LABEL in labels


def installer_os(event_name: str, labels: set[str]) -> list[str]:
    tier = list(PR_TIER)
    return [*tier, MACOS] if includes_macos(event_name, labels) else tier


def contracts_os(event_name: str, labels: set[str]) -> list[str]:
    tier = list(CONTRACTS_PR_TIER)
    return [*tier, MACOS] if includes_macos(event_name, labels) else tier


def outputs(event_name: str, labels: set[str]) -> str:
    """Render ``$GITHUB_OUTPUT`` lines consumed by ``fromJSON`` matrices."""
    return "\n".join(
        (
            "installer_os=" + json.dumps(installer_os(event_name, labels)),
            "installer_contracts_os=" + json.dumps(contracts_os(event_name, labels)),
        )
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--event-name", required=True)
    parser.add_argument("--labels", default="[]", help="JSON array of PR label names")
    args = parser.parse_args(argv)
    print(outputs(args.event_name, parse_labels(args.labels)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
