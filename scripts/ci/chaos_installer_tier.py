#!/usr/bin/env python3
"""
Resolve the ChaosEngine fresh-installer OS tier for one PR Gate run (#6187).

Pull requests run the fresh installer on Linux and Windows only. macOS (27.8
minute median, zero regressions caught in 22 PR runs) runs on every push to
``main`` (post-merge, never cancelled), nightly, and on a pull request that
carries the ``ci:installer-macos`` opt-in label.

The Windows installer UX contracts leg (PowerShell hook launcher, ~11 minute
PR critical path) runs post-merge on every push to ``main`` and on a pull
request that carries ``ci:installer-windows`` (#6990). The same UX modules
still run on Linux inside Agent Guidance on every PR, and the Windows fresh
installer still runs on every installer PR.

#6990 phase 3 also moves three heavy PR legs to post-merge ``main`` (scoped
by the same path filters, never cancelled, tracked by Main red reaction):
SHAFT Capture Browser E2E (``ci:capture-e2e``), the IntelliJ Plugin Verifier
(``ci:intellij-verify``) and the Windows shaft-cli leg (``ci:cli-windows``).
``ci:full`` opts a pull request into every post-merge leg.
"""

from __future__ import annotations

import argparse
import json
import sys

PR_TIER = ("ubuntu-22.04", "windows-2025")
CONTRACTS_PR_TIER: tuple[str, ...] = ()
WINDOWS = "windows-2025"
MACOS = "macos-15"
OPT_IN_LABEL = "ci:installer-macos"
WINDOWS_OPT_IN_LABEL = "ci:installer-windows"
FULL_LABEL = "ci:full"
# A merge-queue group re-checks the PR tier only; post-merge legs run on push (#6990).
PR_TIER_EVENTS = frozenset({"pull_request", "merge_group"})
POST_MERGE_LEGS = {
    "run_capture_browser": "ci:capture-e2e",
    "run_intellij_verify": "ci:intellij-verify",
}
CLI_WINDOWS_LABEL = "ci:cli-windows"
OPT_IN_LABELS = (OPT_IN_LABEL, WINDOWS_OPT_IN_LABEL, FULL_LABEL, CLI_WINDOWS_LABEL, *POST_MERGE_LEGS.values())


def parse_labels(raw: str) -> set[str]:
    """Parse a JSON array of label names; ``null``/empty/invalid means none."""
    try:
        value = json.loads(raw or "[]")
    except json.JSONDecodeError:
        return set()
    if not isinstance(value, list):
        return set()
    return {str(item) for item in value if isinstance(item, str)}


def opted_in(event_name: str, labels: set[str], label: str) -> bool:
    """Post-merge legs run on every non-PR event, or on a PR carrying ``label``/``ci:full``."""
    if event_name not in PR_TIER_EVENTS:
        return True
    return label in labels or FULL_LABEL in labels


def includes_macos(event_name: str, labels: set[str]) -> bool:
    return opted_in(event_name, labels, OPT_IN_LABEL)


def cli_os(event_name: str, labels: set[str]) -> list[str]:
    return ["ubuntu-22.04", *([WINDOWS] if opted_in(event_name, labels, CLI_WINDOWS_LABEL) else [])]


def installer_os(event_name: str, labels: set[str]) -> list[str]:
    tier = list(PR_TIER)
    return [*tier, MACOS] if includes_macos(event_name, labels) else tier


def includes_windows_contracts(event_name: str, labels: set[str]) -> bool:
    return opted_in(event_name, labels, WINDOWS_OPT_IN_LABEL)


def contracts_os(event_name: str, labels: set[str]) -> list[str]:
    tier = list(CONTRACTS_PR_TIER)
    if includes_windows_contracts(event_name, labels):
        tier.append(WINDOWS)
    return [*tier, MACOS] if includes_macos(event_name, labels) else tier


def outputs(event_name: str, labels: set[str]) -> str:
    """Render ``$GITHUB_OUTPUT`` lines consumed by ``fromJSON`` matrices."""
    return "\n".join(
        (
            "installer_os=" + json.dumps(installer_os(event_name, labels)),
            "installer_contracts_os=" + json.dumps(contracts_os(event_name, labels)),
            "cli_os=" + json.dumps(cli_os(event_name, labels)),
            *(
                f"{key}=" + str(opted_in(event_name, labels, label)).lower()
                for key, label in POST_MERGE_LEGS.items()
            ),
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
