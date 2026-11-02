"""#7704 / #7184: merge_queue AWAITING_CHECKS heal predicate + required triggers."""

from __future__ import annotations

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)

import unittest
from pathlib import Path

import yaml

from scripts.ci.merge_queue_awaiting_checks_heal import (
    REQUIRED_MERGE_GROUP_WORKFLOWS,
    is_stalled_awaiting_checks,
    required_workflows_declare_merge_group,
)

ROOT = Path(__file__).resolve().parents[2]


class AwaitingChecksHealPredicateTest(unittest.TestCase):
    def test_no_merge_group_runs_is_a_stall(self) -> None:
        self.assertTrue(
            is_stalled_awaiting_checks({"state": "AWAITING_CHECKS"}, [])
        )

    def test_required_merge_group_runs_clear_the_stall(self) -> None:
        runs = [{"name": "PR Gate"}, {"name": "Release-note governance"}]
        self.assertFalse(
            is_stalled_awaiting_checks({"state": "AWAITING_CHECKS"}, runs)
        )

    def test_unrelated_runs_do_not_clear_the_stall(self) -> None:
        self.assertTrue(
            is_stalled_awaiting_checks(
                {"state": "AWAITING_CHECKS"},
                [{"name": "Docs links"}],
            )
        )

    def test_non_waiting_states_are_not_stalls(self) -> None:
        self.assertFalse(is_stalled_awaiting_checks({"state": "MERGEABLE"}, []))
        self.assertFalse(is_stalled_awaiting_checks({"state": "LOCKED"}, []))


class RequiredWorkflowsMergeGroupTest(unittest.TestCase):
    """Required status checks must declare merge_group or the queue stalls."""

    REQUIRED_FILES = (
        "pr-gate.yml",
        "release-note-governance.yml",
    )

    def test_required_workflows_declare_merge_group(self) -> None:
        documents = {}
        for name in self.REQUIRED_FILES:
            path = ROOT / ".github" / "workflows" / name
            documents[name] = yaml.safe_load(path.read_text(encoding="utf-8"))
        missing = required_workflows_declare_merge_group(documents)
        self.assertEqual([], missing)
        self.assertEqual(
            REQUIRED_MERGE_GROUP_WORKFLOWS,
            {"PR Gate", "Release-note governance"},
        )


if __name__ == "__main__":
    unittest.main()
