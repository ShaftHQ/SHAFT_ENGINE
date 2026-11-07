"""Dependabot schedule stays read-only. Event enqueue uses BOT_TOKEN."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "dependabot-auto-merge.yml"
REFERENCE = ROOT / "chaos-engine" / "references" / "automerge-rearm.md"
def _real_gh_lines(text: str) -> list[str]:
    """Return shell lines that invoke gh (not comment / warning echo text)."""
    lines: list[str] = []
    for raw in text.splitlines():
        stripped = raw.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if stripped.startswith("echo ") or stripped.startswith("::"):
            continue
        if re.search(r"\bgh\s+", stripped):
            lines.append(stripped)
    return lines


class DependabotAutoMergeWorkflowTest(unittest.TestCase):
    def test_schedule_and_dispatch_are_read_only(self) -> None:
        text = WORKFLOW.read_text(encoding="utf-8")
        diagnose, enqueue = text.split("  enqueue:", 1)
        self.assertIn("schedule:", text)
        self.assertIn("workflow_dispatch:", text)
        self.assertIn("pull_request_target:", text)
        self.assertNotIn("gh pr merge", diagnose)
        self.assertNotIn("secrets.BOT_TOKEN", diagnose)
        self.assertNotIn("dequeuePullRequest", text)
        self.assertNotIn("enqueuePullRequest", text)
        self.assertNotIn("may_enqueue", text)
        self.assertNotIn("exit 0", text)
        self.assertIn("contents: read", text)
        self.assertNotIn("contents: write", text)
        self.assertNotIn("pull-requests: write", text)
        self.assertIn("secrets.GITHUB_TOKEN", diagnose)
        self.assertNotIn("secrets.GITHUB_TOKEN", enqueue)
        self.assertIn("secrets.BOT_TOKEN", enqueue)
        self.assertIn("gh pr merge", enqueue)
        self.assertIn("--auto", enqueue)
        self.assertIn("enqueue-decision", enqueue)
        self.assertNotIn("DEPENDABOT_TOKEN", text)
        self.assertIn("persist-credentials: false", text)
        self.assertIn("Checkout default branch for the diagnostic script", diagnose)
        self.assertIn("Checkout default branch for the enqueue decision", enqueue)
        self.assertNotIn("github.event.pull_request.head.sha", text)
        self.assertNotIn("github.event.pull_request.head.ref", text)

    def test_inventory_uses_the_synthetic_queue_sha_and_both_checks(self) -> None:
        text = WORKFLOW.read_text(encoding="utf-8")
        self.assertIn("headCommit", text)
        self.assertIn("PR Gate Summary", text)
        self.assertIn("Release-note governance", text)
        self.assertIn("repos/${repo}/commits/${queue_sha}/check-runs", text)
        self.assertIn("merge_queue_awaiting_checks_heal.py diagnose", text)
        self.assertIn("merge_queue_awaiting_checks_heal.py shas", text)
        after_checks = text.split("check-runs", 1)[1]
        self.assertNotIn("headRefOid", after_checks)
        self.assertIn('gh pr list -R "$repo"', text)
        self.assertIn('GH_REPO="$repo"', text)
        self.assertIn('repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}"', text)

    def test_every_real_gh_invocation_names_the_repository(self) -> None:
        text = WORKFLOW.read_text(encoding="utf-8")
        gh_lines = _real_gh_lines(text)
        self.assertGreaterEqual(len(gh_lines), 3, gh_lines)
        for line in gh_lines:
            self.assertTrue(
                '-R "$repo"' in line or 'GH_REPO="$repo"' in line,
                msg=f"gh call missing repo scope: {line}",
            )

    def test_reference_is_fail_closed_and_still_names_the_repo_flag(self) -> None:
        text = REFERENCE.read_text(encoding="utf-8")
        self.assertIn("-R", text)
        self.assertIn("GITHUB_REPOSITORY", text)
        self.assertIn("empty list", text.lower())
        for needle in ("re-arm", "soft-fail", "eviction"):
            self.assertIn(needle, text)
        self.assertIn("read-only", text.lower())
        self.assertIn("BOT_TOKEN", text)
        self.assertIn("events start no workflows", text)
        self.assertNotIn("gh pr merge", text)


class ChaosEngineVersionFloorTest(unittest.TestCase):
    def test_version_at_least_2026_10_31_12(self) -> None:
        version = (ROOT / "chaos-engine" / "VERSION").read_text(encoding="utf-8").strip()
        import importlib.util as u
        spec = u.spec_from_file_location("ce_version", ROOT / "scripts/ci/ce_version.py")
        mod = u.module_from_spec(spec)
        spec.loader.exec_module(mod)
        self.assertGreaterEqual(mod.version_key(version), mod.version_key("2026.10.31.12"))


if __name__ == "__main__":
    unittest.main()
