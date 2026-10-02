"""The shipped git-cleanup skill is the portable cleanup procedure."""

from __future__ import annotations

import importlib.util
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/git-cleanup/SKILL.md"


def _load_guard():
    path = ROOT / "chaos-engine/hooks/guard.py"
    spec = importlib.util.spec_from_file_location("ce_guard_6299", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class GitCleanupSkillTest(unittest.TestCase):
    def test_procedure_and_limits(self) -> None:
        text = SKILL.read_text(encoding="utf-8")
        self.assertIn("configured default branch", text)
        self.assertIn("`HEAD` equals that remote tip", text)
        self.assertIn("`git status` is clean", text)
        self.assertIn("land (commit, push, pull request, merge), delete, or gitignore", text)
        self.assertIn("only after that classification", text)
        self.assertIn("clean, unlocked, and not concurrently owned", text)
        self.assertIn("Fetch and prune the configured upstream.", text)
        self.assertIn("Do not stash.", text)
        self.assertIn("Do not use `--force-with-lease`.", text)
        self.assertIn("Do not rewrite remote history.", text)
        self.assertIn("Do not `reset --hard`.", text)
        self.assertIn("Do not force-push.", text)
        self.assertIn("Do not delete remote branches.", text)
        self.assertIn("Do not discard unique commits without explicit authorization.", text)
        self.assertIn("`git branch -r --contains`", text)
        self.assertIn("that exact tip on an origin ref", text)
        self.assertIn("Check out the configured default branch and fast-forward it.", text)
        self.assertIn("Write the verification transcript in the same process, after that fast-forward.", text)
        self.assertIn("Do not check out another branch afterward.", text)
        self.assertIn("Attended: ask first", text)
        self.assertIn("Unattended: decide and run it.", text)
        self.assertNotIn("`main`", text)


class ReflectionReceiptAdapterTest(unittest.TestCase):
    def test_same_controller_adapter_is_accepted_and_denial_names_only_the_hook(self) -> None:
        guard = _load_guard()
        previous = Path.cwd()
        os.chdir(ROOT)
        try:
            operation = guard.reflection_recovery(
                "py -3 scripts/agents/reflection.py receipt --session-id recovery"
            )
        finally:
            os.chdir(previous)
        self.assertEqual("receipt", operation)
        reason = guard.checkpoint_reason({"depth": "task", "failureFingerprints": ["abc"]})
        controller = str(guard._reflection_controller())
        self.assertIn(f"The gate executed `{controller}`", reason)
        self.assertIn(f"`{controller}` receipt", reason)
        self.assertNotIn("scripts/agents/reflection.py", reason)

    def test_denied_reflection_retry_does_not_append_a_fingerprint(self) -> None:
        guard = _load_guard()
        with tempfile.TemporaryDirectory() as temporary, patch.dict(
            os.environ, {"TMPDIR": temporary, "TEMP": temporary}
        ):
            session = "denied-receipt"
            for _ in range(3):
                guard.reflection.record_failure(
                    session,
                    phase="tool-outcome",
                    target="command-seed",
                    failure_class="tool-failure",
                    platform="linux",
                    attempted=True,
                )
            before = guard.reflection.pending_checkpoint(session)["failureFingerprints"]
            recorded = guard._record_failed_result(
                {
                    "hook_event_name": "PostToolUseFailure",
                    "tool_response": {"isError": True},
                    "tool_use_id": "denied-receipt-1",
                },
                "PostToolUseFailure",
                ("py -3 /tmp/not-the-controller/reflection.py receipt --session-id recovery",),
                "Bash",
                session,
            )
            self.assertFalse(recorded)
            after = guard.reflection.pending_checkpoint(session)["failureFingerprints"]
            self.assertEqual(before, after)


class OwnerCommitAttributionGuidanceTest(unittest.TestCase):
    def test_profile_does_not_treat_owner_email_as_unattributed_without_a_reviewer(self) -> None:
        text = (ROOT / "shaft-skills/ce-pack/entrypoint.md").read_text(encoding="utf-8")
        # Epic #6342 / CE-12: the key and owner email live in user-level config.
        self.assertNotIn("F79E3F65DB762CE1", text)
        self.assertNotIn("Mohab.MohieElDeen@outlook.com", text)
        self.assertIn("owner email attribution", text)
