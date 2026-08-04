"""R11: refuse an MCP memory write issued from a linked worktree."""
# Issue #4505, observed twice in one session by two different agents on two
# unrelated tasks: an agent working in a linked worktree wrote a memory object,
# and the `.json`, the `.md` and the appended `.memory/events.jsonl` line landed
# in the *primary* checkout instead -- uncommitted, on whatever branch that tree
# happened to have checked out, which belonged to a different session.
#
# The issue guessed the cause was the CLI resolving the shared `.git` to the
# primary tree. It is not: `@aictx/memory@0.1.55` resolves its root with
# `git rev-parse --show-toplevel` from its own cwd (dist/cli/main.js,
# findGitRoot), which is correct. The real mechanism is that `.mcp.json`
# declares the server with `"cwd": "."` and one server process serves the whole
# session, so *every* agent's MCP memory write lands in the tree the client
# started in, no matter which worktree the agent itself occupies.
#
# That is not something this repository can fix inside the server. What it can
# do is what the issue asks for as the fallback: "a refusal an agent can see
# beats a success it cannot verify". The write is refused at the moment it is
# attempted, and the agent is pointed at the CLI, which honours its own cwd.
#
# Every case below builds a real linked worktree with the real `git` rather than
# faking the layout, because the tell being detected -- `.git` as a file rather
# than a directory -- is a property git creates, not one worth hand-writing.

from __future__ import annotations

import io
import json
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from scripts.agents.guard import is_linked_worktree, run_pretooluse


def git(cwd: Path, *arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
        ["git", "-c", "core.longpaths=true", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


class MemoryWriteFromLinkedWorktreeTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.container = Path(self.temporary_directory.name)
        self.primary = self.container / "primary"
        self.primary.mkdir()
        git(self.primary, "init", "-q", "-b", "main", ".")
        git(self.primary, "config", "user.email", "harness@example.invalid")
        git(self.primary, "config", "user.name", "Harness")
        (self.primary / "notes.md").write_text("committed\n", encoding="utf-8")
        git(self.primary, "add", "notes.md")
        git(self.primary, "commit", "-qm", "initial")

        self.linked = self.container / "linked"
        git(
            self.primary,
            "worktree",
            "add",
            str(self.linked),
            "-b",
            "ChaosEngine/fixture",
        )

    def tearDown(self):
        self.temporary_directory.cleanup()

    def decision(self, tool_name: str, cwd: Path) -> dict | None:
        """Return the guard's hook output for one PreToolUse call, or None."""
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            run_pretooluse({"tool_name": tool_name, "cwd": str(cwd)})
        printed = buffer.getvalue().strip()
        return json.loads(printed) if printed else None

    def denial_reason(self, output: dict | None) -> str | None:
        if output is None:
            return None
        specific = output.get("hookSpecificOutput", {})
        if specific.get("permissionDecision") != "deny":
            return None
        return specific.get("permissionDecisionReason")

    def test_the_fixture_really_is_a_linked_worktree(self):
        # The whole rule keys off this distinction, so pin it rather than
        # assume git's layout: a linked worktree's `.git` is a *file* holding a
        # `gitdir:` pointer; the primary checkout's is a directory.
        self.assertTrue((self.linked / ".git").is_file())
        self.assertTrue((self.primary / ".git").is_dir())
        self.assertTrue(is_linked_worktree(str(self.linked)))
        self.assertFalse(is_linked_worktree(str(self.primary)))

    def test_memory_write_from_a_linked_worktree_is_denied(self):
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__remember_memory", self.linked)
        )
        self.assertIsNotNone(reason)
        self.assertIn("R11", reason)
        # The refusal has to be actionable: the CLI resolves its own cwd
        # correctly, so it is the way out, not a dead end.
        self.assertIn("memory remember", reason)

    def test_memory_patch_write_from_a_linked_worktree_is_denied_too(self):
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__save_memory_patch", self.linked)
        )
        self.assertIsNotNone(reason)

    def test_memory_write_from_the_primary_checkout_is_allowed(self):
        # The rule must not fire where the write is already correct, or every
        # solo session pays for a defect only concurrent sessions have.
        self.assertIsNone(
            self.denial_reason(
                self.decision("mcp__shaft-memory__remember_memory", self.primary)
            )
        )

    def test_memory_reads_from_a_linked_worktree_are_allowed(self):
        # Reads are the session-entry point AGENTS.md mandates. Blocking them
        # would make worktree isolation cost an agent its memory entirely, and
        # a read cannot strand work in the wrong tree.
        for tool in (
            "mcp__shaft-memory__load_memory",
            "mcp__shaft-memory__search_memory",
            "mcp__shaft-memory__inspect_memory",
            "mcp__shaft-memory__diff_memory",
        ):
            with self.subTest(tool=tool):
                self.assertIsNone(
                    self.denial_reason(self.decision(tool, self.linked))
                )

    def test_an_unrelated_mcp_tool_from_a_linked_worktree_is_allowed(self):
        # R11 is about one server's write path, not about linked worktrees
        # being second-class.
        self.assertIsNone(
            self.denial_reason(
                self.decision("mcp__mempalace__mempalace_add_drawer", self.linked)
            )
        )

    def test_outside_a_repository_the_rule_fails_open(self):
        # A guard that denies where it cannot tell would block memory writes in
        # any checkout it does not understand.
        self.assertFalse(is_linked_worktree(str(self.container)))
        self.assertIsNone(
            self.denial_reason(
                self.decision("mcp__shaft-memory__remember_memory", self.container)
            )
        )


class MemoryWriteGuardIsReachableTest(unittest.TestCase):
    """A rule the host never invokes is a rule that does not exist.

    R11 lives behind a PreToolUse matcher. The matcher shipped as
    `Bash|PowerShell|shell_command`, which no MCP tool name matches -- so the
    rule could pass every test above and still never run once in production.
    That is the same shape as the defect it guards: a check whose pass is
    independent of the thing it verifies.
    """

    ROOT = Path(__file__).resolve().parents[2]

    def pretooluse_matchers(self, relative_path: str, *keys: str) -> list[str]:
        configuration = json.loads(
            (self.ROOT / relative_path).read_text(encoding="utf-8")
        )
        for key in keys:
            configuration = configuration[key]
        return [entry.get("matcher", "") for entry in configuration]

    def assert_matches_the_write_tools(self, matchers: list[str], host: str):
        import re

        for tool in (
            "mcp__shaft-memory__remember_memory",
            "mcp__shaft-memory__save_memory_patch",
        ):
            with self.subTest(host=host, tool=tool):
                self.assertTrue(
                    any(re.search(matcher, tool) for matcher in matchers if matcher),
                    f"{host} PreToolUse never invokes the guard for {tool}",
                )
        # The tools R11 deliberately leaves alone must not be dragged in.
        with self.subTest(host=host, tool="load_memory"):
            self.assertFalse(
                any(
                    re.search(matcher, "mcp__shaft-memory__load_memory")
                    for matcher in matchers
                    if matcher
                ),
                f"{host} PreToolUse fires on a memory read, which R11 allows",
            )

    def test_claude_invokes_the_guard_for_memory_writes(self):
        self.assert_matches_the_write_tools(
            self.pretooluse_matchers(".claude/settings.json", "hooks", "PreToolUse"),
            "claude",
        )

    def test_codex_invokes_the_guard_for_memory_writes(self):
        self.assert_matches_the_write_tools(
            self.pretooluse_matchers(".codex/hooks.json", "hooks", "PreToolUse"),
            "codex",
        )


if __name__ == "__main__":
    unittest.main()
