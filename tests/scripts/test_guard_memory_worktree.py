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
import os
import shutil
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch

from scripts.agents.guard import is_linked_worktree, run_pretooluse

ROOT = Path(__file__).resolve().parents[2]


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

    def decision(
        self, tool_name: str, cwd: Path, tool_input: dict | None = None
    ) -> dict | None:
        """Return the guard's hook output for one PreToolUse call, or None."""
        hook_input = {"tool_name": tool_name, "cwd": str(cwd)}
        if tool_input is not None:
            hook_input["tool_input"] = tool_input
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            run_pretooluse(hook_input)
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

    def test_memory_write_from_a_linked_worktree_without_a_target_is_denied(self):
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__remember_memory", self.linked)
        )
        self.assertIsNotNone(reason)
        self.assertIn("R11", reason)
        # The refusal has to be actionable, and BOTH remedies have to appear.
        # An earlier revision named only the CLI, on the false premise that the
        # server could not be targeted at all; the adversarial review of #4507
        # refuted that from the shipped bundle.
        self.assertIn("project_root", reason)
        self.assertIn("memory remember", reason)

    def test_the_refusal_does_not_claim_where_the_write_would_land(self):
        # The first revision asserted "this write would land in the PRIMARY
        # checkout". With `project_root` supplied that is false, and the rule
        # cannot see the argument unless it reads tool_input -- so the message
        # stated a falsehood it had no way to check. A guard that lies about
        # its reason teaches agents to distrust the guard.
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__remember_memory", self.linked)
        )
        self.assertNotIn("would land in the PRIMARY", reason)

    def test_a_write_explicitly_targeted_at_this_worktree_is_allowed(self):
        # `@aictx/memory@0.1.55` resolves every tool call's root as
        # resolve(server_cwd, args.project_root ?? ".") -- server.js:11538,
        # used by remember_memory at :11821 and save_memory_patch at :11965.
        # An absolute project_root naming this worktree therefore writes HERE,
        # and denying it would refuse the correct call.
        self.assertIsNone(
            self.denial_reason(
                self.decision(
                    "mcp__shaft-memory__remember_memory",
                    self.linked,
                    {"project_root": str(self.linked)},
                )
            )
        )

    def test_a_write_targeted_at_another_tree_is_still_denied(self):
        # Supplying the argument is not the point; supplying it correctly is.
        reason = self.denial_reason(
            self.decision(
                "mcp__shaft-memory__remember_memory",
                self.linked,
                {"project_root": str(self.primary)},
            )
        )
        self.assertIsNotNone(reason)
        self.assertIn("project_root", reason)

    def test_a_relative_project_root_is_denied_because_it_resolves_elsewhere(self):
        # The server's own argument description says relative paths resolve
        # from the MCP server launch directory, not from the agent's cwd, and
        # recommends absolute paths. "." is the default that produced the bug.
        #
        # The process cwd is forced to the fixture worktree, and that is the
        # whole point of this test. Without it `realpath(".")` resolved against
        # the test runner's directory -- never the fixture -- so all three
        # values were rejected INCIDENTALLY and the `isabs` clause the
        # docstring rests its safety argument on could be deleted with the
        # suite green. With cwd pinned here, `realpath(".")` equals the root and
        # only `isabs` can still refuse it.
        for value in (".", "..", "some/relative/path"):
            with self.subTest(project_root=value):
                with patch("os.getcwd", return_value=str(self.linked)):
                    self.assertIsNotNone(
                        self.denial_reason(
                            self.decision(
                                "mcp__shaft-memory__remember_memory",
                                self.linked,
                                {"project_root": value},
                            )
                        )
                    )

    def test_the_isabs_clause_is_what_refuses_the_default_project_root(self):
        # The sharpest form of the case above, as its own case so the reason it
        # exists cannot be lost: run the guard in a subprocess whose cwd IS the
        # worktree, so `os.path.realpath(".")` genuinely equals the root. The
        # only thing left standing between `project_root: "."` and an allow is
        # `os.path.isabs`.
        payload = json.dumps(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "mcp__shaft-memory__remember_memory",
                "tool_input": {"task": "t", "project_root": "."},
                "cwd": str(self.linked),
            }
        )
        completed = subprocess.run(  # nosec B603 B607 - the repository's own guard.
            [sys.executable, str(ROOT / "scripts/agents/guard.py")],
            input=payload,
            cwd=str(self.linked),
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertIn("R11", completed.stdout, completed.stdout)

    def test_a_write_targeted_at_a_subdirectory_of_this_worktree_is_allowed(self):
        # The server does not use the path it is given: `resolveProjectPaths`
        # runs it through `findGitRoot` (`git rev-parse --show-toplevel`) and
        # adopts that (server.js:1226-1240). Confirmed against the real server,
        # which reported the repository root for a `project_root` naming a
        # subdirectory. Demanding exact equality was therefore stricter than the
        # server's own semantics, and refused a write that would have landed
        # correctly -- an agent working one directory deep, which in this
        # repository is the normal shape for any Maven module.
        module = self.linked / "shaft-engine"
        module.mkdir()
        self.assertIsNone(
            self.denial_reason(
                self.decision(
                    "mcp__shaft-memory__remember_memory",
                    self.linked,
                    {"project_root": str(module)},
                )
            )
        )

    def test_the_rule_still_fires_from_a_subdirectory_of_the_worktree(self):
        # `worktree_root`'s upward walk carries the common case and nothing
        # pinned it: every other case passes the root itself, so deleting the
        # walk left the suite green while R11 silently stopped firing for every
        # agent one directory deep.
        module = self.linked / "shaft-engine"
        module.mkdir()
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__remember_memory", module)
        )
        self.assertIsNotNone(reason)
        self.assertIn("R11", reason)

    def test_the_refusal_names_this_worktree_as_the_value_to_pass(self):
        # The interpolated path is what an agent copies verbatim out of the
        # refusal. Asserting only the literal substring "project_root" let the
        # value itself be wrong with the suite green.
        reason = self.denial_reason(
            self.decision("mcp__shaft-memory__remember_memory", self.linked)
        )
        self.assertIn(str(self.linked), reason)

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

    def test_a_separate_git_dir_checkout_is_not_a_linked_worktree(self):
        # `git init --separate-git-dir` gives a PRIMARY checkout a `.git` file
        # holding a gitdir: pointer -- the same shape a linked worktree has.
        # Classifying it as linked would refuse memory writes in an ordinary
        # checkout and explain it with worktree isolation, which has nothing to
        # do with it. The real tell is where the pointer lands: a linked
        # worktree's gitdir sits under `<common>/worktrees/<name>`.
        separate = self.container / "separate"
        elsewhere = self.container / "elsewhere.git"
        separate.mkdir()
        git(separate, "init", "-q", "-b", "main", "--separate-git-dir", str(elsewhere), ".")
        self.assertTrue((separate / ".git").is_file())
        self.assertFalse(is_linked_worktree(str(separate)))

    def test_a_separate_git_dir_under_a_folder_named_worktrees_is_not_linked(self):
        # The first fix for this replaced one path heuristic with another: a
        # `gitdir:` pointer containing a `/worktrees/` segment. A
        # separate-git-dir checkout whose admin directory happens to sit under
        # any folder with that name still matched, and was still refused with a
        # message about worktree isolation. The structural tell -- git writes
        # `gitdir` and `commondir` INSIDE a linked worktree's admin directory
        # and nowhere else -- has no such surface.
        decoy = self.container / "decoy"
        admin = self.container / "worktrees" / "x.git"
        decoy.mkdir()
        admin.parent.mkdir(parents=True, exist_ok=True)
        git(decoy, "init", "-q", "-b", "main", "--separate-git-dir", str(admin), ".")
        pointer = (decoy / ".git").read_text(encoding="utf-8")
        self.assertIn("worktrees", pointer)
        self.assertFalse(is_linked_worktree(str(decoy)))

    def synthetic_admin(self, name: str, *files: str) -> Path:
        """A checkout whose `.git` points at an admin dir holding only `files`."""
        checkout = self.container / name
        admin = self.container / f"{name}-admin"
        checkout.mkdir()
        admin.mkdir()
        for filename in files:
            (admin / filename).write_text("x\n", encoding="utf-8")
        (checkout / ".git").write_text(f"gitdir: {admin}\n", encoding="utf-8")
        return checkout

    def test_both_admin_files_are_required_not_either(self):
        # The predicate rests on git writing BOTH `gitdir` and `commondir`
        # inside a linked worktree's admin directory. Nothing verified the
        # conjunction: the only negative fixture -- the separate-git-dir decoy
        # -- has NEITHER, so each half alone still classified it correctly and
        # either could be deleted with the suite green.
        self.assertFalse(is_linked_worktree(str(self.synthetic_admin("only-gitdir", "gitdir"))))
        self.assertFalse(
            is_linked_worktree(str(self.synthetic_admin("only-commondir", "commondir")))
        )
        self.assertTrue(
            is_linked_worktree(str(self.synthetic_admin("both", "gitdir", "commondir")))
        )

    def test_a_submodule_is_not_a_linked_worktree(self):
        # The docstring names submodules as a case the path-based predicates got
        # wrong, so it needs a fixture rather than an assertion.
        #
        # Built to git's real submodule layout instead of by running `git
        # submodule add`: that needs `protocol.file.allow=always` for a local
        # source and is refused outright in some environments, which turned this
        # into a permanent skip -- and a test that always skips is not a test.
        # What the predicate reads is the admin directory's contents, and a
        # submodule's `.git/modules/<name>` carries neither marker file.
        parent = self.container / "super"
        admin = parent / ".git" / "modules" / "sub"
        submodule = parent / "sub"
        admin.mkdir(parents=True)
        submodule.mkdir(parents=True)
        # Real submodule admin dirs hold these; neither is `gitdir`/`commondir`.
        (admin / "HEAD").write_text("ref: refs/heads/main\n", encoding="utf-8")
        (admin / "config").write_text("[core]\n", encoding="utf-8")
        (submodule / ".git").write_text(f"gitdir: {admin}\n", encoding="utf-8")
        self.assertFalse(is_linked_worktree(str(submodule)))

    def test_a_gitdir_pointer_with_a_bom_is_still_read(self):
        # `str.strip()` does not remove U+FEFF, so a BOM made the `gitdir:`
        # match miss and the rule fail open silently. Git never writes one; an
        # editor that rewrote the file might. The `utf-8-sig` that handles it
        # was unbound.
        checkout = self.container / "bom"
        admin = self.container / "bom-admin"
        checkout.mkdir()
        admin.mkdir()
        for filename in ("gitdir", "commondir"):
            (admin / filename).write_text("x\n", encoding="utf-8")
        (checkout / ".git").write_bytes(
            "﻿".encode("utf-8") + f"gitdir: {admin}\n".encode("utf-8")
        )
        self.assertTrue(is_linked_worktree(str(checkout)))

    def test_a_pruned_admin_directory_fails_open(self):
        # `git worktree prune`, a moved repository before `git worktree repair`,
        # or a half-deleted worktree leaves a `.git` file pointing at an admin
        # dir that is gone. R11 then stops firing. That is the documented
        # fail-open direction rather than a false denial, and it is new relative
        # to the path-based predicate, so pin the direction deliberately.
        checkout = self.synthetic_admin("pruned", "gitdir", "commondir")
        self.assertTrue(is_linked_worktree(str(checkout)))
        shutil.rmtree(self.container / "pruned-admin")
        self.assertFalse(is_linked_worktree(str(checkout)))

    def test_an_absolute_target_outside_any_repository_is_denied(self):
        # The "cannot prove it" branch of the project_root check: if the target
        # resolves to no checkout at all, nothing has been proven and the write
        # must still be refused. Inverting that branch left the suite green.
        outside = self.container / "not-a-repo"
        outside.mkdir()
        reason = self.denial_reason(
            self.decision(
                "mcp__shaft-memory__remember_memory",
                self.linked,
                {"project_root": str(outside)},
            )
        )
        self.assertIsNotNone(reason)

    def test_a_relative_gitdir_pointer_is_still_detected(self):
        # `git worktree add --relative-paths` writes `gitdir: ../p/.git/
        # worktrees/name`, which must resolve against the checkout holding the
        # `.git` file rather than against the process cwd.
        #
        # Built on a fresh directory pointing at the REAL admin directory
        # rather than by rewriting the real worktree's `.git`: that file is
        # read-only on Windows and the attribute survives `chmod`, so
        # overwriting it raises PermissionError. Pointing a new checkout at the
        # same admin dir exercises the relative branch without mutating a
        # worktree git owns.
        admin = (
            (self.linked / ".git").read_text(encoding="utf-8").split(":", 1)[1].strip()
        )
        synthetic = self.container / "relative-pointer"
        synthetic.mkdir()
        (synthetic / ".git").write_text(
            f"gitdir: {os.path.relpath(admin, synthetic)}\n", encoding="utf-8"
        )
        self.assertTrue(is_linked_worktree(str(synthetic)))

    def test_the_real_guard_process_denies_a_real_mcp_payload(self):
        # The unit cases call `run_pretooluse` in-process. This drives the
        # tracked script the hosts actually launch, over stdin, with the JSON
        # shape a host sends -- so the payload contract (tool_name, cwd,
        # tool_input) is exercised end to end rather than modelled.
        #
        # It still cannot prove that Claude or Codex *route* an MCP tool name
        # through PreToolUse at all; that is asserted by the matcher test below
        # and remains the one link in this chain nothing here executes.
        payload = json.dumps(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "mcp__shaft-memory__remember_memory",
                "tool_input": {"task": "t"},
                "cwd": str(self.linked),
            }
        )
        completed = subprocess.run(  # nosec B603 B607 - the repository's own guard.
            [sys.executable, str(ROOT / "scripts/agents/guard.py")],
            input=payload,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        output = json.loads(completed.stdout)
        self.assertEqual(
            output["hookSpecificOutput"]["permissionDecision"], "deny", completed.stdout
        )

    def test_the_real_guard_process_allows_a_targeted_mcp_payload(self):
        payload = json.dumps(
            {
                "hook_event_name": "PreToolUse",
                "tool_name": "mcp__shaft-memory__remember_memory",
                "tool_input": {"task": "t", "project_root": str(self.linked)},
                "cwd": str(self.linked),
            }
        )
        completed = subprocess.run(  # nosec B603 B607 - the repository's own guard.
            [sys.executable, str(ROOT / "scripts/agents/guard.py")],
            input=payload,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertEqual(completed.stdout.strip(), "", completed.stdout)

    def test_a_payload_without_cwd_falls_back_to_the_process_directory(self):
        # Not every host sends `cwd`. `_hook_working_directory` then falls back
        # to the hook process's own directory.
        #
        # The first version of this test asserted "no output" while letting the
        # fallback pick up the TEST RUNNER's real cwd -- so it passed from a
        # primary checkout and FAILED ON CLEAN CODE from a linked worktree,
        # where R11 correctly fires on the runner's own tree. CI runs
        # `actions/checkout`, a primary checkout, so it would have stayed green
        # forever while every agent working in the `ChaosEngine/*` worktree the
        # entrypoint mandates saw a red test they did not cause. It also masked
        # ten surviving mutations, which all reddened the suite through this one
        # failure rather than through the behaviour they attacked.
        #
        # Control the directory instead of inheriting it, and assert both
        # branches, so what the fallback does is pinned rather than ambient.
        with patch("os.getcwd", return_value=str(self.primary)):
            buffer = io.StringIO()
            with redirect_stdout(buffer):
                run_pretooluse(
                    {
                        "tool_name": "mcp__shaft-memory__remember_memory",
                        "tool_input": {"task": "t"},
                    }
                )
            self.assertEqual(buffer.getvalue().strip(), "")

        with patch("os.getcwd", return_value=str(self.linked)):
            buffer = io.StringIO()
            with redirect_stdout(buffer):
                run_pretooluse(
                    {
                        "tool_name": "mcp__shaft-memory__remember_memory",
                        "tool_input": {"task": "t"},
                    }
                )
            self.assertIn("R11", buffer.getvalue())

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
