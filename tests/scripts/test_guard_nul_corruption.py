"""R10: refuse to stage or commit NUL-corrupted files."""
# Reproduces the 2026-08-04 incident (issue #4437): after an unclean shutdown,
# 652 of 653 files in an abandoned worktree were entirely NUL-filled with
# plausible sizes -- a 676-byte `.gitignore` had become 726 bytes of zeros.
# `git status` showed ordinary ' M' entries and only `git diff --shortstat`
# hinted at it ("653 files changed, 0 insertions(+), 0 deletions(-)").
#
# Every case below builds the corruption as a real fixture on disk and runs the
# real `git`, rather than asserting against a hand-written diff string.

from __future__ import annotations

import json
import os
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.agents.guard import (
    check_r10_nul_corruption,
    evaluate_command,
    nul_byte_ratio,
)

ROOT = Path(__file__).resolve().parents[2]
GUARD = ROOT / "scripts/agents/guard.py"

# A minimal, genuinely binary PNG: real binary content that is NOT NUL-filled.
PNG_BYTES = bytes.fromhex(
    "89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c4"
    "890000000a49444154789c6360000002000100ffff03000006000557bfabd400"
    "00000049454e44ae426082"
)


class ShellMultilineGuardTest(unittest.TestCase):
    """R23: shell multiline text must not write source or commit metadata."""

    def test_rejects_multiline_shell_metadata_but_allows_single_line_message(self):
        self.assertIsNotNone(evaluate_command('git commit -m "first\nsecond"'))
        self.assertIsNotNone(evaluate_command('git commit -m"first\nsecond"'))
        self.assertIsNotNone(evaluate_command('git commit --message="first\nsecond"'))
        self.assertIsNotNone(evaluate_command("cat <<EOF\ntext\nEOF"))
        self.assertIsNotNone(evaluate_command("cat <<'END-TEXT'\ntext\nEND-TEXT"))
        self.assertIsNotNone(evaluate_command("cat <<END.TEXT\ntext\nEND.TEXT"))
        self.assertIsNotNone(evaluate_command("cat <<'END TEXT'\ntext\nEND TEXT"))
        self.assertIsNotNone(evaluate_command("cat <<\\END-TEXT\ntext\nEND-TEXT"))
        self.assertIsNotNone(evaluate_command("cat <<-EOF\n\ttext\n\tEOF"))
        self.assertIsNotNone(evaluate_command("gh issue create --body x; cat <<EOF\ntext\nEOF"))
        self.assertIsNone(evaluate_command('git commit -m "single line"'))
        self.assertIsNone(evaluate_command('python -c "print(1 << 2)"'))
        self.assertIsNone(
            evaluate_command("gh pr create --title 'Fix A & B' --body-file - <<'EOF'\ntext\nEOF")
        )
        self.assertIsNone(evaluate_command("gh pr create --body-file - <<'END-TEXT'\ntext\nEND-TEXT"))


def git(cwd: Path, *arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
        ["git", *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


class NulCorruptionGuardTest(unittest.TestCase):
    """R10: refuse a write that would commit over NUL-corrupted files.

    An unclean shutdown zeroes files while leaving plausible sizes behind, so
    the damage reads as ordinary content until something opens it. The tell is
    `0 insertions(+), 0 deletions(-)` in `git diff --shortstat` -- a diff that
    changed nothing against files that visibly changed.

    R10 exists so that state is caught before it is committed, since a commit
    over corrupt files replaces the last good copy with the damaged one.
    """

    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        git(self.root, "init", "-q", "-b", "main", ".")
        git(self.root, "config", "user.email", "harness@example.invalid")
        git(self.root, "config", "user.name", "Harness")
        self.write_bytes(".gitignore", b"target/\nallure-results/\n")
        self.write_bytes("src/Example.java", b"class Example {\n}\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "initial")

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write_bytes(self, relative_path: str, content: bytes) -> Path:
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
        return path

    def corrupt(self, relative_path: str) -> None:
        """Overwrite a tracked file with NUL bytes at a plausible size."""
        path = self.root / relative_path
        self.write_bytes(relative_path, b"\x00" * (path.stat().st_size + 50))

    # --- the incident itself -------------------------------------------------

    def test_staging_a_nul_filled_tracked_file_is_denied(self):
        self.corrupt(".gitignore")
        reason = check_r10_nul_corruption("git add -A", str(self.root))
        self.assertIsNotNone(reason)
        self.assertIn("R10", reason)
        self.assertIn(".gitignore", reason)

    def test_committing_a_nul_filled_tracked_file_is_denied(self):
        self.corrupt("src/Example.java")
        git(self.root, "add", "-A")
        reason = check_r10_nul_corruption('git commit -m "wip"', str(self.root))
        self.assertIsNotNone(reason)
        self.assertIn("src/Example.java", reason)

    def test_commit_dash_a_is_denied_without_an_explicit_add(self):
        self.corrupt("src/Example.java")
        reason = check_r10_nul_corruption('git commit -am "wip"', str(self.root))
        self.assertIsNotNone(reason)

    def test_plain_commit_ignores_unrelated_unstaged_corruption(self):
        self.write_bytes("src/Example.java", b"class Example { // healthy work\n}\n")
        git(self.root, "add", "src/Example.java")
        self.corrupt(".gitignore")

        reason = check_r10_nul_corruption('git commit -m "healthy work"', str(self.root))

        self.assertIsNone(reason)

    def test_denial_names_the_restore_command_and_the_corruption_cause(self):
        self.corrupt(".gitignore")
        reason = check_r10_nul_corruption("git add -A", str(self.root))
        self.assertIn("git restore", reason)
        self.assertIn("NUL", reason)

    def test_git_dash_c_inspects_the_repository_it_names(self):
        # `git -C <worktree> ...` is this repository's documented way to touch
        # another worktree, so taking the directory from the calling process
        # instead of the command leaves that whole shape unguarded.
        self.corrupt(".gitignore")
        with tempfile.TemporaryDirectory() as elsewhere:
            reason = check_r10_nul_corruption(
                f'git -C "{self.root}" add -A', elsewhere
            )
        self.assertIsNotNone(reason)
        self.assertIn(".gitignore", reason)

    def test_git_work_tree_flag_inspects_the_repository_it_names(self):
        self.corrupt(".gitignore")
        with tempfile.TemporaryDirectory() as elsewhere:
            reason = check_r10_nul_corruption(
                f'git --git-dir="{self.root}/.git" --work-tree="{self.root}" add -A',
                elsewhere,
            )
        self.assertIsNotNone(reason)

    def test_git_dash_c_naming_a_clean_repository_is_allowed(self):
        # The mirror image: corruption in the calling directory must not deny a
        # command that operates somewhere else entirely.
        with tempfile.TemporaryDirectory() as clean:
            path = Path(clean)
            git(path, "init", "-q", "-b", "main", ".")
            git(path, "config", "user.email", "harness@example.invalid")
            git(path, "config", "user.name", "Harness")
            (path / "a.txt").write_bytes(b"content\n")
            git(path, "add", "-A")
            git(path, "commit", "-qm", "initial")
            (path / "a.txt").write_bytes(b"edited content\n")

            self.corrupt(".gitignore")
            reason = check_r10_nul_corruption(f'git -C "{clean}" add -A', str(self.root))
        self.assertIsNone(reason)

    def test_untracked_nul_file_is_caught_when_add_and_commit_are_chained(self):
        # `git diff HEAD` cannot see an untracked path, so a single-line
        # `add && commit` would otherwise commit a newly zeroed file.
        self.write_bytes("fresh.md", b"\x00" * 300)
        reason = check_r10_nul_corruption(
            'git add -A && git commit -m "wip"', str(self.root)
        )
        self.assertIsNotNone(reason)
        self.assertIn("fresh.md", reason)

    def test_untracked_healthy_file_is_allowed(self):
        self.write_bytes("fresh.md", b"# Fresh notes\n")
        self.assertIsNone(check_r10_nul_corruption("git add -A", str(self.root)))

    def test_ignored_files_are_not_scanned(self):
        self.write_bytes(".gitignore", b"target/\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "ignore target")
        self.write_bytes("target/classes.bin", b"\x00" * 4096)
        self.assertIsNone(check_r10_nul_corruption("git add -A", str(self.root)))

    def test_non_ascii_paths_are_scanned(self):
        self.write_bytes("café-日本.md", "# Café\n".encode("utf-8"))
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "add non-ascii")
        self.corrupt("café-日本.md")
        reason = check_r10_nul_corruption("git add -A", str(self.root))
        self.assertIsNotNone(reason)

    def test_powershell_single_quote_escape_cannot_bypass_a_corrupt_path(self):
        self.write_bytes("O'Brien.md", b"healthy\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "apostrophe path")
        self.corrupt("O'Brien.md")

        reason = check_r10_nul_corruption("git add 'O''Brien.md'", str(self.root))

        self.assertIsNotNone(reason)
        self.assertIn("O'Brien.md", reason)

    def test_powershell_leading_apostrophe_cannot_resolve_to_a_clean_sibling(self):
        self.write_bytes("lead.md", b"clean sibling\n")
        self.write_bytes("'lead.md", b"healthy\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "leading apostrophe")
        self.corrupt("'lead.md")

        reason = check_r10_nul_corruption("git add '''lead.md'", str(self.root))

        self.assertIsNotNone(reason)
        self.assertIn("'lead.md", reason)

    def test_powershell_backtick_escape_cannot_resolve_to_a_clean_sibling(self):
        self.write_bytes("a`$b.md", b"clean sibling\n")
        self.write_bytes("a$b.md", b"healthy\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "backtick path")
        self.corrupt("a$b.md")

        reason = check_r10_nul_corruption('git add "a`$b.md"', str(self.root))

        self.assertIsNotNone(reason)
        self.assertIn("a$b.md", reason)

    def test_an_explicit_pathspec_limits_the_scan(self):
        # One corrupt file must not make it impossible to rescue healthy work:
        # `git add <healthy>` names what it stages, so honour it.
        self.corrupt(".gitignore")
        self.write_bytes("src/Example.java", b"class Example {\n  // real work\n}\n")

        self.assertIsNone(
            check_r10_nul_corruption("git add src/Example.java", str(self.root))
        )
        self.assertIsNotNone(
            check_r10_nul_corruption("git add .gitignore", str(self.root))
        )
        self.assertIsNotNone(check_r10_nul_corruption("git add -A", str(self.root)))
        self.assertIsNotNone(check_r10_nul_corruption("git add .", str(self.root)))

    def test_a_pathspec_directory_covers_the_files_beneath_it(self):
        self.corrupt("src/Example.java")
        self.assertIsNotNone(check_r10_nul_corruption("git add src", str(self.root)))
        self.assertIsNone(check_r10_nul_corruption("git add .gitignore", str(self.root)))

    def test_commit_message_arguments_are_never_read_as_pathspecs(self):
        # `git commit -m src/Example.java` must not be read as "only scan that
        # path". Only an explicit `--` separator delimits a commit pathspec.
        self.corrupt(".gitignore")
        git(self.root, "add", "-A")
        self.assertIsNotNone(
            check_r10_nul_corruption(
                'git commit -m "touched src/Example.java"', str(self.root)
            )
        )

    def test_staging_nested_in_an_interpreter_argument_is_inspected(self):
        self.corrupt(".gitignore")
        self.assertIsNotNone(
            check_r10_nul_corruption('bash -c "git add -A"', str(self.root))
        )

    def test_many_corrupt_files_report_a_count_rather_than_every_path(self):
        for index in range(12):
            self.write_bytes(f"module{index}/File.java", b"class File {\n}\n")
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "more files")
        for index in range(12):
            self.corrupt(f"module{index}/File.java")
        reason = check_r10_nul_corruption("git add -A", str(self.root))
        self.assertIsNotNone(reason)
        self.assertIn("12 of", reason)
        self.assertIn("+7 more", reason)
        self.assertLess(reason.count("module"), 12)

    def test_candidate_limit_fails_closed_instead_of_skipping_late_corruption(self):
        candidates = [f"healthy-{index}.txt" for index in range(2000)] + ["corrupt.txt"]

        def ratio(path: str) -> float:
            return 1.0 if path.endswith("corrupt.txt") else 0.0

        with patch(
            "scripts.agents.guard._candidate_paths", return_value=candidates
        ), patch("scripts.agents.guard.nul_byte_ratio", side_effect=ratio):
            reason = check_r10_nul_corruption("git add -A", str(self.root))

        self.assertIsNotNone(reason)
        self.assertIn("2000", reason)
        self.assertIn("name smaller path sets", reason)
        self.assertIn("git commit -- <paths>", reason)

        with patch(
            "scripts.agents.guard._candidate_paths", return_value=candidates
        ), patch("scripts.agents.guard.nul_byte_ratio", side_effect=ratio):
            scoped_reason = check_r10_nul_corruption(
                "git commit -- healthy-0.txt", str(self.root)
            )
        self.assertIsNone(scoped_reason)

    def test_denial_does_not_recommend_discarding_the_whole_worktree(self):
        # The remedy must not destroy the healthy uncommitted work an agent is
        # trying to save -- that is the data loss this rule exists to prevent.
        self.corrupt(".gitignore")
        self.write_bytes("src/Example.java", b"class Example {\n  // real work\n}\n")
        reason = check_r10_nul_corruption("git add -A", str(self.root))
        self.assertIn("git restore --source=HEAD --staged --worktree -- .gitignore", reason)
        self.assertIn("Do not restore the whole worktree", reason)
        self.assertNotIn("--worktree -- .\n", reason)
        self.assertNotIn("for all of them", reason)

    # --- must not fire on healthy work --------------------------------------

    def test_ordinary_text_edit_is_allowed(self):
        self.write_bytes("src/Example.java", b"class Example {\n  // edited\n}\n")
        self.assertIsNone(check_r10_nul_corruption("git add -A", str(self.root)))

    def test_genuinely_binary_content_is_allowed(self):
        self.write_bytes("assets/logo.png", PNG_BYTES)
        git(self.root, "add", "-A")
        git(self.root, "commit", "-qm", "add logo")
        self.write_bytes("assets/logo.png", PNG_BYTES + PNG_BYTES)
        self.assertIsNone(check_r10_nul_corruption("git add -A", str(self.root)))

    def test_empty_file_is_allowed(self):
        self.write_bytes("src/Example.java", b"")
        self.assertIsNone(check_r10_nul_corruption("git add -A", str(self.root)))

    def test_staging_chained_after_another_git_command_is_still_inspected(self):
        # Agents routinely chain `git status && git add -A`. Stopping at the
        # first git segment would let the corrupt commit straight through.
        self.corrupt(".gitignore")
        for command in (
            "git status && git add -A",
            'git fetch origin; git commit -am "wip"',
            'git status | cat && git add . && git commit -m "wip"',
        ):
            self.assertIsNotNone(
                check_r10_nul_corruption(command, str(self.root)), command
            )

    def test_read_only_git_commands_are_not_inspected(self):
        self.corrupt(".gitignore")
        for command in ("git status", "git diff", "git log --oneline", "git push"):
            self.assertIsNone(
                check_r10_nul_corruption(command, str(self.root)), command
            )

    def test_prose_mentioning_git_add_is_not_a_real_command(self):
        self.corrupt(".gitignore")
        self.assertIsNone(
            check_r10_nul_corruption(
                'gh pr create --body "run git add -A first"', str(self.root)
            )
        )

    # --- fail open rather than block real work ------------------------------

    def test_missing_working_directory_fails_open(self):
        self.assertIsNone(check_r10_nul_corruption("git add -A", None))

    def test_directory_outside_any_repository_fails_open(self):
        with tempfile.TemporaryDirectory() as outside:
            self.assertIsNone(check_r10_nul_corruption("git add -A", outside))

    def test_repository_without_a_commit_fails_open(self):
        with tempfile.TemporaryDirectory() as fresh:
            path = Path(fresh)
            git(path, "init", "-q", "-b", "main", ".")
            (path / "a.txt").write_bytes(b"\x00" * 64)
            self.assertIsNone(check_r10_nul_corruption("git add -A", str(path)))

    def test_pure_command_evaluation_never_blocks_on_shape_alone(self):
        # evaluate_command stays repository-independent: R10 needs a working
        # directory, so a bare `git add`/`git commit` must never be blocked by
        # the command string on its own.
        for command in ("git add -A", 'git commit -m "message"', "git add ."):
            self.assertIsNone(evaluate_command(command), command)

    # --- the sampling primitive ---------------------------------------------

    def test_nul_byte_ratio_samples_beyond_the_first_block(self):
        # A file that merely starts with a long zero run is not corruption;
        # sampling only the head would misread it.
        padded = self.write_bytes("padded.bin", b"\x00" * 200_000 + b"payload" * 20_000)
        self.assertLess(nul_byte_ratio(padded), 0.95)

        wholly_nul = self.write_bytes("zeroed.bin", b"\x00" * 400_000)
        self.assertEqual(nul_byte_ratio(wholly_nul), 1.0)

    def test_nul_byte_ratio_returns_none_for_unreadable_or_empty_paths(self):
        self.assertIsNone(nul_byte_ratio(self.write_bytes("empty.txt", b"")))
        self.assertIsNone(nul_byte_ratio(self.root / "missing.txt"))

    # --- end to end through the hook ----------------------------------------

    def test_hook_denies_a_corrupt_commit_from_the_repository_working_directory(self):
        self.corrupt(".gitignore")
        completed = subprocess.run(  # nosec B603 - trusted interpreter and repo script.
            [sys.executable, str(GUARD)],
            input=json.dumps(
                {
                    "hook_event_name": "PreToolUse",
                    "tool_name": "Bash",
                    "tool_input": {"command": "git add -A"},
                }
            ),
            cwd=self.root,
            env=dict(os.environ, SHAFT_GUARD_HOST="claude"),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        specific = payload["hookSpecificOutput"]
        self.assertEqual(specific["permissionDecision"], "deny")
        self.assertIn("R10", specific["permissionDecisionReason"])

    def test_hook_prefers_the_supplied_working_directory_over_the_process_one(self):
        self.corrupt(".gitignore")
        completed = subprocess.run(  # nosec B603 - trusted interpreter and repo script.
            [sys.executable, str(GUARD)],
            input=json.dumps(
                {
                    "hook_event_name": "PreToolUse",
                    "tool_name": "Bash",
                    "tool_input": {"command": "git add -A"},
                    "cwd": str(self.root),
                }
            ),
            cwd=ROOT,
            env=dict(os.environ, SHAFT_GUARD_HOST="claude"),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertIn("R10", completed.stdout)


if __name__ == "__main__":
    unittest.main()
