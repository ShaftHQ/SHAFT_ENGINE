"""Shared repository-context resolution contract tests (#4726)."""

import unittest
import json
import subprocess  # nosec B404 - fixed test doubles only.
import tempfile
from pathlib import Path

try:
    from scripts.agents.repository_context import (
        RepositoryContextError,
        infer_repository,
        parse_git_remote,
        resolve_repository_context,
    )
except ImportError:
    RepositoryContextError = ValueError
    infer_repository = None
    parse_git_remote = None
    resolve_repository_context = None


class RepositoryContextTest(unittest.TestCase):
    def test_precedence_is_explicit_repo_then_pr_url_then_explicit_root_then_cwd(self):
        self.assertTrue(
            callable(resolve_repository_context),
            "the canonical repository-context resolver must exist",
        )
        inferred_from: list[Path] = []

        def runner(command, *, cwd, **_kwargs):
            inferred_from.append(cwd)
            return subprocess.CompletedProcess(
                command,
                0,
                stdout=json.dumps({"nameWithOwner": f"resolved/{cwd.name}"}),
                stderr="",
            )

        temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(temporary_directory.cleanup)
        base = Path(temporary_directory.name)
        explicit_root = base / "explicit-root"
        cwd_root = base / "cwd-root"
        explicit_root.mkdir()
        cwd_root.mkdir()
        cases = (
            (
                {
                    "explicit_repo": "explicit/repo",
                    "pr": "https://github.com/url/repo/pull/17",
                    "explicit_root": explicit_root,
                    "cwd": cwd_root,
                },
                "explicit/repo",
                explicit_root,
                17,
                0,
            ),
            (
                {
                    "explicit_repo": None,
                    "pr": "https://github.com/url/repo/pull/18",
                    "explicit_root": explicit_root,
                    "cwd": cwd_root,
                },
                "url/repo",
                explicit_root,
                18,
                0,
            ),
            (
                {
                    "explicit_repo": None,
                    "pr": 19,
                    "explicit_root": explicit_root,
                    "cwd": cwd_root,
                },
                "resolved/explicit-root",
                explicit_root,
                19,
                1,
            ),
            (
                {
                    "explicit_repo": None,
                    "pr": None,
                    "explicit_root": None,
                    "cwd": cwd_root,
                },
                "resolved/cwd-root",
                cwd_root,
                None,
                2,
            ),
        )

        for arguments, expected_repo, expected_root, expected_pr, inference_count in cases:
            with self.subTest(arguments=arguments):
                context = resolve_repository_context(
                    runner=runner,
                    executable_resolver=lambda name: name,
                    **arguments,
                )
                self.assertEqual(expected_repo, context.repo)
                self.assertEqual(expected_root.resolve(), context.root)
                self.assertEqual(expected_pr, context.pr_number)
                self.assertEqual(inference_count, len(inferred_from))

    def test_git_remote_parser_accepts_ssh_and_https(self):
        self.assertTrue(callable(parse_git_remote), "canonical git remote parsing must exist")
        self.assertEqual("owner/project", parse_git_remote("git@github.com:owner/project.git"))
        self.assertEqual("owner/project", parse_git_remote("https://github.com/owner/project.git"))

    def test_inference_uses_gh_then_git_and_rejects_unavailable_or_malformed_context(self):
        self.assertTrue(callable(infer_repository), "canonical repository inference must exist")
        root = Path("consumer").resolve()
        calls: list[tuple[list[str], Path]] = []

        def git_fallback(command, *, cwd, **_kwargs):
            calls.append((command, cwd))
            if command[0] == "gh":
                return subprocess.CompletedProcess(command, 1, stdout="", stderr="not authenticated")
            return subprocess.CompletedProcess(command, 0, stdout="git@github.com:owner/project.git\n", stderr="")

        self.assertEqual(
            "owner/project",
            infer_repository(root, runner=git_fallback, executable_resolver=lambda name: name),
        )
        self.assertEqual(["gh", "git"], [command[0] for command, _ in calls])
        self.assertTrue(all(cwd == root for _, cwd in calls))

        with self.assertRaises(RepositoryContextError):
            infer_repository(root, executable_resolver=lambda _name: None)
        with self.assertRaises(RepositoryContextError):
            infer_repository(
                root,
                runner=lambda command, **_kwargs: subprocess.CompletedProcess(
                    command, 0, stdout="not-a-remote", stderr=""
                ),
                executable_resolver=lambda name: name if name == "git" else None,
            )

    def test_invalid_root_and_runner_os_errors_are_context_errors(self):
        missing = Path(tempfile.gettempdir()) / "shaft-missing-repository-context-root"
        self.assertFalse(missing.exists())
        with self.assertRaisesRegex(RepositoryContextError, "directory"):
            resolve_repository_context(
                explicit_repo="owner/project",
                pr=1,
                explicit_root=missing,
                cwd=Path.cwd(),
            )

        with tempfile.TemporaryDirectory() as temporary_directory:
            with self.assertRaises(RepositoryContextError):
                infer_repository(
                    Path(temporary_directory),
                    runner=lambda *_args, **_kwargs: (_ for _ in ()).throw(OSError("missing")),
                    executable_resolver=lambda name: name,
                )


if __name__ == "__main__":
    unittest.main()
