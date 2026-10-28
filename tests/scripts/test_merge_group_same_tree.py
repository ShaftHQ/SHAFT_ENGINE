"""A merge-queue commit is skippable only when its tree is already the PR head."""

from __future__ import annotations

import os
import subprocess
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "merge_group_same_tree.py"


def git_env() -> dict[str, str]:
    env = os.environ.copy()
    env["GIT_CONFIG_GLOBAL"] = os.devnull
    env["GIT_CONFIG_SYSTEM"] = os.devnull
    env["GIT_AUTHOR_NAME"] = "Merge Tree Test"
    env["GIT_AUTHOR_EMAIL"] = "merge-tree-test@example.com"
    env["GIT_COMMITTER_NAME"] = "Merge Tree Test"
    env["GIT_COMMITTER_EMAIL"] = "merge-tree-test@example.com"
    return env


class TempRepo:
    def __init__(self, path: Path) -> None:
        self.path = path
        self.env = git_env()
        self.git("init", "-b", "main")
        self.git("config", "commit.gpgsign", "false")

    def git(self, *args: str) -> str:
        result = subprocess.run(
            ["git", "-C", str(self.path), *args],
            check=True,
            capture_output=True,
            text=True,
            env=self.env,
        )
        return result.stdout.strip()

    def commit(self, message: str, *, allow_empty: bool = False) -> None:
        command = ["commit", "--allow-empty", "-m", message] if allow_empty else ["commit", "-m", message]
        self.git(*command)

    def run_script(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["python3", str(SCRIPT)],
            cwd=self.path,
            capture_output=True,
            text=True,
            check=False,
            env=self.env,
        )


class MergeGroupSameTreeScriptTest(unittest.TestCase):
    def assert_token(self, result: subprocess.CompletedProcess[str], token: str) -> None:
        self.assertEqual(0, result.returncode)
        self.assertEqual(token + "\n", result.stdout)
        self.assertEqual("", result.stderr)

    def test_up_to_date_two_parent_merge_is_true(self) -> None:
        with TemporaryDirectory() as tmp:
            repo = TempRepo(Path(tmp))
            repo.commit("base", allow_empty=True)
            repo.git("checkout", "-b", "pr")
            repo.commit("feature", allow_empty=True)
            repo.git("checkout", "main")
            repo.git("merge", "--no-ff", "pr", "-m", "merge")
            result = repo.run_script()
        self.assert_token(result, "true")

    def test_main_moved_so_merge_tree_differs_is_false(self) -> None:
        with TemporaryDirectory() as tmp:
            repo = TempRepo(Path(tmp))
            (repo.path / "base.txt").write_text("base\n", encoding="utf-8")
            repo.git("add", "base.txt")
            repo.commit("base")
            repo.git("checkout", "-b", "pr")
            (repo.path / "feature.txt").write_text("feature\n", encoding="utf-8")
            repo.git("add", "feature.txt")
            repo.commit("feature")
            repo.git("checkout", "main")
            (repo.path / "base.txt").write_text("moved\n", encoding="utf-8")
            repo.git("add", "base.txt")
            repo.commit("main moved")
            repo.git("merge", "--no-ff", "pr", "-m", "merge")
            result = repo.run_script()
        self.assert_token(result, "false")

    def test_ordinary_commit_is_false(self) -> None:
        with TemporaryDirectory() as tmp:
            repo = TempRepo(Path(tmp))
            repo.commit("only", allow_empty=True)
            result = repo.run_script()
        self.assert_token(result, "false")

    def test_three_parent_commit_is_false(self) -> None:
        with TemporaryDirectory() as tmp:
            repo = TempRepo(Path(tmp))
            repo.commit("base", allow_empty=True)
            base = repo.git("rev-parse", "HEAD")
            repo.git("checkout", "-b", "p2")
            repo.commit("p2", allow_empty=True)
            second = repo.git("rev-parse", "HEAD")
            repo.git("checkout", "main")
            repo.git("checkout", "-b", "p3")
            repo.commit("p3", allow_empty=True)
            third = repo.git("rev-parse", "HEAD")
            repo.git("checkout", "main")
            tree = repo.git("rev-parse", "HEAD^{tree}")
            octopus = repo.git(
                "commit-tree", tree, "-p", base, "-p", second, "-p", third, "-m", "octopus"
            )
            repo.git("reset", "--hard", octopus)
            result = repo.run_script()
        self.assert_token(result, "false")


if __name__ == "__main__":
    unittest.main()
