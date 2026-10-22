"""#7278: shared stores resolve inside the worktree when the git dir is separate."""

from __future__ import annotations

import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load():
    spec = importlib.util.spec_from_file_location("ce_stores_7278", ROOT / "chaos-engine" / "stores.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def git(cwd: Path, *args: str) -> None:
    subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True)


class SeparateGitDir7278Test(unittest.TestCase):
    def test_separate_git_dir_resolves_to_the_worktree(self):
        stores = load()
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary).resolve()
            git(base, "init", "-q", "--separate-git-dir", str(base / "ws.git"), "work")
            work = base / "work"
            self.assertEqual(work, stores.resolve_primary_root(work))
            self.assertEqual(work / "graphify-out", stores.resolve_graph_out(work))

    def test_plain_repo_is_unchanged(self):
        stores = load()
        with tempfile.TemporaryDirectory() as temporary:
            work = Path(temporary).resolve()
            git(work, "init", "-q")
            self.assertEqual(work, stores.resolve_primary_root(work))
            self.assertEqual(work / "graphify-out", stores.resolve_graph_out(work))


if __name__ == "__main__":
    unittest.main()
