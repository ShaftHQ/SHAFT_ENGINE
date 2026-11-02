"""#7323: a chained hooks dir that holds secret_guard.py must not overwrite the installed guard."""
from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess  # nosec B404 - runs git in a temp repo only
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GUARD = ROOT / "chaos-engine" / "secret_guard.py"
HOOKS = ROOT / "chaos-engine" / "git_hooks.py"
GIT = shutil.which("git") or "git"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def git(cwd: Path, *args: str) -> subprocess.CompletedProcess:
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@e", GIT_COMMITTER_NAME="t",
               GIT_COMMITTER_EMAIL="t@e", CHAOS_ENGINE_SECRET_GUARD="1")  # nosec B106 - a switch, not a secret
    return subprocess.run([GIT, *args], cwd=cwd, capture_output=True, text=True, env=env)  # nosec B603


class ChainedSecretGuardTest(unittest.TestCase):
    def setUp(self):
        self.hooks = load(HOOKS, "git_hooks_7323")
        self.temp = tempfile.TemporaryDirectory()
        self.repo = Path(self.temp.name) / "project"
        self.repo.mkdir()
        git(self.repo, "init", "-q", "-b", "feature")
        chained = self.repo / "tools/git-hooks"
        chained.mkdir(parents=True)
        for name, text in {"secret_guard.py": "import sys\n", "commit-msg": "#!/bin/sh\nexit 0\n",
                           "__init__.py": "", "README": "docs\n"}.items():
            (chained / name).write_text(text, encoding="utf-8")
            (chained / name).chmod(0o755)
        git(self.repo, "config", "core.hooksPath", "tools/git-hooks")

    def tearDown(self):
        self.temp.cleanup()

    def test_chained_secret_guard_py_is_not_shimmed_and_commits_work(self):
        result = self.hooks.install_secret_guard(self.repo, GUARD)
        self.assertEqual(result["shims"], ["commit-msg"])
        target = Path(result["hooksPath"])
        self.assertEqual((target / "secret_guard.py").read_bytes(), GUARD.read_bytes())
        self.assertFalse((target / "__init__.py").exists())
        self.assertEqual(self.hooks.guard_health(self.repo)[0], "pass")
        (self.repo / "a.txt").write_text("hello\n", encoding="utf-8")
        git(self.repo, "add", "a.txt")
        committed = git(self.repo, "commit", "-q", "-m", "c")
        self.assertEqual(committed.returncode, 0, committed.stderr)

    def test_guard_health_fails_on_a_guard_that_does_not_compile(self):
        result = self.hooks.install_secret_guard(self.repo, GUARD)
        (Path(result["hooksPath"]) / "secret_guard.py").write_text('case "$x" in\n  ""|/*) ;;\n', encoding="utf-8")
        status, reason = self.hooks.guard_health(self.repo)
        self.assertEqual(status, "fail")
        self.assertIn("secret_guard.py", reason)


if __name__ == "__main__":
    unittest.main()
