"""Shared MemPalace path resolver (#5068)."""

import os
import shutil
import subprocess  # nosec B404 - tests run fixed local Git and Python commands.
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/repository-map/resolve_mempalace.py"


class ResolveMempalaceTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.sandbox = Path(self.temporary.name)
        self.primary = self.sandbox / "primary"
        self.primary.mkdir()
        self.git("init", cwd=self.primary)
        self.git("config", "user.email", "mempalace-test@example.invalid", cwd=self.primary)
        self.git("config", "user.name", "MemPalace Test", cwd=self.primary)
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=self.primary)
        self.git("commit", "-m", "indexed source", cwd=self.primary)
        self.palace = self.primary / ".git" / "chaos-engine" / "mempalace"
        self.palace.mkdir(parents=True)

    def git(self, *args, cwd):
        git_executable = shutil.which("git")
        self.assertIsNotNone(git_executable)
        return subprocess.run(  # nosec B603 - resolved Git executable and controlled fixture arguments.
            [git_executable, *args],
            cwd=cwd,
            check=True,
            capture_output=True,
            text=True,
        )

    def resolver(self, *args, cwd=None, env=None):
        return subprocess.run(  # nosec B603 - current interpreter and repository-owned resolver.
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd or self.primary,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_primary_and_linked_worktree_resolve_the_same_palace(self):
        linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(linked), cwd=self.primary)

        primary = self.resolver()
        worktree = self.resolver(cwd=linked)

        self.assertEqual(0, primary.returncode, primary.stderr)
        self.assertEqual(0, worktree.returncode, worktree.stderr)
        self.assertEqual(str(self.palace.resolve()), primary.stdout.strip())
        self.assertEqual(primary.stdout.strip(), worktree.stdout.strip())

    def test_absolute_environment_override_selects_external_palace(self):
        external = self.sandbox / "external-palace"
        environment = os.environ.copy()
        environment["SHAFT_MEMPALACE"] = str(external)

        completed = self.resolver(env=environment)

        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual(str(external.resolve()), completed.stdout.strip())

    def test_relative_environment_override_fails_closed(self):
        environment = os.environ.copy()
        environment["SHAFT_MEMPALACE"] = "relative/palace"

        completed = self.resolver(env=environment)

        self.assertEqual(1, completed.returncode)
        self.assertIn("SHAFT_MEMPALACE must be absolute", completed.stderr)

    def test_blank_environment_override_fails_closed(self):
        environment = os.environ.copy()
        environment["SHAFT_MEMPALACE"] = "   "

        completed = self.resolver(env=environment)

        self.assertEqual(1, completed.returncode)
        self.assertIn("SHAFT_MEMPALACE must not be blank", completed.stderr)

    def test_pr_gate_and_guidance_name_the_resolver_and_operator_command(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        readme = (ROOT / "tools/repository-map/README.md").read_text(encoding="utf-8")
        entrypoint = (ROOT / "chaos-engine/profiles/shaft/entrypoint.md").read_text(
            encoding="utf-8"
        )
        routing = (ROOT / "chaos-engine/profiles/shaft/references/routing.md").read_text(
            encoding="utf-8"
        )

        self.assertIn("tests.scripts.test_resolve_mempalace", workflow)
        self.assertIn("tests.scripts.test_knowledge_stores", workflow)
        self.assertIn("'tools/repository-map/resolve_mempalace.py'", workflow)
        self.assertIn("resolve_mempalace.py", readme)
        self.assertIn("scripts/agents/knowledge_stores.py", readme)
        self.assertIn("scripts/agents/knowledge_stores.py", entrypoint)
        self.assertIn("tools/repository-map/resolve_mempalace.py", entrypoint)
        self.assertIn("SHAFT-Nightly-Knowledge-Refresh", readme)
        self.assertIn("knowledge_stores.py", routing)
        self.assertNotIn("daydream", readme.lower())
        self.assertNotIn("daydream", entrypoint.lower())
        self.assertNotIn("daydream", routing.lower())


if __name__ == "__main__":
    unittest.main()
