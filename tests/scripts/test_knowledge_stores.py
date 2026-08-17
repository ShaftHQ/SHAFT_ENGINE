"""Operator CLI for centralized SHAFT knowledge stores (#5068)."""

import os
import shutil
import subprocess  # nosec B404 - tests run fixed local Git and Python commands.
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agents/knowledge_stores.py"


class KnowledgeStoresTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.sandbox = Path(self.temporary.name)
        self.primary = self.sandbox / "primary"
        self.primary.mkdir()
        self.git("init", cwd=self.primary)
        self.git("config", "user.email", "knowledge-stores@example.invalid", cwd=self.primary)
        self.git("config", "user.name", "Knowledge Stores Test", cwd=self.primary)
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=self.primary)
        self.git("commit", "-m", "indexed source", cwd=self.primary)
        self.palace = self.primary / ".git" / "chaos-engine" / "mempalace"
        self.palace.mkdir(parents=True)
        self.linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(self.linked), cwd=self.primary)
        self.log = self.sandbox / "mempalace.log"
        fake_bin = self.sandbox / "fake-bin"
        fake_bin.mkdir()
        if os.name == "nt":
            (fake_bin / "mempalace.cmd").write_text(
                "@echo off\r\n"
                "echo mempalace %* >>\"%KNOWLEDGE_STORES_LOG%\"\r\n"  # space before >>; cmd treats N>> as fd N
                "echo MEMPALACE-FAKE-OUTPUT\r\n",
                encoding="utf-8",
            )
        else:
            (fake_bin / "mempalace").write_text(
                "#!/bin/sh\n"
                "printf 'mempalace %s\\n' \"$*\" >>\"$KNOWLEDGE_STORES_LOG\"\n"
                "printf 'MEMPALACE-FAKE-OUTPUT\\n'\n",
                encoding="utf-8",
            )
            (fake_bin / "mempalace").chmod(0o755)
        self.env = os.environ.copy()
        self.env["PATH"] = str(fake_bin) + os.pathsep + self.env.get("PATH", "")
        self.env["KNOWLEDGE_STORES_LOG"] = str(self.log)

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

    def cli(self, *args, cwd=None):
        return subprocess.run(  # nosec B603 - current interpreter and repository-owned harness.
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd or self.linked,
            env=self.env,
            check=False,
            capture_output=True,
            text=True,
        )

    def checkout_palace_created(self, root):
        return (root / ".chaos-engine-state" / "mempalace").exists()

    def mempalace_tokens(self):
        logged = self.log.read_text(encoding="utf-8")
        line = next((item for item in logged.splitlines() if item.strip()), "")
        return line.split()

    def assert_global_flags_before(self, subcommand):
        tokens = self.mempalace_tokens()
        palace_at = tokens.index("--palace")
        backend_at = tokens.index("--backend")
        command_at = tokens.index(subcommand)
        self.assertLess(palace_at, command_at, tokens)
        self.assertLess(backend_at, command_at, tokens)
        self.assertEqual("sqlite_exact", tokens[backend_at + 1], tokens)
        self.assertEqual(str(self.palace.resolve()), tokens[palace_at + 1], tokens)
        return tokens

    def test_status_from_linked_worktree_uses_central_palace_and_graphify_check(self):
        completed = self.cli("status")
        combined = completed.stdout + completed.stderr

        self.assertEqual(0, completed.returncode, combined)
        self.assertIn(str(self.palace.resolve()), combined)
        self.assertIn("MEMPALACE-FAKE-OUTPUT", completed.stdout)
        self.assertTrue(self.log.exists(), combined)
        self.assert_global_flags_before("status")
        self.assertRegex(completed.stderr + completed.stdout, r"(?:absent|stale) -")
        self.assertIn("Graphify: degraded", completed.stderr)
        self.assertFalse(self.checkout_palace_created(self.linked))
        self.assertFalse(self.checkout_palace_created(self.primary))

    def test_search_from_linked_worktree_forwards_query_to_resolved_palace(self):
        completed = self.cli("search", "shared cache", "--wing", "shaft_engine_main")
        combined = completed.stdout + completed.stderr

        self.assertEqual(0, completed.returncode, combined)
        self.assertIn("MEMPALACE-FAKE-OUTPUT", completed.stdout)
        tokens = self.assert_global_flags_before("search")
        logged = self.log.read_text(encoding="utf-8")
        self.assertIn("shared cache", logged)
        self.assertIn("shaft_engine_main", tokens)
        self.assertFalse(self.checkout_palace_created(self.linked))

    def test_search_query_flag_forwards_to_resolved_palace(self):
        completed = self.cli(
            "search",
            "--query",
            "flag query",
            "--wing",
            "shaft_engine_main",
            "--room",
            "scripts",
            "--results",
            "4",
        )
        combined = completed.stdout + completed.stderr

        self.assertEqual(0, completed.returncode, combined)
        self.assertIn("MEMPALACE-FAKE-OUTPUT", completed.stdout)
        tokens = self.assert_global_flags_before("search")
        logged = self.log.read_text(encoding="utf-8")
        self.assertIn("flag query", logged)
        self.assertIn("shaft_engine_main", tokens)
        self.assertIn("scripts", tokens)
        self.assertIn("--results", tokens)
        self.assertIn("4", tokens)
        self.assertFalse(self.checkout_palace_created(self.linked))

    def test_top_level_query_flag_runs_search(self):
        completed = self.cli(
            "--query",
            "top level query",
            "--wing",
            "shaft_engine_main",
            "--room",
            "scripts",
            "--results",
            "3",
        )
        combined = completed.stdout + completed.stderr

        self.assertEqual(0, completed.returncode, combined)
        self.assertIn("MEMPALACE-FAKE-OUTPUT", completed.stdout)
        tokens = self.assert_global_flags_before("search")
        logged = self.log.read_text(encoding="utf-8")
        self.assertIn("top level query", logged)
        self.assertIn("shaft_engine_main", tokens)
        self.assertIn("scripts", tokens)
        self.assertIn("--results", tokens)
        self.assertIn("3", tokens)
        self.assertFalse(self.checkout_palace_created(self.linked))

    def test_search_rejects_disagreeing_positional_and_query_flag(self):
        completed = self.cli("search", "alpha", "--query", "beta")
        combined = completed.stdout + completed.stderr

        self.assertNotEqual(0, completed.returncode, combined)
        self.assertIn("must match", combined)
        self.assertFalse(self.log.exists())
        self.assertFalse(self.checkout_palace_created(self.linked))

    def test_search_matching_positional_and_query_flag_forwards_once(self):
        completed = self.cli("search", "same query", "--query", "same query")
        combined = completed.stdout + completed.stderr

        self.assertEqual(0, completed.returncode, combined)
        tokens = self.assert_global_flags_before("search")
        logged = self.log.read_text(encoding="utf-8")
        self.assertIn("same query", logged)
        self.assertEqual(1, logged.count("same query"))
        self.assertEqual("search", tokens[tokens.index("search")])
        self.assertFalse(self.checkout_palace_created(self.linked))

    def test_refresh_refuses_linked_worktree_and_ordinary_checkout(self):
        linked = self.cli("refresh", cwd=self.linked)
        primary = self.cli("refresh", cwd=self.primary)

        for completed in (linked, primary):
            combined = completed.stdout + completed.stderr
            self.assertNotEqual(0, completed.returncode, combined)
            self.assertIn("SHAFT-Nightly-Knowledge-Refresh", combined)
            self.assertIn("graphify_maintenance.py refresh", combined)
            self.assertFalse(self.log.exists())
        self.assertFalse(self.checkout_palace_created(self.linked))
        self.assertFalse(self.checkout_palace_created(self.primary))

    def test_relative_override_fails_closed_without_creating_a_palace(self):
        self.env["SHAFT_MEMPALACE"] = "relative/palace"
        completed = self.cli("status")

        self.assertEqual(1, completed.returncode)
        self.assertIn("SHAFT_MEMPALACE must be absolute", completed.stderr)
        self.assertFalse(self.checkout_palace_created(self.linked))
        self.assertFalse(self.log.exists())


if __name__ == "__main__":
    unittest.main()
