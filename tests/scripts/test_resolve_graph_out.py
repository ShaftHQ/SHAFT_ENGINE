"""Freshness tests for the shared Graphify cache resolver (#4639)."""

import json
import shutil
import subprocess  # nosec B404 - tests run fixed local Git and Python commands.
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/repository-map/resolve_graph_out.py"


class ResolveGraphOutTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.sandbox = Path(self.temporary.name)
        self.primary = self.sandbox / "primary"
        self.primary.mkdir()
        self.git("init", cwd=self.primary)
        self.git("config", "user.email", "graphify-test@example.invalid", cwd=self.primary)
        self.git("config", "user.name", "Graphify Test", cwd=self.primary)
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=self.primary)
        self.git("commit", "-m", "indexed source", cwd=self.primary)
        self.graph_out = self.primary / "graphify-out"
        self.graph_out.mkdir()
        (self.graph_out / "manifest.json").write_text("{}\n", encoding="utf-8")

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

    def resolver(self, *args, cwd=None):
        return subprocess.run(  # nosec B603 - current interpreter and repository-owned resolver.
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd or self.primary,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_non_empty_cache_without_revision_marker_is_stale(self):
        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("no indexed revision marker", completed.stderr)
        self.assertNotIn("absent -", completed.stderr)

    def test_non_empty_cache_without_manifest_is_stale_not_absent(self):
        (self.graph_out / "manifest.json").unlink()
        (self.graph_out / "partial-cache").write_text("partial\n", encoding="utf-8")

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("no manifest.json", completed.stderr)
        self.assertNotIn("absent -", completed.stderr)

    def test_recorded_cache_passes_at_the_same_revision(self):
        recorded = self.resolver("--record-current")
        checked = self.resolver("--check")

        self.assertEqual(0, recorded.returncode, recorded.stderr)
        self.assertEqual(0, checked.returncode, checked.stderr)
        marker = json.loads(
            (self.graph_out / ".shaft-source-revision.json").read_text(encoding="utf-8")
        )
        self.assertEqual(1, marker["schema_version"])
        self.assertEqual(
            self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip(),
            marker["indexed_revision"],
        )
        self.assertEqual(64, len(marker["manifest_sha256"]))

    def test_later_tracked_source_revision_makes_cache_stale(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        indexed = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        (self.primary / "later.py").write_text("print('later')\n", encoding="utf-8")
        self.git("add", "later.py", cwd=self.primary)
        self.git("commit", "-m", "later tracked source", cwd=self.primary)
        requested = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn(f"indexed={indexed}", completed.stderr)
        self.assertIn(f"requested={requested}", completed.stderr)

    def test_linked_worktree_is_checked_against_its_own_revision(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(linked), cwd=self.primary)
        (linked / "feature.py").write_text("print('feature')\n", encoding="utf-8")
        self.git("add", "feature.py", cwd=linked)
        self.git("commit", "-m", "feature source", cwd=linked)

        completed = self.resolver("--check", cwd=linked)
        record_attempt = self.resolver("--record-current", cwd=linked)

        self.assertEqual(1, completed.returncode)
        self.assertIn("stale -", completed.stderr)
        self.assertIn("requested=", completed.stderr)
        self.assertEqual(1, record_attempt.returncode)
        self.assertIn("primary checkout", record_attempt.stderr)

    def test_manifest_changed_after_marker_is_stale(self):
        self.assertEqual(0, self.resolver("--record-current").returncode)
        (self.graph_out / "manifest.json").write_text('{"changed": {}}\n', encoding="utf-8")

        completed = self.resolver("--check")

        self.assertEqual(1, completed.returncode)
        self.assertIn("manifest changed after revision marker", completed.stderr)

    def test_pr_gate_and_guidance_use_the_freshness_check(self):
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        guidance = (
            ROOT / ".agents/skills/act-as-mohab/references/graphify.md"
        ).read_text(encoding="utf-8")
        readme = (ROOT / "tools/repository-map/README.md").read_text(encoding="utf-8")

        self.assertIn("tests.scripts.test_resolve_graph_out", workflow)
        self.assertIn("'tools/repository-map/README.md'", workflow)
        self.assertIn("stale", guidance.lower())
        self.assertIn("--record-current", readme)
        self.assertIn("primary checkout", readme)


if __name__ == "__main__":
    unittest.main()
