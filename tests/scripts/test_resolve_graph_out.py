"""Freshness tests for the shared Graphify cache resolver (#4639)."""

import json
import os
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

    def test_nightly_refresh_records_only_after_both_graphify_steps_succeed(self):
        wrapper = (ROOT / "tools/agent-infra/graphify-refresh.cmd").read_text(encoding="utf-8")
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        commands = [
            line.strip()
            for line in wrapper.splitlines()
            if line.strip().lower().startswith(("py ", "if errorlevel", "if not errorlevel"))
        ]

        self.assertEqual(
            [
                'py -3 -m graphify . > "%USERPROFILE%\\.agent-infra\\logs\\graphify-refresh.log" 2>&1',
                "if errorlevel 1 exit /b 1",
                "if not errorlevel 0 exit /b 1",
                'py -3 -m graphify cluster-only . >> "%USERPROFILE%\\.agent-infra\\logs\\graphify-refresh.log" 2>&1',
                "if errorlevel 1 exit /b 1",
                "if not errorlevel 0 exit /b 1",
                'py -3 tools\\repository-map\\resolve_graph_out.py --record-current >> "%USERPROFILE%\\.agent-infra\\logs\\graphify-refresh.log" 2>&1',
            ],
            commands,
        )
        self.assertIn("'tools/agent-infra/graphify-refresh.cmd'", workflow)

    @unittest.skipUnless(os.name == "nt", "Windows command wrapper regression")
    def test_nightly_refresh_rejects_positive_and_negative_stage_failures(self):
        wrapper_dir = self.sandbox / "tools/agent-infra"
        wrapper_dir.mkdir(parents=True)
        wrapper = wrapper_dir / "graphify-refresh.cmd"
        shutil.copy2(ROOT / "tools/agent-infra/graphify-refresh.cmd", wrapper)
        fake_python = self.sandbox / "fake-python"
        fake_python.mkdir()
        (fake_python / "graphify.py").write_text(
            """import os
import sys

if sys.argv[1:] == ["."]:
    raise SystemExit(int(os.environ["GRAPHIFY_BUILD_EXIT"]))
if sys.argv[1:] == ["cluster-only", "."]:
    raise SystemExit(int(os.environ["GRAPHIFY_CLUSTER_EXIT"]))
raise SystemExit(99)
""",
            encoding="utf-8",
        )
        marker = self.sandbox / "recorded.marker"
        (self.sandbox / ".agent-infra/logs").mkdir(parents=True)
        resolver_dir = self.sandbox / "tools/repository-map"
        resolver_dir.mkdir(parents=True)
        (resolver_dir / "resolve_graph_out.py").write_text(
            """import os
from pathlib import Path

Path(os.environ["GRAPHIFY_MARKER"]).touch()
""",
            encoding="utf-8",
        )

        base_env = os.environ.copy()
        base_env["PYTHONPATH"] = str(fake_python)
        base_env["USERPROFILE"] = str(self.sandbox)
        base_env["GRAPHIFY_MARKER"] = str(marker)
        cmd_executable = shutil.which("cmd.exe")
        self.assertIsNotNone(cmd_executable)
        cases = ((1, 0), (-1, 0), (0, 1), (0, -1))
        for build_exit, cluster_exit in cases:
            with self.subTest(build_exit=build_exit, cluster_exit=cluster_exit):
                marker.unlink(missing_ok=True)
                env = base_env.copy()
                env["GRAPHIFY_BUILD_EXIT"] = str(build_exit)
                env["GRAPHIFY_CLUSTER_EXIT"] = str(cluster_exit)
                result = subprocess.run(  # nosec B603 - fixed local command wrapper with controlled PATH fixture.
                    [cmd_executable, "/d", "/c", str(wrapper)],
                    cwd=ROOT,
                    env=env,
                    check=False,
                    capture_output=True,
                    text=True,
                )
                self.assertNotEqual(0, result.returncode)
                self.assertFalse(marker.exists())

        success_env = base_env.copy()
        success_env["GRAPHIFY_BUILD_EXIT"] = "0"
        success_env["GRAPHIFY_CLUSTER_EXIT"] = "0"
        success = subprocess.run(  # nosec B603 - fixed local command wrapper with controlled PATH fixture.
            [cmd_executable, "/d", "/c", str(wrapper)],
            cwd=ROOT,
            env=success_env,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, success.returncode, success.stderr)
        self.assertTrue(marker.exists())


if __name__ == "__main__":
    unittest.main()
