"""Shared MemPalace and Graphify resolution for every ChaosEngine checkout."""

import hashlib
import json
import os
import shutil
import subprocess  # nosec B404 - tests run fixed local Git commands.
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
STORES = ROOT / "chaos-engine" / "stores.py"
TOOL = ROOT / "chaos-engine" / "tool.py"


def load_stores():
    import importlib.util

    spec = importlib.util.spec_from_file_location("chaos_engine_stores", STORES)
    module = importlib.util.module_from_spec(spec)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {STORES}")
    spec.loader.exec_module(module)
    return module


def load_tool():
    import importlib.util

    spec = importlib.util.spec_from_file_location("chaos_engine_tool_stores", TOOL)
    module = importlib.util.module_from_spec(spec)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {TOOL}")
    spec.loader.exec_module(module)
    return module


class SharedStoreTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.sandbox = Path(self.temporary.name)
        self.primary = self.sandbox / "primary"
        self.primary.mkdir()
        self.git("init", cwd=self.primary)
        self.git("config", "user.email", "stores@example.invalid", cwd=self.primary)
        self.git("config", "user.name", "Stores Test", cwd=self.primary)
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=self.primary)
        self.git("commit", "-m", "indexed source", cwd=self.primary)
        self.git("update-ref", "refs/remotes/origin/main", "HEAD", cwd=self.primary)
        self.git(
            "symbolic-ref",
            "refs/remotes/origin/HEAD",
            "refs/remotes/origin/main",
            cwd=self.primary,
        )
        self.linked = self.sandbox / "linked"
        self.git("worktree", "add", "-b", "feature", str(self.linked), cwd=self.primary)
        self.stores = load_stores()
        self.home = self.sandbox / "home"
        self.home.mkdir()

    def git(self, *args, cwd):
        git_executable = shutil.which("git")
        self.assertIsNotNone(git_executable)
        return subprocess.run(  # nosec B603 - resolved Git and fixture arguments.
            [git_executable, *args],
            cwd=cwd,
            check=True,
            capture_output=True,
            text=True,
        )

    def test_primary_and_linked_worktree_share_palace_and_graph_without_a_shaft_resolver(self):
        self.assertFalse((self.primary / "tools/repository-map/resolve_mempalace.py").exists())

        primary_palace = self.stores.resolve_palace(self.primary)
        linked_palace = self.stores.resolve_palace(self.linked)
        primary_graph = self.stores.resolve_graph_out(self.primary)
        linked_graph = self.stores.resolve_graph_out(self.linked)

        self.assertEqual(primary_palace, linked_palace)
        self.assertEqual(
            self.primary / ".git" / "chaos-engine" / "mempalace",
            primary_palace,
        )
        self.assertEqual(primary_graph, linked_graph)
        self.assertEqual(self.primary / "graphify-out", primary_graph)

    def test_tool_search_binds_the_shared_palace_before_the_subcommand(self):
        tool = load_tool()
        palace = self.stores.resolve_palace(self.linked)
        bound = tool.bind_store_invocation(
            "mempalace",
            ["/tools/mempalace", "search", "worktree palace"],
            self.linked,
            ROOT / "chaos-engine",
        )

        self.assertEqual(
            [
                "/tools/mempalace",
                "--palace",
                str(palace),
                "--backend",
                "sqlite_exact",
                "search",
                "worktree palace",
            ],
            bound,
        )
        self.assertLess(bound.index("--palace"), bound.index("search"))
        self.assertNotIn(str(Path.home() / ".mempalace"), " ".join(bound))

    def test_relative_and_blank_overrides_fail_closed(self):
        with mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE": "relative/palace"}):
            with self.assertRaisesRegex(RuntimeError, "CHAOS_ENGINE_MEMPALACE must be absolute"):
                self.stores.resolve_palace(self.primary)
        with mock.patch.dict(os.environ, {"SHAFT_GRAPHIFY_OUT": "   "}):
            with self.assertRaisesRegex(RuntimeError, "SHAFT_GRAPHIFY_OUT must not be blank"):
                self.stores.resolve_graph_out(self.primary)

    def test_second_refresh_sees_the_lock_and_does_not_build(self):
        calls = []

        def runner(command, cwd):
            calls.append(command)
            return 0

        with self.stores.refresh_lock(self.primary / ".git"):
            with self.assertRaisesRegex(RuntimeError, "already running"):
                self.stores.refresh(self.linked, runner=runner)

        self.assertEqual([], calls)

    def test_if_stale_does_not_build_when_the_marker_matches(self):
        revision = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        graph_out = self.primary / "graphify-out"
        graph_out.mkdir()
        (graph_out / "graph.json").write_text("{}\n", encoding="utf-8")
        manifest = graph_out / "manifest.json"
        manifest.write_text('{"nodes": 1}\n', encoding="utf-8")
        digest = hashlib.sha256(manifest.read_bytes()).hexdigest()
        (graph_out / ".chaos-engine-source-revision.json").write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "indexed_revision": revision,
                    "manifest_sha256": digest,
                }
            ),
            encoding="utf-8",
        )
        palace = self.stores.resolve_palace(self.primary)
        palace.mkdir(parents=True)
        (palace / "sqlite_exact.sqlite3").write_bytes(b"")
        calls = []

        fresh, _message = self.stores.graph_freshness(self.primary)
        self.assertTrue(fresh)
        result = self.stores.refresh(
            self.linked,
            if_stale=True,
            runner=lambda command, cwd: calls.append(command),
        )

        self.assertEqual(0, result)
        self.assertEqual([], calls)

    def test_refresh_does_not_create_a_home_palace(self):
        bare = self.sandbox / "no-origin"
        bare.mkdir()
        self.git("init", cwd=bare)
        with mock.patch.dict(os.environ, {"HOME": str(self.home)}):
            with self.assertRaisesRegex(RuntimeError, "fix-next: git fetch"):
                self.stores.refresh(bare, runner=lambda command, cwd: 0)

        self.assertFalse((self.home / ".mempalace").exists())

    def test_doctor_names_a_stale_graph_and_the_repair_command(self):
        graph_out = self.primary / "graphify-out"
        graph_out.mkdir()
        (graph_out / "graph.json").write_text("{}\n", encoding="utf-8")
        (graph_out / "manifest.json").write_text("{}\n", encoding="utf-8")
        (graph_out / ".chaos-engine-source-revision.json").write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "indexed_revision": "a" * 40,
                    "manifest_sha256": "b" * 64,
                }
            ),
            encoding="utf-8",
        )

        row = self.stores.graphify_doctor_row(self.linked)

        self.assertIsNotNone(row)
        self.assertEqual("degraded", row["status"])
        self.assertIn("stale", row["detail"])
        self.assertIn("repair --project . --component graphify", row["fixNext"])

    def test_session_start_spawns_once_and_does_not_wait(self):
        spawned = []

        def popen(command, **kwargs):
            spawned.append((command, kwargs))
            self.assertTrue(kwargs.get("start_new_session"))
            self.assertIs(kwargs.get("stdout"), subprocess.DEVNULL)
            return object()

        with mock.patch.dict(os.environ, {"CHAOS_ENGINE_STORE_REFRESH": "1"}):
            first = self.stores.maybe_spawn_refresh(self.linked, popen=popen)
            second = self.stores.maybe_spawn_refresh(self.linked, popen=popen)

        self.assertEqual("spawned", first)
        self.assertEqual("cooldown", second)
        self.assertEqual(1, len(spawned))
        self.assertIn("stores", spawned[0][0])
        self.assertIn("refresh", spawned[0][0])
        self.assertIn("--if-stale", spawned[0][0])

    def test_install_schedule_writes_a_daily_timer_without_a_home_palace(self):
        timer = self.stores.install_schedule(self.primary, home=self.home)
        service = timer.with_name("chaosengine-stores.service")
        text = timer.read_text(encoding="utf-8") + service.read_text(encoding="utf-8")

        self.assertIn("stores refresh --if-stale", text)
        self.assertIn("OnCalendar=daily", text)
        self.assertFalse((self.home / ".mempalace").exists())


if __name__ == "__main__":
    unittest.main()
