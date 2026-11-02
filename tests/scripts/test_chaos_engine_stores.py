"""Shared MemPalace and Graphify resolution for every ChaosEngine checkout."""

import contextlib
import hashlib
import io
import json
import os
import shutil
import sqlite3
import subprocess  # nosec B404 - tests run fixed local Git commands.
import sys
import tempfile
import time
import unittest
import unittest.mock
from pathlib import Path


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

    def test_incremental_mine_paths_stays_incremental_for_large_change_sets(self):
        """#7465: a tip delta above the old 400-file limit must not force a full mine."""
        previous = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        for index in range(450):
            relative = self.primary / f"bulk_{index:04d}.py"
            relative.write_text(f"print({index})\n", encoding="utf-8")
        self.git("add", ".", cwd=self.primary)
        self.git("commit", "-m", "bulk change set", cwd=self.primary)
        tip = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        # Use the primary checkout as the snapshot stand-in for path listing.
        snapshot = self.primary
        paths = self.stores.incremental_mine_paths(snapshot, previous, tip)
        self.assertIsNotNone(paths)
        self.assertGreater(len(paths), 400)
        self.assertTrue(all(name.startswith("bulk_") for name in paths))
        # Missing stamp / non-ancestor still force full.
        self.assertIsNone(self.stores.incremental_mine_paths(snapshot, None, tip))
        self.assertIsNone(self.stores.incremental_mine_paths(snapshot, "f" * 40, tip))

    def test_second_clone_of_the_same_origin_reuses_populated_stores(self):
        registry = self.sandbox / "registry"
        registry.mkdir()
        origin = "https://example.invalid/org/repo.git"
        self.git("remote", "add", "origin", origin, cwd=self.primary)
        clone = self.sandbox / "clone"
        clone.mkdir()
        self.git("init", cwd=clone)
        self.git("config", "user.email", "stores@example.invalid", cwd=clone)
        self.git("config", "user.name", "Stores Test", cwd=clone)
        (clone / "keep.py").write_text("print('clone')\n", encoding="utf-8")
        self.git("add", "keep.py", cwd=clone)
        self.git("commit", "-m", "clone source", cwd=clone)
        self.git("remote", "add", "origin", origin, cwd=clone)
        palace = self.primary / ".git" / "chaos-engine" / "mempalace"
        palace.mkdir(parents=True)
        database = palace / "sqlite_exact.sqlite3"
        connection = sqlite3.connect(database)
        connection.execute(
            "create table documents (collection_id integer, id text, metadata_json text)"
        )
        connection.execute(
            "insert into documents values (1, 'd1', ?)",
            (json.dumps({"source_file": str(self.primary / "source.py")}),),
        )
        connection.commit()
        connection.close()
        graph = self.primary / "graphify-out"
        graph.mkdir()
        (graph / "graph.json").write_text("{}\n", encoding="utf-8")
        tool = self.sandbox / "mempalace"
        tool.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        tool.chmod(0o755)
        receipt = {
            "schemaVersion": 2,
            "scope": "user",
            "components": {"mempalace": {"action": "reuse"}},
            "commands": {"mempalace": str(tool)},
        }
        (self.primary / ".chaos-engine-dependencies.json").write_text(
            json.dumps(receipt), encoding="utf-8"
        )
        (self.primary / "source.py").write_text("print('indexed')\n", encoding="utf-8")
        (clone / "source.py").write_text("print('indexed')\n", encoding="utf-8")

        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STORE_REGISTRY": str(registry)}):
            self.assertEqual(palace, self.stores.resolve_palace(self.primary))
            self.assertEqual(graph, self.stores.resolve_graph_out(self.primary))
            self.assertEqual(palace, self.stores.resolve_palace(clone))
            self.assertEqual(graph, self.stores.resolve_graph_out(clone))
            self.assertTrue(self.stores.palace_records_source(clone, "source.py"))
            self.assertFalse((clone / ".git" / "chaos-engine" / "mempalace").exists())
            self.assertFalse((clone / "graphify-out").exists())

            import importlib.util

            dependencies_path = ROOT / "chaos-engine" / "dependencies.py"
            spec = importlib.util.spec_from_file_location(
                "chaos_engine_dependencies_stores", dependencies_path
            )
            dependencies = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(dependencies)
            self.assertEqual(
                [str(tool.resolve()), "--version"],
                dependencies.active_dispatch(clone, "mempalace", ["--version"]),
            )

            shim_path = ROOT / "chaos-engine" / "mempalace_fast_mine.py"
            shim_spec = importlib.util.spec_from_file_location(
                "chaos_engine_fast_mine_stores", shim_path
            )
            shim = importlib.util.module_from_spec(shim_spec)
            shim_spec.loader.exec_module(shim)

            def original(_collection, source_file, check_mtime=False):
                return source_file == str((self.primary / "source.py").resolve())

            wrapped = shim.wrap_file_already_mined(original)
            self.assertTrue(wrapped(None, str((clone / "source.py").resolve()), check_mtime=True))
            self.assertFalse(wrapped(None, str((clone / "missing.py").resolve())))

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
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE": "relative/palace"}):
            with self.assertRaisesRegex(RuntimeError, "CHAOS_ENGINE_MEMPALACE must be absolute"):
                self.stores.resolve_palace(self.primary)
        with unittest.mock.patch.dict(os.environ, {"SHAFT_GRAPHIFY_OUT": "   "}):
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

    def test_if_stale_refresh_skips_while_another_refresh_runs_6757(self):
        calls = []

        def runner(command, cwd):
            calls.append(command)
            return 0

        with self.stores.refresh_lock(self.primary / ".git"):
            self.assertEqual(0, self.stores.refresh(self.linked, if_stale=True, runner=runner))

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
        # #6377: a palace counts as current only with at least one document row,
        # and that mine must be stamped at the default-branch tip.
        connection = sqlite3.connect(palace / "sqlite_exact.sqlite3")
        connection.execute("create table documents (id integer primary key, body text)")
        connection.execute("insert into documents (body) values ('x')")
        connection.commit()
        connection.close()
        self.stores.write_palace_marker(palace, revision)
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

    def _fresh_graph(self, revision: str) -> None:
        graph_out = self.primary / "graphify-out"
        graph_out.mkdir(exist_ok=True)
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

    def _palace_with_one_row(self) -> Path:
        palace = self.stores.resolve_palace(self.primary)
        palace.mkdir(parents=True, exist_ok=True)
        connection = sqlite3.connect(palace / "sqlite_exact.sqlite3")
        connection.execute("create table documents (id integer primary key, body text)")
        connection.execute("insert into documents (body) values ('x')")
        connection.commit()
        connection.close()
        return palace

    def test_refresh_reuses_one_snapshot_and_skips_unchanged_mtimes(self):
        revision = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        self._fresh_graph(revision)
        palace = self._palace_with_one_row()
        calls: list[list[str]] = []
        self.ignores: list[list[str]] = []

        def runner(command, cwd):
            calls.append(command)
            ignore = Path(cwd) / ".gitignore"
            if "mine" in command and ignore.is_file():
                self.ignores.append(ignore.read_text(encoding="utf-8").split())
            return 0

        original_which = self.stores.shutil.which

        def which(name, *args, **kwargs):
            if name == "mempalace":
                return "/tools/mempalace"
            return original_which(name, *args, **kwargs)

        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which):
            self.assertEqual(0, self.stores.refresh(self.primary, runner=runner))
        common = self.stores.resolve_common_dir(self.primary)
        snapshot = self.stores.store_snapshot_path(common)
        source = snapshot / "source.py"
        self.assertTrue(source.is_file())
        untouched = source.stat().st_mtime_ns
        self.assertEqual(revision, self.stores.palace_indexed_revision(palace))
        mines = [command for command in calls if "mine" in command]
        syncs = [command for command in calls if "sync" in command]
        self.assertEqual(1, len(mines))
        self.assertEqual(1, len(syncs))
        self.assertIn(str(snapshot), mines[0])
        self.assertLess(mines[0].index("--palace"), mines[0].index("mine"))
        self.assertIn("--apply", syncs[0])
        self.assertIn(str(snapshot), syncs[0])

        (self.primary / "other.py").write_text("print('other')\n", encoding="utf-8")
        self.git("add", "other.py", cwd=self.primary)
        self.git("commit", "-m", "other file", cwd=self.primary)
        self.git("update-ref", "refs/remotes/origin/main", "HEAD", cwd=self.primary)
        moved = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        self._fresh_graph(moved)
        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which), \
                contextlib.redirect_stderr(io.StringIO()) as log:
            self.assertEqual(0, self.stores.refresh(self.primary, runner=runner))
        self.assertIn("mempalace mine (incremental, 1 changed file(s))", log.getvalue())
        self.assertEqual(untouched, source.stat().st_mtime_ns)
        self.assertEqual(moved, self.stores.palace_indexed_revision(palace))
        self.assertEqual(snapshot, self.stores.store_snapshot_path(common))
        self.assertEqual(2, len([command for command in calls if "mine" in command]))
        self.assertEqual(["/*", "!/other.py"], self.ignores[-1])
        self.assertFalse((snapshot / ".gitignore").exists())
        self.assertEqual(["other.py"], self.stores.incremental_mine_paths(snapshot, revision, moved))
        self.assertEqual([], self.stores.incremental_mine_paths(snapshot, moved, moved))
        self.assertIsNone(self.stores.incremental_mine_paths(snapshot, None, moved))
        self.assertIsNone(self.stores.incremental_mine_paths(snapshot, "f" * 40, moved))

        held = len(calls)
        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which):
            self.assertEqual(0, self.stores.refresh(self.primary, if_stale=False, runner=runner))
        self.assertEqual(held, len(calls))
        self.assertTrue(source.is_file())

    def test_retrieve_reports_graph_freshness_after_a_new_commit(self):
        # #6409: retrieve never answers from an outdated graph silently.
        import importlib.util

        spec = importlib.util.spec_from_file_location("ce_retrieve_6409", ROOT / "chaos-engine" / "retrieve.py")
        retrieve = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(retrieve)
        self.assertEqual("absent", retrieve.graph_freshness_field(self.primary)["status"])
        graph_out = self.primary / "graphify-out"
        graph_out.mkdir()
        (graph_out / "graph.json").write_text("{}\n", encoding="utf-8")
        (graph_out / "manifest.json").write_text('{"nodes": 1}\n', encoding="utf-8")
        self.stores.write_marker(graph_out, self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip())
        self.assertEqual("fresh", retrieve.graph_freshness_field(self.primary)["status"])
        (self.primary / "source.py").write_text("print('changed')\n", encoding="utf-8")
        self.git("commit", "-am", "new commit", cwd=self.primary)
        self.git("update-ref", "refs/remotes/origin/main", "HEAD", cwd=self.primary)
        stale = retrieve.graph_freshness_field(self.primary)
        self.assertEqual("stale", stale["status"])
        self.assertIn("stores refresh", stale["refresh"])

    def test_refresh_does_not_create_a_home_palace(self):
        bare = self.sandbox / "no-origin"
        bare.mkdir()
        self.git("init", cwd=bare)
        with unittest.mock.patch.dict(os.environ, {"HOME": str(self.home)}):
            with self.assertRaisesRegex(RuntimeError, "fix-next: git fetch"):
                self.stores.refresh(bare, runner=lambda command, cwd: 0)

        self.assertFalse((self.home / ".mempalace").exists())

    def test_local_only_repository_uses_its_local_default_branch(self):
        """#6493: without a remote the graph could never be marked fresh after install."""
        local = self.sandbox / "local-only"
        local.mkdir()
        self.git("init", "-b", "main", cwd=local)
        self.git("config", "user.email", "stores@example.invalid", cwd=local)
        self.git("config", "user.name", "Stores Test", cwd=local)
        (local / "source.py").write_text("print('local')\n", encoding="utf-8")
        self.git("add", "source.py", cwd=local)
        self.git("commit", "-m", "local source", cwd=local)
        head = self.git("rev-parse", "HEAD", cwd=local).stdout.strip()

        self.assertEqual(head, self.stores.default_branch_commit(local))

        self.git("remote", "add", "origin", "https://example.invalid/never-fetched.git", cwd=local)
        with self.assertRaisesRegex(RuntimeError, "fix-next: git fetch"):
            self.stores.default_branch_commit(local)

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

    def test_tip_lag_is_sync_advisory_and_ancestor_refresh_is_incremental(self):
        revision = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        self._fresh_graph(revision)
        (self.primary / "graphify-out" / "graph.json").write_text(
            '{"nodes":[{"id":"keep"}]}\n', encoding="utf-8"
        )
        self.stores.write_marker(
            self.primary / "graphify-out", revision
        )
        (self.primary / "other.py").write_text("print('other')\n", encoding="utf-8")
        self.git("add", "other.py", cwd=self.primary)
        self.git("commit", "-m", "other file", cwd=self.primary)
        self.git("update-ref", "refs/remotes/origin/main", "HEAD", cwd=self.primary)
        moved = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        row = self.stores.graphify_doctor_row(self.primary)
        self.assertEqual("sync-advisory", row["status"])
        self.assertIn(f"indexed={revision}", row["detail"])
        self.assertIn("stores refresh --if-stale", row["fixNext"])

        palace = self._palace_with_one_row()
        self.stores.write_palace_marker(palace, revision)
        seen = {"seeded": "", "mine": []}

        def runner(command, cwd):
            if "extract" in command:
                produced = Path(command[command.index("--out") + 1]) / "graphify-out"
                seen["seeded"] = (produced / "graph.json").read_text(encoding="utf-8")
                produced.mkdir(parents=True, exist_ok=True)
                (produced / "graph.json").write_text('{"nodes":[]}\n', encoding="utf-8")
                (produced / "manifest.json").write_text("{}\n", encoding="utf-8")
            if "mine" in command:
                seen["mine"].append(Path(cwd) / ".gitignore")
            return 0

        original_which = self.stores.shutil.which

        def which(name, *args, **kwargs):
            if name in {"graphify", "mempalace"}:
                return name
            return original_which(name, *args, **kwargs)

        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which), \
                contextlib.redirect_stderr(io.StringIO()) as log:
            self.assertEqual(0, self.stores.refresh(self.primary, runner=runner))
        self.assertIn('{"id":"keep"}', seen["seeded"])
        self.assertIn("stores: graphify incremental extract", log.getvalue())
        self.assertIn("mempalace mine (incremental, 1 changed file(s))", log.getvalue())
        self.assertEqual(
            ["other.py"],
            self.stores.incremental_mine_paths(
                self.stores.store_snapshot_path(self.stores.resolve_common_dir(self.primary)),
                revision,
                moved,
            ),
        )

        self._fresh_graph("f" * 40)
        (self.primary / "graphify-out" / "graph.json").write_text(
            '{"nodes":[{"id":"foreign"}]}\n', encoding="utf-8"
        )
        self.stores.write_marker(self.primary / "graphify-out", "f" * 40)
        (palace / self.stores.GENERIC_MARKER).unlink()

        def full_runner(command, cwd):
            if "extract" in command:
                produced = Path(command[command.index("--out") + 1]) / "graphify-out"
                self.assertFalse((produced / "graph.json").exists())
                produced.mkdir(parents=True, exist_ok=True)
                (produced / "graph.json").write_text('{"nodes":[]}\n', encoding="utf-8")
                (produced / "manifest.json").write_text("{}\n", encoding="utf-8")
            return 0

        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which), \
                contextlib.redirect_stderr(io.StringIO()) as full_log:
            self.assertEqual(0, self.stores.refresh(self.primary, runner=full_runner))
        self.assertIn("stores: graphify full extract", full_log.getvalue())
        self.assertIn("mempalace mine (full tree)", full_log.getvalue())

        current = self.git("rev-parse", "HEAD", cwd=self.primary).stdout.strip()
        self._fresh_graph(current)
        self.stores.write_palace_marker(palace, current)
        calls = []
        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which):
            self.assertEqual(
                0,
                self.stores.refresh(self.primary, runner=lambda command, cwd: calls.append(command)),
            )
        self.assertEqual([], calls)

    def test_busy_if_stale_refresh_drops_the_attempt_stamp(self):
        common = self.stores.resolve_common_dir(self.primary)
        stamp = common / "chaos-engine" / self.stores.ATTEMPT_STAMP
        stamp.parent.mkdir(parents=True, exist_ok=True)
        stamp.write_text(("a" * 40) + "\n", encoding="utf-8")
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STORE_REFRESH": "1"}):
            with self.stores.refresh_lock(common):
                self.assertEqual(0, self.stores.refresh(self.primary, if_stale=True, runner=lambda command, cwd: 0))
                self.assertEqual(
                    "running",
                    self.stores.maybe_spawn_refresh(
                        self.primary,
                        popen=lambda *_args, **_kwargs: (_ for _ in ()).throw(AssertionError("spawned")),
                    ),
                )
        self.assertFalse(stamp.exists())

    def test_session_start_spawns_once_and_does_not_wait(self):
        spawned = []

        def popen(command, **kwargs):
            spawned.append((command, kwargs))
            self.assertTrue(kwargs.get("start_new_session"))
            self.assertIs(kwargs.get("stdout"), subprocess.DEVNULL)
            return object()

        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STORE_REFRESH": "1"}):
            first = self.stores.maybe_spawn_refresh(self.linked, popen=popen)
            second = self.stores.maybe_spawn_refresh(self.linked, popen=popen)

        self.assertEqual("spawned", first)
        self.assertEqual("cooldown", second)
        self.assertEqual(1, len(spawned))
        self.assertIn("stores", spawned[0][0])
        self.assertIn("refresh", spawned[0][0])
        self.assertIn("--if-stale", spawned[0][0])

        stamp = self.stores.resolve_common_dir(self.linked) / "chaos-engine" / self.stores.ATTEMPT_STAMP
        stamp.write_text(("c" * 40) + "\n", encoding="utf-8")
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STORE_REFRESH": "1"}):
            moved = self.stores.maybe_spawn_refresh(self.linked, popen=popen)
        self.assertEqual("spawned", moved)
        self.assertEqual(2, len(spawned))

    def test_install_schedule_writes_a_daily_timer_without_a_home_palace(self):
        timer = self.stores.install_schedule(self.primary, home=self.home)
        service = timer.with_name("chaosengine-stores.service")
        text = timer.read_text(encoding="utf-8") + service.read_text(encoding="utf-8")

        self.assertIn("stores refresh --if-stale", text)
        self.assertIn("OnCalendar=daily", text)
        self.assertFalse((self.home / ".mempalace").exists())

    def test_mcp_args_and_cli_resolver_use_the_same_palace(self):
        import importlib.util

        hosts_spec = importlib.util.spec_from_file_location(
            "ce_hosts_palace_6248", ROOT / "chaos-engine" / "hosts.py"
        )
        if hosts_spec is None or hosts_spec.loader is None:
            raise AssertionError("cannot load chaos-engine/hosts.py")
        hosts = importlib.util.module_from_spec(hosts_spec)
        hosts_spec.loader.exec_module(hosts)
        deps_spec = importlib.util.spec_from_file_location(
            "ce_deps_palace_6248", ROOT / "chaos-engine" / "dependencies.py"
        )
        if deps_spec is None or deps_spec.loader is None:
            raise AssertionError("cannot load chaos-engine/dependencies.py")
        deps = importlib.util.module_from_spec(deps_spec)
        deps_spec.loader.exec_module(deps)

        canonical = self.stores.resolve_palace(self.linked)
        self.assertEqual(canonical, deps.mempalace_project_palace(self.linked))
        tool_spec = importlib.util.spec_from_file_location(
            "ce_tool_palace_6248", ROOT / "chaos-engine" / "tool.py"
        )
        if tool_spec is None or tool_spec.loader is None:
            raise AssertionError("cannot load chaos-engine/tool.py")
        tool = importlib.util.module_from_spec(tool_spec)
        tool_spec.loader.exec_module(tool)
        tool_palace = tool._load_stores(ROOT / "chaos-engine")["resolve_palace"](self.linked)
        self.assertEqual(canonical, tool_palace)
        arguments = hosts.mempalace_mcp_arguments(self.linked)
        self.assertEqual([".chaos-engine/tool.py", "mempalace-mcp"], arguments)
        self.assertNotIn("--palace", arguments)
        self.assertNotIn("--backend", arguments)

        legacy = self.linked / ".chaos-engine-state" / "mempalace"
        legacy.mkdir(parents=True)
        (legacy / "keep.txt").write_text("legacy data\n", encoding="utf-8")
        hint = self.stores.palace_migration_hint(self.linked)
        self.assertIsNotNone(hint)
        self.assertIn(str(legacy.resolve()), hint)
        self.assertIn(str(canonical), hint)
        self.assertTrue((legacy / "keep.txt").is_file())
        digest = hosts.doctor_digest(
            {
                "status": "healthy",
                "components": {"mempalace": {"status": "healthy", "migrationHint": hint}},
            }
        )
        self.assertIn("legacy MemPalace directory", digest)
        self.assertIsNone(self.stores.palace_migration_hint(self.primary))

    def test_documentation_refresh_files_mdx_headings_and_links(self):
        corpus = self.sandbox / "docs-corpus"
        (corpus / "journeys").mkdir(parents=True)
        (corpus / "guide.md").write_text("# Guide page\n\n[Start](journeys/start.mdx)\n", encoding="utf-8")
        (corpus / "journeys" / "start.mdx").write_text(
            "# Journey start\n\n[Back](../guide.md)\n",
            encoding="utf-8",
        )
        indexed = self.stores.index_documentation(corpus)
        paths = {page["path"] for page in indexed["pages"]}
        self.assertEqual(paths, {"guide.md", "journeys/start.mdx"})
        mdx = next(page for page in indexed["pages"] if page["path"].endswith(".mdx"))
        self.assertEqual(mdx["headings"], ["Journey start"])
        self.assertEqual(mdx["links"], ["../guide.md"])

        scratch = self.sandbox / "graph-scratch"
        original_which = self.stores.shutil.which

        def which(name, *args, **kwargs):
            if name in {"graphify", "mempalace"}:
                return name
            return original_which(name, *args, **kwargs)

        def graph_runner(command, cwd):
            produced = scratch / "graph-out" / "graphify-out"
            produced.mkdir(parents=True, exist_ok=True)
            (produced / "graph.json").write_text('{"nodes": [], "edges": []}\n', encoding="utf-8")
            (produced / "manifest.json").write_text("{}\n", encoding="utf-8")
            return 0

        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which):
            self.stores._refresh_graphify(
                self.primary,
                snapshot=corpus,
                scratch=scratch,
                revision="c" * 40,
                invoke=graph_runner,
            )
        graph = json.loads((self.stores.resolve_graph_out(self.primary) / "graph.json").read_text(encoding="utf-8"))
        document = next(node for node in graph["nodes"] if node["path"] == "journeys/start.mdx")
        self.assertEqual(document["label"], "Journey start")
        self.assertIn(
            {"source": "docs:journeys/start.mdx", "target": "../guide.md", "kind": "link"},
            graph["edges"],
        )
        filing = json.loads(
            (self.stores.resolve_graph_out(self.primary) / "documentation-pages.json").read_text(encoding="utf-8")
        )
        self.assertIn("journeys/start.mdx", {page["path"] for page in filing["pages"]})

        def mine_runner(command, cwd):
            return 0

        with unittest.mock.patch.object(self.stores.shutil, "which", side_effect=which):
            self.stores._refresh_mempalace(
                self.primary,
                snapshot=corpus,
                primary=self.primary,
                revision="c" * 40,
                invoke=mine_runner,
            )
        palace_filing = json.loads(
            (self.stores.resolve_palace(self.primary) / "documentation-pages.json").read_text(encoding="utf-8")
        )
        self.assertIn("journeys/start.mdx", {page["path"] for page in palace_filing["pages"]})


class DocumentationPassTest(unittest.TestCase):
    """The Markdown pass honours .graphifyignore and files every section, not just the title."""

    def setUp(self):
        self.stores = load_stores()
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)

    def _write(self, relative: str, text: str = "# Page\n") -> None:
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def test_graphifyignore_drops_harness_pages_but_not_bare_doc_suffix_rules(self):
        for relative in (".chaos-engine/identity.md", ".agents/skills/x/SKILL.md", "notes/keep.md",
                         "notes/drafts/skip.md", "notes/drafts/keep-me.md", "deep/target-like/page.mdx"):
            self._write(relative)
        (self.root / ".graphifyignore").write_text(
            "# harness\n.chaos-engine/\n.agents/\n*.md\n**/*.mdx\nnotes/drafts/\n!notes/drafts/keep-me.md\n",
            encoding="utf-8",
        )
        paths = [page["path"] for page in self.stores.index_documentation(self.root)["pages"]]
        self.assertEqual(["deep/target-like/page.mdx", "notes/drafts/keep-me.md", "notes/keep.md"], paths)

    def test_sections_become_graph_nodes_with_trail_labels_and_body_text(self):
        self._write("teaching/leveling-up.md", (
            "# Leveling Up\n\nIntro line.\n\n## Round 34\n\nAI-native cohort of 40 engineers.\n\n"
            "### Outcomes\n\nShipped SHAFT pipelines.\n\n```bash\n# not a heading\n```\n\n## Round 34\n\nRepeat.\n"
        ))
        page = self.stores.index_documentation(self.root)["pages"][0]
        self.assertEqual(["Leveling Up", "Round 34", "Outcomes", "Round 34"], page["headings"])
        graph = self.stores.merge_documentation_graph(self.root / "graph.json", {"pages": [page]})
        nodes = {node["id"]: node for node in graph["nodes"]}
        top = nodes["docs:teaching/leveling-up.md"]
        self.assertEqual("Leveling Up", top["label"])
        self.assertEqual("Intro line.", top["rationale"])
        round_34 = nodes["docs:teaching/leveling-up.md#round-34"]
        self.assertEqual("Leveling Up › Round 34", round_34["label"])
        self.assertEqual("AI-native cohort of 40 engineers.", round_34["rationale"])
        self.assertEqual("teaching/leveling-up.md", round_34["source_file"])
        outcomes = nodes["docs:teaching/leveling-up.md#outcomes"]
        self.assertEqual("Leveling Up › Round 34 › Outcomes", outcomes["label"])
        self.assertIn("# not a heading", outcomes["rationale"])
        self.assertIn("docs:teaching/leveling-up.md#round-34-1", nodes)
        self.assertIn(
            {"source": "docs:teaching/leveling-up.md", "target": "docs:teaching/leveling-up.md#outcomes",
             "kind": "contains"},
            graph["edges"],
        )

    @unittest.skipUnless(shutil.which("graphify"), "graphify CLI not installed")
    def test_graphify_query_finds_text_under_a_sub_heading(self):
        self._write("teaching/leveling-up.md", "# Leveling Up\n\n## Cohorts\n\nRound thirtyfour mentoring.\n")
        self._write("other.md", "# Other\n\nUnrelated.\n")
        graph = self.root / "graph.json"
        graph.write_text('{"directed": false, "multigraph": false, "graph": {}, "nodes": [], "edges": []}\n',
                         encoding="utf-8")
        self.stores.merge_documentation_graph(graph, self.stores.index_documentation(self.root))
        completed = subprocess.run(  # nosec B603 B607 - fixed graphify argv.
            ["graphify", "query", "thirtyfour mentoring", "--graph", str(graph)],
            capture_output=True, text=True, check=False, timeout=60,
        )
        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertIn("Leveling Up › Cohorts", completed.stdout)


class StoreIndexAttributionTest(unittest.TestCase):
    """Doc nodes name their file; an incremental mine stays confined to its paths."""

    PATHS = ["chaos-engine/skills/architect/SKILL.md", "chaos-engine/skills/architect/references/design.md", "top.md"]

    def setUp(self):
        self.stores = load_stores()
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)

    def _tree(self) -> None:
        for relative in [*self.PATHS, "chaos-engine/skills/architect/references/other.md",
                         "chaos-engine/skills/kanban/SKILL.md", "chaos-engine/tool.py", "drop.md"]:
            (self.root / relative).parent.mkdir(parents=True, exist_ok=True)
            (self.root / relative).write_text("x\n", encoding="utf-8")
        (self.root / ".gitignore").write_text("\n".join(self.stores.confine_rules(self.PATHS)) + "\n", encoding="utf-8")

    def test_doc_node_carries_source_file_and_retrieve_names_it(self):
        graph = self.root / "graph.json"
        merged = self.stores.merge_documentation_graph(
            graph, {"pages": [{"path": self.PATHS[0], "headings": ["Architect"], "links": []}]})
        node = next(n for n in merged["nodes"] if n["id"] == "docs:" + self.PATHS[0])
        self.assertEqual(self.PATHS[0], node["source_file"])
        import importlib.util
        spec = importlib.util.spec_from_file_location("ce_retrieve_doc_src", ROOT / "chaos-engine" / "retrieve.py")
        retrieve = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(retrieve)
        line = f"NODE {node['label']} [src={node['source_file']} loc= community=]"
        self.assertEqual([{"path": self.PATHS[0]}], retrieve._structured_hits(line, query="architect"))

    def test_confine_rules_have_no_wildcard_negation_and_git_keeps_only_paths(self):
        self._tree()
        rules = self.stores.confine_rules(self.PATHS)
        self.assertNotIn("!*/", rules)
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)  # nosec B603 B607
        listed = subprocess.run(  # nosec B603 B607
            ["git", "ls-files", "--others", "--exclude-standard"], cwd=self.root,
            capture_output=True, text=True, check=True).stdout.split()
        self.assertEqual(sorted(self.PATHS), sorted(listed))

    def test_mempalace_scanner_mines_only_the_paths(self):
        try:
            from mempalace.miner import scan_project
        except ImportError:
            self.skipTest("mempalace is not importable")
        self._tree()
        found = sorted(Path(p).relative_to(self.root.resolve()).as_posix() for p in scan_project(str(self.root)))
        self.assertEqual(sorted(self.PATHS), found)


class PalaceSnapshot7118Test(unittest.TestCase):
    """#7118: a snapshot during a mine is refused; a consistent snapshot passes quick_check."""

    def setUp(self):
        self.stores = load_stores()
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)  # nosec B603 B607
        self.palace = self.root / ".git" / "chaos-engine" / "mempalace"
        self.palace.mkdir(parents=True)
        self.database = self.palace / "sqlite_exact.sqlite3"
        connection = sqlite3.connect(self.database)
        connection.execute("create table documents(id integer primary key, body text)")
        connection.execute("insert into documents(body) values ('live')")
        connection.commit()
        connection.close()

    def test_snapshot_is_consistent_and_refused_while_a_mine_holds_the_lease(self):
        destination = self.root / "snap.sqlite3"
        self.assertEqual("ok", self.stores.snapshot_palace_database(self.root, destination))
        self.assertEqual("ok", self.stores.sqlite_quick_check(destination))
        copied = sqlite3.connect(destination)
        try:
            self.assertEqual("live", copied.execute("select body from documents").fetchone()[0])
        finally:
            copied.close()
        flag = self.root / "held"
        holder = subprocess.Popen(  # nosec B603 - fixed interpreter holding the store lease.
            [sys.executable, "-c", """import importlib.util, sys, time
from pathlib import Path
spec = importlib.util.spec_from_file_location('stores', sys.argv[1])
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
flag = Path(sys.argv[3])
with mod.refresh_lock(Path(sys.argv[2])):
    flag.write_text('held')
    time.sleep(30)
""", str(STORES), str(self.root / ".git"), str(flag)],
        )
        try:
            for _ in range(50):
                if flag.is_file():
                    break
                time.sleep(0.05)
            self.assertTrue(flag.is_file())
            with self.assertRaises(self.stores.SnapshotRefused):
                self.stores.snapshot_palace_database(self.root, self.root / "during-mine.sqlite3")
            self.assertFalse((self.root / "during-mine.sqlite3").exists())
        finally:
            holder.terminate()
            holder.wait(timeout=5)

    def test_corrupt_palace_is_quarantined_instead_of_returned_as_hits(self):
        self.database.write_bytes(b"not a database")
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "ce_retrieve_7118", ROOT / "chaos-engine" / "retrieve.py"
        )
        retrieve = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(retrieve)
        project = self.root
        (project / "chaos-engine").mkdir()
        (project / "chaos-engine" / "tool.py").write_text("# tool\n", encoding="utf-8")
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE": str(self.palace)}), \
             unittest.mock.patch.object(retrieve.subprocess, "run", side_effect=AssertionError("searched")):
            receipt = retrieve._run_store(project, "mempalace", "lease")
        self.assertEqual("degraded", receipt["status"])
        self.assertEqual("corrupt-quarantined", receipt["reason"])
        self.assertEqual([], receipt["hits"])
        self.assertEqual(self.stores.CORRUPT_PALACE_DETAIL, receipt["detail"])
        self.assertFalse(self.database.exists())
        self.assertTrue(list((self.palace / ".chaos-engine-quarantine").glob("corrupt-*")))
        status = load_hosts_directory_status(self.palace)
        # #7431: quarantine leftovers without a live sqlite are recovery-required
        # (not initialization-required); see test_quarantined_palace_without_sqlite_is_recovery_required_7431.
        self.assertEqual("recovery-required", status["status"])

    def test_doctor_reports_corrupt_before_the_file_is_already_gone(self):
        fresh = self.palace / "other"
        fresh.mkdir()
        torn = fresh / "sqlite_exact.sqlite3"
        torn.write_bytes(b"not a database")
        status = load_hosts_directory_status(fresh)
        self.assertEqual("recovery-required", status["status"])
        self.assertEqual(self.stores.CORRUPT_PALACE_DETAIL, status["detail"])
        self.assertFalse(torn.exists())


def load_hosts_directory_status(palace: Path) -> dict:
    import importlib.util

    spec = importlib.util.spec_from_file_location(
        "ce_hosts_7118", ROOT / "chaos-engine" / "hosts.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.mempalace_directory_status(palace)


if __name__ == "__main__":
    unittest.main()
