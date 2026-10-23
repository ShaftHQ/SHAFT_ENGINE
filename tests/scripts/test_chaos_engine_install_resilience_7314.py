"""#7314: install resilience, MemPalace prune, consumer tracked core, no piped pre-push."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import sqlite3
import subprocess  # nosec B404 - fixed git argv in a temp repository.
import sys
import tempfile
import time
import unittest
import unittest.mock
from contextlib import redirect_stderr
from io import StringIO
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GIT = shutil.which("git") or "git"


def load(relative: str, name: str):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


INSTALL = load("chaos-engine/install.py", "ce_install_7314")
DEPS = load("chaos-engine/dependencies.py", "ce_dependencies_7314")
CONSUMER = load("chaos-engine/consumer_mode.py", "ce_consumer_7314")
GUARD = load("scripts/agents/guard.py", "ce_guard_7314")


class Temp(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()


class SnapshotReap(Temp):
    def test_dead_owner_snapshot_is_reaped_live_owner_kept(self):
        dead = subprocess.Popen([sys.executable, "-c", "pass"])  # nosec B603
        dead.wait()
        stale = self.root / f"{INSTALL.SNAPSHOT_PREFIX}{dead.pid}-abc"
        live = self.root / f"{INSTALL.SNAPSHOT_PREFIX}{os.getppid()}-def"
        legacy_new = self.root / f"{INSTALL.SNAPSHOT_PREFIX}xyz123"
        legacy_old = self.root / f"{INSTALL.SNAPSHOT_PREFIX}old999"
        for path in (stale, live, legacy_new, legacy_old):
            (path / "graphify-out").mkdir(parents=True)
        old = time.time() - INSTALL.LEGACY_SNAPSHOT_STALE_SECONDS - 60
        os.utime(legacy_old, (old, old))
        with redirect_stderr(StringIO()) as err:
            reaped = INSTALL.reap_stale_project_setup_snapshots(self.root)
        self.assertEqual(sorted(reaped), sorted([stale, legacy_old]))
        self.assertTrue(live.is_dir() and legacy_new.is_dir())
        self.assertIn("removed 2 stale install snapshot", err.getvalue())

    def test_exit_trap_removes_live_snapshot(self):
        snapshot = self.root / "snap"
        snapshot.mkdir()
        INSTALL._LIVE_SNAPSHOTS.add(snapshot)
        INSTALL._cleanup_live_snapshots()
        self.assertFalse(snapshot.exists())

    def test_sigterm_mid_install_leaves_no_snapshot(self):
        tmp = self.root / "tmp"
        tmp.mkdir()
        script = (
            "import importlib.util,sys,os,signal,tempfile\n"
            f"spec=importlib.util.spec_from_file_location('i',{str(ROOT / 'chaos-engine/install.py')!r})\n"
            "m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)\n"
            "m.install_snapshot_trap()\n"
            "from pathlib import Path\n"
            "d=Path(tempfile.mkdtemp(prefix=m.SNAPSHOT_PREFIX+str(os.getpid())+'-'));m._LIVE_SNAPSHOTS.add(d)\n"
            "os.kill(os.getpid(),signal.SIGTERM)\n"
        )
        if os.name == "nt":
            self.skipTest("POSIX signals")
        completed = subprocess.run(  # nosec B603
            [sys.executable, "-c", script], env={**os.environ, "TMPDIR": str(tmp)},
            capture_output=True, timeout=60, check=False)
        self.assertEqual(completed.returncode, 128 + 15, completed.stderr)
        self.assertEqual(list(tmp.iterdir()), [])


class DiskPreflight(Temp):
    def test_refuses_when_free_space_below_snapshot_plus_margin(self):
        (self.root / "graphify-out").mkdir()
        (self.root / "graphify-out/graph.json").write_bytes(b"x" * 4096)
        with self.assertRaisesRegex(ValueError, "not enough free disk space for the install snapshot"):
            INSTALL.require_snapshot_disk_space(self.root, free=1024)
        INSTALL.require_snapshot_disk_space(self.root, free=INSTALL.SNAPSHOT_DISK_MARGIN + 8192)


class JournalScratch(Temp):
    def scratch(self) -> Path:
        return self.root / (INSTALL.JOURNAL_NAME + ".tmp")

    def test_empty_scratch_is_quarantined_not_deleted(self):
        self.scratch().write_bytes(b"")
        with redirect_stderr(StringIO()) as err:
            journal = INSTALL.write_journal(self.root, "install", "abc")
        self.assertEqual(json.loads(journal.read_text())["commit"], "abc")
        kept = list((self.root / ".chaos-engine-state/quarantine").iterdir())
        self.assertEqual(len(kept), 1)
        self.assertIn("quarantined stale transaction journal scratch", err.getvalue())

    def test_invalid_scratch_is_quarantined(self):
        self.scratch().write_bytes(b"{trunc")
        with redirect_stderr(StringIO()):
            INSTALL.write_journal(self.root, "install", "abc")
        kept = list((self.root / ".chaos-engine-state/quarantine").iterdir())
        self.assertEqual(kept[0].read_bytes(), b"{trunc")

    def test_valid_scratch_still_collides(self):
        self.scratch().write_text('{"schemaVersion": 1}')
        with self.assertRaisesRegex(ValueError, "scratch path collision"):
            INSTALL.write_journal(self.root, "install", "abc")


class GraphifyCount(Temp):
    def test_count_matches_listed_nested_repositories(self):
        (self.root / ".graphifyignore").write_text("a/\nb/\n")
        nested = ["a", "b", "c"]
        stores = {"nested_repository_roots": lambda _project: nested}
        with unittest.mock.patch.object(INSTALL, "_load_stores_module", return_value=stores), \
                redirect_stderr(StringIO()) as err:
            INSTALL.seed_graphify_ignore(self.root)
        self.assertIn("graphify skips 3 nested repositories (1 newly ignored): a, b, c", err.getvalue())


def make_exact_palace(database: Path, sources: list[str]) -> None:
    database.parent.mkdir(parents=True, exist_ok=True)
    connection = sqlite3.connect(database)
    connection.execute("create table documents (collection_id integer, id text, document text, "
                       "metadata_json text, primary key (collection_id, id))")
    connection.execute("create virtual table docs_fts using fts5(collection_id UNINDEXED, "
                       "doc_id UNINDEXED, document)")
    for index, source in enumerate(sources):
        meta = json.dumps({"source_file": source})
        connection.execute("insert into documents values (1, ?, 'x', ?)", (f"d{index}", meta))
        connection.execute("insert into docs_fts values (1, ?, 'x')", (f"d{index}",))
    connection.commit()
    connection.close()


class MemPalacePrune(Temp):
    def test_default_excludes_cover_rollback_and_generated_bundles(self):
        for pattern in (".chaos-engine-account-rollback/**", "**/*.min.js", "**/*-embedded.html"):
            self.assertIn(pattern, DEPS.MEMPALACE_OWNED_EXCLUDES)

    def test_prune_drops_excluded_and_deleted_sources_only(self):
        project = self.root
        for name in ("keep.py", "SHAFT_ENGINE/a.java", "web/app.min.js"):
            (project / name).parent.mkdir(parents=True, exist_ok=True)
            (project / name).write_text("x")
        (project / "mempalace.yaml").write_text(
            "wing: w\nexclude_patterns:\n  - 'SHAFT_ENGINE/**'\n  - '**/*.min.js'\nother: 1\n")
        sources = [str(project / "keep.py"), str(project / "SHAFT_ENGINE/a.java"),
                   str(project / "SHAFT_ENGINE/a.java"), str(project / "web/app.min.js"),
                   str(project / "gone.md"), "/elsewhere/outside.py"]
        database = self.root / "palace/sqlite_exact.sqlite3"
        make_exact_palace(database, sources)
        with unittest.mock.patch.object(DEPS, "mempalace_project_palace", return_value=database.parent):
            self.assertEqual(DEPS.prune_mempalace_drawers(project), (3, 4))
            self.assertEqual(DEPS.prune_mempalace_drawers(project), (0, 0))
        connection = sqlite3.connect(database)
        left = [json.loads(row[0])["source_file"] for row in
                connection.execute("select metadata_json from documents order by id")]
        fts = connection.execute("select count(*) from docs_fts").fetchone()[0]
        connection.close()
        self.assertEqual(left, [str(project / "keep.py"), "/elsewhere/outside.py"])
        self.assertEqual(fts, 2)

    def test_exclude_glob_semantics(self):
        self.assertTrue(DEPS._exclude_regex("**/*.min.js").match("a/b/c.min.js"))
        self.assertTrue(DEPS._exclude_regex("**/*.min.js").match("c.min.js"))
        self.assertFalse(DEPS._exclude_regex("SHAFT_ENGINE/**").match("SHAFT_ENGINE_x/a"))
        self.assertFalse(DEPS._exclude_regex(".chaos-engine-*").match(".chaos-engine-a/b"))


class ConsumerTrackedCore(Temp):
    def test_committed_core_reports_adopter_mode(self):
        git = [GIT, "-C", str(self.root)]
        subprocess.run([*git, "init", "-q"], check=True)  # nosec B603
        (self.root / ".chaos-engine").mkdir()
        (self.root / ".chaos-engine/install.py").write_text("x")
        subprocess.run([*git, "add", "."], check=True)  # nosec B603
        self.assertEqual(CONSUMER.tracked_core(self.root), [".chaos-engine/install.py"])
        result: dict = {}
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_CONSUMER": "1"}), \
                redirect_stderr(StringIO()) as err:
            INSTALL.report_consumer_tracked_core(self.root, result)
        self.assertEqual(result["consumerMode"]["trackedCore"], 1)
        self.assertIn("git rm -r --cached .chaos-engine", err.getvalue())

    def test_untracked_core_is_silent(self):
        subprocess.run([GIT, "-C", str(self.root), "init", "-q"], check=True)  # nosec B603
        result: dict = {}
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_CONSUMER": "1"}):
            INSTALL.report_consumer_tracked_core(self.root, result)
        self.assertEqual(result, {})


class PipedPrePush(unittest.TestCase):
    def test_guard_blocks_piped_pre_push(self):
        for command in ("python3 scripts/ci/overlay_pre_push.py | tail -3 && git push",
                        "python3 scripts/ci/overlay_pre_push.py 2>&1 | tail && git push",
                        "python3 scripts/ci/overlay_pre_push.py |& cat"):
            self.assertIn("R32", GUARD.evaluate_command(command) or "", command)

    def test_guard_allows_unpiped_pre_push(self):
        for command in ("python3 scripts/ci/overlay_pre_push.py && git push",
                        "python3 scripts/ci/overlay_pre_push.py || echo failed"):
            reason = GUARD.check_r32_piped_pre_push(command)
            self.assertIsNone(reason, command)

    def test_bot_entry_states_rule(self):
        setup = load("scripts/ci/validate_agent_setup.py", "ce_validate_setup_7314")
        self.assertEqual(setup.validate_pre_push_pipe_rule(ROOT), [])
        with tempfile.TemporaryDirectory() as temporary:
            doc = Path(temporary) / setup.PRE_PUSH_RULE_DOC
            doc.parent.mkdir(parents=True)
            doc.write_text("Run `python3 scripts/ci/overlay_pre_push.py | tail && git push`.\n")
            codes = [error["code"] for error in setup.validate_pre_push_pipe_rule(Path(temporary))]
        self.assertEqual(codes, ["pre-push-pipe", "pre-push-pipe"])


if __name__ == "__main__":
    unittest.main()
