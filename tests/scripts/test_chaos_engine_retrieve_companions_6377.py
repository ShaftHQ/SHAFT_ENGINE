"""#6377: retrieve-first and companions are concrete; Grok Bot loads the core card."""

import importlib.util
import os
import shutil
import sqlite3
import subprocess  # nosec B404 - tests run fixed local Git and Python commands.
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
BOT_ENTRY = "chaos-engine/references/bot-entry.md"
CORE = (SOURCE / "skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")


def _load(name: str):
    spec = importlib.util.spec_from_file_location(f"_t6377_{name}", SOURCE / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class CoreCardTest(unittest.TestCase):
    def test_retrieve_is_a_concrete_step(self):
        self.assertIn("tool.py retrieve", CORE)
        self.assertIn("skipped(<reason>)", CORE)
        self.assertNotIn("first when it pays", CORE)

    def test_companions_load_at_task_start(self):
        self.assertIn("at task start", CORE)
        self.assertNotIn("cards before the first edit", CORE)


class GrokBotTest(unittest.TestCase):
    def test_grok_bot_has_no_auto_loaded_instructions(self):
        hosts = _load("hosts")
        entry = hosts.INSTRUCTION_ONLY_HOSTS["grok-bot"]
        self.assertEqual(BOT_ENTRY, entry["instructions"])

    def test_independent_bots_share_the_entry(self):
        hosts = _load("hosts")
        self.assertEqual(BOT_ENTRY, hosts.INSTRUCTION_ONLY_HOSTS["independent-bot"]["instructions"])
        overlay = _load("worktree_overlay")
        self.assertEqual((BOT_ENTRY,), overlay.HOST_FILES["grok-bot"])

    def test_bot_entry_names_every_startup_step(self):
        body = (ROOT / BOT_ENTRY).read_text(encoding="utf-8")
        for needle in ("tool.py entry", "skills/chaos-engine/SKILL.md", "caveman-ultra.md",
                       "ponytail-ultra.md", "tool.py retrieve", "Grok Bot", "GPT"):
            self.assertIn(needle, body)

    def test_entry_bundle_carries_core_and_companions(self):
        tool = _load("tool")
        bundle = tool.entry_bundle(SOURCE)
        self.assertIn("# ChaosEngine", bundle)
        self.assertIn("Caveman", bundle)
        self.assertIn("Ponytail", bundle)
        self.assertIn("tool.py retrieve", bundle)

    def test_entry_reprints_one_line_while_unchanged(self):
        tool = _load("tool")
        with tempfile.TemporaryDirectory() as tmp:
            installed = Path(tmp) / ".chaos-engine"
            shutil.copytree(SOURCE / "skills", installed / "skills")
            shutil.copytree(SOURCE / "companions", installed / "companions")
            shutil.copy(SOURCE / "identity.md", installed / "identity.md")
            first = tool.entry_output(installed)
            self.assertIn("# ChaosEngine", first)
            again = tool.entry_output(installed)
            self.assertEqual(1, len(again.strip().splitlines()))
            self.assertIn("entry --full", again)
            self.assertIn("# ChaosEngine", tool.entry_output(installed, full=True))
            (installed / "companions/caveman-ultra.md").write_text("changed", encoding="utf-8")
            self.assertIn("changed", tool.entry_output(installed))

    def test_source_tree_entry_always_prints_the_bundle(self):
        tool = _load("tool")
        self.assertIn("# ChaosEngine", tool.entry_output(SOURCE))
        self.assertIn("# ChaosEngine", tool.entry_output(SOURCE))

    def test_matrix_row_does_not_claim_agents_md(self):
        matrix = (SOURCE / "references/host-parity-matrix.md").read_text(encoding="utf-8")
        row = next(line for line in matrix.splitlines() if line.startswith("| GAP-GROKBOT-HOOKS"))
        self.assertNotIn("reads `AGENTS.md`", row)
        self.assertIn("core card", row)


class StallWatchdogTest(unittest.TestCase):
    """Graphify/MemPalace runs have no wall-clock cap; only a stall stops them (#6377)."""

    def setUp(self):
        self.stores = _load("stores")

    def test_retrieve_has_no_fixed_store_timeout(self):
        retrieve = (SOURCE / "retrieve.py").read_text(encoding="utf-8")
        self.assertNotIn("timeout=store_timeout", retrieve)
        self.assertIn("run_until_stalled", retrieve)

    def test_progressing_process_outlives_the_stall_window(self):
        script = "import time\nend = time.monotonic() + 2.5\nwhile time.monotonic() < end:\n    pass\nprint('ok')"
        completed = self.stores.run_until_stalled(
            [sys.executable, "-c", script], stall_seconds=1, capture_output=True, text=True,
            timeout=0.1,
        )
        self.assertEqual(0, completed.returncode)
        self.assertIn("ok", completed.stdout)

    def test_idle_process_is_stopped_as_stalled(self):
        with self.assertRaises(subprocess.TimeoutExpired):
            self.stores.run_until_stalled(
                [sys.executable, "-c", "import time; time.sleep(30)"], stall_seconds=1,
                capture_output=True, text=True,
            )

    def test_ps_fallback_measures_cpu_without_proc(self):
        with unittest.mock.patch.object(self.stores, "_descendant_cpu_ticks", return_value=None):
            self.assertIsNotNone(self.stores.process_tree_cpu(os.getpid()))

    def test_unmeasurable_cpu_still_stops_a_silent_hang(self):
        with unittest.mock.patch.object(self.stores, "process_tree_cpu", return_value=None), \
                unittest.mock.patch.object(self.stores, "OUTPUT_ONLY_STALL_FACTOR", 2):
            with self.assertRaises(subprocess.TimeoutExpired):
                self.stores.run_until_stalled(
                    [sys.executable, "-c", "import time; time.sleep(30)"], stall_seconds=1,
                    capture_output=True, text=True,
                )

    def test_independent_bots_never_drift_from_the_grok_bot_path(self):
        hosts = _load("hosts")
        overlay = _load("worktree_overlay")
        bot = hosts.INSTRUCTION_ONLY_HOSTS["independent-bot"]["instructions"]
        self.assertEqual(hosts.INSTRUCTION_ONLY_HOSTS["grok-bot"]["instructions"], bot)
        self.assertEqual((bot,), overlay.HOST_FILES["grok-bot"])
        matrix = (SOURCE / "references/host-parity-matrix.md").read_text(encoding="utf-8")
        self.assertIn("GAP-BOT-ENTRY", matrix)
        self.assertIn("bot-entry.md", matrix)
        parity = (ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8")
        self.assertIn(bot, parity)

    def test_stall_window_env_override(self):
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STALL_SECONDS": "7"}):
            self.assertEqual(7, self.stores.stall_seconds())
        with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_STALL_SECONDS": "junk"}):
            self.assertEqual(self.stores.DEFAULT_STALL_SECONDS, self.stores.stall_seconds())

    def test_mine_paths_use_the_watchdog(self):
        dependencies = (SOURCE / "dependencies.py").read_text(encoding="utf-8")
        self.assertNotIn("BACKGROUND_MINE_TIMEOUT_SECONDS", dependencies)
        self.assertIn("run_until_stalled", dependencies)


def _palace(root: Path, documents: int) -> Path:
    palace = root / "palace"
    palace.mkdir()
    connection = sqlite3.connect(palace / "sqlite_exact.sqlite3")
    connection.execute("create table documents (id integer primary key, body text)")
    connection.executemany("insert into documents (body) values (?)", [("x",)] * documents)
    connection.commit()
    connection.close()
    return palace


class EmptyPalaceTest(unittest.TestCase):
    """An existing but empty palace is not an indexed palace (#6377)."""

    def setUp(self):
        self.stores = _load("stores")
        self.tmp = Path(tempfile.mkdtemp())

    def test_drawer_count(self):
        self.assertEqual(0, self.stores.palace_drawer_count(self._make(0)))
        self.assertEqual(3, self.stores.palace_drawer_count(self._make(3, "b")))
        self.assertIsNone(self.stores.palace_drawer_count(self.tmp / "missing"))

    def _make(self, documents, name="a"):
        (self.tmp / name).mkdir()
        return _palace(self.tmp / name, documents)

    def test_empty_palace_is_not_current(self):
        empty = self._make(0)
        with unittest.mock.patch.dict(self.stores._component_current.__globals__, {"resolve_palace": lambda _cwd: empty}):
            self.assertFalse(self.stores._component_current(self.tmp, "mempalace"))
        full = self._make(2, "b")
        with unittest.mock.patch.dict(self.stores._component_current.__globals__, {"resolve_palace": lambda _cwd: full}):
            self.assertTrue(self.stores._component_current(self.tmp, "mempalace"))

    def test_setup_incomplete_when_empty(self):
        dependencies = _load("dependencies")
        empty = self._make(0)
        with unittest.mock.patch.dict(dependencies.mempalace_project_setup_complete.__globals__, {"mempalace_project_palace": lambda _p: empty}):
            self.assertFalse(dependencies.mempalace_project_setup_complete(self.tmp))


class DoctorEmptyPalaceTest(unittest.TestCase):
    """Doctor degrades an empty palace only after a recorded mine stops (#6377)."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)
        self.install = _load("install")
        self.empty = _palace(self.tmp, 0)

    def _finding(self, index):
        stores = {"palace_drawer_count": lambda _p: 0, "resolve_palace": lambda _p: self.empty}
        with unittest.mock.patch.object(self.install, "_load_stores_module", return_value=stores):
            return self.install._mempalace_empty_finding(self.tmp, index)

    def test_no_mine_record_or_running_mine_is_not_degraded(self):
        self.assertIsNone(self._finding(None))
        self.assertIsNone(self._finding({"status": "running"}))

    def test_stopped_mine_with_zero_drawers_is_degraded(self):
        for state in ("complete", "failed", "interrupted"):
            self.assertEqual("degraded", self._finding({"status": state})["status"])


class SynchronousMineTest(unittest.TestCase):
    """The synchronous upgrade mine never hands float('inf') to a runner (#6377)."""

    def test_unbounded_mine_passes_no_infinite_timeout(self):
        dependencies = _load("dependencies")
        seen = {}

        def runner(args, **kwargs):
            seen.update(kwargs)
            return subprocess.CompletedProcess(args, 0, "", "")

        with unittest.mock.patch.object(dependencies, "resolve_account_launcher", lambda c, **_k: c):
            dependencies._run_transient_mempalace_mine([sys.executable, "-V"], Path.cwd(), runner=runner)
        self.assertIsNone(seen["timeout"])

    def test_traced_subprocess_runner_gets_the_stall_watchdog(self):
        dependencies = _load("dependencies")
        install = _load("install")

        class Reporter:
            def trace(self, _line):
                pass

        traced = install._tracing_dependency_runner(Reporter(), subprocess.run)
        with unittest.mock.patch.object(dependencies, "run_until_stalled") as watchdog, \
                unittest.mock.patch.object(dependencies, "resolve_account_launcher", lambda c, **_k: c):
            watchdog.return_value = subprocess.CompletedProcess([], 0, "", "")
            dependencies._run_transient_mempalace_mine([sys.executable, "-V"], Path.cwd(), runner=traced)
        watchdog.assert_called_once()


class NoResultsTest(unittest.TestCase):
    def test_no_results_text_is_not_used(self):
        retrieve = _load("retrieve")
        self.assertTrue(retrieve.is_empty_result('No results found for: "x"'))
        self.assertFalse(retrieve.is_empty_result("[1] src/Foo.java hit"))


def _git(path: Path, *args: str) -> str:
    git = shutil.which("git") or "git"
    return subprocess.run(  # nosec B603 B607 - resolved git, fixed test argv.
        [git, *args], cwd=path, capture_output=True, text=True, check=True
    ).stdout.strip()


class MaintainTest(unittest.TestCase):
    """`tool.py maintain` keeps checkouts from drifting (#6377)."""

    def setUp(self):
        self.tool = _load("tool")
        self.tmp = Path(tempfile.mkdtemp())
        self.origin = self.tmp / "origin.git"
        _git(self.tmp, "init", "-q", "--bare", "-b", "main", str(self.origin))
        self.work = self.tmp / "work"
        _git(self.tmp, "clone", "-q", str(self.origin), str(self.work))
        for repo in (self.work,):
            _git(repo, "config", "user.email", "t@example.invalid")
            _git(repo, "config", "user.name", "t")
        (self.work / "a.txt").write_text("one\n", encoding="utf-8")
        (self.work / "b.txt").write_text("keep\n", encoding="utf-8")
        _git(self.work, "add", ".")
        _git(self.work, "commit", "-q", "-m", "one")
        _git(self.work, "push", "-q", "origin", "main")
        other = self.tmp / "other"
        _git(self.tmp, "clone", "-q", str(self.origin), str(other))
        _git(other, "config", "user.email", "t@example.invalid")
        _git(other, "config", "user.name", "t")
        (other / "a.txt").write_text("two\n", encoding="utf-8")
        _git(other, "commit", "-qam", "two")
        _git(other, "push", "-q", "origin", "main")

    def test_fast_forwards_and_keeps_tracked_edits(self):
        (self.work / "b.txt").write_text("local edit\n", encoding="utf-8")
        ok, summary = self.tool.maintain_sync(self.work)
        self.assertTrue(ok, summary)
        self.assertIn("fast-forwarded 1", summary)
        self.assertEqual("two\n", (self.work / "a.txt").read_text(encoding="utf-8"))
        self.assertEqual("local edit\n", (self.work / "b.txt").read_text(encoding="utf-8"))

    def test_skips_off_default_branch(self):
        _git(self.work, "checkout", "-q", "-b", "feature")
        ok, summary = self.tool.maintain_sync(self.work)
        self.assertTrue(ok)
        self.assertIn("skipped", summary)

    def test_reinstall_uses_recorded_source(self):
        root = self.tmp / "core"
        root.mkdir()
        (root / "manifest.json").write_text(
            '{"source": {"repository": "o/r", "branch": "main"}, "distribution": {"id": "portable"}}',
            encoding="utf-8",
        )
        commands = self.tool.maintain_commands(root, self.work)
        self.assertIn("o/r", commands[0])
        self.assertIn("portable", commands[0])
        self.assertIn("doctor", commands[1])
        self.assertEqual(["stores", "refresh", "--if-stale"], commands[2][-3:])

    def test_kanban_dod_runs_maintain(self):
        kanban = (SOURCE / "skills/kanban/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("tool.py maintain", kanban)
