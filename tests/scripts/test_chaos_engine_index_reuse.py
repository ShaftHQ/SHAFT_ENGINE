"""Index reuse: nested repositories, incremental by default, relocation reuse."""

from __future__ import annotations

import importlib.util
import json
import os
import sqlite3
import tempfile
import unittest
from contextlib import redirect_stderr
from io import StringIO
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"


def load_controller():
    spec = importlib.util.spec_from_file_location("chaos_engine_dependencies_reuse", CONTROLLER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def make_palace(palace: Path, drawers: int = 2) -> None:
    palace.mkdir(parents=True, exist_ok=True)
    connection = sqlite3.connect(palace / "sqlite_exact.sqlite3")
    try:
        connection.execute("create table documents (id integer primary key, body text)")
        connection.executemany("insert into documents (body) values (?)", [("x",)] * drawers)
        connection.commit()
    finally:
        connection.close()


class NoGitEnvironment(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name).resolve()
        patcher = mock.patch.dict(os.environ, {"GIT_CEILING_DIRECTORIES": str(self.project.parent)})
        patcher.start()
        self.addCleanup(patcher.stop)
        os.environ.pop("CHAOS_ENGINE_MEMPALACE", None)
        os.environ.pop("CHAOS_ENGINE_MEMPALACE_FULL", None)
        self.module = load_controller()

    def palace(self) -> Path:
        return self.module.mempalace_project_palace(self.project)


class NestedRepositoryTest(NoGitEnvironment):
    def test_nested_repos_and_worktrees_are_found_and_not_descended(self):
        (self.project / "SHAFT_ENGINE/.git").mkdir(parents=True)
        (self.project / "SHAFT_ENGINE/inner/.git").mkdir(parents=True)
        (self.project / "work/tree").mkdir(parents=True)
        (self.project / "work/tree/.git").write_text("gitdir: /elsewhere\n", encoding="utf-8")
        (self.project / "plain/docs").mkdir(parents=True)
        stores = self.module._stores_namespace()
        self.assertEqual(["SHAFT_ENGINE", "work/tree"], stores["nested_repository_roots"](self.project))

    def test_owned_config_gets_managed_nested_block_idempotently(self):
        (self.project / "site/.git").mkdir(parents=True)
        configuration = self.project / "mempalace.yaml"
        configuration.write_text("wing: x\n", encoding="utf-8")
        self.module.apply_owned_mempalace_excludes(self.project)
        self.module.apply_nested_repository_excludes(self.project)
        self.module.apply_nested_repository_excludes(self.project)
        text = configuration.read_text(encoding="utf-8")
        self.assertEqual(1, text.count("'site/**'"))
        self.assertEqual(1, text.count(self.module.MEMPALACE_NESTED_MARKER))
        (self.project / "site/.git").rmdir()
        self.module.apply_nested_repository_excludes(self.project)
        self.assertNotIn("'site/**'", configuration.read_text(encoding="utf-8"))

    def test_user_config_is_untouched_and_advised(self):
        (self.project / "site/.git").mkdir(parents=True)
        configuration = self.project / "mempalace.yaml"
        configuration.write_text("wing: x\nexclude_patterns:\n  - 'mine/**'\n", encoding="utf-8")
        before = configuration.read_text(encoding="utf-8")
        errors = StringIO()
        with redirect_stderr(errors):
            self.module.apply_nested_repository_excludes(self.project)
        self.assertEqual(before, configuration.read_text(encoding="utf-8"))
        self.assertIn("site/**", errors.getvalue())


class MineDecisionTest(NoGitEnvironment):
    def test_missing_index_is_full(self):
        self.assertEqual(("full", "index missing"), self.module.mempalace_mine_decision(self.project))

    def test_corrupt_index_is_full(self):
        self.palace().mkdir(parents=True)
        (self.palace() / "sqlite_exact.sqlite3").write_bytes(b"not a database")
        self.assertEqual("full", self.module.mempalace_mine_decision(self.project)[0])
        self.assertIn("corrupt", self.module.mempalace_mine_decision(self.project)[1])

    def test_populated_index_is_incremental(self):
        make_palace(self.palace())
        mode, reason = self.module.mempalace_mine_decision(self.project)
        self.assertEqual("incremental", mode)
        self.assertIn("2 drawers", reason)

    def test_explicit_full(self):
        make_palace(self.palace())
        mode, reason = self.module.mempalace_mine_decision(
            self.project, environ={"CHAOS_ENGINE_MEMPALACE_FULL": "1"})
        self.assertEqual("full", mode)
        self.assertIn("explicit", reason)

    def test_schema_change_is_full(self):
        make_palace(self.palace())
        (self.palace().parent / "mempalace-index-meta.json").write_text(json.dumps({"schema": 0}), encoding="utf-8")
        self.assertIn("schema", self.module.mempalace_mine_decision(self.project)[1])

    def test_embedder_change_is_full_and_same_is_incremental(self):
        make_palace(self.palace())
        (self.palace().parent / "mempalace-index-meta.json").write_text(
            json.dumps({"schema": self.module.MEMPALACE_INDEX_SCHEMA, "embedder": "3.10.0"}),
            encoding="utf-8")
        with mock.patch.object(self.module, "mempalace_embedder_version", return_value="3.11.0"):
            self.assertEqual("full", self.module.mempalace_mine_decision(self.project, ["mp"])[0])
        with mock.patch.object(self.module, "mempalace_embedder_version", return_value="3.10.0"):
            self.assertEqual("incremental", self.module.mempalace_mine_decision(self.project, ["mp"])[0])

    def test_legacy_marker_stays_incremental(self):
        make_palace(self.palace())
        (self.palace() / ".mined").write_bytes(b"current\n")  # pre-meta marker only
        self.assertEqual("incremental", self.module.mempalace_mine_decision(self.project)[0])

    def test_full_keeps_previous_index_aside(self):
        make_palace(self.palace())
        aside = self.module.set_aside_mempalace_index(self.project)
        self.assertTrue((aside / "sqlite_exact.sqlite3").is_file())
        self.assertFalse((self.palace() / "sqlite_exact.sqlite3").exists())


class PalaceContentsTest(NoGitEnvironment):
    def test_index_meta_lives_beside_the_palace_not_inside(self):
        self.module.mark_mempalace_project_setup(self.project, "3.11.0")
        names = {child.name for child in self.palace().iterdir()}
        self.assertNotIn("mempalace-index-meta.json", names)
        meta = json.loads((self.palace().parent / "mempalace-index-meta.json").read_text(encoding="utf-8"))
        self.assertEqual("3.11.0", meta["embedder"])


class RelocationReuseTest(NoGitEnvironment):
    def test_state_to_git_location_reuses_populated_index(self):
        legacy = self.project / ".chaos-engine-state/mempalace"
        make_palace(legacy, drawers=3)
        (self.project / ".git").mkdir()
        target = self.project / ".git/chaos-engine/mempalace"
        stores = self.module._stores_namespace()
        with mock.patch.object(self.module, "mempalace_project_palace", return_value=target):
            with redirect_stderr(StringIO()):
                self.assertEqual(legacy, self.module.adopt_relocated_mempalace_palace(self.project))
            self.assertEqual(3, stores["palace_drawer_count"](target))
            self.assertEqual("incremental", self.module.mempalace_mine_decision(self.project)[0])
        self.assertEqual(3, stores["palace_drawer_count"](legacy))

    def test_git_to_state_location_reuses_populated_index(self):
        in_tree = self.project / ".git/chaos-engine/mempalace"
        make_palace(in_tree)
        self.assertEqual(self.project / ".chaos-engine-state/mempalace", self.palace())
        with redirect_stderr(StringIO()):
            self.assertEqual(in_tree, self.module.adopt_relocated_mempalace_palace(self.project))
        self.assertEqual("incremental", self.module.mempalace_mine_decision(self.project)[0])

    def test_populated_target_is_never_overwritten(self):
        make_palace(self.palace(), drawers=5)
        make_palace(self.project / ".git/chaos-engine/mempalace", drawers=1)
        self.assertIsNone(self.module.adopt_relocated_mempalace_palace(self.project))



class GraphifyNestedIgnoreTest(NoGitEnvironment):
    def test_seed_graphify_ignore_adds_nested_repos_once(self):
        import runpy

        install = runpy.run_path(str(ROOT / "chaos-engine/install.py"), run_name="_ce_install_nested")
        (self.project / "site/.git").mkdir(parents=True)
        with redirect_stderr(StringIO()):
            install["seed_graphify_ignore"](self.project)
            install["seed_graphify_ignore"](self.project)
        lines = (self.project / ".graphifyignore").read_text(encoding="utf-8").splitlines()
        self.assertEqual(1, lines.count("site/"))
        self.assertIn(".chaos-engine/", lines)


if __name__ == "__main__":
    unittest.main()
