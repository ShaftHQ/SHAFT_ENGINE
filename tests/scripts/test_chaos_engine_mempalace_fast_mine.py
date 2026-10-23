"""Tests for chaos-engine/mempalace_fast_mine.py and its stores wiring."""

from __future__ import annotations

import importlib.util
import sqlite3
import stat
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str):
    spec = importlib.util.spec_from_file_location(f"ce_{name}", ROOT / "chaos-engine" / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class _Collection:
    def _fts_available(self, cur):
        return True


class FastMineTest(unittest.TestCase):
    def setUp(self) -> None:
        self.shim = _load("mempalace_fast_mine")
        self.db = sqlite3.connect(":memory:")
        cur = self.db.cursor()
        cur.execute("CREATE TABLE documents(collection_id INTEGER, id TEXT, metadata_json TEXT)")
        cur.execute("CREATE VIRTUAL TABLE docs_fts USING fts5(collection_id UNINDEXED, doc_id UNINDEXED, document)")
        for doc in ("a", "b"):
            cur.execute("INSERT INTO docs_fts(collection_id, doc_id, document) VALUES (1, ?, ?)", (doc, f"old {doc}"))
        self.replace = self.shim.replace_fts(None)

    def rows(self):
        return sorted(self.db.execute("SELECT doc_id, document FROM docs_fts").fetchall())

    def test_replace_keeps_exactly_one_row_per_drawer(self) -> None:
        collection = _Collection()
        self.replace(collection, self.db.cursor(), 1, "a", "new a")
        self.replace(collection, self.db.cursor(), 1, "a", "newer a")
        self.replace(collection, self.db.cursor(), 1, "c", "new c")
        self.assertEqual([("a", "newer a"), ("b", "old b"), ("c", "new c")], self.rows())
        self.assertEqual(1, self.db.execute(
            "SELECT count(*) FROM sqlite_master WHERE name = 'chaos_engine_idx_source_file'").fetchone()[0])

    def test_stale_map_falls_back_to_the_full_delete(self) -> None:
        collection = _Collection()
        self.replace(collection, self.db.cursor(), 1, "a", "new a")
        self.db.execute("UPDATE fts_rowid_map SET fts_rowid = 999 WHERE doc_id = 'a'")
        self.replace(collection, self.db.cursor(), 1, "a", "fixed a")
        self.assertEqual([("a", "fixed a"), ("b", "old b")], self.rows())

    def test_shim_retires_when_upstream_deletes_by_rowid(self) -> None:
        """#6912: an upstream _replace_fts with the rowid delete is never wrapped."""
        def upstream(self, cur, collection_id, doc_id, document):
            cur.execute("DELETE FROM docs_fts WHERE rowid = ?", (1,))

        def legacy(self, cur, collection_id, doc_id, document):
            cur.execute("DELETE FROM docs_fts WHERE collection_id = ? AND doc_id = ?", (collection_id, doc_id))

        self.assertTrue(self.shim.upstream_deletes_by_rowid(upstream))
        self.assertFalse(self.shim.upstream_deletes_by_rowid(legacy))
        self.assertFalse(self.shim.upstream_deletes_by_rowid(len))

    def test_unsupported_mempalace_is_left_unpatched(self) -> None:
        self.assertNotIn("9.9.9", self.shim.SUPPORTED_VERSIONS)
        self.assertIn(self.shim.install(), {True, False})


class FastMinePinTest(unittest.TestCase):
    def test_pinned_mempalace_is_a_supported_shim_version(self) -> None:
        """A MemPalace pin bump silently disables the 4.8x shim; re-measure and extend SUPPORTED_VERSIONS."""
        import json

        shim = _load("mempalace_fast_mine")
        pin = json.loads((ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))
        package = json.dumps(pin)
        version = package.split("mempalace==", 1)[1].split('"', 1)[0]
        self.assertIn(version, shim.SUPPORTED_VERSIONS)


class FastMinePrefixTest(unittest.TestCase):
    def test_launcher_shebang_selects_the_shim(self) -> None:
        stores = _load("stores")
        with tempfile.TemporaryDirectory() as temporary:
            launcher = Path(temporary) / "mempalace"
            launcher.write_text(f"#!{sys.executable}\nprint()\n", encoding="utf-8")
            launcher.chmod(launcher.stat().st_mode | stat.S_IEXEC)
            prefix = stores.fast_mine_prefix(str(launcher))
            self.assertEqual(sys.executable, prefix[0])
            self.assertTrue(prefix[1].endswith("mempalace_fast_mine.py"))
            self.assertEqual(["/missing/mempalace"], stores.fast_mine_prefix("/missing/mempalace"))


class ShimRetirementTripwire6912Test(unittest.TestCase):
    """#6912: a MemPalace pin bump must re-decide the shim, never silently orphan it."""

    def test_pinned_mempalace_is_a_version_the_shim_knows(self):
        import json
        import re

        deps = json.loads((ROOT / "chaos-engine/dependencies.json").read_text(encoding="utf-8"))
        pins = set(re.findall(r"mempalace==([0-9][0-9.]*)", json.dumps(deps)))
        self.assertTrue(pins, "mempalace pin not found")
        shim = _load("mempalace_fast_mine")
        self.assertLessEqual(
            pins, set(shim.SUPPORTED_VERSIONS),
            "MemPalace pin bumped: if the release deletes FTS rows by rowid (upstream #2727, f0a23f0), "
            "delete mempalace_fast_mine.py and its wiring; otherwise add the version to SUPPORTED_VERSIONS",
        )

if __name__ == "__main__":
    unittest.main()
