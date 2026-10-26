"""Run ``mempalace`` with two write-path speedups for the sqlite_exact backend.

Runs under MemPalace's own interpreter: ``<mempalace python> mempalace_fast_mine.py <mempalace args>``.

MemPalace 3.8 rewrites each drawer's full-text row with ``DELETE FROM docs_fts WHERE doc_id = ?``.
``doc_id`` is an UNINDEXED FTS5 column, so every drawer scans the whole FTS table. On a palace with
126k drawers that was 145 s of a 227 s mine of 36 files. Measured on the same palace: 230 s before,
48 s after (4.8x), with identical search results.

1. ``fts_rowid_map(collection_id, doc_id) -> rowid`` turns that delete into a rowid lookup. The
   rowid is re-checked against ``doc_id`` before deleting; a miss falls back to the original delete.
2. An expression index on ``source_file`` serves ``file_already_mined`` and per-file deletes.

Anything unexpected (another MemPalace version, a changed method) runs MemPalace unpatched.
The shim also retires itself when the installed ``_replace_fts`` already deletes by rowid
(upstream MemPalace/mempalace#2727, commit f0a23f0 on develop; unreleased as of 3.11.0, #6912).
"""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

SUPPORTED_VERSIONS = frozenset({"3.8.0", "3.10.0", "3.11.0"})
SOURCE_FILE_INDEX = (
    "CREATE INDEX IF NOT EXISTS chaos_engine_idx_source_file "
    "ON documents(collection_id, json_extract(metadata_json, '$.source_file'))"
)


def _ensure_map(cur) -> None:
    cur.execute(
        "CREATE TABLE IF NOT EXISTS fts_rowid_map("
        "collection_id INTEGER, doc_id TEXT, fts_rowid INTEGER, PRIMARY KEY(collection_id, doc_id))"
    )
    mapped = cur.execute("SELECT count(*) FROM fts_rowid_map").fetchone()[0]
    indexed = cur.execute("SELECT count(*) FROM docs_fts").fetchone()[0]
    if mapped != indexed:
        cur.execute("DELETE FROM fts_rowid_map")
        cur.execute("INSERT OR REPLACE INTO fts_rowid_map SELECT collection_id, doc_id, rowid FROM docs_fts")


def replace_fts(original):
    """Build the rowid-mapped ``_replace_fts``; ``original`` is the fallback."""
    prepared: set[int] = set()

    def _replace_fts(self, cur, collection_id, doc_id, document):
        if not self._fts_available(cur):
            return
        if id(self) not in prepared:
            _ensure_map(cur)
            cur.execute(SOURCE_FILE_INDEX)
            prepared.add(id(self))
        row = cur.execute(
            "SELECT fts_rowid FROM fts_rowid_map WHERE collection_id = ? AND doc_id = ?",
            (collection_id, doc_id),
        ).fetchone()
        deleted = 0
        if row is not None:
            deleted = cur.execute(
                "DELETE FROM docs_fts WHERE rowid = ? AND collection_id = ? AND doc_id = ?",
                (row[0], collection_id, doc_id),
            ).rowcount
        if row is None or deleted != 1:
            # Unmapped or moved: the original full-scan delete is always correct.
            cur.execute("DELETE FROM docs_fts WHERE collection_id = ? AND doc_id = ?", (collection_id, doc_id))
        cur.execute(
            "INSERT INTO docs_fts(collection_id, doc_id, document) VALUES (?, ?, ?)",
            (collection_id, doc_id, document),
        )
        cur.execute(
            "INSERT OR REPLACE INTO fts_rowid_map VALUES (?, ?, ?)", (collection_id, doc_id, cur.lastrowid)
        )

    _replace_fts.chaos_engine_original = original
    return _replace_fts


def upstream_deletes_by_rowid(method) -> bool:
    """True when MemPalace's own ``_replace_fts`` already uses the rowid delete (#6912)."""
    import inspect

    try:
        source = inspect.getsource(method)
    except (OSError, TypeError):
        return False
    return "DELETE FROM docs_fts WHERE rowid" in source


def _stores():
    path = Path(__file__).resolve().with_name("stores.py")
    spec = importlib.util.spec_from_file_location("_chaos_engine_fast_mine_stores", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def wrap_file_already_mined(original):
    """Treat another checkout's absolute ``source_file`` as the same repository file."""

    def file_already_mined(collection, source_file, check_mtime=False, *args, **kwargs):
        if original(collection, source_file, check_mtime=check_mtime, *args, **kwargs):
            return True
        try:
            stores = _stores()
            start = Path(source_file) if Path(str(source_file)).is_absolute() else Path.cwd()
            roots = stores.registered_roots(start if start.is_dir() else start.parent)
            alternates = stores.equivalent_source_files(str(source_file), roots)
        except (OSError, RuntimeError, TypeError, ValueError):
            return False
        for alternate in alternates:
            try:
                if original(collection, alternate, check_mtime=False):
                    return True
            except (OSError, RuntimeError, TypeError, ValueError):
                continue
        return False

    file_already_mined.chaos_engine_source_alias = True
    file_already_mined.chaos_engine_original = original
    return file_already_mined


def _install_source_alias() -> bool:
    """Patch ``file_already_mined`` when MemPalace is installed. Never required to import."""
    import importlib

    patched = False
    for module_name in ("mempalace.miner", "mempalace.palace"):
        try:
            module = importlib.import_module(module_name)
        except ImportError:
            continue
        original = getattr(module, "file_already_mined", None)
        if not callable(original) or getattr(original, "chaos_engine_source_alias", False):
            continue
        setattr(module, "file_already_mined", wrap_file_already_mined(original))
        patched = True
    return patched


def install() -> bool:
    """Patch MemPalace in this process; False (unpatched) for any unexpected shape."""
    aliased = False
    try:
        aliased = _install_source_alias()
    except (ImportError, OSError, RuntimeError, TypeError, ValueError):
        aliased = False
    try:
        import mempalace
        from mempalace.backends import sqlite_exact
    except ImportError:
        return aliased
    collection = getattr(sqlite_exact, "SQLiteExactCollection", None)
    original = getattr(collection, "_replace_fts", None)
    if getattr(mempalace, "__version__", None) not in SUPPORTED_VERSIONS or original is None:
        return aliased
    if upstream_deletes_by_rowid(original):
        return aliased
    collection._replace_fts = replace_fts(original)
    return True


def main(argv: list[str]) -> int:
    """Patch when supported, then run the MemPalace CLI with ``argv``."""
    install()
    from mempalace import cli

    sys.argv = ["mempalace", *argv]
    result = cli.main()
    return int(result or 0)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
