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
"""

from __future__ import annotations

import sys

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


def install() -> bool:
    """Patch MemPalace in this process; False (unpatched) for any unexpected shape."""
    try:
        import mempalace
        from mempalace.backends import sqlite_exact
    except ImportError:
        return False
    collection = getattr(sqlite_exact, "SQLiteExactCollection", None)
    original = getattr(collection, "_replace_fts", None)
    if getattr(mempalace, "__version__", None) not in SUPPORTED_VERSIONS or original is None:
        return False
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
