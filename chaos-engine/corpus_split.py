"""One palace, one graph: harness paths and the product tree stay separate at query time.

Harness rows can be mined into the ``chaos-engine-harness`` collection. Search
still reads the shared sqlite file and does not start a second Graphify, a
second palace, or another embedding model.
"""

from __future__ import annotations

import re
import sqlite3
from pathlib import Path

HARNESS_COLLECTION = "chaos-engine-harness"
PRODUCT_COLLECTION = "mempalace_drawers"
COLLECTION_ENV = "CHAOS_ENGINE_MEMPALACE_COLLECTION"
GENERIC_SEEDS = frozenset({"harness", "test", "contract", "gate", "engine"})
ALWAYS_LOADED = frozenset(
    {
        "chaos-engine/references/bot-entry.md",
        "chaos-engine/identity.md",
        "chaos-engine/skills/chaos-engine/SKILL.md",
        ".chaos-engine/references/bot-entry.md",
        ".chaos-engine/identity.md",
        ".chaos-engine/skills/chaos-engine/SKILL.md",
    }
)
_EXCERPT_BYTES = 800
_CALLER = re.compile(
    r"\b(call(?:s|er|ees|ers)?|depend(?:s|ency|encies)?|import(?:s|ed|ing)?|edges?)\b",
    re.IGNORECASE,
)
_POLICY = re.compile(r"\b(pre-push|which test|pins|queue card)\b", re.IGNORECASE)
_JAVA_TYPE = re.compile(r"[A-Z][a-z0-9]+(?:[A-Z][A-Za-z0-9]+)+")
_STOP = frozenset(
    {
        "and",
        "are",
        "for",
        "from",
        "how",
        "into",
        "not",
        "that",
        "the",
        "this",
        "was",
        "what",
        "with",
        "failure",
        "tail",
    }
)


def is_harness_path(path: str) -> bool:
    text = str(path or "").replace("\\", "/").lstrip("./")
    if text.startswith(("chaos-engine/", ".chaos-engine/")):
        return True
    if text == "scripts/ci/harness_pr_gate.py":
        return True
    name = text.rsplit("/", 1)[-1]
    return text.startswith("tests/scripts/") and name.startswith("test_ce_") and name.endswith(".py")


def collection_for_path(path: str) -> str:
    return HARNESS_COLLECTION if is_harness_path(path) else PRODUCT_COLLECTION


def mempalace_mine_groups(
    paths: list[str] | None,
) -> list[tuple[str | None, list[str] | None]]:
    """``None`` collection is the drawers default. ``None`` paths is a full mine."""
    if paths is None:
        return [(None, None)]
    harness = [path for path in paths if is_harness_path(path)]
    product = [path for path in paths if not is_harness_path(path)]
    groups: list[tuple[str | None, list[str] | None]] = []
    if product:
        groups.append((None, product))
    if harness:
        groups.append((HARNESS_COLLECTION, harness))
    return groups


def _java_type(token: str) -> bool:
    return token.casefold() not in GENERIC_SEEDS and bool(_JAVA_TYPE.fullmatch(token))


def is_rare(token: str) -> bool:
    if not token or token.casefold() in GENERIC_SEEDS or token.casefold() in _STOP:
        return False
    if "_" in token or "/" in token or "." in token:
        return True
    if token.isupper() and len(token) >= 4:
        return True
    return _java_type(token)


def rare_tokens(query: str) -> list[str]:
    return [token for token in str(query).split() if is_rare(token)]


def walk_seeds(query: str) -> list[str]:
    words = str(query).split()
    if len(words) == 1 and words[0].casefold() in GENERIC_SEEDS:
        return [words[0]]
    return [word for word in words if is_rare(word)]


def _blob(hit: dict) -> str:
    return " ".join(str(hit.get(key) or "") for key in ("path", "section", "label", "text"))


def drop_semantic_misses(query: str, hits: list[dict]) -> list[dict]:
    rare = rare_tokens(query)
    if not rare:
        return list(hits)
    kept = []
    for hit in hits:
        blob = _blob(hit).casefold()
        if any(token.casefold() in blob for token in rare):
            kept.append(hit)
    return kept


def drop_always_loaded(query: str, hits: list[dict]) -> list[dict]:
    rare = rare_tokens(query)
    kept = []
    for hit in hits:
        path = str(hit.get("path") or "")
        if path in ALWAYS_LOADED and not any(token.casefold() in path.casefold() for token in rare):
            continue
        kept.append(hit)
    return kept


def apply_path_predicate(query: str, hits: list[dict]) -> list[dict]:
    rare = rare_tokens(query)
    if not rare:
        return list(hits)
    kept = drop_always_loaded(query, drop_semantic_misses(query, hits))
    pathish = [token for token in rare if "_" in token or "/" in token or "." in token]
    javaish = [token for token in rare if _java_type(token)]
    if pathish and not javaish:
        harness = [hit for hit in kept if is_harness_path(str(hit.get("path") or ""))]
        if harness:
            return harness
    if javaish and not pathish:
        product = [hit for hit in kept if not is_harness_path(str(hit.get("path") or ""))]
        if product:
            return product
    return kept


def relativize(source: str) -> str:
    text = str(source or "").replace("\\", "/").strip()
    marker = "/store-snapshot/"
    if marker in text:
        text = text.split(marker, 1)[1]
    if text.startswith("/"):
        for needle in ("/chaos-engine/", "/.chaos-engine/", "/scripts/", "/tests/"):
            at = text.find(needle)
            if at != -1:
                return text[at + 1 :]
        parts = [part for part in text.split("/") if part]
        for index, part in enumerate(parts):
            if part == "src" and index:
                return "/".join(parts[index - 1 :])
    while text.startswith("./"):
        text = text[2:]
    return text


def _rank(hit: dict, rare: list[str]) -> tuple[int, str]:
    path = str(hit.get("path") or "")
    if any(token.casefold() in path.casefold() for token in rare):
        score = 0
    elif path.startswith("chaos-engine/references/") and path.endswith(".md"):
        score = 1
    elif path.startswith("tests/scripts/test_ce_"):
        score = 2
    elif is_harness_path(path):
        score = 3
    else:
        score = 4
    return (score, path)


def _palace_sqlite(project: Path) -> Path | None:
    try:
        import importlib.util

        path = Path(__file__).resolve().with_name("stores.py")
        spec = importlib.util.spec_from_file_location("chaos_engine_corpus_split_stores", path)
        if spec is None or spec.loader is None:
            return None
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        palace = module.resolve_palace(project)
    except (OSError, RuntimeError, TypeError, ValueError, ImportError):
        return None
    if not isinstance(palace, Path):
        palace = Path(str(palace))
    candidate = palace / "sqlite_exact.sqlite3" if palace.is_dir() else palace
    return candidate if candidate.is_file() else None


def _path_hits(connection: sqlite3.Connection, token: str) -> list[dict]:
    rows = connection.execute(
        """
        SELECT json_extract(metadata_json, '$.source_file')
        FROM documents
        WHERE json_extract(metadata_json, '$.source_file') LIKE '%' || ? || '%'
        LIMIT 30
        """,
        (token,),
    ).fetchall()
    hits = []
    for (source,) in rows:
        path = relativize(str(source or ""))
        if token.isupper():
            if token not in path:
                continue
        elif token.casefold() not in path.casefold():
            continue
        hits.append({"path": path, "label": path.rsplit("/", 1)[-1], "section": path})
    return hits


def _symbol_hits(connection: sqlite3.Connection, token: str) -> list[dict]:
    try:
        rows = connection.execute(
            """
            SELECT d.document, json_extract(d.metadata_json, '$.source_file')
            FROM docs_fts
            JOIN fts_rowid_map AS map ON map.fts_rowid = docs_fts.rowid
            JOIN documents AS d ON d.collection_id = map.collection_id AND d.id = map.doc_id
            WHERE docs_fts MATCH ?
            LIMIT 40
            """,
            ('"' + token.replace('"', '""') + '"',),
        ).fetchall()
    except sqlite3.OperationalError:
        return []
    hits = []
    for document, source in rows:
        text = str(document or "")
        if token not in text:
            continue
        path = relativize(str(source or ""))
        at = text.find(token)
        section = text[max(0, at - 80) : at + len(token) + 80].replace("\n", " ")
        hits.append({"path": path, "label": path.rsplit("/", 1)[-1], "section": section})
    return hits


def resolve_hits(project: Path, query: str, rare: list[str] | None = None) -> list[dict]:
    tokens = list(rare if rare is not None else rare_tokens(query))
    database = _palace_sqlite(project)
    if database is None or not tokens:
        return []
    connection = sqlite3.connect(f"file:{database}?mode=ro", uri=True)
    try:
        connection.execute("PRAGMA query_only=ON")
        connection.execute("PRAGMA busy_timeout=2000")
        found: list[dict] = []
        for token in tokens:
            if "_" in token or "/" in token or "." in token:
                found.extend(_path_hits(connection, token))
            if (token.isupper() and len(token) >= 4) or _java_type(token):
                found.extend(_symbol_hits(connection, token))
            if _java_type(token):
                found.extend(_path_hits(connection, token))
    finally:
        connection.close()
    found = apply_path_predicate(query, found)
    if _POLICY.search(query) and not any(_java_type(token) for token in tokens):
        harness = [hit for hit in found if is_harness_path(str(hit.get("path") or ""))]
        if harness:
            found = harness
    found.sort(key=lambda hit: _rank(hit, tokens))
    unique: list[dict] = []
    seen: set[str] = set()
    for hit in found:
        path = str(hit.get("path") or "")
        if not path or path in seen:
            continue
        seen.add(path)
        unique.append(hit)
    return unique[:8]


def direct_graphify_outcome(project: Path, query: str) -> dict | None:
    """Exact path or text hits. ``None`` means Graphify should still run."""
    cleaned = " ".join(str(query).split())
    rare = rare_tokens(cleaned)
    if not rare:
        if len(cleaned.split()) == 1 and cleaned.casefold() in GENERIC_SEEDS:
            return None
        return {
            "status": "skipped",
            "reason": "no-rare-token",
            "hits": [],
            "query": cleaned,
            "excerpt": "",
            "bytes": 0,
        }
    if _CALLER.search(cleaned):
        return None
    hits = resolve_hits(project, cleaned, rare)
    if not hits:
        if _POLICY.search(cleaned):
            return {
                "status": "skipped",
                "reason": "no-relevant-hits",
                "hits": [],
                "query": cleaned,
                "excerpt": "",
                "bytes": 0,
            }
        return None
    excerpt = "\n".join(str(hit["path"]) for hit in hits)
    raw = excerpt.encode("utf-8")
    if len(raw) > _EXCERPT_BYTES:
        excerpt = raw[:_EXCERPT_BYTES].decode("utf-8", "ignore")
    return {
        "status": "used",
        "reason": "hits",
        "query": cleaned,
        "hits": hits,
        "excerpt": excerpt,
        "bytes": len(excerpt.encode("utf-8")),
    }
