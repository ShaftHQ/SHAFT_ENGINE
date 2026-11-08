"""One palace, one graph: harness paths and the product tree stay separate at query time.

Harness rows are mined into the ``chaos-engine-harness`` collection. A harness
query reads that collection when the palace has it, and the whole sqlite file
when it does not. Search does not start a second Graphify, a second palace,
or another embedding model.
"""

from __future__ import annotations

import json
import re
import sqlite3
import subprocess  # nosec B404 - checkout git ls-files and git grep only.
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


def drop_path_section_misses(query: str, hits: list[dict]) -> list[dict]:
    """Drop a semantic hit whose path and section lack every rare query token.

    An all-caps token must appear in that case. A lowercase path fragment is
    not the symbol.
    """
    rare = rare_tokens(query)
    if not rare:
        return list(hits)
    kept = []
    for hit in hits:
        blob = f"{hit.get('path') or ''} {hit.get('section') or ''}"
        folded = blob.casefold()
        if any(
            token in blob if token.isupper() and len(token) >= 4 else token.casefold() in folded
            for token in rare
        ):
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


def _harness_collection_id(connection: sqlite3.Connection) -> int | None:
    try:
        row = connection.execute(
            "SELECT id FROM collections WHERE name = ? LIMIT 1",
            (HARNESS_COLLECTION,),
        ).fetchone()
    except sqlite3.OperationalError:
        return None
    if not row or row[0] is None:
        return None
    return int(row[0])


def _query_scopes_harness(query: str, tokens: list[str]) -> bool:
    if any(_java_type(token) for token in tokens):
        return False
    if any("_" in token or "/" in token or "." in token for token in tokens):
        return True
    return _POLICY.search(query) is not None


def _path_rows(
    connection: sqlite3.Connection, token: str, collection_id: int | None
) -> list[tuple]:
    if collection_id is None:
        sql = """
            SELECT json_extract(metadata_json, '$.source_file')
            FROM documents
            WHERE json_extract(metadata_json, '$.source_file') LIKE '%' || ? || '%'
            LIMIT 30
        """
        params: tuple = (token,)
    else:
        sql = """
            SELECT json_extract(d.metadata_json, '$.source_file')
            FROM documents AS d
            WHERE d.collection_id = ?
              AND json_extract(d.metadata_json, '$.source_file') LIKE '%' || ? || '%'
            LIMIT 30
        """
        params = (collection_id, token)
    try:
        return list(connection.execute(sql, params).fetchall())
    except sqlite3.OperationalError:
        return []


def _path_hits(
    connection: sqlite3.Connection, token: str, collection_id: int | None = None
) -> list[dict]:
    scoped = collection_id is not None
    rows = _path_rows(connection, token, collection_id)
    if scoped and not rows:
        rows = _path_rows(connection, token, None)
        scoped = False
    hits = []
    for (source,) in rows:
        path = relativize(str(source or ""))
        if token.isupper():
            if token not in path:
                continue
        elif token.casefold() not in path.casefold():
            continue
        hit = {"path": path, "label": path.rsplit("/", 1)[-1], "section": path}
        if scoped:
            hit["collection"] = HARNESS_COLLECTION
        hits.append(hit)
    if collection_id is not None and not hits:
        return _path_hits(connection, token, None)
    return hits


def _symbol_hits(
    connection: sqlite3.Connection, token: str, collection_id: int | None = None
) -> list[dict]:
    match = '"' + token.replace('"', '""') + '"'
    sql = """
        SELECT d.document, json_extract(d.metadata_json, '$.source_file')
        FROM docs_fts
        JOIN fts_rowid_map AS map ON map.fts_rowid = docs_fts.rowid
        JOIN documents AS d ON d.collection_id = map.collection_id AND d.id = map.doc_id
        WHERE docs_fts MATCH ?
    """
    params: list = [match]
    if collection_id is not None:
        sql += " AND d.collection_id = ?"
        params.append(collection_id)
    sql += " LIMIT 40"
    try:
        rows = connection.execute(sql, params).fetchall()
    except sqlite3.OperationalError:
        rows = []
    hits = []
    for document, source in rows:
        text = str(document or "")
        if token not in text:
            continue
        path = relativize(str(source or ""))
        at = text.find(token)
        section = text[max(0, at - 80) : at + len(token) + 80].replace("\n", " ")
        hit = {"path": path, "label": path.rsplit("/", 1)[-1], "section": section}
        if collection_id is not None:
            hit["collection"] = HARNESS_COLLECTION
        hits.append(hit)
    if collection_id is not None and not hits:
        return _symbol_hits(connection, token, None)
    return hits


def _git_text(project: Path, args: list[str], timeout: int) -> tuple[int, str] | None:
    """Real git, even when a test has patched ``subprocess.run``."""
    process = None
    try:
        process = subprocess.Popen(  # nosec B603 - fixed git argv, no shell.
            args,
            cwd=project,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        stdout, _stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        if process is not None:
            process.kill()
            process.communicate()
        return None
    except (OSError, subprocess.SubprocessError):
        return None
    return process.returncode, stdout or ""


def _tracked_files(project: Path) -> list[str]:
    completed = _git_text(project, ["git", "ls-files", "-z"], 20)
    if completed is None or completed[0] != 0:
        return []
    return [line for line in completed[1].split("\0") if line]


def _disk_path_hits(paths: list[str], token: str) -> list[dict]:
    hits = []
    for path in paths:
        if token.isupper():
            if token not in path:
                continue
        elif token.casefold() not in path.casefold():
            continue
        hits.append({"path": path, "label": path.rsplit("/", 1)[-1], "section": path})
    return hits


def _disk_text_hits(project: Path, token: str) -> list[dict]:
    """Exact token in checkout file text. No palace and no Graphify index."""
    completed = _git_text(project, ["git", "grep", "-l", "-I", "-F", "--", token], 30)
    if completed is None or completed[0] not in {0, 1}:
        return []
    hits = []
    for path in completed[1].splitlines():
        if not path:
            continue
        hits.append({"path": path, "label": path.rsplit("/", 1)[-1], "section": token})
    return hits


def _checkout_hits(project: Path, tokens: list[str]) -> list[dict]:
    paths = _tracked_files(project)
    found: list[dict] = []
    for token in tokens:
        if "_" in token or "/" in token or "." in token:
            found.extend(_disk_path_hits(paths, token))
        if token.isupper() and len(token) >= 4:
            found.extend(_disk_text_hits(project, token))
        if _java_type(token):
            found.extend(_disk_path_hits(paths, token))
    return found


def _palace_found(project: Path, query: str, tokens: list[str]) -> list[dict]:
    database = _palace_sqlite(project)
    if database is None or not tokens:
        return []
    connection = sqlite3.connect(f"file:{database}?mode=ro", uri=True)
    try:
        connection.execute("PRAGMA query_only=ON")
        connection.execute("PRAGMA busy_timeout=2000")
        collection_id = (
            _harness_collection_id(connection) if _query_scopes_harness(query, tokens) else None
        )
        found: list[dict] = []
        for token in tokens:
            if "_" in token or "/" in token or "." in token:
                found.extend(_path_hits(connection, token, collection_id))
            if (token.isupper() and len(token) >= 4) or _java_type(token):
                found.extend(_symbol_hits(connection, token, collection_id))
            if _java_type(token):
                found.extend(_path_hits(connection, token, collection_id))
    finally:
        connection.close()
    return found


def _finish_hits(query: str, tokens: list[str], found: list[dict]) -> list[dict]:
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


def resolve_hits(project: Path, query: str, rare: list[str] | None = None) -> list[dict]:
    tokens = list(rare if rare is not None else rare_tokens(query))
    if not tokens:
        return []
    found = _finish_hits(query, tokens, _palace_found(project, query, tokens))
    if found:
        return found
    return _finish_hits(query, tokens, _checkout_hits(project, tokens))


def direct_graphify_outcome(project: Path, query: str) -> dict | None:
    """Exact path or text hits. ``None`` means Graphify should still run."""
    cleaned = " ".join(str(query).split())
    rare = rare_tokens(cleaned)
    if not rare:
        return None
    hits = resolve_hits(project, cleaned, rare)
    if not hits:
        if _CALLER.search(cleaned) or not _POLICY.search(cleaned):
            return None
        return {
            "status": "skipped",
            "reason": "no-relevant-hits",
            "hits": [],
            "query": cleaned,
            "excerpt": "",
            "bytes": 0,
        }
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


_INVENTORY_START = "<!-- inventory:external-services:start -->"
_INVENTORY_END = "<!-- inventory:external-services:end -->"
_COMPANION_ABSENT = "companion page was not searched"
_IMPACT_TOKEN = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]{2,}")
_IMPACT_FLAG = re.compile(r"--[A-Za-z0-9][\w-]*")


def _impact_needles(query: str) -> list[str]:
    flags = [item.casefold() for item in _IMPACT_FLAG.findall(query)]
    if flags:
        return flags
    return [
        token.casefold()
        for token in _IMPACT_TOKEN.findall(query)
        if token.casefold() not in _STOP
    ]


def _page_blob(page: dict) -> str:
    return json.dumps(page, ensure_ascii=False).casefold()


def _line_of(text: str, needle: str) -> int | None:
    at = text.casefold().find(needle)
    if at < 0:
        return None
    return text[:at].count("\n") + 1


def _documentation_pages(project: Path) -> list[dict]:
    path = project / "graphify-out" / "documentation-pages.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return []
    pages = payload.get("pages") if isinstance(payload, dict) else None
    return [page for page in pages if isinstance(page, dict) and page.get("path")] if isinstance(pages, list) else []


def _graph_code_nodes(project: Path) -> list[dict]:
    path = project / "graphify-out" / "graph.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return []
    nodes = payload.get("nodes") if isinstance(payload, dict) else None
    return [node for node in nodes if isinstance(node, dict)] if isinstance(nodes, list) else []


def _companion_checkout(project: Path) -> dict[str, str]:
    """Name the companion page when that checkout is present. Never invent a path."""
    readme = project / "chaos-engine" / "README.md"
    if not readme.is_file():
        readme = project / "README.md"
    try:
        text = readme.read_text(encoding="utf-8")
    except (OSError, UnicodeError):
        return {"companion": _COMPANION_ABSENT}
    start = text.find(_INVENTORY_START)
    end = text.find(_INVENTORY_END)
    repo = ""
    if start >= 0 and end > start:
        for line in text[start:end].splitlines():
            if not line.startswith("|"):
                continue
            cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
            if len(cells) < 2:
                continue
            purpose = cells[1].casefold()
            item = cells[0].strip()
            if "companion" in purpose and "documentation" in purpose and "/" in item and " " not in item:
                repo = item
                break
    if not repo or "/" not in repo:
        return {"companion": _COMPANION_ABSENT}
    slug = repo.split("/", 1)[1]
    checkout = next(
        (path for path in (project.parent / slug, project / slug) if path.is_dir()),
        None,
    )
    if checkout is None:
        return {"companion": _COMPANION_ABSENT}
    return {"companion": _COMPANION_ABSENT, "companionCheckout": str(checkout)}


def _impact_documentation_hits(project: Path, needles: list[str]) -> list[tuple[int, dict]]:
    scored: list[tuple[int, dict]] = []
    for page in _documentation_pages(project):
        path = str(page.get("path") or "")
        blob = _page_blob(page)
        if needles and not any(needle in blob for needle in needles):
            continue
        score = 20
        folded = path.casefold()
        if "skill" in folded:
            score += 50
        if "install" in folded or folded.endswith(".md"):
            score += 30
        line = None
        for needle in needles:
            line = _line_of(blob, needle)
            if line is not None:
                break
        hit = {"path": path, "label": path.rsplit("/", 1)[-1], "section": path}
        if line is not None:
            hit["line"] = line
        scored.append((score, hit))
    return scored


def _impact_code_hits(project: Path, needles: list[str]) -> list[tuple[int, dict]]:
    scored: list[tuple[int, dict]] = []
    for node in _graph_code_nodes(project):
        path = str(node.get("source_file") or node.get("path") or "")
        if not path or path.startswith("docs:"):
            continue
        blob = " ".join(
            str(node.get(key) or "") for key in ("label", "rationale", "source_file", "path")
        ).casefold()
        matched = any(needle in blob for needle in needles) if needles else False
        folded = path.casefold()
        score = 5
        if matched and ("skill" in folded or folded.endswith(".md")):
            score = 40
        elif matched:
            score = 15
        scored.append((score, {"path": path, "label": path.rsplit("/", 1)[-1], "section": path}))
    return scored


def impact_graphify_outcome(project: Path, query: str) -> dict:
    """Rank skill and documentation pages for one contract. No network, cap 8.

    The companion documentation checkout is searched only when the origin README
    inventory names it and that directory exists. An absent checkout says so and
    does not invent ``companionPath``.
    """
    cleaned = " ".join(str(query).split())
    needles = _impact_needles(cleaned)
    scored = _impact_documentation_hits(project, needles) + _impact_code_hits(project, needles)
    scored.sort(key=lambda item: (-item[0], str(item[1].get("path") or "")))
    unique: list[dict] = []
    seen: set[str] = set()
    for _score, hit in scored:
        path = str(hit.get("path") or "")
        if not path or path in seen:
            continue
        seen.add(path)
        unique.append(hit)
        if len(unique) == 8:
            break
    excerpt = "\n".join(str(hit["path"]) for hit in unique)
    receipt = {
        "status": "used" if unique else "skipped",
        "reason": "hits" if unique else "no-relevant-hits",
        "query": cleaned,
        "hits": unique,
        "excerpt": excerpt,
        "bytes": len(excerpt.encode("utf-8")),
    }
    companion = _companion_checkout(project)
    checkout = companion.pop("companionCheckout", None)
    if checkout and needles:
        page = _companion_page(Path(checkout), needles)
        if page is not None:
            receipt["companionPath"] = page
            receipt["companion"] = page
            return receipt
    receipt["companion"] = _COMPANION_ABSENT
    return receipt


def _companion_page(checkout: Path, needles: list[str]) -> str | None:
    try:
        files = sorted(checkout.rglob("*.md"))
    except OSError:
        return None
    for path in files:
        if path.name.startswith("."):
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeError):
            continue
        if any(needle in text.casefold() for needle in needles):
            return path.relative_to(checkout).as_posix()
    return None
