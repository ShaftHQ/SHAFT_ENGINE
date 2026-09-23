#!/usr/bin/env python3
"""Zero-LLM retrieve orchestrator: one store, bounded query, used|skipped|degraded (#5624)."""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess  # nosec B404 - optional advisory store CLIs only.
import sys
from pathlib import Path
from typing import Any

STORES = ("memory", "mempalace", "graphify")
STATUS_USED = "used"
STATUS_SKIPPED = "skipped"
STATUS_DEGRADED = "degraded"
QUERY_MAX = 240
TIMEOUT_SECONDS = 8

_GRAPHIFY_HINT = re.compile(r"\b(call(s|er|ees?)?|depend|graph|import|edge)\b", re.I)
_MEMPALACE_HINT = re.compile(r"\b(history|palace|session|timeline|before)\b", re.I)


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def pick_store(query: str, store: str | None = None) -> str:
    if store:
        chosen = str(store).strip().casefold()
        if chosen not in STORES:
            raise ValueError(f"unsupported store: {store}")
        return chosen
    if _GRAPHIFY_HINT.search(query):
        return "graphify"
    if _MEMPALACE_HINT.search(query):
        return "mempalace"
    return "memory"


def _justification(project: Path):
    path = Path(__file__).resolve().with_name("hooks") / "retrieve_justification.py"
    if not path.is_file():
        return None
    import importlib.util

    spec = importlib.util.spec_from_file_location("chaos_engine_retrieve_justification", path)
    if spec is None or spec.loader is None:
        return None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


_BACKEND_MISMATCH = re.compile(r"backend[-_ ]mismatch", re.IGNORECASE)


def _is_backend_mismatch(*parts: str) -> bool:
    blob = " ".join(parts)
    if _BACKEND_MISMATCH.search(blob):
        return True
    folded = blob.casefold()
    return "chroma" in folded and "mismatch" in folded


def _record_store_outcome(
    project: Path, store: str, status: str, query: str, text: str = "", reason: str = ""
) -> None:
    if store not in {"mempalace", "graphify"}:
        return
    module = _justification(project)
    if module is None:
        return
    module.record_store_outcome(project, store, status, query, text, reason)


def _tool_py(project: Path) -> Path | None:
    for relative in (".chaos-engine/tool.py", "chaos-engine/tool.py"):
        path = project / relative
        if path.is_file():
            return path
    return None


_HIT = re.compile(
    r"(?P<path>(?:[\w.-]+/)+[\w.-]+\.[A-Za-z0-9]+)"
    r"(?::L?(?P<colon>\d+)|\s+L(?P<space>\d+))?"
)
_HIT_LIMIT = 8
_EXCERPT_WITH_HITS = 800


def _query_tokens(query: str) -> set[str]:
    return {token.casefold() for token in re.findall(r"[A-Za-z0-9_.-]{3,}", query or "")}


def _hit_matches_query(path: str, tokens: set[str]) -> bool:
    if not tokens:
        return False
    lowered = path.casefold()
    parts = [part for part in re.split(r"[/_.-]+", lowered) if part]
    return any(token in parts or token in lowered for token in tokens)


def _structured_hits(
    body: str, limit: int = _HIT_LIMIT, query: str = ""
) -> list[dict[str, object]]:
    """Repo-relative path and line. Token-sharing paths fill the slots first."""
    found: list[dict[str, object]] = []
    seen: set[tuple[str, str | None]] = set()
    for match in _HIT.finditer(body):
        path = match.group("path")
        line_text = match.group("colon") or match.group("space")
        key = (path, line_text)
        if key in seen:
            continue
        seen.add(key)
        item: dict[str, object] = {"path": path}
        if line_text:
            item["line"] = int(line_text)
        found.append(item)
        if len(found) >= 200:
            break
    tokens = _query_tokens(query)
    matched = [item for item in found if _hit_matches_query(str(item["path"]), tokens)]
    chosen = matched if matched else found
    return chosen[:limit]


_BUDGET_HINT = re.compile(
    r"raise the token budget \(CLI: --budget\)[^.]*\.?\s*"
)


def _bounded_excerpt(body: str, limit: int = 4096) -> str:
    """Return the store text the caller can use, capped so a receipt stays small."""
    body = _BUDGET_HINT.sub("", body)
    raw = body.encode("utf-8")
    if len(raw) <= limit:
        return body
    return raw[:limit].decode("utf-8", errors="ignore")


def _run_store(project: Path, store: str, query: str) -> dict[str, Any]:
    """One attempt; no retries. Missing/unhealthy → degraded; empty relevance → skipped."""
    tool = _tool_py(project)
    if tool is None:
        return {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": "tool.py-absent",
            "query": query,
        }
    # Map store → advisory CLI shape (bounded; never writes).
    if store == "memory":
        args = [sys.executable, str(tool), "memory", "search", query]
    elif store == "mempalace":
        args = [sys.executable, str(tool), "mempalace", "search", query]
    else:
        args = [sys.executable, str(tool), "graphify", "query", query]
    env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1", "CHAOS_ENGINE_RETRIEVE": "1"}
    try:
        completed = subprocess.run(  # nosec B603 - fixed owned tool.py only.
            args,
            cwd=project,
            capture_output=True,
            text=True,
            timeout=TIMEOUT_SECONDS,
            env=env,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": "timeout",
            "query": query,
        }
    except OSError as error:
        return {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": f"os-error:{type(error).__name__}",
            "query": query,
        }
    detail = (completed.stderr or completed.stdout or "").strip().splitlines()
    tip = detail[0][:120] if detail else ""
    origin_sync = "not synchronized with origin/main" in (
        (completed.stderr or "") + (completed.stdout or "")
    )
    if completed.returncode != 0:
        if origin_sync:
            return {
                "store": store,
                "status": STATUS_SKIPPED,
                "reason": "origin-sync",
                "originSync": "advisory",
                "storeHealth": "unchecked",
                "query": query,
            }
        mismatch = store == "mempalace" and _is_backend_mismatch(
            completed.stderr or "", completed.stdout or "", tip
        )
        return {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": "backend-mismatch" if mismatch else (tip or "nonzero-exit"),
            "query": query,
            "exitCode": completed.returncode,
        }
    body = (completed.stdout or "").strip()
    if not body or body.casefold() in {"[]", "{}", "none", "no results"}:
        return {
            "store": store,
            "status": STATUS_SKIPPED,
            "reason": "no-relevant-hits",
            "query": query,
        }
    hits = _structured_hits(body, query=query)
    excerpt = _bounded_excerpt(body, _EXCERPT_WITH_HITS if hits else 4096)
    receipt = {
        "store": store,
        "status": STATUS_USED,
        "reason": "hits",
        "query": query,
        "hits": hits,
        "excerpt": excerpt,
        "bytes": len(excerpt.encode("utf-8")),
    }
    _record_store_outcome(project, store, STATUS_USED, query, body)
    if origin_sync:
        receipt["originSync"] = "advisory"
    return receipt


def retrieve(
    query: str,
    *,
    store: str | None = None,
    project: Path | None = None,
    dry_run: bool = False,
) -> dict[str, Any]:
    cleaned = " ".join(str(query).split())
    if not cleaned:
        raise ValueError("query required")
    if len(cleaned) > QUERY_MAX:
        raise ValueError(f"query exceeds {QUERY_MAX} characters")
    root = project_root(project)
    chosen = pick_store(cleaned, store)
    receipt: dict[str, Any] = {
        "schemaVersion": 1,
        "kind": "retrieve-orchestrator",
        "store": chosen,
        "query": cleaned,
        "policy": "one-store-one-attempt",
    }
    if dry_run:
        receipt["status"] = STATUS_SKIPPED
        receipt["reason"] = "dry-run"
        return receipt
    if chosen == "mempalace":
        module = _justification(root)
        if module is not None and module.backend_mismatch_recorded(root):
            receipt["status"] = STATUS_DEGRADED
            receipt["reason"] = "backend-mismatch"
            receipt["scheduled"] = False
            return receipt
    outcome = _run_store(root, chosen, cleaned)
    receipt.update(outcome)
    if receipt.get("status") in {STATUS_DEGRADED, STATUS_SKIPPED}:
        _record_store_outcome(
            root,
            chosen,
            str(receipt["status"]),
            cleaned,
            "",
            str(receipt.get("reason") or ""),
        )
    return receipt



def heuristics_retrieve(top: int = 3, project: Path | None = None) -> dict[str, Any]:
    """Once-per-task ERL heuristic retrieve (#5656); never SessionStart prose."""
    path = Path(__file__).resolve().with_name("heuristics.py")
    if not path.is_file():
        return {
            "schemaVersion": 1,
            "kind": "heuristics-retrieve",
            "status": STATUS_DEGRADED,
            "reason": "heuristics.py missing",
            "items": [],
        }
    import importlib.util as _ilu

    spec = _ilu.spec_from_file_location("chaos_engine_heuristics_retrieve", path)
    if spec is None or spec.loader is None:
        return {
            "schemaVersion": 1,
            "kind": "heuristics-retrieve",
            "status": STATUS_DEGRADED,
            "reason": "heuristics loader failed",
            "items": [],
        }
    mod = _ilu.module_from_spec(spec)
    spec.loader.exec_module(mod)
    items = mod.retrieve_top(top, project)
    return {
        "schemaVersion": 1,
        "kind": "heuristics-retrieve",
        "status": STATUS_USED if items else STATUS_SKIPPED,
        "store": "heuristics",
        "top": top,
        "items": items,
        "policy": "once-per-task",
    }


def main(argv: list[str] | None = None) -> int:
    raw = list(sys.argv[1:] if argv is None else argv)
    if raw and raw[0] == "heuristics":
        parser = argparse.ArgumentParser(description="ERL heuristic retrieve once per task")
        parser.add_argument("--top", type=int, default=3)
        parser.add_argument("--project", type=Path, default=None)
        args = parser.parse_args(raw[1:])
        try:
            receipt = heuristics_retrieve(args.top, args.project)
        except ValueError as error:
            print(str(error), file=sys.stderr)
            return 2
        print(json.dumps(receipt, sort_keys=True))
        return 0
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("query", nargs="?", default="")
    parser.add_argument("--store", choices=STORES, default=None)
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(raw)
    try:
        receipt = retrieve(
            args.query, store=args.store, project=args.project, dry_run=args.dry_run
        )
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 2
    print(json.dumps(receipt, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
