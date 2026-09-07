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


def _tool_py(project: Path) -> Path | None:
    for relative in (".chaos-engine/tool.py", "chaos-engine/tool.py"):
        path = project / relative
        if path.is_file():
            return path
    return None


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
    try:
        completed = subprocess.run(  # nosec B603 - fixed owned tool.py only.
            args,
            cwd=project,
            capture_output=True,
            text=True,
            timeout=TIMEOUT_SECONDS,
            env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
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
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout or "").strip().splitlines()
        tip = detail[0][:120] if detail else "nonzero-exit"
        return {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": tip,
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
    return {
        "store": store,
        "status": STATUS_USED,
        "reason": "hits",
        "query": query,
        "bytes": min(len(body.encode("utf-8")), 4096),
    }


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
    outcome = _run_store(root, chosen, cleaned)
    receipt.update(outcome)
    return receipt


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("query", nargs="?", default="")
    parser.add_argument("--store", choices=STORES, default=None)
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
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
