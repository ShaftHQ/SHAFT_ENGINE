#!/usr/bin/env python3
"""Zero-LLM retrieve orchestrator: one store, bounded query, used|skipped|degraded (#5624)."""

from __future__ import annotations

import argparse
import json
import os
import re
import runpy
import subprocess  # nosec B404 - optional advisory store CLIs only.
import sys
from pathlib import Path
from typing import Any

STORES = ("memory", "mempalace", "graphify", "deja")
DEJA_LIMIT = "8"
DEJA_MODES = ("search", "how", "fix")
_NO_TRANSCRIPT_HOSTS = frozenset({"grok-bot", "copilot-cloud"})
STATUS_USED = "used"
STATUS_SKIPPED = "skipped"
STATUS_DEGRADED = "degraded"
QUERY_MAX = 240
TIMEOUT_SECONDS = 8
_EMPTY_RESULT = re.compile(r"^\s*no results( found)?\b", re.I)
PALACE_BACKEND = "sqlite_exact"
BACKEND_MISMATCH_FIX = (
    "run `python3 .chaos-engine/install.py doctor --project .`, then "
    "`python3 .chaos-engine/install.py repair --project . --component mempalace`; "
    "a Graphify-only answer is not a fallback for an indexed MemPalace; "
    "rerun retrieve with `--recheck` after repair (#6212)"
)

_GRAPHIFY_HINT = re.compile(r"\b(call(s|er|ees?)?|depend|graph|import|edge)\b", re.I)
_MEMPALACE_HINT = re.compile(r"\b(history|palace|session|timeline|before)\b", re.I)



_REAL_RUN = subprocess.run


def _mindset():
    path = Path(__file__).resolve().with_name("harness_mindset.py")
    if not path.is_file():
        return None
    import importlib.util

    spec = importlib.util.spec_from_file_location("chaos_engine_harness_mindset_retrieve", path)
    if spec is None or spec.loader is None:
        return None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run_until_stalled(args: list[str], **kwargs: Any) -> subprocess.CompletedProcess:
    """Run without a wall-clock cap; stores.py stops it only when it stalls (#6377)."""
    import importlib.util as _ilu

    spec = _ilu.spec_from_file_location(
        "chaos_engine_stores_watchdog", Path(__file__).with_name("stores.py")
    )
    if spec is None or spec.loader is None:
        raise RuntimeError("stores.py watchdog is unavailable")
    module = _ilu.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.run_until_stalled(args, **kwargs)


def is_empty_result(body: str) -> bool:
    """True for a store's plain 'no results' answer (#6377)."""
    text = body.strip()
    return not text or text.casefold() in {"[]", "{}", "none"} or bool(_EMPTY_RESULT.match(text))


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


def _note_cited_usage(project: Path, mindset: Any, paths: list[str] | None = None) -> None:
    """Attach stored citations, including a basename match, onto the new usage row."""
    module = _justification(project)
    if module is None:
        return
    noted: list[str] = []
    if hasattr(module, "load_citations"):
        noted.extend(str(path) for path in module.load_citations(project))
    if hasattr(module, "cites"):
        for path in paths or []:
            if path and module.cites(project, path) and path not in noted:
                noted.append(path)
    for path in noted:
        mindset.note_path_read(project, path)


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
# MemPalace prints only a basename ("Source: CdpWorkers.java"); resolve unique ones to repo paths.
_SOURCE_LINE = re.compile(r"^\s*Source:\s+(?P<name>[\w.-]+\.[A-Za-z0-9]+)\s*$", re.MULTILINE)
_EXCERPT_WITH_HITS = 800


def _query_tokens(query: str) -> set[str]:
    return {token.casefold() for token in re.findall(r"[A-Za-z0-9_.-]{3,}", query or "")}


def _hit_matches_query(path: str, tokens: set[str]) -> bool:
    if not tokens:
        return False
    lowered = path.casefold()
    parts = [part for part in re.split(r"[/_.-]+", lowered) if part]
    return any(token in parts or token in lowered for token in tokens)


def _query_outside_workspace(project: Path, query: str) -> bool:
    """True when every absolute path in the query sits outside this checkout.

    An in-repo hit must not be reported as ``used`` for that query (#6312).
    A query with no absolute path is unchanged.
    """
    found = re.findall(r"(?:(?:[A-Za-z]:)?(?:/|\\)[\w .+@-]{1,80}){2,}", query or "")
    if not found:
        return False
    try:
        root = project.resolve()
    except OSError:
        return False
    saw_outside = False
    for raw in found:
        candidate = Path(raw.strip())
        if not candidate.is_absolute():
            return False
        try:
            resolved = candidate.resolve()
        except OSError:
            return False
        if resolved == root or root in resolved.parents:
            return False
        saw_outside = True
    return saw_outside


def _outside_workspace_receipt(project: Path, store: str, query: str) -> dict[str, Any] | None:
    """Explicit miss when the query is about paths this repo cannot cite (#6312)."""
    if not _query_outside_workspace(project, query):
        return None
    return {
        "store": store,
        "status": STATUS_SKIPPED,
        "reason": "unrelated-in-repo",
        "query": query,
    }


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


def _source_name_hits(project: Path, body: str, limit: int = _HIT_LIMIT) -> list[dict[str, object]]:
    """Repo paths for MemPalace ``Source:`` basenames that name exactly one tracked file."""
    names = list(dict.fromkeys(match.group("name") for match in _SOURCE_LINE.finditer(body)))
    if not names:
        return []
    try:
        listed = subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
            ["git", "ls-files"], cwd=project, capture_output=True, text=True, check=False, timeout=20
        ).stdout.splitlines()
    except (OSError, subprocess.SubprocessError):
        return []
    by_name: dict[str, list[str]] = {}
    for path in listed:
        by_name.setdefault(path.rsplit("/", 1)[-1], []).append(path)
    hits = [{"path": by_name[name][0]} for name in names if len(by_name.get(name, [])) == 1]
    return hits[:limit]


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


def _host_lacks_transcripts(host: str | None) -> bool:
    if not host:
        return False
    folded = host.strip().casefold()
    return folded in _NO_TRANSCRIPT_HOSTS or folded.endswith("-cloud")


def _deja_on_path() -> bool:
    names = ("deja.exe", "deja") if os.name == "nt" else ("deja",)
    for entry in os.environ.get("PATH", "").split(os.pathsep):
        if not entry:
            continue
        for name in names:
            if (Path(entry) / name).is_file():
                return True
    return False


def deja_invocation(query: str, mode: str = "search") -> tuple[list[str], dict[str, str]]:
    """CLI-only deja argv. No absolute paths, no installer, no MCP."""
    chosen = mode if mode in DEJA_MODES else "search"
    args = ["deja", chosen, "--json", "--project", "."]
    if chosen in {"search", "how"}:
        args.extend(["--limit", DEJA_LIMIT])
    args.append(query)
    env = {
        "DEJA_OFFLINE": "1",
        "DEJA_EMBED_OFF": "1",
    }
    return args, env


def _deja_rows(payload: object) -> list[dict[str, Any]]:
    if isinstance(payload, list):
        return [item for item in payload if isinstance(item, dict)]
    if isinstance(payload, dict):
        for key in ("results", "hits", "sessions"):
            value = payload.get(key)
            if isinstance(value, list):
                return [item for item in value if isinstance(item, dict)]
    return []


def _relative_hit_path(value: object) -> str | None:
    if not isinstance(value, str) or not value.strip():
        return None
    text = value.strip().replace("\\", "/")
    if text.startswith("/") or re.match(r"^[A-Za-z]:", text):
        return None
    return text


def _relative_paths(value: object) -> list[str]:
    if isinstance(value, str):
        path = _relative_hit_path(value)
        return [path] if path else []
    if isinstance(value, dict):
        return _relative_paths(value.get("path") or value.get("file"))
    if isinstance(value, list):
        found: list[str] = []
        for item in value:
            for path in _relative_paths(item):
                if path not in found:
                    found.append(path)
        return found
    return []


def _deja_hit_paths(row: dict[str, Any]) -> list[str]:
    """Paths a hit may name. v0.21.2 keeps files on session.touched, not path."""
    paths: list[str] = []
    direct = _relative_hit_path(row.get("path") or row.get("file"))
    if direct:
        paths.append(direct)
    else:
        files = row.get("files")
        if isinstance(files, list) and files:
            first = files[0]
            legacy = _relative_hit_path(
                first if isinstance(first, str) else first.get("path") if isinstance(first, dict) else None
            )
            if legacy:
                paths.append(legacy)
    session = row.get("session")
    touched = session.get("touched") if isinstance(session, dict) else None
    if touched is None:
        touched = row.get("touched")
    for path in _relative_paths(touched):
        if path not in paths:
            paths.append(path)
    if isinstance(session, dict):
        session_path = _relative_hit_path(session.get("path"))
        if session_path and session_path not in paths:
            paths.append(session_path)
    return paths


def _deja_hits(payload: object) -> list[dict[str, object]]:
    hits: list[dict[str, object]] = []
    for row in _deja_rows(payload):
        paths = _deja_hit_paths(row)
        if not paths:
            continue
        line = row.get("line")
        if not isinstance(line, int) or isinstance(line, bool):
            session = row.get("session")
            if isinstance(session, dict):
                line = session.get("line")
        for path in paths:
            item: dict[str, object] = {"path": path}
            if isinstance(line, int) and not isinstance(line, bool):
                item["line"] = line
            hits.append(item)
            if len(hits) >= _HIT_LIMIT:
                return hits
    return hits


def _snippet_texts(value: object) -> list[str]:
    if isinstance(value, str) and value.strip():
        return [value.strip()]
    if not isinstance(value, list):
        return []
    parts: list[str] = []
    for item in value:
        if isinstance(item, str) and item.strip():
            parts.append(item.strip())
            continue
        if isinstance(item, dict):
            for key in ("text", "snippet", "excerpt", "content"):
                text = item.get(key)
                if isinstance(text, str) and text.strip():
                    parts.append(text.strip())
                    break
    return parts


def _deja_excerpt(payload: object, hits: list[dict[str, object]]) -> str:
    parts: list[str] = []
    for row in _deja_rows(payload):
        for text in _snippet_texts(row.get("snippets")):
            if text not in parts:
                parts.append(text)
        for key in ("excerpt", "snippet", "text"):
            value = row.get(key)
            if isinstance(value, str) and value.strip() and value.strip() not in parts:
                parts.append(value.strip())
                break
    return _bounded_excerpt("\n".join(parts), _EXCERPT_WITH_HITS if hits else 4096)


def _run_deja(project: Path, query: str, *, host: str | None, mode: str) -> dict[str, Any]:
    args, overlay = deja_invocation(query, mode)
    env = {**os.environ, **overlay, "PYTHONDONTWRITEBYTECODE": "1", "CHAOS_ENGINE_RETRIEVE": "1"}
    invocation = {"argv": args, "env": overlay}
    if _host_lacks_transcripts(host):
        return {
            "store": "deja",
            "status": STATUS_SKIPPED,
            "reason": "no-history",
            "query": query,
            "invocation": invocation,
            "untrusted": True,
            "verify": "live-files",
        }
    if not _deja_on_path():
        return {
            "store": "deja",
            "status": STATUS_DEGRADED,
            "reason": "missing-binary",
            "query": query,
            "invocation": invocation,
            "untrusted": True,
            "verify": "live-files",
        }
    try:
        completed = subprocess.run(  # nosec B603 - fixed deja CLI name, no shell.
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
            "store": "deja",
            "status": STATUS_DEGRADED,
            "reason": "timeout",
            "query": query,
            "invocation": invocation,
        }
    except OSError as error:
        return {
            "store": "deja",
            "status": STATUS_DEGRADED,
            "reason": f"os-error:{type(error).__name__}",
            "query": query,
            "invocation": invocation,
        }
    body = (completed.stdout or "").strip()
    if completed.returncode != 0:
        detail = (completed.stderr or body).strip().splitlines()
        tip = detail[0][:120] if detail else "nonzero-exit"
        return {
            "store": "deja",
            "status": STATUS_DEGRADED,
            "reason": tip or "nonzero-exit",
            "query": query,
            "exitCode": completed.returncode,
            "invocation": invocation,
        }
    payload: object
    try:
        payload = json.loads(body) if body else {}
    except json.JSONDecodeError:
        payload = body
    if isinstance(payload, dict) and str(payload.get("reason") or "") == "no-history":
        return {
            "store": "deja",
            "status": STATUS_SKIPPED,
            "reason": "no-history",
            "query": query,
            "invocation": invocation,
        }
    if isinstance(payload, str) and re.search(r"no (transcripts|history|sessions)", payload, re.I):
        return {
            "store": "deja",
            "status": STATUS_SKIPPED,
            "reason": "no-history",
            "query": query,
            "invocation": invocation,
        }
    hits = _deja_hits(payload)
    if not hits:
        # Rows that only name absolute session files are not a used/hits receipt.
        if _deja_rows(payload) and body not in {"", "[]", "{}", "none", "no results"}:
            return {
                "store": "deja",
                "status": STATUS_DEGRADED,
                "reason": "no-relative-paths",
                "query": query,
                "invocation": invocation,
                "untrusted": True,
                "verify": "live-files",
            }
        return {
            "store": "deja",
            "status": STATUS_SKIPPED,
            "reason": "no-relevant-hits",
            "query": query,
            "invocation": invocation,
        }
    excerpt = _deja_excerpt(payload, hits)
    return {
        "store": "deja",
        "status": STATUS_USED,
        "reason": "hits",
        "query": query,
        "hits": hits,
        "excerpt": excerpt,
        "bytes": len(excerpt.encode("utf-8")),
        "untrusted": True,
        "verify": "live-files",
        "invocation": invocation,
    }


def _load_stores():
    import importlib.util as _ilu

    spec = _ilu.spec_from_file_location(
        "chaos_engine_stores_retrieve_gate", Path(__file__).resolve().with_name("stores.py")
    )
    if spec is None or spec.loader is None:
        return None
    module = _ilu.module_from_spec(spec)
    previous = sys.dont_write_bytecode
    sys.dont_write_bytecode = True
    try:
        spec.loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = previous
    return module


def _git_common_without_process(project: Path) -> Path | None:
    """Shared git dir from `.git` / `commondir` text. Never spawns git."""
    git_path = project / ".git"
    try:
        if git_path.is_dir():
            git_dir = git_path
        elif git_path.is_file():
            line = git_path.read_text(encoding="utf-8").strip()
            if not line.startswith("gitdir:"):
                return None
            raw = Path(line.split(":", 1)[1].strip())
            git_dir = raw if raw.is_absolute() else (project / raw).resolve()
        else:
            return None
        common_file = git_dir / "commondir"
        if common_file.is_file():
            raw = Path(common_file.read_text(encoding="utf-8").strip())
            return raw.resolve() if raw.is_absolute() else (git_dir / raw).resolve()
        return git_dir.resolve()
    except OSError:
        return None


def _palace_for_gate(project: Path) -> Path | None:
    """Palace path for the corrupt-store gate, without a git subprocess."""
    for name in ("CHAOS_ENGINE_MEMPALACE", "SHA" + "FT_MEMPALACE"):
        configured = os.environ.get(name, "").strip()
        if not configured:
            continue
        path = Path(configured).expanduser()
        return path if path.is_absolute() else None
    common = _git_common_without_process(project)
    if common is not None:
        return common / "chaos-engine" / "mempalace"
    return project / ".chaos-engine-state" / "mempalace"


def _corrupt_mempalace_gate(project: Path) -> dict[str, str] | None:
    """quick_check the palace before search. Corrupt → quarantine, never partial hits (#7118)."""
    stores = _load_stores()
    if stores is None:
        return None
    palace = _palace_for_gate(project)
    if palace is None:
        return None
    return stores.corrupt_palace_gate(palace)


def _store_hit_receipt(
    project: Path, store: str, query: str, completed: subprocess.CompletedProcess[str], origin_sync: bool
) -> dict[str, Any]:
    """Turn a zero-exit store command into a used, skipped, or outside-workspace receipt."""
    body = (completed.stdout or "").strip()
    if is_empty_result(body):
        return {
            "store": store,
            "status": STATUS_SKIPPED,
            "reason": "no-relevant-hits",
            "query": query,
        }
    outside = _outside_workspace_receipt(project, store, query)
    if outside is not None:
        return outside
    hits = _structured_hits(body, query=query)
    if not hits and store == "mempalace":
        hits = _source_name_hits(project, body)
    mindset = _mindset()
    if store == "graphify" and mindset is not None:
        hits = mindset.relevance_floor(hits, query, project=project)
        if not hits:
            return {
                "store": store,
                "status": STATUS_SKIPPED,
                "reason": "no-relevant-hits",
                "query": query,
                "hits": [],
                "bytes": 0,
            }
    if store == "memory" and not hits and mindset is not None:
        return mindset.memory_no_hit_receipt()
    excerpt_limit = _EXCERPT_WITH_HITS if hits else 4096
    if mindset is not None:
        excerpt_limit = min(excerpt_limit, mindset.store_budget_bytes(store))
    excerpt = _bounded_excerpt(body, excerpt_limit)
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
    if store == "deja":
        return _run_deja(project, query, host=None, mode="search")
    # Map store → advisory CLI shape (bounded; never writes).
    if store == "mempalace":
        gate = _corrupt_mempalace_gate(project)
        if gate is not None:
            return {
                "store": store,
                "status": STATUS_DEGRADED,
                "reason": "corrupt-quarantined",
                "query": query,
                "hits": [],
                "detail": gate["detail"],
                "fixNext": gate["fixNext"],
                "quarantine": gate.get("quarantine", ""),
            }
    if store == "memory":
        args = [sys.executable, str(tool), "memory", "query", query]
    elif store == "mempalace":
        args = [sys.executable, str(tool), "mempalace", "search", query]
    else:
        args = [sys.executable, str(tool), "graphify", "query", query]
    env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1", "CHAOS_ENGINE_RETRIEVE": "1"}
    if store == "mempalace":
        # An ambient selection (e.g. chroma) must not override the owned palace (#6212).
        env["MEMPALACE_BACKEND"] = PALACE_BACKEND
    try:
        # A patched subprocess.run (tests) is used as-is; the real one gets the watchdog.
        runner = run_until_stalled if subprocess.run is _REAL_RUN else subprocess.run
        completed = runner(  # nosec B603 - fixed owned tool.py only.
            args,
            cwd=project,
            capture_output=True,
            text=True,
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
    origin_sync = "not synchronized with origin/" in (
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
        degraded = {
            "store": store,
            "status": STATUS_DEGRADED,
            "reason": "backend-mismatch" if mismatch else (tip or "nonzero-exit"),
            "query": query,
            "exitCode": completed.returncode,
        }
        if mismatch:
            degraded.update(blocking=True, fixNext=BACKEND_MISMATCH_FIX)
            print(f"retrieve: MemPalace backend mismatch; {BACKEND_MISMATCH_FIX}", file=sys.stderr)
        return degraded
    return _store_hit_receipt(project, store, query, completed, origin_sync)


def _recorded_mismatch(project: Path, store: str, recheck: bool) -> dict[str, Any] | None:
    """Return the recorded MemPalace mismatch without re-running the store (#6212)."""
    if store != "mempalace":
        return None
    module = _justification(project)
    if module is None or not hasattr(module, "backend_mismatch_recorded"):
        return None
    if recheck and hasattr(module, "clear_backend_mismatch"):
        module.clear_backend_mismatch(project)
        return None
    if not module.backend_mismatch_recorded(project):
        return None
    return {
        "status": STATUS_DEGRADED,
        "reason": "backend-mismatch",
        "scheduled": False,
        "blocking": True,
        "fixNext": BACKEND_MISMATCH_FIX,
    }


def graph_freshness_field(project: Path) -> dict[str, str]:
    """Never answer from an outdated graph silently (#6409): report fresh, stale or absent."""
    try:
        fresh, detail = runpy.run_path(
            str(Path(__file__).resolve().with_name("stores.py")), run_name="_chaos_engine_stores"
        )["graph_freshness"](project)
    except (OSError, KeyError, RuntimeError, ValueError) as error:
        return {"status": "unknown", "detail": str(error)}
    if fresh:
        return {"status": "fresh", "detail": detail}
    return {
        "status": detail.split(" ", 1)[0] if detail.startswith(("stale", "absent")) else "stale",
        "detail": detail,
        "refresh": "python3 .chaos-engine/tool.py stores refresh --if-stale",
    }


def retrieve(
    query: str,
    *,
    store: str | None = None,
    project: Path | None = None,
    dry_run: bool = False,
    recheck: bool = False,
    host: str | None = None,
    mode: str = "search",
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
    recorded = _recorded_mismatch(root, chosen, recheck)
    if recorded is not None:
        receipt.update(recorded)
        return receipt
    if chosen == "deja":
        outcome = _run_deja(root, cleaned, host=host, mode=mode)
    else:
        outcome = _run_store(root, chosen, cleaned)
    receipt.update(outcome)
    mindset = _mindset()
    if mindset is not None and receipt.get("status") in {STATUS_USED, STATUS_SKIPPED}:
        hits = receipt.get("hits") if isinstance(receipt.get("hits"), list) else []
        paths = [str(hit.get("path")) for hit in hits if isinstance(hit, dict) and hit.get("path")]
        try:
            mindset.record_retrieve_usage(
                root,
                store=chosen,
                hit_count=len(hits),
                byte_size=int(receipt.get("bytes") or 0),
                paths=paths,
            )
            _note_cited_usage(root, mindset, paths)
        except (OSError, TypeError, ValueError):
            # Hit accounting is best-effort and must not fail the retrieve.
            pass
    if chosen == "graphify":
        receipt["freshness"] = graph_freshness_field(root)
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
    parser.add_argument("--host", default=os.environ.get("CHAOS_ENGINE_HOST"))
    parser.add_argument("--mode", choices=DEJA_MODES, default="search")
    parser.add_argument(
        "--recheck", action="store_true", help="re-probe MemPalace after a backend repair"
    )
    args = parser.parse_args(raw)
    try:
        receipt = retrieve(
            args.query,
            store=args.store,
            project=args.project,
            dry_run=args.dry_run,
            recheck=args.recheck,
            host=args.host,
            mode=args.mode,
        )
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 2
    print(json.dumps(receipt, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
