#!/usr/bin/env python3
"""
Check a context budget and compact by dropping unmarked segments (#6518).

Chroma Context Rot: a context over the character budget is reported as rot.
Compaction keeps only segments marked keep, must shrink, and must land inside
the budget. The compacted file stays under project state.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "context-rot" / "index.json"
EVAL_TASK_ID = "reg-context-rot-6518"
EVAL_MODULE = "tests.scripts.test_chaos_engine_context_rot_6518"
BUDGET_CHARS = 400
MAX_SEGMENT = 2000
PRIVATE = (
    re.compile(
        r"(?i)(?:gh[oprsu]_|github_pat_|sk-|api[_-]?key|password|secret|token)"
        r"[A-Za-z0-9_:=./+\-]{8,}"
    ),
    re.compile(r"(?i)(?:[A-Z]:\\|/(?:home|users|root|private|opt)/)"),
    re.compile(r"(?i)https?://"),
    re.compile(r"`"),
)


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def index_path(project: Path | None = None) -> Path:
    return project_root(project) / INDEX_RELATIVE


def _empty() -> dict[str, Any]:
    return {"schemaVersion": SCHEMA_VERSION, "segments": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("context-rot index schema is unsupported")
    document.setdefault("segments", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _clean(value: str) -> str:
    text = " ".join(value.split())
    if not text or len(text) > MAX_SEGMENT:
        raise ValueError(f"segment must be 1-{MAX_SEGMENT} characters")
    for pattern in PRIVATE:
        if pattern.search(text):
            raise ValueError("segment contains private text")
    return text


def manifest_lists_task(manifest: Path | None = None) -> bool:
    if manifest is None:
        module_dir = Path(__file__).resolve().parent
        candidates = (
            module_dir.parent / "chaos-engine/evals/harness-suite/manifest.json",
            module_dir / "evals/harness-suite/manifest.json",
        )
        manifest = next((path for path in candidates if path.is_file()), None)
    if manifest is None or not Path(manifest).is_file():
        return False
    document = json.loads(Path(manifest).read_text(encoding="utf-8"))
    tasks = document.get("tasks") if isinstance(document, dict) else None
    if not isinstance(tasks, list):
        return False
    return any(
        isinstance(task, dict)
        and task.get("id") == EVAL_TASK_ID
        and task.get("module") == EVAL_MODULE
        and task.get("set") == "regression"
        for task in tasks
    )


def record_segment(text: str, *, keep: bool = False, project: Path | None = None) -> dict[str, Any]:
    body = _clean(text)
    item = {
        "id": hashlib.sha256(f"{int(keep)}\n{body}".encode()).hexdigest()[:16],
        "text": body,
        "keep": bool(keep),
    }
    document = load_index(project)
    rows = document["segments"]
    if not isinstance(rows, list):
        raise ValueError("context-rot index is invalid")
    kept = [row for row in rows if isinstance(row, dict) and row.get("id") != item["id"]]
    kept.append(item)
    document["segments"] = kept[-32:]
    _save(document, project)
    return item


def _rows(project: Path | None) -> list[dict[str, Any]]:
    document = load_index(project)
    return [row for row in document.get("segments") or [] if isinstance(row, dict)]


def _chars(rows: list[dict[str, Any]]) -> int:
    return sum(len(str(row.get("text") or "")) for row in rows)


def check_budget(project: Path | None = None) -> dict[str, Any]:
    rows = _rows(project)
    chars = _chars(rows)
    over = chars > BUDGET_CHARS
    return {
        "chars": chars,
        "budget": BUDGET_CHARS,
        "status": "over" if over else "within",
        "compactionNeeded": over,
        "evalGateOpen": manifest_lists_task(),
    }


def compact(project: Path | None = None, *, eval_manifest: Path | None = None) -> dict[str, Any]:
    if not manifest_lists_task(eval_manifest):
        raise ValueError("context compaction stays behind the harness eval suite")
    rows = _rows(project)
    before = _chars(rows)
    kept = [row for row in rows if row.get("keep") is True]
    after = _chars(kept)
    if not rows or after >= before:
        raise ValueError("compaction must drop unmarked text")
    if after > BUDGET_CHARS:
        raise ValueError("kept text still exceeds the context budget")
    destination = index_path(project).parent / "compacted.md"
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text("\n".join(str(row["text"]) for row in kept) + "\n", encoding="utf-8")
    document = load_index(project)
    document["segments"] = kept
    _save(document, project)
    return {"path": str(destination), "before": before, "after": after, "budget": BUDGET_CHARS}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    add = sub.add_parser("record")
    add.add_argument("--text", required=True)
    add.add_argument("--keep", action="store_true")
    add.add_argument("--project", type=Path, default=None)
    audit = sub.add_parser("check")
    audit.add_argument("--project", type=Path, default=None)
    shrink = sub.add_parser("compact")
    shrink.add_argument("--project", type=Path, default=None)
    shrink.add_argument("--eval-manifest", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "record":
        payload = record_segment(args.text, keep=args.keep, project=args.project)
    elif args.command == "check":
        payload = check_budget(args.project)
    else:
        payload = compact(args.project, eval_manifest=args.eval_manifest)
    print(json.dumps(payload, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
