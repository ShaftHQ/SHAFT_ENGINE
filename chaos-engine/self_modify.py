#!/usr/bin/env python3
"""
Apply a harness candidate only behind the eval suite, and keep an archive (#6518).

DGM (2505.22954) and SICA (2504.15228): the previous body is archived before a
new body is stored. Nothing here edits a shipped chaos-engine file.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "self-modify" / "index.json"
EVAL_TASK_ID = "reg-self-modify-6518"
EVAL_MODULE = "tests.scripts.test_chaos_engine_self_modify_6518"
MAX_TEXT = 240
LABEL = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")
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
    return {"schemaVersion": SCHEMA_VERSION, "candidates": [], "archive": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("self-modify index schema is unsupported")
    document.setdefault("candidates", [])
    document.setdefault("archive", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _text(value: str) -> str:
    text = " ".join(value.split())
    if not text or len(text) > MAX_TEXT:
        raise ValueError(f"text must be 1-{MAX_TEXT} characters")
    for pattern in PRIVATE:
        if pattern.search(text):
            raise ValueError("text contains private text")
    return text


def _label(value: str) -> str:
    if not LABEL.fullmatch(value) or len(value) > 48:
        raise ValueError("label must be a short slug")
    return value


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


def _find(rows: list[Any], candidate_id: str) -> dict[str, Any]:
    for row in rows:
        if isinstance(row, dict) and row.get("id") == candidate_id:
            return row
    raise ValueError("candidate id is unknown")


def propose(label: str, text: str, *, project: Path | None = None) -> dict[str, Any]:
    name = _label(label)
    body = _text(text)
    item = {
        "id": hashlib.sha256(f"{name}\n{body}".encode()).hexdigest()[:16],
        "label": name,
        "text": body,
        "applied": False,
        "score": None,
    }
    document = load_index(project)
    rows = document["candidates"]
    if not isinstance(rows, list):
        raise ValueError("self-modify index is invalid")
    kept = [row for row in rows if isinstance(row, dict) and row.get("id") != item["id"]]
    kept.append(item)
    document["candidates"] = kept[-64:]
    _save(document, project)
    return item


def _passing_task(report: dict[str, Any]) -> dict[str, Any]:
    results = report.get("results")
    if not isinstance(results, list) or report.get("passed") is not True:
        raise ValueError("suite report did not pass")
    try:
        suite_score = float(report.get("pass_at_k"))
        required = float(report.get("threshold_pass_at_k"))
    except (TypeError, ValueError) as exc:
        raise ValueError("suite report pass_at_k is missing") from exc
    if suite_score < required:
        raise ValueError("suite report is below its threshold")
    matched = next(
        (
            row
            for row in results
            if isinstance(row, dict)
            and row.get("id") == EVAL_TASK_ID
            and row.get("module") == EVAL_MODULE
            and row.get("passed") is True
            and float(row.get("pass_at_k", 0)) >= required
        ),
        None,
    )
    if matched is None:
        raise ValueError("suite report did not pass the self-modify task")
    return {"pass_at_k": suite_score, "task_pass_at_k": float(matched["pass_at_k"]), "passed": True}


def score_candidate(
    candidate_id: str,
    report_path: Path,
    *,
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> dict[str, Any]:
    if not manifest_lists_task(eval_manifest):
        raise ValueError("self-modify stays behind the harness eval suite")
    report = json.loads(Path(report_path).read_text(encoding="utf-8"))
    if not isinstance(report, dict):
        raise ValueError("suite report is invalid")
    document = load_index(project)
    chosen = _find(document["candidates"], candidate_id)
    chosen["score"] = _passing_task(report)
    chosen["applied"] = False
    _save(document, project)
    return chosen


def _applied_path(project: Path | None, label: str) -> Path:
    destination = index_path(project).parent / "applied" / f"{label}.md"
    root = index_path(project).parent.resolve()
    if root not in destination.resolve().parents and destination.resolve() != root:
        raise ValueError("applied path escapes the state directory")
    return destination


def _archive_current(document: dict[str, Any], label: str, project: Path | None) -> None:
    path = _applied_path(project, label)
    if not path.is_file():
        return
    prior = path.read_text(encoding="utf-8").strip()
    if not prior:
        return
    digest = hashlib.sha256(prior.encode()).hexdigest()[:16]
    archive_dir = index_path(project).parent / "archive"
    archive_dir.mkdir(parents=True, exist_ok=True)
    (archive_dir / f"{digest}.md").write_text(prior + "\n", encoding="utf-8")
    rows = document["archive"]
    if not any(isinstance(row, dict) and row.get("id") == digest for row in rows):
        rows.append({"id": digest, "label": label, "text": prior})
    document["archive"] = rows[-64:]


def apply_candidate(
    candidate_id: str,
    *,
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> dict[str, Any]:
    if not manifest_lists_task(eval_manifest):
        raise ValueError("self-modify stays behind the harness eval suite")
    document = load_index(project)
    chosen = _find(document["candidates"], candidate_id)
    score = chosen.get("score")
    if not isinstance(score, dict) or score.get("passed") is not True:
        raise ValueError("candidate has no passing suite score")
    _archive_current(document, chosen["label"], project)
    destination = _applied_path(project, chosen["label"])
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(chosen["text"] + "\n", encoding="utf-8")
    chosen["applied"] = True
    _save(document, project)
    return {"path": str(destination), "archiveCount": len(document["archive"])}


def rollback(archive_id: str, *, project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    chosen = _find(document["archive"], archive_id)
    _archive_current(document, chosen["label"], project)
    destination = _applied_path(project, chosen["label"])
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(chosen["text"] + "\n", encoding="utf-8")
    _save(document, project)
    return {"path": str(destination), "archiveCount": len(document["archive"])}


def summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    candidates = [row for row in document.get("candidates") or [] if isinstance(row, dict)]
    archive = document.get("archive") or []
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "self-modify-summary",
        "candidateCount": len(candidates),
        "appliedCount": sum(1 for row in candidates if row.get("applied") is True),
        "archiveCount": len(archive) if isinstance(archive, list) else 0,
        "evalGateOpen": manifest_lists_task(),
        "status": "healthy" if candidates else "absent",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    add = sub.add_parser("propose")
    add.add_argument("--label", required=True)
    add.add_argument("--text", required=True)
    add.add_argument("--project", type=Path, default=None)
    grade = sub.add_parser("score")
    grade.add_argument("--id", required=True)
    grade.add_argument("--report", type=Path, required=True)
    grade.add_argument("--project", type=Path, default=None)
    grade.add_argument("--eval-manifest", type=Path, default=None)
    keep = sub.add_parser("apply")
    keep.add_argument("--id", required=True)
    keep.add_argument("--project", type=Path, default=None)
    keep.add_argument("--eval-manifest", type=Path, default=None)
    undo = sub.add_parser("rollback")
    undo.add_argument("--archive-id", required=True)
    undo.add_argument("--project", type=Path, default=None)
    report = sub.add_parser("summary")
    report.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    project = args.project
    if args.command == "propose":
        payload = propose(args.label, args.text, project=project)
    elif args.command == "score":
        payload = score_candidate(
            args.id, args.report, project=project, eval_manifest=args.eval_manifest
        )
    elif args.command == "apply":
        payload = apply_candidate(args.id, project=project, eval_manifest=args.eval_manifest)
    elif args.command == "rollback":
        payload = rollback(args.archive_id, project=project)
    else:
        payload = summary(project)
    print(json.dumps(payload, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
