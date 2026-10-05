#!/usr/bin/env python3
"""
Score prompt and skill candidates on the harness eval suite (#6518; GEPA 2507.19457).

A candidate is accepted only when a suite report shows this regression task at
pass@k 1.0 and the suite itself passed. Accepted text stays under
.chaos-engine-state and does not edit shipped skills.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "prompt-evolution" / "index.json"
EVAL_TASK_ID = "reg-prompt-evolution-6518"
EVAL_MODULE = "tests.scripts.test_chaos_engine_prompt_evolution_6518"
KINDS = ("prompt", "skill")
MAX_TEXT = 240

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
    return {"schemaVersion": SCHEMA_VERSION, "candidates": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("prompt evolution index schema is unsupported")
    document.setdefault("candidates", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _check_text(value: str, *, label: str, limit: int) -> str:
    text = " ".join(value.split())
    if not text or len(text) > limit:
        raise ValueError(f"{label} must be 1-{limit} characters")
    for pattern in PRIVATE:
        if pattern.search(text):
            raise ValueError(f"{label} contains private text")
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


def propose(
    kind: str,
    name: str,
    text: str,
    *,
    project: Path | None = None,
) -> dict[str, Any]:
    if kind not in KINDS:
        raise ValueError("candidate kind must be prompt or skill")
    label = _check_text(name, label="name", limit=64)
    body = _check_text(text, label="text", limit=MAX_TEXT)
    item = {
        "id": hashlib.sha256(f"{kind}\n{label}\n{body}".encode()).hexdigest()[:16],
        "kind": kind,
        "name": label,
        "text": body,
        "accepted": False,
        "score": None,
    }
    document = load_index(project)
    rows = document["candidates"]
    if not isinstance(rows, list):
        raise ValueError("prompt evolution index is invalid")
    kept = [row for row in rows if isinstance(row, dict) and row.get("id") != item["id"]]
    kept.append(item)
    document["candidates"] = kept[-64:]
    _save(document, project)
    return item


def _find(document: dict[str, Any], candidate_id: str) -> dict[str, Any]:
    for row in document["candidates"]:
        if isinstance(row, dict) and row.get("id") == candidate_id:
            return row
    raise ValueError("candidate id is unknown")


def score_candidate(
    candidate_id: str,
    report_path: Path,
    *,
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> dict[str, Any]:
    """Attach the suite score. Refuses unless this task passed inside a passing suite."""
    if not manifest_lists_task(eval_manifest):
        raise ValueError("prompt evolution stays behind the harness eval suite")
    report = json.loads(Path(report_path).read_text(encoding="utf-8"))
    if not isinstance(report, dict):
        raise ValueError("suite report is invalid")
    results = report.get("results")
    if not isinstance(results, list) or report.get("passed") is not True:
        raise ValueError("suite report did not pass")
    try:
        suite_score = float(report.get("pass_at_k"))
        required = float(report.get("threshold_pass_at_k", 1.0))
    except (TypeError, ValueError) as exc:
        raise ValueError("suite report pass_at_k is missing") from exc
    if suite_score < required:
        raise ValueError("suite report pass_at_k is below the threshold")
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
        raise ValueError("suite report did not pass the prompt-evolution task")
    document = load_index(project)
    chosen = _find(document, candidate_id)
    chosen["score"] = {
        "pass_at_k": suite_score,
        "task_pass_at_k": float(matched["pass_at_k"]),
        "passed": True,
    }
    chosen["accepted"] = False
    _save(document, project)
    return chosen


def accept_candidate(
    candidate_id: str,
    *,
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> Path:
    if not manifest_lists_task(eval_manifest):
        raise ValueError("prompt evolution stays behind the harness eval suite")
    document = load_index(project)
    chosen = _find(document, candidate_id)
    score = chosen.get("score")
    if not isinstance(score, dict) or score.get("passed") is not True:
        raise ValueError("candidate has no passing suite score")
    destination = index_path(project).parent / "accepted" / f"{candidate_id}.md"
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(
        "\n".join(
            [
                "---",
                f"name: {chosen['name']}",
                f"kind: {chosen['kind']}",
                "scored_by: harness-eval-suite",
                "---",
                "",
                chosen["text"],
                "",
            ]
        ),
        encoding="utf-8",
    )
    chosen["accepted"] = True
    _save(document, project)
    return destination


def summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    rows = [row for row in document.get("candidates") or [] if isinstance(row, dict)]
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "prompt-evolution-summary",
        "candidateCount": len(rows),
        "acceptedCount": sum(1 for row in rows if row.get("accepted") is True),
        "evalGateOpen": manifest_lists_task(),
        "status": "healthy" if rows else "absent",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    add = sub.add_parser("propose")
    add.add_argument("--kind", required=True, choices=KINDS)
    add.add_argument("--name", required=True)
    add.add_argument("--text", required=True)
    add.add_argument("--project", type=Path, default=None)
    grade = sub.add_parser("score")
    grade.add_argument("--id", required=True)
    grade.add_argument("--report", type=Path, required=True)
    grade.add_argument("--project", type=Path, default=None)
    grade.add_argument("--eval-manifest", type=Path, default=None)
    keep = sub.add_parser("accept")
    keep.add_argument("--id", required=True)
    keep.add_argument("--project", type=Path, default=None)
    keep.add_argument("--eval-manifest", type=Path, default=None)
    report = sub.add_parser("summary")
    report.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "propose":
        print(
            json.dumps(
                propose(args.kind, args.name, args.text, project=args.project),
                sort_keys=True,
            )
        )
        return 0
    if args.command == "score":
        print(
            json.dumps(
                score_candidate(
                    args.id,
                    args.report,
                    project=args.project,
                    eval_manifest=args.eval_manifest,
                ),
                sort_keys=True,
            )
        )
        return 0
    if args.command == "accept":
        print(
            json.dumps(
                {
                    "path": str(
                        accept_candidate(
                            args.id, project=args.project, eval_manifest=args.eval_manifest
                        )
                    )
                },
                sort_keys=True,
            )
        )
        return 0
    print(json.dumps(summary(args.project), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
