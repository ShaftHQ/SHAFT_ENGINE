#!/usr/bin/env python3
"""Induce reusable workflows from successful trajectories (#6518; AWM 2409.07429).

Publishing an induced workflow as a skill stays behind the harness eval suite:
materialize refuses unless the suite manifest lists this regression task.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "workflows" / "index.json"
EVAL_TASK_ID = "reg-workflow-induction-6518"
EVAL_MODULE = "tests.scripts.test_chaos_engine_workflow_induction_6518"
MIN_STEPS = 2
MAX_WINDOW = 6
MIN_SUPPORT = 2
MAX_TRAJECTORIES = 64
MAX_WORKFLOWS = 32
MAX_STEP = 80

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
    return {"schemaVersion": SCHEMA_VERSION, "updatedAt": 0, "trajectories": [], "workflows": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("workflow index schema is unsupported")
    document.setdefault("trajectories", [])
    document.setdefault("workflows", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    document["updatedAt"] = int(time.time())
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _check_text(value: str, *, label: str, limit: int) -> str:
    text = " ".join(value.split())
    if not text or len(text) > limit:
        raise ValueError(f"{label} must be 1-{limit} characters")
    for pattern in PRIVATE:
        if pattern.search(text):
            raise ValueError(f"{label} contains private text")
    return text


def _slug(step: str) -> str:
    lowered = re.sub(r"[^a-z0-9]+", "-", step.casefold()).strip("-")
    return lowered or "step"


def record_trajectory(
    name: str,
    steps: list[str],
    *,
    success: bool = True,
    project: Path | None = None,
) -> dict[str, Any]:
    """Record one trajectory. Only successes feed induction."""
    label = _check_text(name, label="name", limit=64)
    if not isinstance(steps, list) or not steps:
        raise ValueError("steps must be a non-empty list")
    cleaned = [_check_text(step, label="step", limit=MAX_STEP) for step in steps]
    document = load_index(project)
    trajectories = document["trajectories"]
    if not isinstance(trajectories, list):
        raise ValueError("workflow index trajectories are invalid")
    item = {
        "id": hashlib.sha256(
            (f"{label}\n" + "\n".join(cleaned)).encode()
        ).hexdigest()[:16],
        "name": label,
        "steps": cleaned,
        "success": bool(success),
    }
    kept = [row for row in trajectories if isinstance(row, dict) and row.get("id") != item["id"]]
    kept.append(item)
    document["trajectories"] = kept[-MAX_TRAJECTORIES:]
    _save(document, project)
    return item


def _windows(steps: list[str]) -> list[tuple[str, ...]]:
    found: list[tuple[str, ...]] = []
    limit = min(MAX_WINDOW, len(steps))
    for size in range(MIN_STEPS, limit + 1):
        for start in range(0, len(steps) - size + 1):
            found.append(tuple(steps[start : start + size]))
    return found


def induce(project: Path | None = None) -> list[dict[str, Any]]:
    """Induce workflows that repeat across at least two successful trajectories."""
    document = load_index(project)
    support: dict[tuple[str, ...], set[str]] = {}
    for row in document["trajectories"]:
        if not isinstance(row, dict) or not row.get("success"):
            continue
        identity = str(row.get("id") or "")
        steps = row.get("steps")
        if not identity or not isinstance(steps, list):
            continue
        for window in set(_windows([str(step) for step in steps])):
            support.setdefault(window, set()).add(identity)
    workflows: list[dict[str, Any]] = []
    ranked = sorted(
        ((window, ids) for window, ids in support.items() if len(ids) >= MIN_SUPPORT),
        key=lambda item: (-len(item[1]), item[0]),
    )
    for window, ids in ranked[:MAX_WORKFLOWS]:
        digest = hashlib.sha256("\n".join(window).encode()).hexdigest()[:16]
        workflows.append(
            {
                "id": digest,
                "name": "wf-" + "-".join(_slug(step) for step in window)[:48],
                "steps": list(window),
                "support": len(ids),
            }
        )
    document["workflows"] = workflows
    _save(document, project)
    return workflows


def eval_gate_open(manifest: Path | None = None) -> bool:
    """True when the harness eval suite still scores this induction task."""
    if manifest is None:
        module_dir = Path(__file__).resolve().parent
        candidates = (
            module_dir.parent / "chaos-engine/evals/harness-suite/manifest.json",
            module_dir / "evals/harness-suite/manifest.json",
        )
        manifest = next((path for path in candidates if path.is_file()), None)
    if manifest is None or not manifest.is_file():
        return False
    document = json.loads(manifest.read_text(encoding="utf-8"))
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


def materialize_skill(
    workflow_id: str,
    *,
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> Path:
    """Write one induced workflow as a skill file. Refuses when the eval gate is closed."""
    if not eval_gate_open(eval_manifest):
        raise ValueError("workflow skill materialize stays behind the harness eval suite")
    document = load_index(project)
    chosen = next(
        (
            row
            for row in document["workflows"]
            if isinstance(row, dict) and row.get("id") == workflow_id
        ),
        None,
    )
    if chosen is None:
        raise ValueError("workflow id is not induced")
    destination = index_path(project).parent / "skills" / workflow_id / "SKILL.md"
    destination.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "---",
        f"name: {chosen['name']}",
        "description: Induced workflow from repeated successful trajectories.",
        "---",
        "",
        f"# {chosen['name']}",
        "",
        f"Support: {chosen['support']} successful trajectories.",
        "",
    ]
    lines.extend(f"{index}. {step}" for index, step in enumerate(chosen["steps"], start=1))
    lines.append("")
    destination.write_text("\n".join(lines), encoding="utf-8")
    return destination


def retrieve(query: str, *, top: int = 3, project: Path | None = None) -> list[dict[str, Any]]:
    needle = _check_text(query, label="query", limit=80).casefold()
    document = load_index(project)
    matched = []
    for row in document["workflows"]:
        if not isinstance(row, dict):
            continue
        haystack = " ".join(str(step) for step in row.get("steps") or []).casefold()
        if needle in haystack or needle in str(row.get("name", "")).casefold():
            matched.append(row)
    matched.sort(key=lambda row: (-int(row.get("support") or 0), str(row.get("id"))))
    return matched[:top]


def summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    trajectories = document.get("trajectories") or []
    workflows = document.get("workflows") or []
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "workflow-induction-summary",
        "trajectoryCount": len(trajectories) if isinstance(trajectories, list) else 0,
        "workflowCount": len(workflows) if isinstance(workflows, list) else 0,
        "evalGateOpen": eval_gate_open(),
        "status": "healthy" if workflows else "absent",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    record = sub.add_parser("trajectory")
    record.add_argument("--name", required=True)
    record.add_argument("--step", action="append", required=True)
    record.add_argument("--failure", action="store_true")
    record.add_argument("--project", type=Path, default=None)
    induce_cmd = sub.add_parser("induce")
    induce_cmd.add_argument("--project", type=Path, default=None)
    show = sub.add_parser("retrieve")
    show.add_argument("--query", required=True)
    show.add_argument("--top", type=int, default=3)
    show.add_argument("--project", type=Path, default=None)
    publish = sub.add_parser("materialize")
    publish.add_argument("--id", required=True)
    publish.add_argument("--project", type=Path, default=None)
    publish.add_argument("--eval-manifest", type=Path, default=None)
    report = sub.add_parser("summary")
    report.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "trajectory":
        print(
            json.dumps(
                record_trajectory(
                    args.name, args.step, success=not args.failure, project=args.project
                ),
                sort_keys=True,
            )
        )
        return 0
    if args.command == "induce":
        print(json.dumps(induce(args.project), sort_keys=True))
        return 0
    if args.command == "retrieve":
        print(json.dumps(retrieve(args.query, top=args.top, project=args.project), sort_keys=True))
        return 0
    if args.command == "materialize":
        print(
            json.dumps(
                {
                    "path": str(
                        materialize_skill(
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
