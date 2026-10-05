#!/usr/bin/env python3
"""
Emit OpenTelemetry GenAI agent spans for a local run (#6518).

Span names and attributes follow the GenAI agent span conventions:
invoke_agent {agent} and execute_tool {tool}, with gen_ai.operation.name and
gen_ai.provider.name required. Spans stay in project state.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
INDEX_RELATIVE = Path(".chaos-engine-state") / "genai-spans" / "index.json"
EVAL_TASK_ID = "reg-genai-spans-6518"
EVAL_MODULE = "tests.scripts.test_chaos_engine_genai_spans_6518"
SPAN_KIND = "INTERNAL"
INVOKE_AGENT = "invoke_agent"
EXECUTE_TOOL = "execute_tool"
OPERATIONS = (INVOKE_AGENT, EXECUTE_TOOL)
ATTR_OPERATION = "gen_ai.operation.name"
ATTR_PROVIDER = "gen_ai.provider.name"
ATTR_AGENT = "gen_ai.agent.name"
ATTR_TOOL = "gen_ai.tool.name"
MAX_FIELD = 64
SLUG = re.compile(r"^[a-z0-9]+(?:[._-][a-z0-9]+)*$")
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
    return {"schemaVersion": SCHEMA_VERSION, "spans": []}


def load_index(project: Path | None = None) -> dict[str, Any]:
    path = index_path(project)
    if not path.is_file():
        return _empty()
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or document.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("genai span index schema is unsupported")
    document.setdefault("spans", [])
    return document


def _save(document: dict[str, Any], project: Path | None) -> None:
    path = index_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _field(value: str, label: str) -> str:
    text = " ".join(value.split())
    if not text or len(text) > MAX_FIELD or not SLUG.fullmatch(text):
        raise ValueError(f"{label} must be a short slug")
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


def _ids(material: str) -> tuple[str, str]:
    digest = hashlib.sha256(material.encode()).hexdigest()
    return digest[:32], digest[32:48]


def emit_span(
    operation: str,
    provider: str,
    *,
    agent: str = "",
    tool: str = "",
    parent_span_id: str = "",
    project: Path | None = None,
    eval_manifest: Path | None = None,
) -> dict[str, Any]:
    if not manifest_lists_task(eval_manifest):
        raise ValueError("genai spans stay behind the harness eval suite")
    if operation not in OPERATIONS:
        raise ValueError("operation must be invoke_agent or execute_tool")
    provider_name = _field(provider, "provider")
    attributes = {ATTR_OPERATION: operation, ATTR_PROVIDER: provider_name}
    if operation == INVOKE_AGENT:
        agent_name = _field(agent, "agent")
        attributes[ATTR_AGENT] = agent_name
        span_name = f"{INVOKE_AGENT} {agent_name}"
    else:
        tool_name = _field(tool, "tool")
        attributes[ATTR_TOOL] = tool_name
        span_name = f"{EXECUTE_TOOL} {tool_name}"
        if not parent_span_id:
            raise ValueError("execute_tool requires a parent span")
    document = load_index(project)
    spans = document["spans"]
    if not isinstance(spans, list):
        raise ValueError("genai span index is invalid")
    if operation == EXECUTE_TOOL and not any(
        isinstance(row, dict) and row.get("spanId") == parent_span_id for row in spans
    ):
        raise ValueError("parent span is unknown")
    trace_id, span_id = _ids(f"{operation}\n{provider_name}\n{span_name}\n{parent_span_id}\n{len(spans)}")
    item = {
        "name": span_name,
        "traceId": trace_id,
        "spanId": span_id,
        "parentSpanId": parent_span_id or None,
        "kind": SPAN_KIND,
        "attributes": attributes,
        "status": "OK",
    }
    spans.append(item)
    document["spans"] = spans[-128:]
    _save(document, project)
    return item


def summary(project: Path | None = None) -> dict[str, Any]:
    document = load_index(project)
    spans = [row for row in document.get("spans") or [] if isinstance(row, dict)]
    counts = {name: sum(1 for row in spans if row.get("attributes", {}).get(ATTR_OPERATION) == name) for name in OPERATIONS}
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "genai-span-summary",
        "spanCount": len(spans),
        "byOperation": counts,
        "evalGateOpen": manifest_lists_task(),
        "status": "healthy" if spans else "absent",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    add = sub.add_parser("emit")
    add.add_argument("--operation", required=True, choices=OPERATIONS)
    add.add_argument("--provider", required=True)
    add.add_argument("--agent", default="")
    add.add_argument("--tool", default="")
    add.add_argument("--parent", default="")
    add.add_argument("--project", type=Path, default=None)
    add.add_argument("--eval-manifest", type=Path, default=None)
    report = sub.add_parser("summary")
    report.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "emit":
        payload = emit_span(
            args.operation,
            args.provider,
            agent=args.agent,
            tool=args.tool,
            parent_span_id=args.parent,
            project=args.project,
            eval_manifest=args.eval_manifest,
        )
    else:
        payload = summary(args.project)
    print(json.dumps(payload, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
