#!/usr/bin/env python3
"""Lean executor prompts + fingerprint-first failed-log budget (#6167).

Executor / Task / ``codex exec`` prompts carry a ``brief_path:`` pointer to a
spilled wave brief plus the delta slice only (goal, constraints, files, RED
command), capped at ``MAX_INLINE_BYTES``. A failed CI log whose summary already
names a known fingerprint (see ``tip_preflight.classify_failure``) is read as at
most ``FAILED_LOG_LINE_CAP`` lines around that fingerprint; otherwise the full
log is spilled to disk and only its path plus tail stays in context.

    python3 chaos-engine/skills/local-agency/scripts/executor_brief.py lint PROMPT.md
    python3 chaos-engine/skills/local-agency/scripts/executor_brief.py log-budget job.log --fingerprint inventory-drift

Stdlib only; identical on Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

MAX_INLINE_BYTES = 4096
FAILED_LOG_LINE_CAP = 40
BRIEF_PATH_LINE = re.compile(r"(?m)^brief_path:\s*\S+")
COMPLETED_TODO = re.compile(r"(?m)^\s*[-*]\s*\[[xX]\]|\b(?i:status:\s*completed)\b|\bCOMPLETED\b")


def lint_executor_prompt(text: str) -> list[str]:
    """Executor/Task prompt schema: brief_path pointer + delta slice, bounded inline bytes."""
    failures = []
    if BRIEF_PATH_LINE.search(text) is None:
        failures.append("missing brief_path: pointer line (spill the wave brief to a file)")
    size = len(text.encode("utf-8"))
    if size > MAX_INLINE_BYTES:
        failures.append(f"inline prompt is {size} bytes; cap is {MAX_INLINE_BYTES}")
    if COMPLETED_TODO.search(text):
        failures.append("completed todos from another wave must not be pasted into an executor prompt")
    return failures


def fingerprint_excerpt(log_text: str, fingerprint: str, *, cap: int = FAILED_LOG_LINE_CAP) -> str:
    """At most `cap` lines centred on the first line containing `fingerprint`; '' when absent."""
    lines = log_text.splitlines()
    i = next((i for i, line in enumerate(lines) if fingerprint in line), None)
    if i is None:
        return ""
    start = max(0, i - cap // 2)
    return "\n".join(lines[start:start + cap])


def failed_log_budget(log_text: str, fingerprint: str | None, spill_dir: Path, *, cap: int = FAILED_LOG_LINE_CAP) -> dict:
    """Fingerprint-first failed-log budget: bounded excerpt, else spill to disk and keep the path."""
    if fingerprint:
        excerpt = fingerprint_excerpt(log_text, fingerprint, cap=cap)
        if excerpt:
            return {"fingerprint": fingerprint, "excerpt": excerpt, "spill_path": None}
    spill_dir.mkdir(parents=True, exist_ok=True)
    path = spill_dir / "failed-log.txt"
    path.write_text(log_text, encoding="utf-8")
    lines = log_text.splitlines()
    tail = "\n".join(lines[-cap:])
    return {"fingerprint": fingerprint, "excerpt": tail, "spill_path": str(path)}


def main(argv: list[str] | None = None) -> int:
    """CLI: `lint PROMPT_FILE` or `log-budget LOG_FILE --fingerprint X --spill-dir D`."""
    parser = argparse.ArgumentParser(description="CLI for linting executor prompts and managing failed log budgets.")
    subparsers = parser.add_subparsers(dest="command", required=True)

    lint_parser = subparsers.add_parser("lint", help="Lint an executor prompt file.")
    lint_parser.add_argument("prompt_file", type=Path, help="Path to the prompt file.")

    log_budget_parser = subparsers.add_parser("log-budget", help="Manage failed log budgets.")
    log_budget_parser.add_argument("log_file", type=Path, help="Path to the log file.")
    log_budget_parser.add_argument("--fingerprint", type=str, default=None, help="Fingerprint to filter log lines.")
    log_budget_parser.add_argument("--spill-dir", type=Path, default=Path(".chaos-engine/runtime/failed-logs"), help="Directory to spill failed logs to.")

    args = parser.parse_args(argv)

    if args.command == "lint":
        failures = lint_executor_prompt(args.prompt_file.read_text(encoding="utf-8"))
        for failure in failures:
            print(f"executor brief: {failure}", file=sys.stderr)
        if failures:
            return 1
        print("executor brief: pass")
        return 0

    if args.command == "log-budget":
        result = failed_log_budget(args.log_file.read_text(encoding="utf-8", errors="replace"), args.fingerprint, args.spill_dir)
        print(json.dumps(result))
        return 0
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
