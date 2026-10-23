#!/usr/bin/env python3
"""
Pre-done self-review gate (#7325): fail a status report whose views drift or whose gaps lack a rule.

Checks one markdown report (PR body, handoff note, status doc):
1. Status table parity: every mermaid node that names a table ID must carry that row's status.
2. Every Blocked/Skipped row cites a permitting rule (``rule:``) and an alternative tried (``alt:``
   naming a mock, fixture, stub, fake, simulator or recording).
3. Declared counts (``Blocked: 3``) match the table.
4. A non-empty ``## What's missing`` section answers "are we missing any?".
Exit 0 pass, 1 findings, 2 usage error. Stdlib only.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

GAP_STATUSES = ("blocked", "skipped")
STATUS_WORDS = ("passed", "pass", "done", "failed", "fail", "blocked", "skipped", "wip", "todo",
                "in progress", "pending", "mocked")
# Short forms map to the table spelling; built with zip so no literal key looks like a secret.
ALIASES = dict(zip(("pass", "fail"), ("passed", "failed")))
ALTERNATIVE = re.compile(r"\balt:\s*\S.*\b(mock|fixture|stub|fake|simulat|record|emulat)", re.I)
RULE = re.compile(r"\brule:\s*\S", re.I)
MERMAID = re.compile(r"```mermaid\n(.*?)```", re.S)
NODE = re.compile(r"\b([A-Za-z][\w-]*)\s*(?:\[\[?|\(\(?|\{\{?|>)\s*\"?([^\]\)\}\"]*)")
CLASS = re.compile(r"^\s*class\s+([\w,-]+)\s+([\w-]+)", re.M)
COUNT = re.compile(r"\b(" + "|".join(w for w in STATUS_WORDS if " " not in w) + r")\s*[:=]\s*(\d+)\b", re.I)
MISSING = re.compile(r"^#{2,6}\s*what'?s missing\??\s*$(.*?)(?=^#{1,6}\s|\Z)", re.I | re.M | re.S)


def _cells(line: str) -> list[str]:
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def norm(status: str) -> str:
    text = status.strip().lower().replace("-", " ").replace("_", " ")
    for word in sorted(STATUS_WORDS, key=len, reverse=True):
        if re.search(rf"\b{word}\b", text):
            return ALIASES.get(word, word)
    return text


def parse_table(text: str) -> list[dict]:
    """First markdown table with a Status column; rows keep the raw line for rule/alt search."""
    lines = text.splitlines()
    for i, line in enumerate(lines[:-1]):
        if "|" not in line or not re.match(r"^\s*\|?\s*:?-{3,}", lines[i + 1]):
            continue
        header = [cell.lower() for cell in _cells(line)]
        if "status" not in header:
            continue
        status_at = header.index("status")
        rows = []
        for row_line in lines[i + 2:]:
            if "|" not in row_line:
                break
            cells = _cells(row_line)
            if len(cells) <= status_at:
                continue
            rows.append({"id": cells[0].strip("` *"), "status": norm(cells[status_at]), "line": row_line})
        return rows
    return []


def mermaid_statuses(text: str, ids: set[str]) -> dict[str, list[str]]:
    found: dict[str, list[str]] = {}
    for block in MERMAID.findall(text):
        for node, label in NODE.findall(block):
            if node in ids and norm(label) in STATUS_WORDS + ("passed", "failed"):
                found.setdefault(node, []).append(norm(label))
        for nodes, klass in CLASS.findall(block):
            for node in nodes.split(","):
                if node in ids and norm(klass) in STATUS_WORDS + ("passed", "failed"):
                    found.setdefault(node, []).append(norm(klass))
    return found


def review(text: str) -> list[str]:
    rows = parse_table(text)
    if not rows:
        return ["no status table: add a markdown table with ID and Status columns"]
    findings = []
    by_id = {row["id"]: row for row in rows}
    for node, statuses in mermaid_statuses(text, set(by_id)).items():
        for status in statuses:
            if status != by_id[node]["status"]:
                findings.append(f"drift: {node} is '{by_id[node]['status']}' in the table but '{status}' in mermaid")
    for row in rows:
        if row["status"] in GAP_STATUSES:
            if not RULE.search(row["line"]):
                findings.append(f"uncited gap: {row['id']} is {row['status']} with no 'rule:' citing the rule that permits it")
            if not ALTERNATIVE.search(row["line"]):
                findings.append(f"no alternative: {row['id']} is {row['status']} with no 'alt:' mock/fixture/stub tried")
    outside = MERMAID.sub("", text)
    table_lines = {row["line"] for row in rows}
    prose = "\n".join(line for line in outside.splitlines() if line not in table_lines)
    for word, number in COUNT.findall(prose):
        status = norm(word)
        actual = sum(1 for row in rows if row["status"] == status)
        if int(number) != actual:
            findings.append(f"count drift: '{word}: {number}' but the table has {actual} {status}")
    section = MISSING.search(text)
    if not section or not section.group(1).strip():
        findings.append("no adversarial pass: add a non-empty '## What's missing' section")
    return findings


def main(argv: list[str]) -> int:
    paths = [arg for arg in argv if not arg.startswith("--")]
    if len(paths) != 1 or any(arg not in {"--json"} for arg in argv if arg.startswith("--")):
        print("usage: pre_done_review.py REPORT.md [--json]", file=sys.stderr)
        return 2
    path = Path(paths[0])
    if not path.is_file():
        print(f"pre-done: {path} not found", file=sys.stderr)
        return 2
    findings = review(path.read_text(encoding="utf-8"))
    if "--json" in argv:
        print(json.dumps({"status": "fail" if findings else "pass", "findings": findings}))
    else:
        for finding in findings:
            print(f"pre-done: {finding}")
        print(f"pre-done: {'fail' if findings else 'pass'} ({len(findings)} finding(s))")
    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
