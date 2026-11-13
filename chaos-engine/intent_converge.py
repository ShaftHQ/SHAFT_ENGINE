"""On-demand intent inventory for a work item against the current tree.

A checked task is not evidence. This module prints a report and does not
write files, delete unrequested paths, or add an empty convergence section.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

_OBLIGATION = re.compile(r"^(?P<id>(?:FR|SC)-\d+)\s*:\s*(?P<body>.+)$", re.M)
_EVIDENCE = re.compile(r"Evidence:\s*(?P<path>\S+)")
_TASK = re.compile(r"^\s*[-*]\s+\[[ xX]\]\s+(?P<body>.+)$", re.M)
_MUST_NOT = re.compile(r"MUST NOT\s+(?P<phrase>.+?)(?:\.|$)", re.I)
_MUST = re.compile(r"MUST\s+(?!NOT\b)(?P<phrase>.+?)(?:\.|$)", re.I)
_CLARIFY = "NEEDS CLARIFICATION"
_STAMP = "Deliver the stated acceptance"
_DECISION = re.compile(r"(?m)^decision:\s*(go|needs-clarification|kill)\s*$")
_VERIFIED = re.compile(r"\bverified\b", re.I)
_EXERCISED = "exercised the original reproduction"


def obligations(text: str) -> list[dict[str, str]]:
    """Return FR and SC obligations. Checkbox lines are not obligations."""
    found: list[dict[str, str]] = []
    for match in _OBLIGATION.finditer(text):
        body = match.group("body").strip()
        evidence = _EVIDENCE.search(body)
        found.append(
            {
                "id": match.group("id"),
                "body": body,
                "source": match.group(0).strip(),
                "evidence": evidence.group("path") if evidence else "",
            }
        )
    return found


def _phrase_conflicts(items: list[dict[str, str]]) -> list[tuple[str, str, str]]:
    """Return MUST versus MUST NOT pairs that name the same phrase."""
    required: dict[str, str] = {}
    banned: dict[str, str] = {}
    for item in items:
        for match in _MUST_NOT.finditer(item["body"]):
            banned[match.group("phrase").strip().casefold()] = item["id"]
        for match in _MUST.finditer(item["body"]):
            required[match.group("phrase").strip().casefold()] = item["id"]
    conflicts: list[tuple[str, str, str]] = []
    for phrase, left in required.items():
        right = banned.get(phrase)
        if right and right != left:
            conflicts.append((left, right, phrase))
    return conflicts


def _contained(base: Path, evidence: str) -> Path | None:
    """Return an evidence path only when it stays inside ``base``."""
    if not evidence:
        return None
    relative = Path(evidence)
    if relative.is_absolute() or ".." in relative.parts:
        return None
    root = base.resolve()
    candidate = (root / relative).resolve()
    try:
        candidate.relative_to(root)
    except ValueError:
        return None
    return candidate


def _finding(
    item_id: str,
    kind: str,
    source: str,
    evidence: str,
    summary: str,
    *,
    severity: str,
    category: str = "",
) -> dict[str, str]:
    return {
        "id": item_id,
        "kind": kind,
        "category": category or kind,
        "severity": severity,
        "source": source,
        "evidence": evidence,
        "locations": source,
        "summary": summary,
        "recommendation": "File the leftover work. Do not rewrite the original acceptance text.",
    }


def converge(root: Path, work_item: str) -> dict[str, object]:
    """Compare obligations with files in ``root``. Leave ``work_item`` unchanged."""
    original = work_item
    base = Path(root)
    items = obligations(work_item)
    findings: list[dict[str, str]] = []
    named: set[str] = set()
    for item in items:
        evidence = item["evidence"]
        if evidence:
            named.add(evidence.replace("\\", "/"))
        path = _contained(base, evidence)
        satisfied = False
        if path is not None and path.is_file():
            try:
                satisfied = item["id"] in path.read_text(encoding="utf-8")
            except (OSError, UnicodeError):
                satisfied = False
        if satisfied:
            continue
        kind = "partial" if path is not None and path.is_file() else "missing"
        findings.append(
            _finding(
                item["id"],
                kind,
                item["source"],
                evidence,
                f"{item['id']} is not satisfied in the current tree",
                severity="HIGH",
            )
        )
    for left, right, phrase in _phrase_conflicts(items):
        findings.append(
            _finding(
                f"{left}/{right}",
                "contradicts",
                phrase,
                "",
                f"{left} and {right} disagree on {phrase}",
                severity="HIGH",
            )
        )
    if base.is_dir():
        for child in sorted(path for path in base.rglob("*") if path.is_file()):
            rel = child.relative_to(base).as_posix()
            if rel in named:
                continue
            findings.append(
                _finding(
                    rel,
                    "unrequested",
                    rel,
                    rel,
                    f"{rel} is not named by an obligation",
                    severity="LOW",
                )
            )
    actionable = [item for item in findings if item["kind"] in {"missing", "partial", "contradicts"}]
    status = "Converged" if not actionable else "findings"
    reported = [item for item in findings if item["kind"] == "unrequested"] if status == "Converged" else findings
    return {"status": status, "findings": reported, "work_item": original}


def analyze(work_item: str, tasks: str) -> dict[str, object]:
    """Read-only coverage and contradiction report. This function writes nothing."""
    items = obligations(work_item)
    task_lines = _TASK.findall(tasks)
    findings: list[dict[str, str]] = []
    covered = 0
    for item in items:
        if any(item["id"] in line for line in task_lines):
            covered += 1
            continue
        findings.append(
            _finding(
                item["id"],
                "coverage",
                item["source"],
                "",
                f"{item['id']} has no task",
                severity="HIGH",
                category="coverage",
            )
        )
    for left, right, phrase in _phrase_conflicts(items):
        findings.append(
            _finding(
                f"{left}/{right}",
                "inconsistency",
                phrase,
                "",
                f"{left} and {right} disagree on {phrase}",
                severity="HIGH",
                category="inconsistency",
            )
        )
    return {
        "findings": findings[:50],
        "coverage": {
            "obligations": len(items),
            "tasks": len(task_lines),
            "covered": covered,
        },
    }


def apply_clarification(work_item: str, answers: list[str]) -> str:
    """Write at most five answers back into the work item. Zero answers leaves it unchanged."""
    if len(answers) > 5:
        raise ValueError("at most five clarification answers")
    if not answers:
        return work_item
    text = work_item
    for answer in answers:
        if _CLARIFY not in text:
            break
        text = text.replace(_CLARIFY, answer, 1)
    return text


def _tracking_row(plan: str) -> bool:
    if "## Complexity Tracking" not in plan:
        return False
    section = plan.split("## Complexity Tracking", 1)[1].split("\n## ", 1)[0]
    for line in section.splitlines():
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) < 3 or not all(cells[:3]):
            continue
        if cells[0] in {"Violation", "---"} or set(cells[0]) <= {"-"}:
            continue
        return True
    return False


def plan_gaps(plan: str) -> list[str]:
    """A consequential plan cites identity or an iron law. A passing plan may omit the table."""
    if "## Constitution Check" not in plan:
        return ["missing Constitution Check"]
    section = plan.split("## Constitution Check", 1)[1].split("\n## ", 1)[0]
    gaps: list[str] = []
    folded = section.casefold()
    if "identity.md" not in section and "iron law" not in folded:
        gaps.append("Constitution Check does not cite identity.md or an iron law")
    if "violation" in folded and not _tracking_row(plan):
        gaps.append("iron-law violation without a Complexity Tracking row")
    return gaps


def bug_verdict_errors(text: str) -> list[str]:
    """``verified`` is invalid unless the original reproduction was exercised."""
    if _VERIFIED.search(text) and _EXERCISED not in text.casefold():
        return ["verified without exercising the original reproduction"]
    return []


def idea_decision_errors(text: str) -> list[str]:
    """A new-product idea records exactly one go, needs-clarification, or kill."""
    found = _DECISION.findall(text)
    if len(found) != 1:
        return ["a new-product idea needs exactly one of go, needs-clarification, or kill"]
    return []


def english_quality_findings(text: str) -> list[str]:
    """Measurable success criteria, and no repeated acceptance stamp."""
    findings: list[str] = []
    if text.count(_STAMP) > 1:
        findings.append("identical acceptance stamp")
    for match in _OBLIGATION.finditer(text):
        if not match.group("id").startswith("SC-"):
            continue
        if not re.search(r"\d", match.group("body")):
            findings.append(f"{match.group('id')} is not measurable")
    return findings


def registry_gaps(skills_root: Path, registered: set[str]) -> list[str]:
    """Skill directories on disk that are absent from the registry."""
    gaps: list[str] = []
    root = Path(skills_root)
    if not root.is_dir():
        return gaps
    for path in sorted(root.glob("*/SKILL.md")):
        name = path.parent.name
        if name not in registered:
            gaps.append(name)
    return gaps


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    """Print a JSON report. Never write the work item or the tree."""
    args = list(sys.argv[1:] if argv is None else argv)
    if len(args) < 2 or args[0] not in {"converge", "analyze"}:
        print("usage: intent_converge.py converge WORK_ITEM TREE | analyze WORK_ITEM TASKS", file=sys.stderr)
        return 2
    work_path = Path(args[1])
    before = work_path.read_bytes()
    work_item = _read(work_path)
    if args[0] == "converge":
        if len(args) != 3:
            print("usage: intent_converge.py converge WORK_ITEM TREE", file=sys.stderr)
            return 2
        report = converge(Path(args[2]), work_item)
    else:
        if len(args) != 3:
            print("usage: intent_converge.py analyze WORK_ITEM TASKS", file=sys.stderr)
            return 2
        report = analyze(work_item, _read(Path(args[2])))
    if work_path.read_bytes() != before:
        print("refusing to rewrite the work item", file=sys.stderr)
        return 1
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
