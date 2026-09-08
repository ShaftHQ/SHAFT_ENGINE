#!/usr/bin/env python3
"""
SkillOpt-style SKILL.md compression audit — propose only, never apply (#5659 / #6).

Offline/script CLI that scores skill bodies for filler/bloat and emits a bounded
propose-only report (optional unified-diff stubs). Never writes SKILL.md.
Future apply remains S4/#8 opt-in and must pass eval-parity + unit tests.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
L2_LINE_BUDGET = 500
MAX_DIFF_LINES = 80
# Deterministic filler / bloat heuristics (zero-LLM).
FILLER = re.compile(
    r"(?i)^\s*(?:"
    r"note that|it is important(?: to)?|in order to|please (?:note|remember)|"
    r"as mentioned(?: above| earlier)?|basically|essentially|"
    r"keep in mind that|make sure (?:to|that)|don't forget|"
    r"this (?:section|document) (?:describes|explains|covers)"
    r")\b"
)
BLANK_RUN = re.compile(r"\n{3,}")
FENCE = re.compile(r"^```")


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def discover_skills(root: Path | None = None) -> list[Path]:
    """Locate ChaosEngine overlay SKILL.md files (portable skills only)."""
    base = project_root(root)
    skills_root = base / "chaos-engine" / "skills"
    if not skills_root.is_dir():
        installed = base / ".chaos-engine" / "skills"
        skills_root = installed if installed.is_dir() else skills_root
    found: list[Path] = []
    if skills_root.is_dir():
        for path in sorted(skills_root.glob("*/SKILL.md")):
            if path.is_file():
                found.append(path)
    return found


def _split_frontmatter(text: str) -> tuple[str, str]:
    if not text.startswith("---\n"):
        return "", text
    end = text.find("\n---\n", 4)
    if end < 0:
        return "", text
    return text[4:end], text[end + 5 :]


def _fence_spans(lines: list[str]) -> list[tuple[int, int]]:
    spans: list[tuple[int, int]] = []
    start: int | None = None
    for index, line in enumerate(lines):
        if FENCE.match(line):
            if start is None:
                start = index
            else:
                spans.append((start, index))
                start = None
    return spans


def audit_skill(path: Path) -> dict[str, Any]:
    """Score one SKILL.md; propose compress actions without mutating the file."""
    text = path.read_text(encoding="utf-8")
    frontmatter, body = _split_frontmatter(text)
    lines = text.splitlines()
    body_lines = body.splitlines()
    line_count = len(lines)
    byte_count = len(text.encode("utf-8"))
    filler_hits: list[dict[str, Any]] = []
    for index, line in enumerate(lines, start=1):
        if FILLER.search(line):
            filler_hits.append({"line": index, "text": line.strip()[:100]})
    blank_runs = len(BLANK_RUN.findall(text + "\n"))
    fences = _fence_spans(lines)
    long_fences = [
        {"start": a + 1, "end": b + 1, "lines": b - a - 1}
        for a, b in fences
        if (b - a - 1) > 24
    ]
    duplicate_pairs = 0
    for index in range(1, len(lines)):
        if lines[index].strip() and lines[index] == lines[index - 1]:
            duplicate_pairs += 1
    over_budget = line_count > L2_LINE_BUDGET
    # Bounded compress proposals (never applied).
    proposals: list[dict[str, Any]] = []
    if over_budget:
        proposals.append(
            {
                "action": "split-refs",
                "reason": f"over L2 budget ({line_count}>{L2_LINE_BUDGET})",
                "hint": "Move long examples/tables into references/; keep router lean",
            }
        )
    if filler_hits:
        proposals.append(
            {
                "action": "drop-filler",
                "reason": f"{len(filler_hits)} filler phrase hit(s)",
                "lines": [hit["line"] for hit in filler_hits[:12]],
            }
        )
    if long_fences:
        proposals.append(
            {
                "action": "extract-fences",
                "reason": f"{len(long_fences)} long fenced block(s)",
                "blocks": long_fences[:6],
                "hint": "Prefer filepath citations over inlined dumps",
            }
        )
    if blank_runs:
        proposals.append(
            {
                "action": "collapse-blank-runs",
                "reason": f"{blank_runs} oversized blank run(s)",
            }
        )
    if duplicate_pairs:
        proposals.append(
            {
                "action": "dedupe-lines",
                "reason": f"{duplicate_pairs} consecutive duplicate line pair(s)",
            }
        )
    score = {
        "lineCount": line_count,
        "byteCount": byte_count,
        "bodyLines": len(body_lines),
        "frontmatterLines": len(frontmatter.splitlines()) if frontmatter else 0,
        "fillerHits": len(filler_hits),
        "longFences": len(long_fences),
        "blankRuns": blank_runs,
        "duplicatePairs": duplicate_pairs,
        "overBudget": over_budget,
        "l2Budget": L2_LINE_BUDGET,
    }
    return {
        "path": path.as_posix(),
        "name": path.parent.name,
        "score": score,
        "fillerSamples": filler_hits[:8],
        "proposals": proposals,
        "mutate": False,
        "applyGate": "eval-parity + unit tests; apply remains S4/#8 opt-in",
    }


def _drop_lines_from_proposals(report: dict[str, Any]) -> set[int]:
    drop_lines: set[int] = set()
    for proposal in report.get("proposals") or []:
        if not isinstance(proposal, dict):
            continue
        if proposal.get("action") != "drop-filler":
            continue
        for line_no in proposal.get("lines") or []:
            if isinstance(line_no, int):
                drop_lines.add(line_no)
    return drop_lines


def _trim_trailing_blanks(lines: list[str]) -> list[str]:
    kept = list(lines)
    while kept and not kept[-1].strip():
        kept.pop()
    return kept


def _diff_body_lines(
    original: list[str], drop_lines: set[int], *, max_lines: int
) -> list[str]:
    body: list[str] = []
    for index, line in enumerate(original, start=1):
        if index in drop_lines:
            body.append(f"-{line}")
            continue
        if len(body) >= max_lines // 2:
            continue
        if any(abs(index - dropped) <= 1 for dropped in drop_lines):
            body.append(f" {line}")
    return body


def propose_diff_stub(report: dict[str, Any], *, max_lines: int = MAX_DIFF_LINES) -> str:
    """
    Build a bounded unified-diff *stub* of filler line removals (never written).
    """
    path = Path(str(report.get("path") or "SKILL.md"))
    try:
        original = path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return ""
    drop_lines = _drop_lines_from_proposals(report)
    over_budget = bool((report.get("score") or {}).get("overBudget"))
    if not drop_lines and not over_budget:
        return ""
    kept = [line for index, line in enumerate(original, start=1) if index not in drop_lines]
    if len(kept) == len(original) and over_budget:
        kept = _trim_trailing_blanks(kept)
    if kept == original:
        return ""
    header = [
        f"--- a/{path.name}",
        f"+++ b/{path.name}  (PROPOSE ONLY — not applied)",
        f"@@ compress stub; gate: {report.get('applyGate')} @@",
    ]
    body = _diff_body_lines(original, drop_lines, max_lines=max_lines)
    if not body and len(kept) < len(original):
        body.append(f"-# trimmed {len(original) - len(kept)} trailing blank line(s)")
    stub = "\n".join(header + body[:max_lines])
    if len(body) > max_lines:
        stub += f"\n# ... truncated ({len(body) - max_lines} more propose lines)"
    return stub + "\n"



def audit_tree(
    root: Path | None = None,
    *,
    skill: str | None = None,
    include_diff: bool = False,
) -> dict[str, Any]:
    paths = discover_skills(root)
    if skill:
        paths = [path for path in paths if path.parent.name == skill]
    skills = [audit_skill(path) for path in paths]
    if include_diff:
        for report in skills:
            stub = propose_diff_stub(report)
            if stub:
                report["diffStub"] = stub
    over = sum(1 for report in skills if report["score"]["overBudget"])
    with_proposals = sum(1 for report in skills if report["proposals"])
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "skill-compress-audit",
        "policy": "propose-only; never auto-apply / auto-merge",
        "applyGate": "eval-parity + unit tests; apply remains S4/#8 opt-in",
        "l2Budget": L2_LINE_BUDGET,
        "skillCount": len(skills),
        "overBudgetCount": over,
        "withProposals": with_proposals,
        "skills": skills,
        "mutate": False,
    }


def assert_no_mutate(before: dict[str, str], after: dict[str, str]) -> None:
    """Test helper: skill file bytes must be unchanged by audit."""
    if before != after:
        raise AssertionError("skill_compress_audit mutated SKILL.md (forbidden)")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        nargs="?",
        default="audit",
        choices=("audit",),
        help="audit (propose only)",
    )
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument("--skill", default=None, help="limit to one skill directory name")
    parser.add_argument(
        "--diff",
        action="store_true",
        help="include bounded unified-diff stubs in JSON (still not applied)",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="exit 1 when any skill exceeds L2 budget (report still propose-only)",
    )
    args = parser.parse_args(argv)
    # Snapshot bytes to prove no mutate even if caller asks for diffs.
    root = project_root(args.project)
    snapshots = {str(path): path.read_bytes() for path in discover_skills(root)}
    report = audit_tree(root, skill=args.skill, include_diff=args.diff)
    after = {str(path): path.read_bytes() for path in discover_skills(root)}
    assert_no_mutate(snapshots, after)
    print(json.dumps(report, indent=2, sort_keys=True))
    if args.strict and report["overBudgetCount"]:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
