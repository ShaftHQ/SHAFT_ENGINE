#!/usr/bin/env python3
"""
Periodic meta-optimize over shared CE logs (#5664 / Top 10 #9).

Offline/script aggregation of significance marks + learning queue metrics +
skill_compress_audit proposals into a bounded review summary / issue candidates.

NOT continuous Task Observer. NOT always-on SessionStart scan.
Operator cadence: run periodically (e.g. weekly) or after a Learning Session
burst — never from SessionStart / every-turn hooks.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
MAX_ISSUE_CANDIDATES = 8
MAX_NOTE_CHARS = 160
SUMMARY_RELATIVE = Path(".chaos-engine-state") / "meta-optimize" / "last-review.json"
CADENCE = (
    "periodic-offline",
    "operator weekly or post Learning Session burst",
    "never SessionStart",
    "never Task Observer / always-on",
)


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def _load_sibling(name: str, attribute: str | None = None):
    path = Path(__file__).resolve().parent / name
    spec = importlib.util.spec_from_file_location(f"ce_meta_{name}", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {name}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    if attribute is None:
        return module
    return getattr(module, attribute)


def _clip(text: str, limit: int = MAX_NOTE_CHARS) -> str:
    cleaned = " ".join(str(text).split())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: max(0, limit - 1)] + "…"


def _significance_slice(project: Path) -> dict[str, Any]:
    significance = _load_sibling("significance.py")
    marks = significance.list_marks(project)
    kinds: dict[str, int] = {}
    samples: list[dict[str, Any]] = []
    for mark in marks:
        if not isinstance(mark, dict):
            continue
        kind = str(mark.get("kind") or "unknown")
        kinds[kind] = kinds.get(kind, 0) + 1
        if len(samples) < 5:
            samples.append(
                {
                    "kind": kind,
                    "note": _clip(str(mark.get("note") or "")),
                    "source": mark.get("source"),
                }
            )
    return {
        "count": len(marks),
        "kinds": dict(sorted(kinds.items())),
        "samples": samples,
        "drained": False,
        "policy": "list-only; Learning Session owns drain",
    }


def _learning_slice(project: Path) -> dict[str, Any]:
    learning = _load_sibling("learning.py")
    metrics = learning.learning_metrics(project=project)
    return {
        "queued": metrics.get("queued"),
        "submitted": metrics.get("submitted"),
        "submittedRate": metrics.get("submittedRate"),
        "estimatedTokens": metrics.get("estimatedTokens"),
        "learningSessions": metrics.get("learningSessions"),
        "sessionStartBytesLast": metrics.get("sessionStartBytesLast"),
        "denials": metrics.get("denials"),
        "status": metrics.get("status"),
    }


def _compress_slice(project: Path) -> dict[str, Any]:
    compress = _load_sibling("skill_compress_audit.py")
    report = compress.audit_tree(project, include_diff=False)
    proposals: list[dict[str, Any]] = []
    for skill in report.get("skills") or []:
        if not isinstance(skill, dict) or not skill.get("proposals"):
            continue
        proposals.append(
            {
                "skill": skill.get("name"),
                "proposalCount": len(skill.get("proposals") or []),
                "overBudget": bool((skill.get("score") or {}).get("overBudget")),
                "score": skill.get("score"),
            }
        )
        if len(proposals) >= MAX_ISSUE_CANDIDATES:
            break
    return {
        "skillCount": report.get("skillCount"),
        "overBudgetCount": report.get("overBudgetCount"),
        "withProposals": report.get("withProposals"),
        "proposals": proposals,
        "mutate": False,
        "applyGate": report.get("applyGate"),
    }


def _issue_candidates(
    significance: dict[str, Any],
    learning: dict[str, Any],
    compress: dict[str, Any],
) -> list[dict[str, Any]]:
    candidates: list[dict[str, Any]] = []
    for sample in significance.get("samples") or []:
        if len(candidates) >= MAX_ISSUE_CANDIDATES:
            break
        candidates.append(
            {
                "track": "harness",
                "source": "significance",
                "title": _clip(f"[CE] Significance: {sample.get('kind')}", 72),
                "bodyStub": _clip(
                    f"From periodic meta-optimize. kind={sample.get('kind')}; "
                    f"note={sample.get('note')}. Propose-only — no auto-mutate."
                ),
                "proposeOnly": True,
            }
        )
    queued = int(learning.get("queued") or 0)
    submitted = int(learning.get("submitted") or 0)
    if queued > submitted and queued - submitted >= 3:
        candidates.append(
            {
                "track": "harness",
                "source": "learning-metrics",
                "title": "[CE] Learning queue backlog review",
                "bodyStub": _clip(
                    f"queued={queued} submitted={submitted} "
                    f"rate={learning.get('submittedRate')}. Review adopt/drop."
                ),
                "proposeOnly": True,
            }
        )
    for item in compress.get("proposals") or []:
        if len(candidates) >= MAX_ISSUE_CANDIDATES:
            break
        candidates.append(
            {
                "track": "harness",
                "source": "skill-compress-audit",
                "title": _clip(f"[CE] Skill compress proposal: {item.get('skill')}", 72),
                "bodyStub": _clip(
                    f"proposals={item.get('proposalCount')} "
                    f"overBudget={item.get('overBudget')}. "
                    "Apply only via opt-in draft-skill-pr gate (#5665 / #8)."
                ),
                "proposeOnly": True,
                "skill": item.get("skill"),
            }
        )
    return candidates[:MAX_ISSUE_CANDIDATES]


def review(project: Path | None = None) -> dict[str, Any]:
    """Aggregate shared logs into a bounded offline review summary."""
    root = project_root(project)
    significance = _significance_slice(root)
    learning = _learning_slice(root)
    compress = _compress_slice(root)
    candidates = _issue_candidates(significance, learning, compress)
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "meta-optimize-review",
        "cadence": list(CADENCE),
        "continuous": False,
        "sessionStart": False,
        "taskObserver": False,
        "generatedAt": int(time.time()),
        "project": str(root),
        "significance": significance,
        "learningMetrics": learning,
        "skillCompress": compress,
        "issueCandidates": candidates,
        "issueCandidateCount": len(candidates),
        "policy": (
            "offline/periodic aggregation; propose issue candidates only; "
            "never Task Observer; never always-on SessionStart; "
            "skill mutations remain S4/#8 opt-in draft PRs"
        ),
        "mutate": False,
        "autoMerge": False,
    }


def write_review(document: dict[str, Any], project: Path | None = None) -> Path:
    root = project_root(project)
    path = root / SUMMARY_RELATIVE
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def cadence_text() -> str:
    return (
        "Operator cadence (periodic, not continuous):\n"
        "  1. After a delivery / Learning Session burst, or weekly,\n"
        "  2. Run: python3 .chaos-engine/meta_optimize.py review\n"
        "  3. Triage issueCandidates → gh issue create (human),\n"
        "  4. Skill compress proposals → draft-skill-pr ONLY with opt-in gate.\n"
        "Never wire this into SessionStart or always-on Task Observer.\n"
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        nargs="?",
        default="review",
        choices=("review", "cadence"),
        help="review (default) or print cadence",
    )
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument(
        "--write",
        action="store_true",
        help=f"persist summary under {SUMMARY_RELATIVE.as_posix()}",
    )
    args = parser.parse_args(argv)
    if args.command == "cadence":
        print(cadence_text(), end="")
        return 0
    document = review(args.project)
    if args.write:
        path = write_review(document, args.project)
        document = dict(document)
        document["writtenTo"] = str(path)
    print(json.dumps(document, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
