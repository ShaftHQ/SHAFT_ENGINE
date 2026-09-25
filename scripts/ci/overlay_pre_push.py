#!/usr/bin/env python3
"""Local pre-push contract for overlay markdown, skills, and hook entrypoints."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess  # nosec B404 - fixed git invocations, list args only.
import sys
from pathlib import Path

# #6195: running this file directly must not need PYTHONPATH=. -- the repo
# root is two levels up and owns the `scripts.ci` package imports below.
REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

PLAYBOOK = "chaos-engine/references/work-github-playbook.md"
TIP_PREFLIGHT = "chaos-engine/skills/local-agency/scripts/tip_preflight.py"
BYTE_BUDGET = 16384
PINNED_CLAUSES = (
    "Before committing any subagent's work",
    "Before reviewing or shipping any nontrivial diff",
    "deferred/out-of-scope/adjacent-finding/follow-up",
)


def touches_overlay_contract(path: str) -> bool:
    """True for overlay markdown or a hook/skill entrypoint."""
    normalized = path.replace("\\", "/").lstrip("./")
    if normalized.startswith("chaos-engine/") and normalized.endswith(".md"):
        return True
    if normalized.startswith("chaos-engine/hooks/") and normalized.endswith(".py"):
        return True
    return normalized.startswith("chaos-engine/skills/") and normalized.endswith("SKILL.md")


def playbook_contract_failures(text: str, *, budget: int = BYTE_BUDGET) -> list[str]:
    """Fail an over-budget playbook and a playbook that drops a pinned clause."""
    failures: list[str] = []
    size = len(text.encode("utf-8"))
    if size > budget:
        failures.append(f"playbook is {size} bytes; budget is {budget}")
    for clause in PINNED_CLAUSES:
        if clause not in text:
            failures.append(f"dropped pinned clause: {clause}")
    return failures


NEAR_CAP_BYTES = 512


def skill_budget_findings(root: Path) -> tuple[list[str], list[str]]:
    """#6176: SKILL.md over its `skill_budgets` cap fails; within 512 B warns."""
    import json

    budget_path = root / "scripts/ci/agent_guidance_budget.json"
    try:
        budgets = json.loads(budget_path.read_text(encoding="utf-8")).get("skill_budgets", {})
    except (OSError, ValueError):
        return [], []
    failures: list[str] = []
    warnings: list[str] = []
    for key, limits in sorted(budgets.items()):
        cap = limits.get("max_skill_md_bytes") if isinstance(limits, dict) else None
        if not isinstance(cap, int):
            continue
        for skill in sorted((root / key).glob("*/SKILL.md")):
            size = len(skill.read_bytes())
            name = skill.relative_to(root).as_posix()
            if size > cap:
                failures.append(f"{name}: {size} bytes exceeds skill cap {cap}")
            elif size > cap - NEAR_CAP_BYTES:
                warnings.append(f"{name}: {size} bytes is within {NEAR_CAP_BYTES} B of cap {cap}")
    return failures, warnings


def _unique(paths: list[str]) -> list[str]:
    seen: set[str] = set()
    ordered: list[str] = []
    for path in paths:
        if path and path not in seen:
            seen.add(path)
            ordered.append(path)
    return ordered


def changed_overlay_paths(root: Path) -> list[str]:
    """Names changed versus origin/main plus the uncommitted worktree."""
    names: list[str] = []
    git = shutil.which("git")
    if git is None:
        return names
    for args in (
        ["diff", "--name-only", "origin/main...HEAD"],
        ["diff", "--name-only", "HEAD"],
        ["diff", "--name-only", "--cached"],
    ):
        try:
            completed = subprocess.run(  # nosec B603 - absolute git from shutil.which, fixed argv.
                [git, *args],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
        except OSError:
            continue
        if completed.returncode == 0:
            names.extend(line.strip() for line in completed.stdout.splitlines() if line.strip())
    return _unique(names)


def tip_preflight_failures(root: Path, paths: list[str]) -> list[str]:
    """Same-tip B607 + README inventory + Memory content_hash preflight (#6164/#6165/#6169)."""
    script = root / TIP_PREFLIGHT
    if not script.is_file():
        return []
    spec = importlib.util.spec_from_file_location("chaos_engine_tip_preflight", script)
    if spec is None or spec.loader is None:
        return ["tip preflight: checker unavailable"]
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return [f"tip preflight: {failure}" for failure in module.preflight_failures(root, paths)]


def overlay_pre_push_failures(root: Path, paths: list[str] | None = None) -> list[str]:
    """Return contract failures for one overlay diff. Empty means the push may proceed."""
    changed = list(paths) if paths is not None else changed_overlay_paths(root)
    failures: list[str] = tip_preflight_failures(root, changed)
    if not any(touches_overlay_contract(path) for path in changed):
        return failures
    playbook = root / PLAYBOOK
    if playbook.is_file():
        failures.extend(playbook_contract_failures(playbook.read_text(encoding="utf-8")))
    elif any(path.replace("\\", "/").endswith(PLAYBOOK) for path in changed):
        failures.append("dropped pinned clause: playbook missing")
    budget_path = root / "scripts/ci/agent_guidance_budget.json"
    if budget_path.is_file():
        from scripts.ci.validate_agent_guidance import load_budget, validate_file_budgets

        for item in validate_file_budgets(root, load_budget(budget_path)):
            failures.append(f"{item.get('path')}: {item.get('message')}")
    skill_failures, skill_warnings = skill_budget_findings(root)
    failures.extend(skill_failures)
    for warning in skill_warnings:
        print(f"overlay pre-push: warning: {warning}", file=sys.stderr)
    skill = root / "chaos-engine/skills/chaos-engine/SKILL.md"
    if skill.is_file():
        from scripts.ci.validate_agent_setup import validate_harness_reachability

        for item in validate_harness_reachability(root):
            failures.append(f"{item.get('path')}: {item.get('message')}")
    return failures


def main(argv: list[str] | None = None) -> int:
    """Exit 1 when the overlay pre-push contract fails."""
    root = Path(argv[1]).resolve() if argv and len(argv) > 1 else Path.cwd()
    failures = overlay_pre_push_failures(root)
    if not failures:
        print("overlay pre-push: pass")
        return 0
    for failure in failures:
        print(f"overlay pre-push: {failure}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
