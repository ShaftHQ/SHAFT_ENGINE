#!/usr/bin/env python3
"""Fail when a workflow test step can stay green on test failures (issue #6870).

shaft-engine's Surefire profile sets testFailureIgnore=true, and E2E steps use continue-on-error so reports
still publish. Either way the job only goes red if a later step asserts the results. This guard requires one.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
_MAVEN_TEST = re.compile(r"\bmvnw?\b[^\n]*\s(test|verify)\b")
_GRADLE_TEST = re.compile(r"\bgradlew?\b[^\n]*\stest\b")
_ASSERTION = re.compile(
    r"post-test-report|testng-results\.xml|surefire-reports|failsafe-reports|TEST-\*\.xml|summary\.json"
)
# Steps that are deliberately non-gating; each entry is "workflow:job:step name".
ALLOWED = frozenset({
    "live-tools-nightly.yml:intellij-tools-authoring:Run authoring externally-networked tests (non-gating)",
})


def ignores_failures(root: Path, run: str) -> bool:
    """Whether a Maven test command reaches a module whose Surefire config sets testFailureIgnore."""
    selected = re.search(r"-pl\s+(\S+)", run)
    modules = selected.group(1).split(",") if selected else ["."]
    for module in modules:
        pom = root / module.strip("'\"") / "pom.xml"
        if pom.is_file() and "<testFailureIgnore>true</testFailureIgnore>" in pom.read_text(encoding="utf-8"):
            return True
    return False


def _step_text(step: dict) -> str:
    return " ".join(str(step.get(key, "")) for key in ("uses", "run", "with"))


def job_findings(root: Path, workflow: str, job_name: str, steps: list) -> list[str]:
    """Findings for one job's steps."""
    findings = []
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            continue
        run = str(step.get("run") or "")
        runs_tests = bool(_MAVEN_TEST.search(run) or _GRADLE_TEST.search(run)) and "skipTests" not in run
        if not runs_tests:
            continue
        masked = step.get("continue-on-error") is True or (_MAVEN_TEST.search(run) and ignores_failures(root, run))
        if not masked or f"{workflow}:{job_name}:{step.get('name')}" in ALLOWED:
            continue
        later = " ".join(_step_text(item) for item in steps[index + 1:] if isinstance(item, dict))
        if not _ASSERTION.search(later):
            findings.append(f"{workflow}: job {job_name}, step {step.get('name')!r} can pass with failing tests; "
                            "add a later result assertion (e.g. ./.github/actions/post-test-report)")
    return findings


def validate(root: Path = ROOT) -> list[str]:
    """Return every masked test step across .github/workflows."""
    findings = []
    for path in sorted((root / ".github/workflows").glob("*.yml")):
        document = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
        for job_name, job in (document.get("jobs") or {}).items():
            if isinstance(job, dict):
                findings.extend(job_findings(root, path.name, job_name, job.get("steps") or []))
    return findings


def main() -> int:
    """Exit 1 when any workflow test step can stay green on failures."""
    findings = validate()
    for finding in findings:
        print(f"::error::{finding}")
    if not findings:
        print("Workflow test gating OK: every masked test step has a later result assertion.")
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
