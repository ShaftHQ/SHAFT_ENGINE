#!/usr/bin/env python3
"""
SkillSpector static gate for ChaosEngine skills (#skillspector / CE SkillSpector gate).

Scans first-party skill directories with a pinned NVIDIA SkillSpector build
(`--no-llm`) and the checked-in baseline. Fails when any skill's risk score is
greater than 50 or any unsuppressed finding has severity HIGH or CRITICAL.

Local overlay pre-push calls this when skill paths change. If the scanner is
not installed locally, the hook warns and continues; CI must set
``CHAOS_SKILLSPECTOR_REQUIRED=1`` so a missing scanner fails the job.

Baseline refresh: install the pin in PIN.json, triage findings, then
``skillspector baseline <skill-dir> --no-llm`` and merge reasons into
``.skillspector-baseline.yaml``. Prefer
``scripts/ci/skillspector/semantic_baseline.py`` so suppressions key by
``rule_id:file`` semantic id (hash updates in place; reason kept).
Never add broad AST4 globs.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess  # nosec B404 - fixed argv list only
import sys
from pathlib import Path
from typing import Any

REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

PIN_PATH = Path("scripts/ci/skillspector/PIN.json")
BASELINE_PATH = Path("scripts/ci/skillspector/.skillspector-baseline.yaml")
FIRST_PARTY_GLOBS = (
    "chaos-engine/skills/*/SKILL.md",
    "chaos-engine/addons/*/SKILL.md",
    "chaos-engine/plugin-adapters/*/SKILL.md",
)
BLOCK_SEVERITIES = frozenset({"HIGH", "CRITICAL"})
MAX_SCORE = 50


def load_pin(root: Path) -> dict[str, Any]:
    """Return the pinned SkillSpector identity."""
    raw = json.loads((root / PIN_PATH).read_text(encoding="utf-8"))
    if not isinstance(raw, dict) or "commit" not in raw:
        raise ValueError(f"invalid pin at {PIN_PATH}")
    return raw


def skill_dirs_from_paths(paths: list[str]) -> list[Path]:
    """Map changed paths to unique skill directories that contain SKILL.md."""
    found: list[Path] = []
    seen: set[Path] = set()
    for raw in paths:
        normalized = raw.replace("\\", "/").removeprefix("./")
        if not normalized.startswith("chaos-engine/"):
            continue
        parts = Path(normalized).parts
        # chaos-engine/skills/<name>/...
        # chaos-engine/addons/<name>/...
        # chaos-engine/plugin-adapters/<name>/...
        if len(parts) < 3:
            continue
        if parts[1] not in {"skills", "addons", "plugin-adapters"}:
            continue
        skill_dir = Path(*parts[:3])
        if skill_dir in seen:
            continue
        # Existence is checked later against the repo root; keep path-shaped hits.
        seen.add(skill_dir)
        found.append(skill_dir)
    return found


def all_first_party_skill_dirs(root: Path) -> list[Path]:
    """Return every first-party skill directory under chaos-engine."""
    dirs: list[Path] = []
    for pattern in FIRST_PARTY_GLOBS:
        for skill_md in sorted(root.glob(pattern)):
            dirs.append(skill_md.parent.relative_to(root))
    return dirs


def resolve_skillspector(root: Path) -> str | None:
    """Locate a skillspector executable or return None."""
    override = os.environ.get("CHAOS_SKILLSPECTOR_BIN", "").strip()
    if override:
        return override
    which = shutil.which("skillspector")
    if which:
        return which
    # Optional local cache from a previous CI/bootstrap install.
    cached = root / ".chaos-engine-state" / "skillspector-tool" / "bin" / "skillspector"
    if cached.is_file():
        return str(cached)
    return None


def run_scan(
    *,
    binary: str,
    skill_dir: Path,
    baseline: Path,
    out_json: Path,
) -> dict[str, Any]:
    """Run one static SkillSpector scan and return the JSON report."""
    out_json.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        binary,
        "scan",
        str(skill_dir),
        "--no-llm",
        "--baseline",
        str(baseline),
        "--format",
        "json",
        "-o",
        str(out_json),
    ]
    # Scrub provider/secret env for the child.
    env = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME") or str(Path.home()),
        "LANG": os.environ.get("LANG", "C.UTF-8"),
        "TERM": "dumb",
    }
    proc = subprocess.run(  # nosec B603 - fixed binary + argv, no shell
        cmd,
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )
    if not out_json.is_file():
        raise RuntimeError(
            f"skillspector produced no report for {skill_dir} (exit {proc.returncode}): "
            f"{proc.stderr[-500:]}"
        )
    return json.loads(out_json.read_text(encoding="utf-8"))


def evaluate_report(report: dict[str, Any], *, max_score: int = MAX_SCORE) -> list[str]:
    """Return gate failures for one skill report (empty means pass)."""
    failures: list[str] = []
    assessment = report.get("risk_assessment") or {}
    score = assessment.get("score")
    name = (report.get("skill") or {}).get("name") or report.get("name") or "skill"
    if not isinstance(score, int):
        failures.append(f"{name}: missing risk_assessment.score")
        return failures
    if score > max_score:
        failures.append(f"{name}: risk score {score} > {max_score}")
    for issue in report.get("issues") or []:
        severity = str(issue.get("severity") or "").upper()
        if severity in BLOCK_SEVERITIES:
            rule = issue.get("id") or issue.get("rule_id") or "?"
            loc = issue.get("location") or {}
            failures.append(
                f"{name}: unsuppressed {severity} {rule} at "
                f"{loc.get('file')}:{loc.get('start_line')}"
            )
    return failures


def _purge_pycache(skill_dir: Path) -> None:
    """Drop untracked bytecode so SkillSpector does not score __pycache__ as SC8/SC9."""
    for path in skill_dir.rglob("__pycache__"):
        if path.is_dir():
            shutil.rmtree(path, ignore_errors=True)



def gate_failures(
    root: Path,
    *,
    paths: list[str] | None = None,
    required: bool | None = None,
    binary: str | None = None,
) -> list[str]:
    """Run the SkillSpector gate; return human-readable failures."""
    pin_file = root / PIN_PATH
    baseline = root / BASELINE_PATH
    if not pin_file.is_file() or not baseline.is_file():
        return [f"skillspector gate: missing {PIN_PATH} or {BASELINE_PATH}"]

    if paths is None:
        skill_dirs = all_first_party_skill_dirs(root)
    else:
        skill_dirs = skill_dirs_from_paths(paths)
        if not skill_dirs:
            return []

    must = (
        bool(int(os.environ.get("CHAOS_SKILLSPECTOR_REQUIRED", "0")))
        if required is None
        else required
    )
    exe = binary or resolve_skillspector(root)
    if exe is None:
        message = (
            "skillspector gate: scanner not installed "
            "(set CHAOS_SKILLSPECTOR_BIN or install pinned NVIDIA/SkillSpector)"
        )
        return [message] if must else []

    load_pin(root)  # validate pin exists/shape; install is CI's job
    failures: list[str] = []
    out_root = root / ".chaos-engine-state" / "skillspector-reports"
    for rel in skill_dirs:
        skill_path = root / rel
        if not (skill_path / "SKILL.md").is_file():
            continue
        _purge_pycache(skill_path)
        report = run_scan(
            binary=exe,
            skill_dir=skill_path,
            baseline=baseline,
            out_json=out_root / f"{rel.as_posix().replace('/', '__')}.json",
        )
        failures.extend(evaluate_report(report))
    return failures


def main(argv: list[str] | None = None) -> int:
    """CLI entry: scan changed or all first-party skills."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument(
        "--all",
        action="store_true",
        help="Scan every first-party skill (default when --paths-file is omitted).",
    )
    parser.add_argument(
        "--paths-file",
        type=Path,
        help="Newline-separated changed paths; only matching skills are scanned.",
    )
    parser.add_argument(
        "--required",
        action="store_true",
        help="Fail when the skillspector binary is missing (CI).",
    )
    parser.add_argument("--bin", dest="binary", default=None)
    args = parser.parse_args(argv)
    root = args.root.resolve()
    paths: list[str] | None
    if args.paths_file is not None:
        paths = [
            line.strip()
            for line in args.paths_file.read_text(encoding="utf-8").splitlines()
            if line.strip()
        ]
    else:
        paths = None  # scan all first-party skills
    failures = gate_failures(
        root,
        paths=paths,
        required=True if args.required else None,
        binary=args.binary,
    )
    if not failures:
        print("skillspector gate: pass")
        return 0
    for item in failures:
        print(f"skillspector gate: {item}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
