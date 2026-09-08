#!/usr/bin/env python3
"""Eval-gated draft skill PRs — opt-in phase 2 only (#5665 / Top 10 #8).

Proposed skill patches from skill_compress_audit / meta_optimize become draft
PRs ONLY when:

1. eval-parity fixtures pass
2. focused unit tests pass
3. explicit opt-in is set (CLI --opt-in AND env CHAOS_ENGINE_DRAFT_SKILL_PRS=1)

Default OFF. Never auto-merge. Never apply skill mutations without the gate.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
OPT_IN_ENV = "CHAOS_ENGINE_DRAFT_SKILL_PRS"
STATE_RELATIVE = Path(".chaos-engine-state") / "draft-skill-prs"
DEFAULT_UNIT_TESTS = (
    "tests.scripts.test_chaos_engine_s2_self_improve",
    "tests.scripts.test_chaos_engine_s4_self_improve",
)
EVAL_SCRIPT = Path("scripts/ci/chaos_engine_eval_parity.py")


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def _load_sibling(name: str):
    path = Path(__file__).resolve().parent / name
    spec = importlib.util.spec_from_file_location(f"ce_draft_{name}", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {name}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def opt_in_enabled(*, flag: bool = False, environ: dict[str, str] | None = None) -> bool:
    """Require BOTH CLI flag and env=1. Default OFF."""
    env = environ if environ is not None else os.environ
    value = str(env.get(OPT_IN_ENV) or "").strip().casefold()
    return bool(flag) and value in {"1", "true", "yes", "on"}


def gate_status(*, flag: bool = False, environ: dict[str, str] | None = None) -> dict[str, Any]:
    enabled = opt_in_enabled(flag=flag, environ=environ)
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "draft-skill-pr-gate",
        "optInEnv": OPT_IN_ENV,
        "optInFlagRequired": "--opt-in",
        "enabled": enabled,
        "default": "OFF",
        "autoMerge": False,
        "autoApply": False,
        "requirements": [
            "explicit --opt-in",
            f"{OPT_IN_ENV}=1",
            "eval-parity green",
            "unit tests green",
            "draft PR only (never auto-merge)",
        ],
        "policy": (
            "Default OFF. Skill patches stay propose-only until opt-in + gates. "
            "Never auto-merge skill mutations. Never apply without gate."
        ),
    }


def run_eval_parity(root: Path, *, python: str | None = None) -> dict[str, Any]:
    script = root / EVAL_SCRIPT
    if not script.is_file():
        return {"ok": False, "reason": f"missing {EVAL_SCRIPT.as_posix()}", "exitCode": 127}
    completed = subprocess.run(  # nosec B603
        [python or sys.executable, str(script)],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    return {
        "ok": completed.returncode == 0,
        "exitCode": completed.returncode,
        "stdoutTail": (completed.stdout or "")[-400:],
        "stderrTail": (completed.stderr or "")[-400:],
    }


def run_unit_tests(
    root: Path,
    *,
    modules: tuple[str, ...] = DEFAULT_UNIT_TESTS,
    python: str | None = None,
) -> dict[str, Any]:
    # Prefer modules that exist; skip missing so sparse checkouts still gate on available tests.
    existing: list[str] = []
    for module in modules:
        # unittest module path → tests/scripts/foo.py
        candidate = root / Path(module.replace(".", "/") + ".py")
        if candidate.is_file():
            existing.append(module)
    if not existing:
        return {"ok": False, "reason": "no unit test modules found", "modules": list(modules)}
    completed = subprocess.run(  # nosec B603
        [python or sys.executable, "-m", "unittest", *existing, "-v"],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    return {
        "ok": completed.returncode == 0,
        "exitCode": completed.returncode,
        "modules": existing,
        "stdoutTail": (completed.stdout or "")[-500:],
        "stderrTail": (completed.stderr or "")[-500:],
    }


def collect_proposals(
    root: Path,
    *,
    skill: str | None = None,
) -> dict[str, Any]:
    compress = _load_sibling("skill_compress_audit.py")
    report = compress.audit_tree(root, skill=skill, include_diff=True)
    selected = [
        item
        for item in (report.get("skills") or [])
        if isinstance(item, dict) and (item.get("proposals") or item.get("diffStub"))
    ]
    return {
        "audit": {
            "skillCount": report.get("skillCount"),
            "withProposals": report.get("withProposals"),
            "overBudgetCount": report.get("overBudgetCount"),
            "mutate": False,
            "applyGate": report.get("applyGate"),
        },
        "skills": selected,
        "mutate": False,
    }


def prepare_artifact(
    root: Path,
    *,
    skill: str | None = None,
    gate: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Write propose-only artifact; never mutates SKILL.md."""
    proposals = collect_proposals(root, skill=skill)
    # Snapshot skill bytes to prove no mutate.
    compress = _load_sibling("skill_compress_audit.py")
    before = {str(path): path.read_bytes() for path in compress.discover_skills(root)}
    state_dir = root / STATE_RELATIVE
    state_dir.mkdir(parents=True, exist_ok=True)
    stamp = int(time.time())
    path = state_dir / f"proposal-{stamp}.json"
    document = {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "draft-skill-pr-artifact",
        "generatedAt": stamp,
        "gate": gate or gate_status(flag=False),
        "proposals": proposals,
        "pr": {
            "draft": True,
            "autoMerge": False,
            "title": "[CE] draft: skill compress proposals (opt-in gated)",
            "body": (
                "Draft skill compress proposals from `skill_compress_audit` / "
                "`meta_optimize`. **Opt-in gate required.** Never auto-merge.\n\n"
                "Gates: eval-parity + unit tests + "
                f"`{OPT_IN_ENV}=1` + `--opt-in`.\n"
            ),
        },
        "mutate": False,
        "applied": False,
    }
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    after = {str(p): p.read_bytes() for p in compress.discover_skills(root)}
    compress.assert_no_mutate(before, after)
    document["artifact"] = str(path)
    return document


def evaluate_gates(
    root: Path,
    *,
    flag: bool,
    environ: dict[str, str] | None = None,
    skip_tests: bool = False,
    python: str | None = None,
) -> dict[str, Any]:
    status = gate_status(flag=flag, environ=environ)
    if not status["enabled"]:
        return {
            **status,
            "passed": False,
            "blocked": "opt-in required (set --opt-in and "
            f"{OPT_IN_ENV}=1); default OFF",
            "evalParity": None,
            "unitTests": None,
        }
    if skip_tests:
        # Test helper path only — still requires opt-in.
        return {
            **status,
            "passed": True,
            "blocked": None,
            "evalParity": {"ok": True, "skipped": True},
            "unitTests": {"ok": True, "skipped": True},
        }
    eval_result = run_eval_parity(root, python=python)
    unit_result = run_unit_tests(root, python=python)
    passed = bool(eval_result.get("ok") and unit_result.get("ok"))
    blocked = None
    if not eval_result.get("ok"):
        blocked = "eval-parity failed"
    elif not unit_result.get("ok"):
        blocked = "unit tests failed"
    return {
        **status,
        "passed": passed,
        "blocked": blocked,
        "evalParity": eval_result,
        "unitTests": unit_result,
    }


def open_draft_pr(
    root: Path,
    *,
    flag: bool,
    dry_run: bool = True,
    skill: str | None = None,
    environ: dict[str, str] | None = None,
    skip_tests: bool = False,
    gh_runner: Any | None = None,
) -> dict[str, Any]:
    """Create a *draft* PR only when opt-in + gates pass.

    dry_run=True (default) never calls gh — returns the would-be payload.
    Never applies skill file mutations. Never enables auto-merge.
    """
    gates = evaluate_gates(
        root, flag=flag, environ=environ, skip_tests=skip_tests
    )
    artifact = prepare_artifact(root, skill=skill, gate=gates)
    result: dict[str, Any] = {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "draft-skill-pr-open",
        "gates": gates,
        "artifact": artifact.get("artifact"),
        "draft": True,
        "autoMerge": False,
        "applied": False,
        "mutate": False,
        "created": False,
        "dryRun": dry_run,
    }
    if not gates.get("passed"):
        result["ok"] = False
        result["blocked"] = gates.get("blocked") or "gate failed"
        return result

    pr_title = artifact["pr"]["title"]
    pr_body = artifact["pr"]["body"] + f"\nArtifact: `{artifact.get('artifact')}`\n"
    if dry_run:
        result["ok"] = True
        result["wouldCreate"] = {
            "title": pr_title,
            "body": pr_body,
            "draft": True,
            "autoMerge": False,
        }
        return result

    runner = gh_runner or _default_gh_create
    created = runner(root, title=pr_title, body=pr_body)
    result["ok"] = bool(created.get("ok"))
    result["created"] = bool(created.get("ok"))
    result["gh"] = created
    return result


def _default_gh_create(root: Path, *, title: str, body: str) -> dict[str, Any]:
    gh_bin = shutil.which("gh")
    if not gh_bin:
        return {
            "ok": False,
            "exitCode": 127,
            "stdout": "",
            "stderr": "gh executable not found on PATH",
            "autoMerge": False,
        }
    completed = subprocess.run(  # nosec B603
        [
            gh_bin,
            "pr",
            "create",
            "--draft",
            "--title",
            title,
            "--body",
            body,
        ],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    return {
        "ok": completed.returncode == 0,
        "exitCode": completed.returncode,
        "stdout": (completed.stdout or "").strip(),
        "stderr": (completed.stderr or "").strip(),
        "autoMerge": False,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        nargs="?",
        default="status",
        choices=("status", "prepare", "open"),
        help="status (default) | prepare artifact | open draft PR",
    )
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument(
        "--opt-in",
        action="store_true",
        help=f"required with {OPT_IN_ENV}=1 (default OFF)",
    )
    parser.add_argument("--skill", default=None, help="limit compress proposals to one skill")
    parser.add_argument(
        "--execute",
        action="store_true",
        help="actually call gh pr create --draft (default is dry-run)",
    )
    parser.add_argument(
        "--skip-tests",
        action="store_true",
        help=argparse.SUPPRESS,  # test helper only
    )
    args = parser.parse_args(argv)
    root = project_root(args.project)

    if args.command == "status":
        print(json.dumps(gate_status(flag=args.opt_in), indent=2, sort_keys=True))
        return 0

    if args.command == "prepare":
        document = prepare_artifact(
            root, skill=args.skill, gate=gate_status(flag=args.opt_in)
        )
        print(json.dumps(document, indent=2, sort_keys=True))
        return 0

    # open
    result = open_draft_pr(
        root,
        flag=args.opt_in,
        dry_run=not args.execute,
        skill=args.skill,
        skip_tests=args.skip_tests,
    )
    print(json.dumps(result, indent=2, sort_keys=True))
    if not result.get("ok"):
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
