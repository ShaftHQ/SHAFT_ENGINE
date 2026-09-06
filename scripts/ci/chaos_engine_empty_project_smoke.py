#!/usr/bin/env python3
"""Empty-project smoke: install → doctor healthy under a stopwatch budget.

Reference profile (documented): empty directory, Ubuntu 22.04, Python 3.13,
no prior ChaosEngine install. Default budget is 300 seconds.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import shutil
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BUDGET_SECONDS = 300
SMOKE_PROFILE = "portable-empty-project"


def load_installer(source: Path):
    path = source / "install.py"
    spec = importlib.util.spec_from_file_location("chaos_engine_empty_smoke_install", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load installer: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def issue_cta(doctor_text: str) -> str:
    return (
        "Actionable next steps:\n"
        "  1. Re-run: python3 .chaos-engine/install.py doctor --project .\n"
        "  2. Follow each fix-next line in the doctor output.\n"
        "  3. If still blocked, open a GitHub issue and paste the doctor output "
        "(include every fix-next line).\n"
        f"Doctor output follows:\n{doctor_text}"
    )


def render_doctor(installer, project: Path) -> tuple[dict[str, object], str]:
    try:
        doctor = installer.doctor_with_dependencies(project, verify_clients=False)
    except (OSError, RuntimeError, ValueError) as error:
        doctor = {
            "status": "Blocked",
            "commit": "unknown",
            "components": {
                "core": {
                    "status": "recovery-required",
                    "taskImpact": "required",
                    "detail": str(error),
                }
            },
        }
    report = installer.format_health_report(
        {
            "kind": "doctor",
            "status": doctor.get("status"),
            "commit": doctor.get("commit"),
            "components": doctor.get("components") or {},
        }
    )
    return doctor, report


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT / "chaos-engine")
    parser.add_argument("--commit", default="0" * 40)
    parser.add_argument("--budget-seconds", type=int, default=BUDGET_SECONDS)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--skip-tools",
        action="store_true",
        help="Bounded CI fixture: install core+hosts without provisioning account tools.",
    )
    parser.add_argument("--keep-project", action="store_true")
    args = parser.parse_args(argv)
    source = args.source.resolve()
    installer = load_installer(source)
    temporary = tempfile.mkdtemp(prefix="ce-empty-smoke-")
    project = Path(temporary) / "empty-project"
    project.mkdir()
    started = time.monotonic()
    evidence: dict[str, object] = {
        "profile": SMOKE_PROFILE,
        "budgetSeconds": args.budget_seconds,
        "skipTools": bool(args.skip_tools),
        "platform": sys.platform,
        "status": "running",
    }
    doctor_text = ""
    try:
        if args.skip_tools:
            installer.install_with_dependencies(
                project,
                source,
                args.commit,
                provisioner=lambda *_a, **_k: None,
            )
        else:
            installer.install_with_dependencies(project, source, args.commit)
        doctor, doctor_text = render_doctor(installer, project)
        elapsed = time.monotonic() - started
        evidence["elapsedSeconds"] = round(elapsed, 3)
        evidence["doctorStatus"] = doctor.get("status")
        evidence["doctorReport"] = doctor_text
        core_ok = (project / ".chaos-engine/skills/chaos-engine/SKILL.md").is_file()
        evidence["corePresent"] = core_ok
        healthy = doctor.get("status") == "healthy"
        if args.skip_tools:
            if not core_ok:
                evidence["status"] = "core-missing"
                args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
                print(issue_cta(doctor_text), file=sys.stderr)
                return 1
            evidence["status"] = "core-fixture-passed"
            evidence["note"] = (
                "skip-tools fixture proves empty-dir install lands core under budget; "
                "full doctor-green stopwatch is the live acceptance fresh-account phase."
            )
        elif not healthy:
            evidence["status"] = "doctor-unhealthy"
            args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
            print(issue_cta(doctor_text), file=sys.stderr)
            return 1
        else:
            evidence["status"] = "passed"
        if elapsed > args.budget_seconds:
            evidence["status"] = "budget-exceeded"
            args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
            print(
                f"empty-project smoke exceeded {args.budget_seconds}s "
                f"(elapsed {elapsed:.1f}s)",
                file=sys.stderr,
            )
            print(issue_cta(doctor_text), file=sys.stderr)
            return 1
        args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
        print(
            f"empty-project smoke {evidence['status']} in {elapsed:.1f}s "
            f"(budget {args.budget_seconds}s)"
        )
        return 0
    except Exception as error:  # noqa: BLE001 - smoke must always write evidence
        evidence["status"] = "failed"
        evidence["error"] = f"{type(error).__name__}: {error}"
        evidence["elapsedSeconds"] = round(time.monotonic() - started, 3)
        args.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
        print(issue_cta(doctor_text or str(error)), file=sys.stderr)
        return 1
    finally:
        if not args.keep_project:
            shutil.rmtree(temporary, ignore_errors=True)


if __name__ == "__main__":
    raise SystemExit(main())
