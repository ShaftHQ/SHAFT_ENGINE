#!/usr/bin/env python3
"""
Live mobile-retro runtime wired into executor surfaces (#7494–7496, #7498–7499).

Call sites (must stay wired; AST tests enforce):
- ``scripts/ci/assemble_shard_blob.assemble`` — post-run green freeze (#7494)
- ``shaft-skills/.../rerun-failures.sh`` — rebuild-loop classify (#7495) + freeze
- ``scripts/ci/run_ubuntu_flutter_emulator_e2e.sh`` — device time-cap (#7496)
- ``scripts/ci/overlay_pre_push.evidence_pin_failures`` — fail-closed pin (#7498)
- ``scripts/ci/extract_allure_failures`` — NamedBlock reporter (#7499)
- ``chaos-engine/hooks/guard._mobile_retro_post_run`` — PostToolUse green tests
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT / "chaos-engine") not in sys.path:
    sys.path.insert(0, str(REPO_ROOT / "chaos-engine"))

import mobile_retro  # noqa: E402


def pin_path(root: Path) -> Path:
    return Path(root) / mobile_retro.PIN_REL


def snapshot_path(root: Path) -> Path:
    return Path(root) / mobile_retro.SNAPSHOT_REL


def _git(root: Path, *args: str) -> str:
    proc = subprocess.run(  # nosec B603 B607
        ["git", "-C", str(root), *args],
        check=False,
        capture_output=True,
        text=True,
    )
    return (proc.stdout or "").strip()


def current_build_and_branch(root: Path) -> tuple[str, str]:
    build_id = (
        os.environ.get("GITHUB_SHA")
        or os.environ.get("CHAOS_ENGINE_EVIDENCE_BUILD_ID")
        or _git(root, "rev-parse", "HEAD")
        or "unknown-build"
    )
    branch = (
        os.environ.get("GITHUB_REF_NAME")
        or os.environ.get("CHAOS_ENGINE_EVIDENCE_BRANCH")
        or _git(root, "rev-parse", "--abbrev-ref", "HEAD")
        or "unknown-branch"
    )
    return build_id, branch


def after_green_allure_run(
    root: Path,
    results_dir: Path,
    *,
    green: bool,
) -> Path | None:
    """Post-run freeze (#7494) + persist pin when green (#7498 persist half)."""
    if not mobile_retro.feature_present(root):
        return None
    results_dir = Path(results_dir)
    if not results_dir.is_dir():
        return None
    build_id, branch = current_build_and_branch(root)
    return mobile_retro.post_run_freeze(
        results_dir,
        snapshot_path(root),
        green=green,
        pin_path=pin_path(root) if green else None,
        build_id=build_id if green else None,
        branch=branch if green else None,
    )


def classify_before_rebuild(signature: str) -> str:
    """Rebuild-loop gate (#7495): Blocked → stop rebuilding."""
    return mobile_retro.rebuild_loop_classify(signature, rebuild=None)


def should_stop_rebuild(classification: str) -> bool:
    return mobile_retro.should_stop_rebuild(classification)


def run_device_with_fallback(
    mode: str,
    *,
    fallback: str = "fallback-ok",
    error_msg: str = "",
    cap_s: float | None = None,
    start_method: str | None = None,
) -> object:
    """Device primary time-cap (#7496); picklable module-level workers."""
    if cap_s is None:
        raw = os.environ.get("CHAOS_ENGINE_DEVICE_CAP_S", "120")
        try:
            cap_s = float(raw)
        except ValueError:
            cap_s = 120.0
    return mobile_retro.device_fallback_mode(
        mode,
        fallback=fallback,
        error_msg=error_msg,
        cap_s=float(cap_s),
        start_method=start_method,
    )


def evidence_pin_failures(root: Path, *, build_id: str | None = None, branch: str | None = None) -> list[str]:
    """
    Fail-closed pin enforce for overlay_pre_push (#7498).

    No pin file → []; mismatch → non-empty failure list (callers treat as block).
    """
    helper = Path(root) / mobile_retro.HELPER_REL
    if not helper.is_file():
        return []
    path = pin_path(root)
    if not path.is_file():
        return []
    bid, br = current_build_and_branch(root)
    if build_id is not None:
        bid = build_id
    if branch is not None:
        br = branch
    try:
        mobile_retro.require_evidence_pin(path, build_id=bid, branch=br)
    except mobile_retro.EvidencePinMismatch as exc:
        return [str(exc)]
    return []


def report_named_blocks_from_failures(
    failures: list[tuple[str, str, str]],
    *,
    report_path: Path | None = None,
) -> list[mobile_retro.NamedBlock]:
    """Reporter (#7499): collapse failures sharing a gap id."""
    return mobile_retro.live_report_named_blocks(failures, report_path=report_path)


def main(argv: list[str] | None = None) -> int:
    """Thin CLI used by shell live paths (rerun-failures / flutter e2e)."""
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_freeze = sub.add_parser("after-green-allure")
    p_freeze.add_argument("--root", type=Path, default=Path.cwd())
    p_freeze.add_argument("--results", type=Path, required=True)
    g = p_freeze.add_mutually_exclusive_group(required=True)
    g.add_argument("--green", action="store_true")
    g.add_argument("--red", action="store_true")

    p_cls = sub.add_parser("classify-before-rebuild")
    p_cls.add_argument("--signature", required=True)

    p_dev = sub.add_parser("device-fallback")
    p_dev.add_argument("--mode", choices=("ok", "error", "hang"), default="ok")
    p_dev.add_argument("--fallback", default="fallback-ok")
    p_dev.add_argument("--error-msg", default="")
    p_dev.add_argument("--cap-s", type=float, default=None)
    p_dev.add_argument("--start-method", default=None)

    p_pin = sub.add_parser("require-pin")
    p_pin.add_argument("--root", type=Path, default=Path.cwd())
    p_pin.add_argument("--build-id")
    p_pin.add_argument("--branch")

    args = parser.parse_args(argv)
    if args.cmd == "after-green-allure":
        kept = after_green_allure_run(args.root, args.results, green=bool(args.green))
        print(json.dumps({"snapshot": str(kept) if kept else None}))
        return 0 if (kept is not None or not args.green) else 1
    if args.cmd == "classify-before-rebuild":
        classification = classify_before_rebuild(args.signature)
        print(json.dumps({"classification": classification, "stop": should_stop_rebuild(classification)}))
        return 2 if should_stop_rebuild(classification) else 0
    if args.cmd == "device-fallback":
        result = run_device_with_fallback(
            args.mode,
            fallback=args.fallback,
            error_msg=args.error_msg,
            cap_s=args.cap_s,
            start_method=args.start_method,
        )
        print(json.dumps({"result": result}))
        return 0
    if args.cmd == "require-pin":
        failures = evidence_pin_failures(args.root, build_id=args.build_id, branch=args.branch)
        if failures:
            for item in failures:
                print(item, file=sys.stderr)
            return 1
        print("evidence_pin: pass")
        return 0
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
