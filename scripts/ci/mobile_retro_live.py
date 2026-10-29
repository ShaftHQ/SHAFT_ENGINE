#!/usr/bin/env python3
"""Live executor entry for mobile-retro harness paths (#7494–7496, #7498–7499).

Dispatches post-run freeze, rebuild-loop classify, device-fallback time-cap,
evidence-pin persist/enforce, and NamedBlock reporter. Keep this file tracked
alongside ``chaos-engine/mobile_retro.py``.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))
if str(REPO_ROOT / "chaos-engine") not in sys.path:
    sys.path.insert(0, str(REPO_ROOT / "chaos-engine"))

import mobile_retro  # noqa: E402


def _cmd_post_run_freeze(args: argparse.Namespace) -> int:
    kept = mobile_retro.post_run_freeze(
        Path(args.results),
        Path(args.snapshot),
        green=bool(args.green),
        pin_path=Path(args.pin) if args.pin else None,
        build_id=args.build_id,
        branch=args.branch,
    )
    if kept is None:
        print("post_run_freeze: no snapshot", file=sys.stderr)
        return 1
    print(f"post_run_freeze: {kept}")
    return 0


def _cmd_rebuild_classify(args: argparse.Namespace) -> int:
    rebuilt: list[bool] = []

    def _rebuild() -> None:
        rebuilt.append(True)

    classification = mobile_retro.rebuild_loop_classify(
        args.signature,
        rebuild=_rebuild if args.allow_rebuild else None,
    )
    print(json.dumps({"classification": classification, "rebuilt": bool(rebuilt)}))
    return 0 if not mobile_retro.should_stop_rebuild(classification) else 2


def _cmd_persist_pin(args: argparse.Namespace) -> int:
    path = mobile_retro.persist_evidence_pin(
        Path(args.pin),
        mobile_retro.EvidencePin(build_id=args.build_id, branch=args.branch),
    )
    print(f"persist_evidence_pin: {path}")
    return 0


def _cmd_enforce_pin(args: argparse.Namespace) -> int:
    ok = mobile_retro.enforce_evidence_pin(
        Path(args.pin), build_id=args.build_id, branch=args.branch
    )
    print(json.dumps({"accepted": ok}))
    return 0 if ok else 1


def _cmd_report_blocks(args: argparse.Namespace) -> int:
    raw = json.loads(Path(args.scenarios_json).read_text(encoding="utf-8"))
    scenarios = [(str(a), str(b), str(c)) for a, b, c in raw]
    blocks = mobile_retro.live_report_named_blocks(
        scenarios,
        report_path=Path(args.out) if args.out else None,
    )
    print(
        json.dumps(
            [
                {"gap_id": b.gap_id, "scenarios": list(b.scenarios), "reason": b.reason}
                for b in blocks
            ]
        )
    )
    return 0


def _cmd_device_fallback(args: argparse.Namespace) -> int:
    """Time-cap a primary marker string; on hang/error print fallback marker."""

    def primary() -> str:
        if args.primary_mode == "hang":
            import time

            time.sleep(60)
            return "primary-ok"
        if args.primary_mode == "error":
            raise RuntimeError(args.primary_error or "ClassNotFoundException")
        return "primary-ok"

    result = mobile_retro.device_fallback_run(
        primary,
        lambda: args.fallback,
        cap_s=float(args.cap_s),
    )
    print(json.dumps({"result": result}))
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    p_freeze = sub.add_parser("post-run-freeze", help="Freeze green allure/results snapshot (#7494)")
    p_freeze.add_argument("--results", required=True)
    p_freeze.add_argument("--snapshot", required=True)
    g = p_freeze.add_mutually_exclusive_group(required=True)
    g.add_argument("--green", action="store_true")
    g.add_argument("--red", action="store_true")
    p_freeze.add_argument("--pin")
    p_freeze.add_argument("--build-id")
    p_freeze.add_argument("--branch")
    p_freeze.set_defaults(func=_cmd_post_run_freeze)

    p_rebuild = sub.add_parser("rebuild-classify", help="Classify blocker in rebuild loop (#7495)")
    p_rebuild.add_argument("--signature", required=True)
    p_rebuild.add_argument("--allow-rebuild", action="store_true")
    p_rebuild.set_defaults(func=_cmd_rebuild_classify)

    p_dev = sub.add_parser("device-fallback", help="Time-cap primary then fallback (#7496)")
    p_dev.add_argument("--cap-s", type=float, default=5.0)
    p_dev.add_argument("--primary-mode", choices=("ok", "error", "hang"), default="ok")
    p_dev.add_argument("--primary-error", default="")
    p_dev.add_argument("--fallback", default="fallback-ok")
    p_dev.set_defaults(func=_cmd_device_fallback)

    p_persist = sub.add_parser("persist-pin", help="Persist evidence pin (#7498)")
    p_persist.add_argument("--pin", required=True)
    p_persist.add_argument("--build-id", required=True)
    p_persist.add_argument("--branch", required=True)
    p_persist.set_defaults(func=_cmd_persist_pin)

    p_enforce = sub.add_parser("enforce-pin", help="Enforce evidence pin (#7498)")
    p_enforce.add_argument("--pin", required=True)
    p_enforce.add_argument("--build-id", required=True)
    p_enforce.add_argument("--branch", required=True)
    p_enforce.set_defaults(func=_cmd_enforce_pin)

    p_report = sub.add_parser("report-blocks", help="NamedBlock reporter by gap id (#7499)")
    p_report.add_argument("--scenarios-json", required=True)
    p_report.add_argument("--out")
    p_report.set_defaults(func=_cmd_report_blocks)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.command == "post-run-freeze":
        # mutually exclusive --green/--red; store bool on args.green for the cmd
        args.green = bool(args.green)
    return int(args.func(args))


if __name__ == "__main__":
    raise SystemExit(main())
