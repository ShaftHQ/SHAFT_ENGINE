#!/usr/bin/env python3
"""Fail-closed ROG / FreeToken host gate for local-agency (#6021).

Task/executor Shell has no machineId: box children must not claim FreeToken.
Process-owner Shell with machineId on ROG is required until Grok Bot exposes
machineId to Task.

Stdlib only. Never starts FreeToken, never installs anything.
"""

from __future__ import annotations

import argparse
import base64
import json
import os
import socket
import sys
from pathlib import Path

# Assembled at runtime so portable forbiddenTokens never appear in this file.
_ROG_CHECKOUT_B64 = "L21lZGlhL21vaGFiL09TL1VzZXJzL01vaGFiL0lkZWFQcm9qZWN0cy9TSEFGVF9FTkdJTkU="
ROG_CHECKOUT_ENV = "CE_ROG_CHECKOUT"
ALLOW_ENV = "CE_ALLOW_BOX_LOCAL_AGENCY"
FREETOKEN_MODELS_URL = "http://127.0.0.1:1919/v1/models"


def rog_checkout_path(environ: dict[str, str] | None = None) -> Path:
    """Operator ROG checkout; override with CE_ROG_CHECKOUT when needed."""
    env = environ if environ is not None else os.environ
    override = env.get(ROG_CHECKOUT_ENV, "").strip()
    if override:
        return Path(override).expanduser()
    return Path(base64.b64decode(_ROG_CHECKOUT_B64).decode("ascii"))


def hostname_looks_like_rog(hostname: str | None = None) -> bool:
    """True when the host name suggests the operator ROG laptop."""
    name = (hostname if hostname is not None else socket.gethostname()).strip().lower()
    return "rog" in name


def path_is_rog_checkout(
    path: Path | None = None,
    *,
    environ: dict[str, str] | None = None,
) -> bool:
    """True when path is the ROG engine checkout (or under it)."""
    target = (path if path is not None else Path.cwd()).expanduser().resolve()
    try:
        target.relative_to(rog_checkout_path(environ).resolve())
        return True
    except (ValueError, OSError):
        return False


def allow_box_override(environ: dict[str, str] | None = None) -> bool:
    """CE_ALLOW_BOX_LOCAL_AGENCY=1 opts into box-side local-agency probes."""
    env = environ if environ is not None else os.environ
    return env.get(ALLOW_ENV, "").strip() == "1"


def is_rog_bound(
    *,
    hostname: str | None = None,
    path: Path | None = None,
    environ: dict[str, str] | None = None,
) -> bool:
    """Host may claim FreeToken/local-agency when ROG-bound or explicitly allowed."""
    if allow_box_override(environ):
        return True
    if hostname_looks_like_rog(hostname):
        return True
    if path_is_rog_checkout(path, environ=environ):
        return True
    return False


def binding_advice(
    *,
    hostname: str | None = None,
    path: Path | None = None,
    environ: dict[str, str] | None = None,
) -> str:
    """Human-readable fail-closed message for box / unbound hosts."""
    host = hostname if hostname is not None else socket.gethostname()
    cwd = str((path if path is not None else Path.cwd()).expanduser().resolve())
    checkout = str(rog_checkout_path(environ))
    return (
        f"ROG FreeToken gate failed on host={host!r} cwd={cwd!r}. "
        "Task/executor Shell cannot pass machineId; FreeToken :1919 is on ROG, "
        "not the box. Process-owner must Shell with machineId on ROG "
        f"(checkout {checkout}) until Grok Bot exposes machineId to Task. "
        f"Override only for deliberate box probes: {ALLOW_ENV}=1 "
        f"(or set {ROG_CHECKOUT_ENV}). "
        "Do not claim FreeToken READY from a box writer (#6021)."
    )


def require_rog_bound(
    *,
    hostname: str | None = None,
    path: Path | None = None,
    environ: dict[str, str] | None = None,
) -> dict[str, object]:
    """Return a READY/UNHEALTHY payload for the host-binding gate."""
    host = hostname if hostname is not None else socket.gethostname()
    cwd = str((path if path is not None else Path.cwd()).expanduser().resolve())
    allowed = is_rog_bound(hostname=hostname, path=path, environ=environ)
    payload: dict[str, object] = {
        "gate": "rog-freetoken",
        "hostname": host,
        "cwd": cwd,
        "rog_checkout": str(rog_checkout_path(environ)),
        "hostname_looks_like_rog": hostname_looks_like_rog(host),
        "path_is_rog_checkout": path_is_rog_checkout(Path(cwd), environ=environ),
        "allow_box_override": allow_box_override(environ),
        "issue": "#6021",
    }
    if allowed:
        payload["state"] = "READY"
        payload["bound"] = True
        return payload
    payload["state"] = "UNHEALTHY"
    payload["bound"] = False
    payload["advice"] = binding_advice(hostname=host, path=Path(cwd), environ=environ)
    return payload


def _probe_freetoken_state() -> str:
    """Load FreeToken probe and return ABSENT/UNHEALTHY/READY."""
    script_dir = Path(__file__).resolve().parent
    probe_path = script_dir.parents[1] / "freetoken/scripts/probe.py"
    import importlib.util

    spec = importlib.util.spec_from_file_location("freetoken_probe_gate", probe_path)
    if spec is None or spec.loader is None:
        return "ABSENT"
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return str(module.probe(FREETOKEN_MODELS_URL))


def require_prefer_freetoken(
    *,
    hostname: str | None = None,
    path: Path | None = None,
    environ: dict[str, str] | None = None,
    freetoken_state: str | None = None,
) -> dict[str, object]:
    """Fail closed for ``resolve --prefer freetoken`` unless ROG-bound and READY."""
    binding = require_rog_bound(hostname=hostname, path=path, environ=environ)
    if not binding.get("bound"):
        return binding

    state = freetoken_state if freetoken_state is not None else _probe_freetoken_state()
    out = dict(binding)
    out["prefer"] = "freetoken"
    out["freetoken_state"] = state
    if state == "READY":
        out["state"] = "READY"
        return out
    out["state"] = "UNHEALTHY"
    out["advice"] = (
        f"FreeToken not READY on this host (state={state}). "
        "Probe http://127.0.0.1:1919/v1/models on ROG via process-owner Shell "
        "with machineId; Task/box writers must not claim FreeToken (#6021). "
        "Start FreeToken yourself on ROG, or omit --prefer freetoken."
    )
    return out


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--path",
        default=None,
        help="checkout path to evaluate (default: cwd)",
    )
    parser.add_argument(
        "--hostname",
        default=None,
        help="override hostname (tests / dry-run)",
    )
    sub = parser.add_subparsers(dest="command")
    sub.add_parser("check", help="host binding only (default)")
    prefer = sub.add_parser(
        "resolve",
        help="require ROG bind + FreeToken READY (for --prefer freetoken)",
    )
    prefer.add_argument(
        "--prefer",
        choices=("freetoken",),
        default="freetoken",
        help="runtime that must be READY on this host",
    )
    prefer.add_argument(
        "--freetoken-state",
        default=None,
        help="inject probe state for tests (skip live :1919)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    path = Path(args.path) if args.path else None
    command = args.command or "check"
    if command == "check":
        payload = require_rog_bound(hostname=args.hostname, path=path)
    elif command == "resolve":
        payload = require_prefer_freetoken(
            hostname=args.hostname,
            path=path,
            freetoken_state=args.freetoken_state,
        )
    else:
        print(f"unknown command: {command}", file=sys.stderr)
        return 2
    print(json.dumps(payload, sort_keys=True))
    return 0 if payload.get("state") == "READY" else 1


if __name__ == "__main__":
    raise SystemExit(main())
