#!/usr/bin/env python3
"""Fail-closed check: ROG local writers must run under parent Shell + machineId (#6051).

Grok Bot Task/executor Shell historically has no machineId parameter. Box
children see hostname like \"cursor\" and cannot reach ROG FreeToken/llama on
127.0.0.1. Until the platform binds machineId into Task, process-owner MUST:

  1. ListMachines → pick connected ROG id
  2. Shell / Read / AwaitShell with that machineId
  3. Never claim ROG/FreeToken delivery from a Task that only has box Shell

This script is for parent (or a child that somehow landed on ROG) to print a
JSON verdict. Exit 0 only when the current host is ROG-bound (or
CE_ALLOW_BOX_LOCAL_AGENCY=1). Exit 2 with HARD_BLOCKER when unbound.

Stdlib only. Never starts servers.
"""

from __future__ import annotations

import argparse
import json
import os
import socket
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))
from require_rog_freetoken import (  # noqa: E402
    ALLOW_ENV,
    binding_advice,
    is_rog_bound,
    require_rog_bound,
)


PLATFORM_GAP = (
    "HARD_BLOCKER: Task/executor Shell has no machineId. "
    "Parent must Shell with machineId on ROG (see #6051 / #6021). "
    "Do not dispatch ROG FreeToken/OpenCode implementers via Task until "
    "Grok Bot exposes machineId to Task children."
)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--expect-rog",
        action="store_true",
        help="Exit 2 with HARD_BLOCKER text when not ROG-bound (default behavior).",
    )
    parser.add_argument(
        "--json-only",
        action="store_true",
        help="Print JSON only (no HARD_BLOCKER line on stderr).",
    )
    args = parser.parse_args(argv)

    host = socket.gethostname()
    bound = is_rog_bound()
    payload = require_rog_bound()
    payload["platform_gap"] = (
        "Task Shell lacks machineId until Grok Bot platform change (#6051)"
    )
    payload["parent_required"] = not bound and not bool(os.environ.get(ALLOW_ENV))
    payload["issue"] = "#6051"
    if bound:
        payload["advice"] = (
            "Host is ROG-bound; OK for FreeToken/local-agency probes on this Shell. "
            "Task children still lack machineId — keep ROG writers on parent Shell (#6051)."
        )
    else:
        payload["advice"] = binding_advice(hostname=host)
    print(json.dumps(payload, indent=2, sort_keys=True))

    if bound:
        return 0

    if not args.json_only:
        print(PLATFORM_GAP, file=sys.stderr)
        print(binding_advice(hostname=host), file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
