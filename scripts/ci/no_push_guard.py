#!/usr/bin/env python3
"""Tracked NO_PUSH / WIP push guard (#7497).

Keep this file tracked. WIP pushes that relied on an untracked assert-push
script broke push-once; the pre-push contract fails closed when this path is
missing from ``git ls-files``.
"""

from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))


def main(argv: list[str] | None = None) -> int:
    root = Path(argv[1]).resolve() if argv and len(argv) > 1 else Path.cwd()
    sys.path.insert(0, str(root / "chaos-engine"))
    import hrsd_j11  # noqa: WPS433

    failures = hrsd_j11.no_push_guard_tracked(root)
    if failures:
        for item in failures:
            print(f"no_push_guard: {item}", file=sys.stderr)
        return 1
    print("no_push_guard: pass")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
