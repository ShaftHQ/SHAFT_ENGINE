"""Record one force-push approval at the git toplevel of the current directory.

Chat text is not approval. The operation-boundary hook consumes the recorded
line once and then refuses again.
"""

from __future__ import annotations

import subprocess  # nosec B404 - fixed git rev-parse of the current directory.
import sys
from pathlib import Path


def main(arguments: list[str]) -> int:
    if arguments[:1] != ["--"] or len(arguments) < 2:
        print("usage: tool.py force-push-approve -- <command>", file=sys.stderr)
        return 2
    approved = " ".join(arguments[1:]).strip()
    top = subprocess.run(  # nosec B603 B607 - fixed git argv, no shell.
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
        check=False,
    )
    if top.returncode != 0 or not top.stdout.strip():
        print("force-push-approve: not a git repository", file=sys.stderr)
        return 2
    dest = Path(top.stdout.strip()) / ".chaos-engine-state" / "force-push-approval.txt"
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(approved + "\n", encoding="utf-8")
    print("force-push-approve: recorded one use")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
