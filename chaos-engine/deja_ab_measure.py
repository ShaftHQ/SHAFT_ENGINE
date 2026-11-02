#!/usr/bin/env python3
"""Record a deja A/B token table through session_token_usage (#6183).

Each run's token count is the UTF-8 size of that process's stdout and stderr
divided by four, at least one. Those counts are stored with ``record`` and
read back with ``summarize``. This is not a vendor invoice. The command does
not change ``defaultOn``.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from session_token_usage import ab_table, format_ab_table, record, summarize  # noqa: E402

TASKS = (
    ("overlay-pre-push", [sys.executable, "-m", "unittest", "tests.scripts.test_overlay_pre_push"]),
    ("deja-store", [sys.executable, "-m", "unittest", "tests.scripts.test_deja_store_6183"]),
    (
        "parent-rog-shell",
        [sys.executable, "chaos-engine/skills/local-agency/scripts/tests/test_assert_parent_rog_shell.py"],
    ),
)
RUNS = 5


def output_tokens(stdout: str, stderr: str) -> int:
    """Map captured process text to a positive token count."""
    size = len((stdout + stderr).encode("utf-8"))
    return max(1, size // 4)


def _run(command: list[str], cwd: Path) -> tuple[str, str]:
    completed = subprocess.run(
        command,
        cwd=cwd,
        check=False,
        capture_output=True,
        text=True,
    )
    return completed.stdout or "", completed.stderr or ""


def measure_run(root: Path, task: str, command: list[str], arm: str, run: int, project: Path) -> dict[str, object]:
    """Run one arm, record its output size, and return the summarized tokens."""
    stdout, stderr = "", ""
    if arm == "deja":
        extra_out, extra_err = _run(
            [sys.executable, "chaos-engine/retrieve.py", "--store", "deja", task],
            root,
        )
        stdout += extra_out
        stderr += extra_err
    task_out, task_err = _run(command, root)
    tokens = output_tokens(stdout + task_out, stderr + task_err)
    session_id = f"ab-{task}-{arm}-{run}"
    record(
        session_id,
        channel="local",
        prompt_tokens=tokens,
        completion_tokens=0,
        runtime_class="host-session",
        project=project,
    )
    summary = summarize(session_id, project=project)
    totals = summary["totals"]
    recorded = int(totals["localPromptTokens"]) if isinstance(totals, dict) else 0
    return {"task": task, "arm": arm, "run": run, "tokens": recorded}


def measure(root: Path, project: Path) -> list[dict[str, object]]:
    """Three tasks, five runs, control and deja."""
    rows: list[dict[str, object]] = []
    for task, command in TASKS:
        for run in range(1, RUNS + 1):
            for arm in ("control", "deja"):
                rows.append(measure_run(root, task, command, arm, run, project))
    return rows


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    project = root / ".chaos-engine-state" / "deja-ab"
    rows = measure(root, project)
    table = ab_table(rows)
    out = root / "chaos-engine" / "references" / "details" / "deja-token-ab-table.md"
    payload = root / "chaos-engine" / "references" / "details" / "deja-token-ab-rows.json"
    body = (
        "# Deja token A/B table\n\n"
        "Counts are UTF-8 bytes of each run's stdout and stderr, divided by four, "
        "recorded with `session_token_usage.py record` and read back with `summarize`. "
        "Not a vendor invoice. `defaultOn` changes only when both median and mean drop.\n\n"
        + format_ab_table(table)
    )
    out.write_text(body, encoding="utf-8")
    payload.write_text(json.dumps(rows, indent=2) + "\n", encoding="utf-8")
    sys.stdout.write(body)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
