#!/usr/bin/env python3
"""Task ledger: resume agent work after a host session cap (#6854).

Some hosts (Grok Bot: about 50 minutes) hard-stop a background session on
wall-clock time. Keepalive pings do not reset such a cap, so the work must be
resumable instead: bounded work units, a short durable ledger line per step,
and a resume brief the next session (or the parent) reads instead of the
transcript. The harness enforces the stop: every in-flight step checkpoint
also checks the session budget (default 35 min), warns in the last 5 minutes,
and exits 3 once it is spent, after recording the step (#7194). A gap longer
than the host cap starts a new session, so a stale ledger never blocks.

    tool.py checkpoint TASK STEP [--branch B] [--pr N] [--next TEXT] [--start]  exit 3 = stop now
    tool.py checkpoint TASK --budget [N | --minutes N] exit 3 = wrap up now
    tool.py resume [TASK]                             brief; exit 6 = absent
    tool.py checkpoint TASK --item ID --group G --status S [--note TEXT]

Delivery ledger (#6980): when a user asks for several tasks, issues, or PRs,
record every item with its target PR group and status before implementing,
show the table, and update the item at each milestone (todo, in-progress,
PR #N, merged, blocked: reason). ``resume`` prints the table, so a resumed
session or the parent sees what is left without the transcript.

Resume at (#7293): the brief's first line is exactly one ``Resume at: <next>``;
a step without ``--next`` warns and keeps the last one.

State: ``$CHAOS_ENGINE_TASKS_DIR`` or ``~/.local/state/chaos-engine/tasks``,
one ``TASK.jsonl`` per task, outside any worktree so it survives cleanup.
Stdlib only.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from pathlib import Path

NAME_RE = re.compile(r"^[A-Za-z0-9_.-]{1,96}$")
DEFAULT_BUDGET_MINUTES = 35
BUDGET_WARN_MINUTES = 5
HOST_CAP_MINUTES = 50
# L80: per-task kind overrides (explicit --minutes/--budget still wins).
KIND_BUDGET_MINUTES = {
    "test-run": 90.0,
    "testrun": 90.0,
    "test_run": 90.0,
}

# Evidence that the task is a real test run (not a budget-bypass label).
TEST_RUN_CMD_RE = re.compile(
    r"(?i)(?:^|[;&|\s])("
    r"mvn(?:\s+\S+)*\s+test\b|"
    r"gradlew?(?:\s+\S+)*\s+test\b|"
    r"pytest\b|"
    r"python(?:3)?\s+-m\s+(?:pytest|unittest)\b|"
    r"npm(?:\s+run)?\s+test\b|"
    r"cargo\s+test\b|"
    r"go\s+test\b"
    r")"
)
TEST_RUN_ABUSE_CLASSES = frozenset({
    "cleanup", "pr", "design", "docs", "review", "chore", "release",
})
MISSING_NEXT = "(missing: rerun checkpoint with --next \"<exact next step>\")"
STOP_NOW = "stop: push WIP, checkpoint --next, end turn"
OWNER_FINISH_CONTINUE = (
    "owner finish-delivery goal active; checkpoint budget warning only (#7424)"
)
OWNER_FINISH_ENV = "CHAOS_ENGINE_OWNER_FINISH_GOAL"
OWNER_FINISH_STATE = "owner-finish-goal.json"
DONE = "done"


def tasks_root() -> Path:
    override = os.environ.get("CHAOS_ENGINE_TASKS_DIR")
    if override:
        return Path(override)
    base = os.environ.get("XDG_STATE_HOME") or str(Path.home() / ".local" / "state")
    return Path(base) / "chaos-engine" / "tasks"


def ledger(name: str) -> Path:
    if not NAME_RE.match(name):
        raise ValueError(f"invalid task name: {name!r}")
    return tasks_root() / f"{name}.jsonl"


def entries(name: str) -> list[dict]:
    path = ledger(name)
    if not path.is_file():
        return []
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        try:
            rows.append(json.loads(line))
        except json.JSONDecodeError:
            continue
    return rows


def record(name: str, step: str, *, branch: str = "", pr: str = "", next_action: str = "",
           start: bool = False, now: float | None = None, spec: str = "", kind: str = "") -> dict:
    row = {"t": round(now if now is not None else time.time()), "step": step}
    for key, value in (("branch", branch), ("pr", pr), ("next", next_action), ("spec", spec), ("kind", kind)):
        if value:
            row[key] = value
    if start:
        row["start"] = True
    path = ledger(name)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(row, separators=(",", ":")) + "\n")
    return row


ITEM_STATUSES = ("todo", "in-progress", "pr", "merged", "blocked", "skipped")


def record_item(name: str, item: str, *, group: str = "", status: str = "todo", note: str = "",
                now: float | None = None) -> dict:
    head = status.split(" ", 1)[0].split(":", 1)[0].casefold()
    if head not in ITEM_STATUSES:
        raise ValueError(f"status must start with one of: {', '.join(ITEM_STATUSES)}")
    if head == "blocked" and not note and ":" not in status:
        raise ValueError("blocked needs a reason: --status 'blocked: why' or --note")
    row = {"t": round(now if now is not None else time.time()), "item": item, "status": status}
    if group:
        row["group"] = group
    if note:
        row["note"] = note
    path = ledger(name)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(row, separators=(",", ":")) + "\n")
    return row


def items(name: str) -> dict[str, dict]:
    """Latest row per delivery item, in first-recorded order."""
    latest: dict[str, dict] = {}
    for row in entries(name):
        if "item" in row:
            merged = {**latest.get(row["item"], {}), **row}
            latest[row["item"]] = merged
    return latest


def items_table(name: str) -> str:
    rows = items(name)
    if not rows:
        return ""
    lines = ["| Item | Group | Status |", "|---|---|---|"]
    for item, row in rows.items():
        status = row["status"] + (f" ({row['note']})" if row.get("note") else "")
        lines.append(f"| {item} | {row.get('group', '')} | {status} |")
    return "\n".join(lines)


def session_start(rows: list[dict]) -> float | None:
    """Latest `--start` row, or the first row after a gap longer than the host cap (a dead session)."""
    rows = [row for row in rows if "t" in row]
    for index in range(len(rows) - 1, -1, -1):
        row = rows[index]
        gap = index > 0 and row["t"] - rows[index - 1]["t"] > HOST_CAP_MINUTES * 60
        if row.get("start") or gap:
            return float(row["t"])
    return float(rows[0]["t"]) if rows else None



def kind_budget_minutes(kind: str | None) -> float | None:
    """L80: return override minutes for a task kind, or None for default."""
    if not kind:
        return None
    return KIND_BUDGET_MINUTES.get(str(kind).strip().casefold())


def test_run_cmd_matches(cmd: str | None) -> bool:
    """True when *cmd* looks like a test-runner invocation."""
    if not cmd:
        return False
    return TEST_RUN_CMD_RE.search(str(cmd)) is not None


def test_run_proof_ok(
    *,
    task_cmd: str | None = None,
    command: str | None = None,
    cmd: str | None = None,
    ledger_has_test_cmd: bool = False,
    test_run_proof: bool = False,
    suite_id: str | None = None,
) -> bool:
    """L80: 90-minute test-run budget requires recorded test-runner proof."""
    if test_run_cmd_matches(task_cmd) or test_run_cmd_matches(command) or test_run_cmd_matches(cmd):
        return True
    if ledger_has_test_cmd and (test_run_proof or test_run_cmd_matches(task_cmd)):
        return True
    if suite_id and str(suite_id).strip() and test_run_cmd_matches(task_cmd or command or cmd or ""):
        return True
    # Explicit proof flag alone is insufficient without a matching cmd.
    return False


def evaluate_test_run_kind(obs: dict | None) -> dict:
    """Fail-closed policy for kind=test-run budget claims."""
    if obs is None or not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"], "error": "obs required"}
    hits: list[str] = []
    kind = str(obs.get("kind") or "").strip().casefold()
    if kind not in KIND_BUDGET_MINUTES:
        return {"ok": True, "hits": [], "error": None}
    task_class = str(obs.get("task_class") or obs.get("class") or "").strip().casefold()
    if task_class in TEST_RUN_ABUSE_CLASSES:
        hits.append("test-run-kind-abuse")
    if not test_run_proof_ok(
        task_cmd=obs.get("task_cmd") or obs.get("command") or obs.get("cmd"),
        ledger_has_test_cmd=bool(obs.get("ledger_has_test_cmd")),
        test_run_proof=bool(obs.get("test_run_proof")),
        suite_id=obs.get("suite_id"),
    ):
        hits.append("test-run-missing-proof")
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L80 test-run budget gate failed",
    }


def resolve_budget_minutes(
    *,
    kind: str | None = None,
    minutes: float | None = None,
    explicit: bool = False,
    task_cmd: str | None = None,
    command: str | None = None,
    cmd: str | None = None,
    ledger_has_test_cmd: bool = False,
    test_run_proof: bool = False,
    suite_id: str | None = None,
    task_class: str | None = None,
) -> float:
    """Pick session budget: explicit minutes win; else kind override; else 35.

    *explicit* is True when the caller passed ``--minutes`` / ``--budget`` with
    a non-default value, or any ``--budget N``.

    L80: ``kind=test-run`` only yields 90 minutes when test-runner proof is
    present (task cmd matches mvn/gradle/pytest/… or ledger-recorded proof).
    Abuse classes (cleanup/pr/design/…) always keep the default 35.
    """
    if explicit and minutes is not None:
        return float(minutes)
    override = kind_budget_minutes(kind)
    if override is not None:
        abuse = str(task_class or "").strip().casefold() in TEST_RUN_ABUSE_CLASSES
        proof = test_run_proof_ok(
            task_cmd=task_cmd,
            command=command,
            cmd=cmd,
            ledger_has_test_cmd=ledger_has_test_cmd,
            test_run_proof=test_run_proof,
            suite_id=suite_id,
        )
        if abuse or not proof:
            return float(DEFAULT_BUDGET_MINUTES)
        return float(override)
    if minutes is not None:
        return float(minutes)
    return float(DEFAULT_BUDGET_MINUTES)


def ledger_kind(name: str) -> str | None:
    """Most recent ``kind`` recorded on the task ledger, if any."""
    for row in reversed(entries(name)):
        kind = row.get("kind")
        if kind:
            return str(kind)
    return None


def over_budget(name: str, minutes: float, now: float | None = None) -> tuple[bool, float]:
    started = session_start(entries(name))
    if started is None:
        return False, 0.0
    elapsed = max(0.0, ((now if now is not None else time.time()) - started) / 60)
    return elapsed >= minutes, elapsed


def brief(name: str) -> str | None:
    rows = entries(name)
    if not rows:
        return None
    table = items_table(name)
    rows = [row for row in rows if "step" in row]
    if not rows:
        return table or None
    merged: dict = {}
    for row in rows:
        merged.update({k: v for k, v in row.items() if k not in {"start"}})
    sessions = sum(1 for row in rows if row.get("start")) or 1
    when = time.strftime("%Y-%m-%d %H:%M", time.localtime(merged["t"]))
    parts = [f"{name}: last step {merged['step']!r} at {when}", f"session {sessions}"]
    parts += [f"{key} {merged[key]}" for key in ("branch", "pr") if key in merged]
    text = "; ".join(parts)
    if merged["step"] == DONE:
        text += "\nstatus: done; nothing to resume"
        return text + ("\n" + table if table else "")
    # #7293: exactly one exact next step, first, so a resumed session never rereads history.
    resume_at = merged.get("next") or MISSING_NEXT
    return f"Resume at: {resume_at}\n{text}" + ("\n" + table if table else "")


SPEC_REFUSAL = (
    "{task}: no spec; write one before coding (problem, evidence/root cause, retrieved prior art, "
    "plan steps, riskiest assumption + the test that proves it, files touched, out of scope) and pass "
    "`--spec PATH` once, or save it as {default}"
)


def spec_dir() -> Path:
    override = os.environ.get("CHAOS_ENGINE_SPEC_DIR")
    return Path(override) if override else tasks_root() / "specs"


def find_spec(name: str, given: str = "") -> Path | None:
    """The task's spec: --spec, else the last recorded one, else spec-<task>.md in spec_dir()."""
    recorded = [row["spec"] for row in entries(name) if row.get("spec")] if ledger(name).is_file() else []
    for candidate in (given, *reversed(recorded), str(spec_dir() / f"spec-{name}.md")):
        if candidate and Path(candidate).is_file() and Path(candidate).stat().st_size > 0:
            return Path(candidate).resolve()
    return None


def open_tasks() -> list[str]:
    root = tasks_root()
    if not root.is_dir():
        return []
    names = []
    for path in sorted(root.glob("*.jsonl")):
        rows = [row for row in entries(path.stem) if "step" in row]
        if rows and rows[-1].get("step") != DONE:
            names.append(path.stem)
    return names


def checkpoint_step(ns: argparse.Namespace) -> int:
    """Record a step; an in-flight (non-done) step needs a spec on disk first (exit 4)."""
    spec = find_spec(ns.task, ns.spec) if ns.step != DONE else None
    if ns.step != DONE and spec is None:
        print(SPEC_REFUSAL.format(task=ns.task, default=spec_dir() / f"spec-{ns.task}.md"), file=sys.stderr)
        return 4
    kind = getattr(ns, "kind", "") or ""
    record(
        ns.task,
        ns.step,
        branch=ns.branch,
        pr=ns.pr,
        next_action=ns.next_action,
        start=ns.start,
        spec=str(spec) if ns.spec and spec else "",
        kind=kind,
    )
    if ns.step != DONE and not ns.next_action:
        print(
            f"{ns.task}: no --next; 'Resume at' keeps the last one. "
            'Pass --next "<exact next step>"',
            file=sys.stderr,
        )
    if ns.step == DONE:
        return 0
    # L80: kind=test-run → 90 min unless --minutes/--budget set explicitly.
    explicit = False
    minutes_arg = float(ns.minutes)
    if ns.budget is not None and ns.budget > 0:
        explicit = True
        minutes_arg = float(ns.budget)
    elif abs(minutes_arg - float(DEFAULT_BUDGET_MINUTES)) > 1e-9:
        explicit = True
    minutes = resolve_budget_minutes(
        kind=kind or ledger_kind(ns.task),
        minutes=minutes_arg,
        explicit=explicit,
    )
    return session_gate(ns.task, minutes)


def write_owner_finish_goal(
    project: Path | None = None,
    *,
    active: bool = True,
    source: str = "/goal",
) -> Path:
    """Persist ``.chaos-engine-state/owner-finish-goal.json`` for #7424.

    Called when the owner starts a ``/goal`` finish-delivery session so the
    35-minute checkpoint stop becomes a warning instead of exit 3.
    """
    root = Path(project) if project is not None else Path.cwd()
    path = root / ".chaos-engine-state" / OWNER_FINISH_STATE
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"active": bool(active), "source": str(source or "/goal")}
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")
    return path


def owner_finish_goal_active(project: Path | None = None) -> bool:
    """True when the owner explicitly asked to finish a named delivery (#7424).

    Marker: env ``CHAOS_ENGINE_OWNER_FINISH_GOAL`` in {1,true,yes}, or
    ``.chaos-engine-state/owner-finish-goal.json`` with ``{"active": true}``.
    Ordinary unattended sessions without that marker still stop at the budget.
    """
    raw = str(__import__("os").environ.get(OWNER_FINISH_ENV, "")).strip().casefold()
    if raw in {"1", "true", "yes", "on"}:
        return True
    root = Path(project) if project is not None else Path.cwd()
    path = root / ".chaos-engine-state" / OWNER_FINISH_STATE
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return False
    return isinstance(payload, dict) and payload.get("active") is True


def session_gate(name: str, minutes: float, now: float | None = None) -> int:
    """Hard stop after the step is recorded: exit 3 past the budget, warn in its last minutes.

    #7424: an explicit owner finish-delivery goal keeps exit 0 past the budget
    (warning only). Sessions without that goal still get exit 3.
    """
    spent, elapsed = over_budget(name, minutes, now)
    if spent:
        if owner_finish_goal_active():
            print(
                f"{name}: {elapsed:.0f}/{minutes:.0f} min; {OWNER_FINISH_CONTINUE}",
                file=sys.stderr,
            )
            return 0
        print(f"{name}: {elapsed:.0f}/{minutes:.0f} min; {STOP_NOW}", file=sys.stderr)
        return 3
    if elapsed >= minutes - BUDGET_WARN_MINUTES:
        print(f"{name}: {elapsed:.0f}/{minutes:.0f} min; finish this step, then stop", file=sys.stderr)
    return 0


def main(argv: list[str] | None = None, command: str = "checkpoint") -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    try:
        if command == "resume":
            if not args:
                names = open_tasks()
                print("\n".join(brief(n) or n for n in names) if names else "no open tasks")
                return 0
            text = brief(args[0])
            if text is None:
                print(f"{args[0]}: no ledger", file=sys.stderr)
                return 6
            print(text)
            return 0
        parser = argparse.ArgumentParser(prog="tool.py checkpoint")
        parser.add_argument("task")
        parser.add_argument("step", nargs="?")
        parser.add_argument("--branch", default="")
        parser.add_argument("--pr", default="")
        parser.add_argument("--next", dest="next_action", default="")
        parser.add_argument("--start", action="store_true", help="mark a new session start")
        parser.add_argument("--spec", default="", help="the task's spec file; required before a non-done step")
        # `--budget 40` and `--budget --minutes 40` both work (#6990).
        parser.add_argument(
            "--budget", nargs="?", const=0.0, default=None, type=float, metavar="MINUTES",
            help="exit 3 once the session budget (MINUTES, default --minutes) is spent",
        )
        parser.add_argument("--minutes", type=float, default=DEFAULT_BUDGET_MINUTES)
        parser.add_argument("--kind", default="", help="task kind (L80: test-run → 90 min budget)")
        parser.add_argument("--item", help="delivery-ledger item (issue/PR/task id)")
        parser.add_argument("--group", default="", help="target PR group for --item")
        parser.add_argument("--status", default="todo", help="todo|in-progress|pr #N|merged|blocked: why")
        parser.add_argument("--note", default="")
        ns = parser.parse_args(args)
        if ns.item:
            record_item(ns.task, ns.item, group=ns.group, status=ns.status, note=ns.note)
            print(items_table(ns.task))
            return 0
        if ns.budget is not None:
            if ns.budget > 0:
                ns.minutes = ns.budget
                explicit = True
            else:
                explicit = False
            kind = getattr(ns, "kind", "") or ledger_kind(ns.task)
            ns.minutes = resolve_budget_minutes(
                kind=kind, minutes=float(ns.minutes), explicit=explicit and ns.budget > 0,
            )
            spent, elapsed = over_budget(ns.task, ns.minutes)
            if spent and owner_finish_goal_active():
                print(
                    f"{ns.task}: {elapsed:.0f}/{ns.minutes:.0f} min; {OWNER_FINISH_CONTINUE}"
                )
                return 0
            verdict = STOP_NOW if spent else "ok"
            print(f"{ns.task}: {elapsed:.0f}/{ns.minutes:.0f} min; {verdict}")
            return 3 if spent else 0
        if not ns.step:
            parser.error("STEP is required unless --budget is given")
        return checkpoint_step(ns)
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
