# OmniRoute process lifecycle

Long implementer launches must not take down the orchestrator shell.

## Start

- Prefer `nohup` (or an equivalent detached spawn) with a **distinct worktree**
  and a recorded child PID/process group.
- Keep launcher argv unique enough that a later cleanup can target **only** that
  child, never the parent session.

## Stop

- Kill by the **recorded** child PID or process group only.
- Do **not** use broad process-name patterns such as `pkill -f` matching the
  OmniRoute or implementer launcher argv — those patterns often match the
  dispatching shell and abort the session mid-flight.

## Related

- Productive dispatch (live catalog, CLI target matrix, preflight, retry
  budgets): `scripts/runner.py` helpers and the OmniRoute skill checklists.
- Learning: #5770.
