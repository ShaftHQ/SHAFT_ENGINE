# Worker keep-alive (KEEP-ALIVE)

## Rule

**KEEP-ALIVE:** a bot never ends a turn idle while it still has its **own**
queued board rows.

1. When a row finishes or is handed off, pull the next **own** row in the
   **same turn** (dispatch a worker, or leave a wake / CI listener that
   resumes the queue).
2. **Sequential mode** (one active task per agent) still applies: while the
   current row is waiting on review, CI, or another bot, pull the next
   **independent** own row rather than idling.
3. Do not idle because "CI is running" if another own row is ready and does
   not conflict with the waiting one.
4. Role-neutral: this gate applies to every bot; board ownership decides
   which rows are "own".

## Why

Sessions die at the host cap. A turn that stops after a merge or handoff
while the next own item sits in Next wastes the window and leaves the board
stale. Waiting on a reviewer is not a reason to leave the rest of the own
queue untouched.

## Check

End-of-turn: own queue empty **or** at least one worker / wake / CI listener
is active for the next own item. Micro-retro line names the next dispatch.
Automated: process-card keyword test in
`tests/scripts/test_ce_memory_abstraction_phase1.py` (`KEEP-ALIVE`,
`same turn`, `independent`).

## Follow-up

See [sequential WIP](sequential-wip.md), [bot entry](bot-entry.md) Keep going,
[durable jobs](durable-jobs.md), [cleanup at close](cleanup-at-close.md).
