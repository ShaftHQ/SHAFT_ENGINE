# Worker keep-alive

## Rule

Before ending a turn with a **non-empty** work queue and **no** running workers,
dispatch the next queued item (or leave a wake that does). Do not idle while
assigned board / queue items remain.

## Why

Sessions die at the host cap. A turn that stops after a merge while the next
item sits in Next wastes the window and leaves the board stale.

## Check

End-of-turn: queue empty **or** at least one worker / wake / CI listener is
active for the next item. Micro-retro line names the next dispatch.

## Follow-up

See [sequential WIP](sequential-wip.md), [bot entry](bot-entry.md) Keep going,
[durable jobs](durable-jobs.md).
