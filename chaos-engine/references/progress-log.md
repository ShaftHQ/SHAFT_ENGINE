# Progress log (every step)

## Rule

After every stage boundary (plan freeze, edit, test, push, review handoff,
SIGN, schedule), append one line to the task progress log **and** run
`tool.py checkpoint`. Use `chaos-engine/progress_log.py` or write
`checkpoints/<task>-progress.md` under the workspace root (or `$CHAOS_CHECKPOINTS_DIR`) (override with
`CHAOS_PROGRESS_LOG`). On resume, read the log before starting new work.

## Why

Session caps interrupt mid-task; chat-only state is lost. Durable step lines
let a parent re-dispatch from the last `--next`.

## Check

- Progress file grows after each step; contains stamp + step + optional next.
- Checkpoint `--budget` still governs wrap-up.

## Follow-up

[checkpoint-resume](checkpoint-resume.md), [bot-entry](bot-entry.md).
