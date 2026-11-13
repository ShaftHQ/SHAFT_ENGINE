# Checkpoint and resume after interruption

## Rule

1. Checkpoint every major step with `tool.py checkpoint TASK STEP --branch B --pr N --next "..."`.
2. An interrupted worker **cannot** be resumed in place. Start a **new** worker
   from the checkpoint file (`tool.py resume TASK`); do not replay chat history
   as source of truth.
3. When pausing a worker mid-item, write a checkpoint with finished plan, head
   SHA, paths, and exact next step before stopping.

## Why

Host session caps (~50 min wall; enforce 35) kill in-flight workers. Chat
context is lossy; a file checkpoint is the handoff.

## Unknown outcome

After a side-effecting tool whose completion is unknown (interrupted, no result,
or a crash mid-body), do not automatically retry the same effect. Record an
unknown outcome, not automatic retry. Prefer `tool.py checkpoint` before an
irreversible or externally visible tool. This overlay does not wrap a host
model stream and does not add a session event log.

A repeated identical tool call is advice only. The counter lives in
[repeat_reminder.py](../hooks/repeat_reminder.py). It does not deny the call.

Shell environment for the host's own command tool is not owned here.
Receipt sanitization already drops credential-shaped keys. Harness git and
gh keep token-shaped variables because those commands need them. No second
env scrub is added.

## Check

`tool.py resume TASK` prints `Resume at:` matching the last checkpoint.
Adversarial: kill mid-step → replacement worker continues from file without
re-deriving the plan from chat.

## Follow-up

[bot entry](bot-entry.md) Session cap; [task isolation](task-isolation.md).

## Generated assets

When a render, encode, or other generated media file lands, checkpoint or
commit that **explicit path** in the same turn (L54).
A box restart must not be the first thing that discovers the file is gone.
Never rely on `git commit -a` / `git add -A` for this — stage the asset paths
by name.

## Per-task budget override (L80)

`--kind test-run` → 90 min budget (default 35). Explicit `--minutes` wins.
