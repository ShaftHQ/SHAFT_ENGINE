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

## Check

`tool.py resume TASK` prints `Resume at:` matching the last checkpoint.
Adversarial: kill mid-step → replacement worker continues from file without
re-deriving the plan from chat.

## Follow-up

[bot entry](bot-entry.md) Session cap; [task isolation](task-isolation.md).
