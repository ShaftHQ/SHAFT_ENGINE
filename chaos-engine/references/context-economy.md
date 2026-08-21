# Context economy

Load this when a task will make many tool calls, return large outputs, or run
long enough that context rot becomes the next failure mode. Host compaction is
the host's job. This page teaches the agent how to stay under it.

Patterns here reimplement published progressive-disclosure, just-in-time
retrieval, compaction, tool-result pruning, and spill ideas. Named sources
and licenses live in [THIRD_PARTY_NOTICES](../THIRD_PARTY_NOTICES.md) and
[RESEARCH](../RESEARCH.md).

## Smallest high-signal set

Every token competes for attention. Gather only the evidence that can change
the next decision. Stop exploring when that decision is supported.

- Bound every read (`offset`/`limit`, `head_limit`, ripgrep context).
- After truncation, narrow once. Do not repeat the same broad query.
- Do not reread an unchanged input.
- Prefer a path plus a discriminating excerpt over a full dump.

## Spill large tool output

When a tool result is larger than the decision needs:

1. Keep the head and tail that prove the outcome.
2. Write or leave the full artifact on disk.
3. Tell the next step the path and the one fact the result established.

Do not paste multi-thousand-line logs, HAR files, or report HTML into the
transcript unless the user asked for the raw body.

## Distill, then continue

- A subagent returns a distillate: what changed, what was proved, what remains.
  It does not return its transcript.
- Structured notes belong at session end or after repeated failure, through the
  existing learning session. They are not a running diary.
- On failure, change the premise or the discriminating observation. Do not
  repeat the same action with different wording.

## What this page is not

It is not a second agent loop, session log, or compaction engine. It does not
authorize skipping safety warnings, irreversible-action confirmations, or an
observed check.
