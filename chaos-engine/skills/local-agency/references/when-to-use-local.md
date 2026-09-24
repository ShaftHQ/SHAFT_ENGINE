# When to use a local writer (#6171)

Session and cloud implementers write code directly. A local model writer
through [local-agency](../SKILL.md) is **optional**: a tool the implementer may
pick, never a mandate for implementation and never a reason to stop writing code
on the host. The same rule applies on Codex, Claude, Grok CLI, Gemini, Copilot,
and Grok Bot (no host-only memory exception).

## Decision rule

Use local only when the **handoff cost is well below generation cost**.

- Handoff cost: everything the implementer pays to delegate. That includes spec
  and context packing, verifying each artifact, grounded feedback, and restarts
  after free-form failures. Every one of those steps re-reads context in the
  implementer's own window.
- Generation cost: the tokens the implementer would otherwise write itself.

A local writer replaces just the writing step. When checking its output costs
about as much as writing the code, write it directly.

## Suitable jobs

- bulk mechanical edits (rename, reformat, apply one pattern across many files)
- spec or ticket drafting that a human or the implementer edits afterwards
- log summarization against a fixed fingerprint table
- offline or private work that must not leave the machine

Poor fits: multi-file design, RED-first contract authoring, and small fixes
where a single direct edit is cheaper than the brief.

When local is chosen, run it under the [coach loop](coach-loop.md).

## Token accounting

Compare costs using real counts, not estimates. For a `llamacpp` runtime server,
sum the per-request `print_timing` lines in its service journal:

```text
journalctl --user -u <local-server-unit> --since <start> | grep print_timing
```

Sum the `prompt eval` token counts (prompt tokens) and the `eval` token counts
(generated tokens) per day. Price the same volume at the host model's published
rates. That total is the most a local writer can save, and you set it against the
implementer's handoff cost. Prompt-cache reuse is not counted in `prompt eval`,
so note it as a caveat.
