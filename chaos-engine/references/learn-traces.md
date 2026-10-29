# Learn traces

Portable map-reduce-verify contract for session traces. Host TUI workflow
runners may implement this; other hosts use isolated subagents and files. Do
not reconstruct a vendor workflow from memory. Do not copy host-only script
or pager mechanics here.

## Phases

1. **Collect** — keep sessions where a human sat; drop synthetic, subagent,
   and (unless asked) headless runs. Redact credentials. Write a manifest with
   counts and drop reasons.
2. **Map** — one agent per batch of sessions. Extract repeated phrases, stale
   skill lines, unused loaded surfaces, and gaps. Cite session ids.
3. **Reduce** — fold map notes until one synthesis remains. Open candidate
   skill files as context; invent nothing.
4. **Verify** — three independent skeptics: phrases, stale lines, deletes.
   Keep or drop; add nothing. Fail closed on missing evidence.
5. **Report** — overview, action tables, coverage line, and a machine-readable
   action list. Git-tracked edits become patches or a PR, not home-dir copies.

A first-ever run is step/curate, never auto-delete. See
[harness-learn](harness-learn.md).
