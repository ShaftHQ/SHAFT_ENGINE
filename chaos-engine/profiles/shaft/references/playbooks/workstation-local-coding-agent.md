# Workstation local coding agent

Use the already-installed workstation loop when the work is cheap, bounded,
and already specified. The default capability's cost and latency are waste
for those slices. The most-intelligent or default model stays the decider
and may hand the slice to this loop.

## When to use it

- Read-only `shaft-architect` with the cited-path gate.
- Mechanical script-string pins the loop can apply without inventing scope.
- A fully specified edit the loop can run, with an allowlist and a test
  command, where default-capability time is waste.

## When not to use it

- Product changes under `src/main/java`.
- PR merge, GitHub delivery, or review.
- Harness architecture, routing, or guidance design.
- Anything the loop cannot run: missing allowlist, missing spec, work that
  needs a public-API judgment, or a host that is not this workstation.

## Commands

Keep one model loaded. Loopback only. No startup apps, tray icons, or cloud
endpoints.

- `scripts/local-coding-agent/shaft-java-agent.ps1` — bounded edit loop.
- `scripts/local-coding-agent/shaft-architect.ps1` — read-only architect;
  cited-path gate fail-closes invented slashy paths.
- `scripts/local-coding-agent/shaft-local-ai-stop.ps1` — stop the loopback
  runtime.
- `scripts/agents/knowledge_stores.py` — `status` or `search` from any
  worktree. `refresh` refuses.

## Output

Return the command that ran, the report path, cited-path or allowlist
failures, and whether the decider must take the work back.
