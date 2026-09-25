# Permanent rules (single home)

Lasting harness rules live in the portable overlay, never only in one
assistant's memory or routine (#6180, [harness parity](host-parity-matrix.md)).
Assistant memory keeps only this pointer: "Follow the ChaosEngine core card
(`.chaos-engine/skills/chaos-engine/SKILL.md`); permanent rules are in
`.chaos-engine/references/permanent-rules.md`."

| Rule | CE home |
| --- | --- |
| One portable implementation for every host; a new rule is one parity row | [host-parity-matrix](host-parity-matrix.md) |
| Learning Session after every delivery; unchanged overlay is not a skip | [router contract](router-contract.md) |
| No traffic proxy of any kind (Headroom-style included); nothing installs one by default | [no-proxy](no-proxy.md) |
| Token optimization is Kanban eliminate-waste | [eliminate-waste](eliminate-waste.md) |
| Retrieve before read, once per task area; harness files are exempt | [retrieve-first](retrieve-first.md) |
| Prefer `gh` for GitHub; CLI over MCP; no GitHub MCP in defaults | [prefer-cli-over-mcp](prefer-cli-over-mcp.md) |
| Codacy `ACTION_REQUIRED` is a hard blocker, never "pending" | [codacy gate](codacy-action-required-gate.md) |
| One watch per PR, bounded polls, digest only, silence when unchanged | [CI status economy](ci-status-economy.md) |
| Delta-only status; full report only on a RAG change or owner request | [process owner](process-owner-scrum-master.md) |
| Cost and avoided-spend lines only when a local channel was actually used | [process owner](process-owner-scrum-master.md) |
| Cloud implementers write code; local runtimes optional for narrow jobs | [identity](../identity.md), [local-runtimes](../skills/local-runtimes/SKILL.md) |
| Work-machine writes are parent-run one-shot jobs | [parent shell](../skills/local-agency/references/parent-rog-shell.md) |
| Task isolation, fresh primary, cleanup scopes | [task isolation](task-isolation.md), [cleanup scopes](cleanup-scopes.md) |
| Delegates load the delegate card, not the router | [delegate card](delegate-card.md) |

Stale memory corrected by this sweep: "Headroom installs by default" is
false; the no-proxy rule forbids it.
