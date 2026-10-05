# Permanent rules (single home)

Lasting harness rules live in the portable overlay, never only in one
assistant's memory or routine ([harness parity](host-parity-matrix.md)).
Assistant memory keeps only this pointer: "Follow the ChaosEngine core card
(`.chaos-engine/skills/chaos-engine/SKILL.md`); permanent rules are in
`.chaos-engine/references/permanent-rules.md`."

| Rule | CE home |
| --- | --- |
| One portable implementation for every host; a new rule is one parity row | [host-parity-matrix](host-parity-matrix.md) |
| Learning Session only on trigger (failure, surprise, owner ask); otherwise one report line | [router contract](router-contract.md) |
| No traffic proxy of any kind (Headroom-style included); nothing installs one by default | [no-proxy](no-proxy.md) |
| Token optimization is Kanban eliminate-waste | [eliminate-waste](eliminate-waste.md) |
| Retrieve before read, once per task area; harness files are exempt | [retrieve-first](retrieve-first.md) |
| Prefer `gh` for GitHub; CLI over MCP; no GitHub MCP in defaults | [prefer-cli-over-mcp](prefer-cli-over-mcp.md) |
| Static-analysis `ACTION_REQUIRED` is a hard blocker, never "pending" | [static-analysis gate](static-analysis-gate.md) |
| One watch per PR, bounded polls, digest only, silence when unchanged | [CI status economy](ci-status-economy.md) |
| Wait for CI by event wake, never an LLM poll: end the turn while checks run; the only exception to single thread is a zero-token waiter | [CI status economy](ci-status-economy.md#wait-by-event-wake-never-by-llm-poll) |
| Learned memory carries origin + trust; untrusted stays quarantined until verified | [memory provenance](memory-provenance.md) |
| Evolving playbook items keep helpful/harmful counters and take delta updates | [evolving playbook](evolving-playbook.md) |
| Insight bank extracts from success/failure pairs with ExpeL votes and failure distill | [insight extract](insight-extract.md) |
| Repeated successful step windows induce a workflow; skill publish stays behind the eval suite | [workflow induction](workflow-induction.md) |
| Reflections are kept only from tests, CI, or doctor, never from self-report | [external reflection](external-reflection.md) |
| Prompt and skill candidates are accepted only after the harness eval suite passes them | [prompt evolution](prompt-evolution.md) |
| A harness body is replaced only after the eval suite passes, and the previous body stays archived | [self-modify](self-modify.md) |
| Context over the character budget is rot; compaction keeps marked text and must shrink under the budget | [context rot](context-rot.md) |
| Agent runs emit OpenTelemetry GenAI invoke_agent and execute_tool spans behind the eval suite | [genai spans](genai-spans.md) |
| Delta-only status; full report only on a RAG change or owner request | [process owner](process-owner-scrum-master.md) |
| Cost and avoided-spend lines only when a local channel was actually used | [process owner](process-owner-scrum-master.md) |
| Cloud implementers write code; local runtimes optional for narrow jobs | [identity](../identity.md), [local-runtimes](../skills/local-runtimes/SKILL.md) |
| Work-machine writes are parent-run one-shot jobs | [parent shell](../skills/local-agency/references/parent-rog-shell.md) |
| Task isolation, fresh primary, cleanup scopes | [task isolation](task-isolation.md), [cleanup scopes](cleanup-scopes.md) |
| Delegates load the delegate card, not the router | [delegate card](delegate-card.md) |
| Repository changes are not done until pushed, a pull request is open, and that pull request is merged to the default branch | [delivery phase gates](delivery-phase-gates.md) |
| Owner autonomy: act within the delegated scope; ask the owner for opinions, never for approval | [kanban](../skills/kanban/SKILL.md) |
| Single thread by default; delegate only when the owner enables delegation | [delegation](delegation.md) |
| Fewest pull requests: group to-deliver issues | [kanban](../skills/kanban/SKILL.md) |
| After each delivery run `tool.py maintain` (fast-forward keeping edits, reinstall, doctor, refresh, reload) | [kanban](../skills/kanban/SKILL.md) |
| Status reports are tables | [core card](../skills/chaos-engine/SKILL.md) |
| Each pull request carries exactly one release-note label, merges by merge commit via auto-merge; static analysis blocks | [static-analysis gate](static-analysis-gate.md) |
| Work in the agent's own sandbox when cost is equal; the owner workstation only runs the final install and doctor | [task isolation](task-isolation.md) |
| Short messages; no duplicate verification; one cheap call per status check | [CI status economy](ci-status-economy.md) |
| Retrieve and load companion cards at task start; store runs stop only on a stall, never a fixed timeout | [retrieve-first](retrieve-first.md) |
| Non-TTY shells: always pass a path to `rg` (it otherwise waits on stdin) | [eliminate-waste](eliminate-waste.md) |

Stale memory corrected by this sweep: "Headroom installs by default" is
false; the no-proxy rule forbids it.
