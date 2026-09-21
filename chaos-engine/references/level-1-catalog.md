# Level-1 progressive-disclosure catalog

Secondary ChaosEngine surfaces reachable from the core
[`chaos-engine` skill](../skills/chaos-engine/SKILL.md). Deterministic and
sorted. Each row is a name, ≤2-line Use-when, and a path — no workflow dumps.

| Name | Use when | Path |
| --- | --- | --- |
| Zero-LLM catalog | Prefer doctor/repair/script paths before opening chat discovery | [`zero-llm-catalog.md`](zero-llm-catalog.md) |
| Heal route | Install drifted, wiped runtime, missing core, or unhealthy doctor fix-next | [`heal-route.md`](heal-route.md) |
| Token budget modes | Choose or override ultra-lean / balanced / deep context spend | [`token-budget-modes.md`](token-budget-modes.md) |
| Host parity / GAP-EXIT2 | Host deny fidelity differs (Grok/Copilot); compensating UX checklist | [`host-parity-matrix.md`](host-parity-matrix.md) |
| Script-first | Multi-hop mechanical transforms belong in a script, not a tool chain | [`script-first.md`](script-first.md) |
| CE brief | Locator-only system brief for local-agency design turns | [`../ce_brief.py`](../ce_brief.py) |
| Dispatch CE brief | `dispatch.py brief` / `--with-ce-brief` / chat system brief | [`../skills/local-agency/scripts/dispatch.py`](../skills/local-agency/scripts/dispatch.py) |
| Coach loop | Host coach cadence for local openai-compat writers | [`../skills/local-agency/references/coach-loop.md`](../skills/local-agency/references/coach-loop.md) |
| Design-turn contract | Verbatim CE_BRIEF_LOCATORS + host reject gates for local design/spec turns | [`../skills/local-agency/references/design-turn-contract.md`](../skills/local-agency/references/design-turn-contract.md) |
| CE brief eval fixtures | Unit/eval contracts for locator-only brief + byte caps | [`../evals/ce-brief-unit-fixtures.json`](../evals/ce-brief-unit-fixtures.json) |
| Silent verify | Check/Stop green must add zero context; fail with one line | [`silent-verify.md`](silent-verify.md) |
| Significance capture | Mid-session friction marks only; Learning Session drain | [`significance-capture.md`](significance-capture.md) |
| Skill compress audit | Propose-only SKILL.md bloat audit; never auto-apply | [`skill-compress-audit.md`](skill-compress-audit.md) |
| Meta-optimize | Periodic offline review of shared logs; not continuous | [`meta-optimize.md`](meta-optimize.md) |
| Draft skill PR gate | Opt-in eval-gated draft skill PRs; default OFF | [`draft-skill-pr.md`](draft-skill-pr.md) |
| Context firewall | Research / multi-file explore → isolated subagent; filepath:line distillate only | [`context-firewall.md`](context-firewall.md) |
| Harness learn | Tune git-tracked harness from traces; never user-home skill copies | [`harness-learn.md`](harness-learn.md) |
| ICM Architect | Design/restructure ICM workspaces (advisory companion) | [`icm-architect.md`](icm-architect.md) |
| Design loop | Write-review-revise a design doc until reviewer reports 0 open issues | [`design-loop.md`](design-loop.md) |
| Deep research | Plan, parallel research, independent verify, cited report | [`deep-research.md`](deep-research.md) |
| Learn traces | Portable /learn map-reduce-verify; host TUI optional | [`learn-traces.md`](learn-traces.md) |
| Prefer CLI over MCP | CLI when both exist; `gh` when configured; no default GitHub MCP | [`prefer-cli-over-mcp.md`](prefer-cli-over-mcp.md) |
| Context economy | Bound reads/searches; prefer path+excerpt over dumps | [`context-economy.md`](context-economy.md) |
| Retrieve-first | A store can shorten discovery; one bounded attempt | [`retrieve-first.md`](retrieve-first.md) |
| Research receipt | Before implementation mutation; triage scales depth | [`research-receipt.md`](research-receipt.md) |
| Delivery phase gates | Phase ledger / research-before-mutation enforcement | [`delivery-phase-gates.md`](delivery-phase-gates.md) |
| Codacy Complexity gate | Classifier / interaction PRs; Complexity ACTION_REQUIRED == unit red | [`codacy-complexity-gate.md`](codacy-complexity-gate.md) |
| Eval-parity fixtures | Cross-host CE policy fixture suite | [`eval-parity-fixtures.md`](eval-parity-fixtures.md) |
| Self-improve | Learning Session dual-track harness + product observations | [`../skills/self-improve/SKILL.md`](../skills/self-improve/SKILL.md) |
| Self-improve master plan | Next-wave self-improve roadmap + Top 10 (post-Learning Session Stop gate); profile may extend under `profiles/<product>/references/` | [`self-improve-master-plan.md`](self-improve-master-plan.md) |
| OmniRoute | Optional local transport; never required for canonical workflows | [`../skills/omniroute/SKILL.md`](../skills/omniroute/SKILL.md) |
| FreeToken | Optional standalone local MoE companion (`:1919`); never CE-installed; not OmniRoute | [`../skills/freetoken/SKILL.md`](../skills/freetoken/SKILL.md) · [guide](../guides/freetoken.md) |
| Colibri | Optional frontier MoE multitier companion (`:8000`); never CE-installed; peer to FreeToken | [`../skills/colibri/SKILL.md`](../skills/colibri/SKILL.md) · [guide](../guides/colibri.md) |
| Local OpenAI-compat | Optional Ollama / LM Studio / llamacpp OpenAI-compat peers; never CE-installed; not OmniRoute/FreeToken plugins | [`../skills/local-openai-compat/SKILL.md`](../skills/local-openai-compat/SKILL.md) · [guide](../guides/local-openai-compat.md) |

Load **one** row when the core router points here; return to the core skill after
the deliverable.
