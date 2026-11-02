# ChaosEngine Self-Improve — Research Synthesis

Companion to [self-improve-master-plan.md](self-improve-master-plan.md).
Dated (Africa/Cairo): 2026-09-08. Citations only — no always-on injection.

## 2. Online research synthesis (cited)

### 2.1 Context surfaces — put the right thing on the right primitive

Sources:

- https://amux.io/guides/context-engineering/
- https://capitalandcompute.net/blog/claude-code-harness-guide/
- https://next.redhat.com/2026/07/28/building-skills-for-ai-agents-pitfalls-and-best-practices/
- https://agentskills.io/specification

| Surface | Load | Enforce | CE mapping |
| --- | --- | --- | --- |
| AGENTS/CLAUDE.md | Every turn | Soft ~70% | Keep lean (&lt;~200–500 lines); pointers only |
| Hooks | Event; **0 context unless output** | Hard 100% | Safety, Learning Session Stop, research gate |
| Skills | Name+desc always; body on demand | Soft | Router + self-improve + companions |
| Subagents | On spawn; isolated | Soft | Research/explore firewall |
| Memory/stores | Locator / on demand | Soft | Wake pack locator; retrieve orchestrator |
| MCP | Names cheap; schemas deferred | Tools | Prefer CLI when training-data CLI exists |

**CE accept:** hooks for must-happen; skills for judgment; lean host md files.  
**CE reject:** stuffing safety or Learning Session duty into AGENTS.md alone.

### 2.2 HumanLayer harness lessons

Sources:

- https://www.humanlayer.dev/blog/skill-issue-harness-engineering-for-coding-agents
- https://www.humanlayer.dev/blog/context-efficient-backpressure
- https://www.humanlayer.dev/blog/advanced-context-engineering
- https://www.humanlayer.dev/blog/writing-a-good-claude-md

1. **CLI over MCP** when CLI is in training data (gh, git, docker, …) — lower schema tax + composability (`jq`/`rg`).
2. **Subagents for context isolation**, not role-play org charts.
3. **Verification silent-on-success / errors-only** — swallow green test dumps; exit 2 on failure to re-engage.
4. Hooks for control flow; Stop hooks for completion gates (maps directly to #5641).

### 2.3 Hooks economics

Sources: capitalandcompute harness guide; Anthropic hooks docs (`code.claude.com/docs/en/hooks-guide`); CE lifecycle-hooks.

- SessionStart / Stop / PreToolUse run **outside** the conversation budget.
- Stop is the right place for Learning Session and verify gates.
- Aligns with CE: locator SessionStart; Learning Session on Stop (#5641); no always-on observer.

### 2.4 Self-improve **without** always-on Task Observer

Sources:

- https://arxiv.org/pdf/2603.24639v2 (ERL — Experiential Reflective Learning)
- https://arxiv.org/pdf/2604.17091 (GenericAgent density / layered memory)
- https://arxiv.org/html/2604.07988 (LogAct shared logs + supervisor)
- CE `research-adopt-reject.md` + #5643

| Pattern | Token-safe shape for CE | Reject |
| --- | --- | --- |
| ERL heuristics | Post-task extract → store; **retrieve top-k once at task start** (locator + bounded) | Inject every turn / full SessionStart scan |
| GenericAgent L0/L1 | Wake pack + Level-1 catalog; L2/L3 on demand | Always-on L2/L3 dumps |
| LogAct supervisor | Shared structured logs (phase ledger, learning queue, `.memory`) → periodic meta-optimize | Continuous Task Observer |
| SkillOpt | Offline compress SKILL.md (~90% volume cut reported by Red Hat ACE) with eval gate | Auto-mutate live skills |

### 2.5 Maximum output, minimal / zero tokens — governing factors

1. **Deterministic scripts & hooks** before LLM narration (Red Hat: scripts cut RCA cost ~26%).
2. **Progressive disclosure** (L1/L2/L3) — description is the router.
3. **Silent success** on verify (HumanLayer).
4. **Fewer sharper tools / CLI&gt;MCP** (HumanLayer; Vercel tool-thinning lesson in prior dossier).
5. **Shared logs → unique work** (LogAct: +17% work / −41% tokens with supervisor over logs — aspirational, not a CE claim of identical gains).
6. **Measure then mutate** — metrics before growing prompts.
7. **Issues-first durable encode** — portable overlay parity (#5645).

Supporting dossier: `/workspace/agentic-harness-research.md` (Winder, Goose, OpenHands, Hermes, agentskills.io, Gu system-scaling).

---

## 7. Key online sources used

1. https://amux.io/guides/context-engineering/
2. https://capitalandcompute.net/blog/claude-code-harness-guide/
3. https://next.redhat.com/2026/07/28/building-skills-for-ai-agents-pitfalls-and-best-practices/
4. https://www.humanlayer.dev/blog/skill-issue-harness-engineering-for-coding-agents
5. https://www.humanlayer.dev/blog/context-efficient-backpressure
6. https://www.humanlayer.dev/blog/advanced-context-engineering
7. https://agentskills.io/specification
8. https://arxiv.org/pdf/2603.24639v2 (ERL)
9. https://arxiv.org/pdf/2604.17091 (GenericAgent)
10. https://arxiv.org/html/2604.07988 (LogAct)
11. https://github.com/microsoft/SkillOpt
12. https://code.claude.com/docs/en/hooks-guide
13. https://winder.ai/ai-agent-harness-comparison/ (prior dossier)
14. Local: `/workspace/agentic-harness-research.md`, `/workspace/chaos-engine-top10-optimizations.md` (superseded)

---

## 8. Consult notes (optional Grok CLI)

Captured `/tmp/ce-self-improve-consult.txt` (model host frontier model). Alignment with this plan:

- Measure first (LogAct counters) before growing prompts.
- Silent verify + CLI&gt;MCP + SkillOpt propose-only + ERL once-per-task.
- Reaffirm reject always-on scanners / weekly auto-mutate.
- Ordering tip from consult: metrics → silent verify → CLI&gt;MCP → submit hygiene → SkillOpt → ERL → eval ratchet → GAP-EXIT2 UX.

---
