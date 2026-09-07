# ChaosEngine Self-Improve Master Plan

**Owner:** Mohab Mohie / ShaftHQ/SHAFT_ENGINE  
**Dated (Africa/Cairo):** 2026-09-08  
**Base tip:** `77a273d2a8` — `fix(ce): enforce Learning Session after every delivery (harness parity) (#5641)`  
**Supersedes:** prior optimization top-10 at analysis SHA `0961058e66` / `/workspace/chaos-engine-top10-optimizations.md` (ideas #1–#10 landed as #5619–#5625 under epic #5618).  
**Method:** tip deep-read of `skills/self-improve/**`, `learning.py`, Stop/Learning Session hooks, token/parity/delivery refs; online harness synthesis; optional Grok CLI consult (`/tmp/ce-self-improve-consult.txt`).  
**Constraint:** NEVER Cursor Cloud Agents; merge commits only; portable overlay parity across Codex / Claude / Grok CLI / Gemini / Copilot / Grok Bot.

---

## 0. Locked prefs (do not reopen)

| Pref | Status |
| --- | --- |
| NO native Task Observer / always-on observation | **Locked reject** (#5643; research-adopt-reject) |
| Lean self-improve: SessionStart locator-only; Learning Session on delivery Stop | **Shipped** (#5641 / #5613) |
| Harness parity across six hosts (overlay, not agent memory) | **Shipped policy** (#5645 / host-parity-matrix) |
| One-command install/configure/update | **Shipped north star** (#5618 / #5620) |
| Headroom `agent-90` default; Caveman + Ponytail | **Shipped** (#5613 / token-budget-modes) |
| Issues-first self-improve; eval-gated draft skill PRs **opt-in later** | **Locked** (#5618 fork #5) |
| Merge commits only | **Locked** |

---

## 1. Current-state assessment (post-#5641)

### 1.1 What CE can already self-enhance

| Loop | Product (SHAFT) | Harness (CE overlay) | Token profile |
| --- | --- | --- | --- |
| Capture | Dual-track Learning Session → `learning.py` queue → GitHub issues | Same path; categories allow-list; privacy gates | Heavy only on delivery Stop |
| Enforce learn | Stop / delivery-complete owes Learning Session even if `chaos-engine/` untouched | Portable `learning_session.py` finalize + monorepo controller | Hook = 0 context unless block reason |
| Discover surfaces | Level-1 catalog + router Heal / zero-LLM | SessionStart locators under `SESSION_START_MAX_BYTES=4096` | ↓ always-on |
| Health / heal | Installer/doctor/repair components | `status ⊆ doctor`, `activationProof`, `repair --component` | Zero-LLM |
| Delivery gates | Triage-scaled research + phase ledger | Stop Learning Session; catastrophic deny | Soft vs hard by triage |
| Memory | Owner wake pack locator; retrieve orchestrator | Soft-degrade Memory/MemPalace/Graphify | No SessionStart prose dump |
| Eval | Product CI / ChaosGauge | Eval-parity fixtures + host matrix | CI, not chat |

### 1.2 Dual-track self-improve ability (honest)

**Strengths**

- Stop-gated Learning Session is **deterministic** (hook), not prompt hope — aligns with industry “hooks = 100% enforce, 0 context unless output.”
- Issues-first + privacy-gated `learning.py` is a durable LogAct-like append log (queue → GitHub), not transcript hoarding.
- Progressive disclosure skeleton exists: Level-1 catalog, locator SessionStart, on-demand self-improve refs.
- Optimization program #5618 children (#5619–#5625) closed the prior top-10 (health truth, repair, zero-LLM locator, L1 catalog, triage research+ledger, wake+retrieve, eval-parity+portable finalize).

**Gaps (self-improve-specific — this plan’s focus)**

1. **No closed-loop metrics:** queued → submitted → adopted/rejected → estimatedTokens delta is not machine-summarized for doctor/zero-LLM.
2. **No once-per-task heuristic retrieve:** lessons live in issues/`.memory` but are not ERL-style top-k retrieved at triage (still avoid every-turn injection).
3. **Verification dumps still risk token rot:** Check/Stop paths lack a universal silent-on-success / errors-only wrapper contract.
4. **Skill body bloat risk:** no SkillOpt-style compression/eval gate before growing SKILL.md (L2 <500 lines discipline is documented, not automated).
5. **CLI vs MCP preference** is tribal in places; HumanLayer lesson not yet an iron-law + zero-LLM catalog row.
6. **Significance filter for mid-session capture** is skill-discipline only — easy to miss under Caveman ultra (without becoming Task Observer).
7. **Phase-2 eval-gated draft skill PRs** still deferred (correct) but needs a ready gate checklist.
8. **Product-track playbook** thinner than harness track (taxonomy exists; SHAFT-specific acceptance/eval linkage weak).
9. **GAP-EXIT2** still soft on Grok/Copilot — compensating UX exists; not a self-improve blocker but limits Stop-gate fidelity.
10. **Periodic meta-optimize** (supervisor over shared logs) not scheduled — only per-delivery Learning Session.

### 1.3 Gap analysis vs #5641

| #5641 deliverable | Status | Remaining self-improve work |
| --- | --- | --- |
| Merge/delivery → Learning Session owed | Shipped | Metrics that prove sessions actually queue/submit |
| Docs forbid CE-untouched skip | Shipped | Product-track yield reporting |
| Harness parity in overlay | Shipped | Encode new lessons as hooks/skills/scripts, never agent-only memory |
| SessionStart locator-only | Shipped | Keep; add **locator** to heuristic index only (no prose) |
| Unit tests for merge→Stop | Shipped | Expand fixtures for metrics + silent-verify + heuristic retrieve |

**Verdict:** #5641 closed the **enforcement** gap. This plan closes the **yield / token-efficiency / closed-loop** gap for both product and harness.

---

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

## 3. Master roadmap waves (self-improve next)

Dependency order. **Do not** re-implement #5619–#5625.

```
Wave S0 — Measure (zero/low LLM)
  [#1 Learning metrics closed loop]

Wave S1 — Token sinks on the hot path
  [#2 Silent-on-success verify] → [#5 CLI-over-MCP iron law]
       ↘ [#3 ERL once-per-task heuristics]

Wave S2 — Capture quality without Observer
  [#4 Significance-filtered mid-session capture] → [#6 SkillOpt compress audit]

Wave S3 — Isolation & product yield
  [#7 Subagent research firewall guidance] → [#10 Product-track playbook + ChaosGauge link]

Wave S4 — Close the mutate loop (opt-in)
  [#9 Periodic meta-optimize] → [#8 Eval-gated draft skill PRs (opt-in)]
```

**Out of scope forever (unless prefs unlock):** always-on SessionStart full skill/frontmatter scan; native Task Observer; auto-merge skill mutations; pretending GAP-EXIT2 hard process blocks.

---

## 4. Top 10 recommendations (ranked)

Each item: why · token impact · dependency · proposed CE shape · Task Observer accept/reject.

### 1. Learning metrics closed loop (queued → adopted → token delta)

- **Why:** LogAct shows shared structured logs + light supervisor yield more unique work at lower tokens. CE already appends via `learning.py` / `.memory` / phase ledger but does not summarize adoption.
- **Token impact:** ↓↓ (zero-LLM doctor/CLI; avoids re-discovering known lessons)
- **Dependency:** none (S0)
- **CE shape:** `learning.py metrics|summary` + `doctor --json.learningMetrics` fields: queued, submitted, open/closed issues, estimatedTokens sum, optional adopt/reject labels; link issue numbers only.
- **vs Task Observer:** **Reject Observer.** Metrics are offline/zero-LLM over existing logs.

### 2. Silent-on-success / errors-only verification wrappers

- **Why:** HumanLayer: green test dumps rotate agents into the dumb zone; success must add **zero** context.
- **Token impact:** ↓↓↓ on Check/Stop
- **Dependency:** after #1 optional; independent OK
- **CE shape:** `references/silent-verify.md` + thin `scripts`/`chaos-engine` helper used by delivery Check + optional Stop verify; exit 0 → no stdout; fail → stderr + exit 2 where honored.
- **vs Task Observer:** **Reject Observer.** Pure hook/script back-pressure.

### 3. ERL-style heuristic retrieve once per task (not every turn)

- **Why:** ERL: post-task heuristics + top-k inject at **new task start** beats always-on observation.
- **Token impact:** ↓ always-on; slight ↑ once at triage (bounded top-k, privacy-safe)
- **Dependency:** prefers #1 metrics for ranking; wake pack / retrieve patterns (#5624)
- **CE shape:** `.chaos-engine-state/heuristics/` or Memory category; SessionStart **locator only**; triage/router calls `retrieve.py heuristics --top 3` once; never PreToolUse spam.
- **vs Task Observer:** **Reject Observer.** Adopt ERL retrieve-once; reject continuous scan (#5643).

### 4. Significance-filtered mid-session capture (skill + optional soft PostToolUse)

- **Why:** Under Caveman ultra, dual-track capture is easy to miss until Stop; need cheap friction marks without full observation protocol.
- **Token impact:** ↓ vs batching failures; ↑ tiny if soft hook emits one-line marks
- **Dependency:** taxonomy + learning.py; must not load self-improve refs mid-turn
- **CE shape:** optional PostToolUse recorder for deny/fail signatures → state file; Learning Session reads marks. Default off or ultra-cheap. Significance filter: errors, repeated denials, doctor recovery — not every edit.
- **vs Task Observer:** **Reject Observer.** Filter + deferred heavy path only.

### 5. CLI-over-MCP preference (iron law + zero-LLM catalog)

- **Why:** HumanLayer: CLI in training data beats MCP schema tax for gh/git/docker/db.
- **Token impact:** ↓↓ system prompt + tool-result size
- **Dependency:** zero-llm-catalog / Level-1 (#5621/#5622)
- **CE shape:** router iron-law row + catalog entries; MCP only when no equivalent CLI; Headroom/MCP remain for compression/proxy cases.
- **vs Task Observer:** N/A (tooling policy).

### 6. SkillOpt-style SKILL.md compression audit (propose, don’t auto-apply)

- **Why:** Red Hat ACE / SkillOpt: ~90% volume cut with quality preserved in experiments; L2 &lt;500 lines.
- **Token impact:** ↓↓ on skill activation
- **Dependency:** eval-parity (#5625); metrics #1 help prioritize which skills
- **CE shape:** offline CLI proposing compressed SKILL.md + refs split; CI eval gate; issues-first PR by human. No live auto-edit.
- **vs Task Observer:** **Reject auto-mutate.** Aligns with issues-first / opt-in draft PRs later.

### 7. Subagent / context-firewall guidance for research & explore

- **Why:** HumanLayer + capitalandcompute: isolation keeps parent in smart zone; explore noise stays in child.
- **Token impact:** ↓ parent context (child spend may ↑ — net quality win)
- **Dependency:** host adapters; local-coding-delegate / omniroute optional
- **CE shape:** short Level-1 + router rule: research/multi-file explore → subagent/Task when host supports; return filepath:line citations only.
- **vs Task Observer:** N/A.

### 8. Eval-gated draft skill PRs — **opt-in phase 2**

- **Why:** Hermes/SkillOpt closed loops need eval; CE locked issues-first until ready.
- **Token impact:** ↑ CI only; ↓ bad skill merges long-term
- **Dependency:** #1 metrics, #6 compress, #5625 fixtures green, portable finalize stable
- **CE shape:** flag `CHAOS_ENGINE_SELF_IMPROVE_DRAFT_PR=1`; draft PR from staged branch after eval-parity + unit tests; never auto-merge.
- **vs Task Observer:** **Reject Observer.** Opt-in mutate path only after eval.

### 9. Periodic meta-optimize over shared logs (not continuous)

- **Why:** LogAct supervisor: periodic introspection of shared logs beats per-turn gossip.
- **Token impact:** ↓ vs continuous; bounded scheduled LLM or zero-LLM cluster
- **Dependency:** #1 metrics; Learning Session history
- **CE shape:** operator `/learning meta` or weekly script clustering open learning issues + `.memory` facts → ranked backlog issue; no SessionStart involvement.
- **vs Task Observer:** **Reject Observer.** Periodic only.

### 10. Product-track self-improve playbook (SHAFT + ChaosGauge)

- **Why:** Dual-track requires equal product discipline; recent #5644/#5646/#5647 show product lessons already valuable.
- **Token impact:** ↓ rediscovery; slight ↑ at Learning Session when product work occurred
- **Dependency:** taxonomy; metrics #1; silent-verify #2 for product Check
- **CE shape:** `skills/self-improve/references/product-track.md` — issue templates, link tests/ChaosGauge, prefer CLI/doctor/silent-verify product fixes over essay tickets.
- **vs Task Observer:** **Reject Observer.** Delivery-Stop capture only.

---

## 5. Accept / reject matrix (Task Observer principles)

| Principle | Decision | CE encoding |
| --- | --- | --- |
| Always-on SessionStart full scan | **REJECT** | Locator-only; `SESSION_START_MAX_BYTES` |
| Continuous observation every turn | **REJECT** | #5643; research-adopt-reject |
| Lean core + on-demand refs | **ACCEPT** | self-improve SKILL + references |
| Capture vs review split | **ACCEPT** | queue now; human/CI apply later |
| Dual-track harness + product | **ACCEPT** | taxonomy + Learning Session checklist |
| Auto-stage / auto-install skill patches | **REJECT** (phase-2 draft PR opt-in only) | learning.py issues-first |
| Weekly auto-mutate live skills | **REJECT** | meta-optimize proposes backlog only |
| Post-delivery Learning Session gate | **ACCEPT** | #5641 Stop hooks |
| ERL retrieve-once heuristics | **ACCEPT (adapted)** | Top 10 #3 |
| LogAct metrics / periodic supervisor | **ACCEPT (adapted)** | Top 10 #1 #9 |
| Silent-on-success verify | **ACCEPT** | Top 10 #2 |

---

## 6. Implementation notes (when executing children)

1. Prefer **one condensed PR per wave** (S0→S4); children under the epic; `Fixes #<child>` never the epic.
2. Every lasting behavior → portable `chaos-engine/` (hooks/skills/scripts/doctor) + host adapters as needed — **harness parity**.
3. Expand eval-parity fixtures when adding gates; never weaken fixtures.
4. Keep Headroom `agent-90` / Ponytail XOR OUTPUT_SHAPER / Caveman ultra.
5. No Cursor Cloud Agents for implementation workstreams.

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

Captured `/tmp/ce-self-improve-consult.txt` (model `grok-4.5`). Alignment with this plan:

- Measure first (LogAct counters) before growing prompts.
- Silent verify + CLI&gt;MCP + SkillOpt propose-only + ERL once-per-task.
- Reaffirm reject always-on scanners / weekly auto-mutate.
- Ordering tip from consult: metrics → silent verify → CLI&gt;MCP → submit hygiene → SkillOpt → ERL → eval ratchet → GAP-EXIT2 UX.

---

## 9. Supersession notice

The 2026-09-07 dependency-ranked top-10 (health plane → repair → zero-LLM locator → L1 catalog → research ledger → wake/retrieve → Headroom/budget → eval-gated improve → fixtures → Heal/GAP-EXIT2) is **complete** via #5619–#5625 + #5641. **Do not reopen those tickets.** Track new work from **§4 Top 10** under the self-improve epic filed alongside this document.
