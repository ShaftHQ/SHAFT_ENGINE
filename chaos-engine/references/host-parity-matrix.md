# Host Parity Matrix v0

Living adapter-outcome matrix for ChaosEngine across Claude Code, Codex, Grok, Gemini, and GitHub Copilot.

Parity means the same workflow *outcomes* on all five hosts (not UI chrome parity).
Machine-checkable capability pins also live in `scripts/ci/agent_harness_parity.json`.

## Harness parity (permanent)

Any lasting behavior or policy — including Learning Session after every
delivery, the ban on traffic proxies ([no-proxy](no-proxy.md)), and token optimization as Kanban eliminate-waste ([eliminate-waste](eliminate-waste.md)) — MUST live in the portable ChaosEngine overlay: hooks, skills,
installer/doctor, and host guidance adapters (`AGENTS.md` / `CLAUDE.md` /
`GEMINI.md` equivalents). Never leave lasting harness rules only in one
agent's memory or routines. Unchanged `chaos-engine/` sources are not a
valid Learning Session skip on any host.

Legend: P = parity (outcome available), A = adapter-shaped equivalent, G = gap (see below), N = not applicable.


## Matrix

| Surface | Claude Code | Codex | Grok | Gemini | Copilot |
| --- | --- | --- | --- | --- | --- |
| Install / doctor human UX | P | P | P | P | P |
| Host onboarding / activation path | A marketplace/plugin | A marketplace/plugin | A file/hook | A file/hook | A file/hook |
| Router skill (`chaos-engine`) | P | P | P | P | P |
| Lifecycle hooks | A | A | A | A | A |
| Exit-2 / blocking denial fidelity | A | A | G | A | G |
| SessionStart locator-only / progressive disclosure | A | A | A | A | A |
| Skills discovery | A | A | A | A | A |
| Companions (Caveman + Ponytail) | P | P | P | P | P |
| Self-improve skill (Learning Session) | P | P | P | P | P |
| Retrieval soft-degrade (Memory/MemPalace/Graphify) | P | P | P | P | P |
| Work-item to merge playbook | P | P | P | P | P |
| Learning Session | P | P | P | P | P |

## Measured gaps (severity)

| ID | Host(s) | Severity | Gap | Evidence / next |
| --- | --- | --- | --- | --- |
| GAP-EXIT2 | Grok, Copilot | high | `HostCapability.process_exit2_honored=False`; ChaosEngine still returns deny exit 2 + native deny payload (`decision`/`permissionDecision`). Owner doctor surfaces `blockingGap`. | Proven by `tests/scripts/test_chaos_engine_exit2_fidelity.py`; fields on HOST_CAPABILITIES (#5579). |
| GAP-SESSIONSTART | — (cleared) | info | ChaosEngine emits identical locator-only SessionStart context (`SESSION_START_MAX_BYTES`=4096) on all five hosts; residual risk is a host ignoring SessionStart output (companions still load via entrypoint). | Proven by `tests/scripts/test_chaos_engine_sessionstart_locator_parity.py` (#5580). |
| GAP-HOOK-TRUST | Grok | medium | Project hook trust (`/hooks-trust`, projectTrusted) is host-gated; doctor stays healthy and reports sync-advisory until the operator trusts hooks. Never flip overall doctor to recovery-required for Grok trust alone when hosts verify is healthy. | Host onboarding card + grok_runtime_status (advisory); #5791. |
| GAP-GROK-LEAN | Grok | medium | Dual Claude-compat + native `.grok` hooks double CE context; installer merges lean `[compat.*]` into user `~/.grok/config.toml`; optional `--lean-grok-skills`; skill adapters stay pointers. | [`grok_lean_config.py`](../grok_lean_config.py) + doctor sync-advisory; #5802 #5804 #5805. |
| GAP-IMPL-COMPANIONS | all | medium | Implementation must load Caveman+Ponytail at ultra via portable overlay; not optional and not SessionStart-only. Doctor **self-heals** missing companions from CE vendor (official publish); agentic handoff with official repair/install only if heal fails. | `rematerialize_companions` + doctor; #5806 #5811. |
| GAP-IDENTITY-MD | all | medium | Durable `.chaos-engine/identity.md` with protected Truth markers; SessionStart/instruction pointer inject; doctor create-on-heal. | [`identity_md.py`](../identity_md.py) + seed [`identity.md`](../identity.md); #5807. |
| GAP-MARKETPLACE-CLI | Claude, Codex | low | Marketplace/plugin auto-activation needs host CLI on PATH; absent CLI still installs adapters but activation is manual. | Onboarding cards. |
| GAP-COPILOT-DETECT | Copilot | low | Detection is soft (`gh` / `code` / `cursor`); IDE/cloud hosting is outside install probes. | Onboarding card. |
| GAP-GEMINI-NODE | Gemini | low | Hook launcher needs Node.js; unsupported native events remain explicit capability gaps. | Onboarding card + launch.js. |
| GAP-GROK-BUNDLED | Grok | info | Grok product bundled skills (pdf/pptx/imagine/game-*) and session GitHub MCP cannot be deleted from the install tree. CE does not vendor them; doctor strips user GitHub MCP when gh is healthy and documents this limit. | #5780 #5785; prefer-cli-over-mcp. |
| GAP-GROK-CAVEMAN | — (cleared) | info | Always-on CE card (`caveman=ultra` in `hooks/lifecycle.py`) is the Grok communication constitution. Do not copy Caveman skill bodies into `AGENTS.md`. | Closed: locator-only host guidance plus lifecycle ultra selector. No proxy. |



## Official self-heal (#5811)

Doctor runs each required third party's **official install command** (or CE vendor rematerialize for Caveman/Ponytail) before agentic handoff. Inventory and heal wiring live in `official_self_heal.py` + `INSTALL.md`. Opt-out `--without-*` stays off. Parent epic: #5803.

## How to refresh

1. Update rows when adapter contracts or Host Parity Wave B issues land.
2. Keep `scripts/ci/agent_harness_parity.json` as the machine-checked pin set.
3. Prefer outcome language (P/A/G) over host UI chrome comparisons.
4. Re-run the eval / parity fixture suite (`python3 scripts/ci/chaos_engine_eval_parity.py`;
   see [eval-parity-fixtures](eval-parity-fixtures.md)) after hook or adapter changes.
   Fixture failures ratchet into hooks, skills, or a documented matrix gap — never
   weaken the fixture to look green.

Checked-in memory for one-router / CLI-owned MCP / project-mode Caveman:
[`.memory/memory/constraints/host-parity-one-router-cli-owned-mcp-project-mode-caveman.md`](../../.memory/memory/constraints/host-parity-one-router-cli-owned-mcp-project-mode-caveman.md).
See also the [hook trigger map](hook-trigger-map.md).


## GAP-EXIT2 compensating UX checklist (Grok / Copilot)

Do **not** pretend a hard process exit-2 block on these hosts. ChaosEngine still
emits `decision=block` / `permissionDecision=deny` and exit 2; owners must
verify trust and static surfaces.

- [ ] Doctor human output shows `blockingGap` / GAP-EXIT2 warning for Grok and Copilot
- [ ] Host parity row `GAP-EXIT2` stays documented with severity and proof test
- [ ] After a deny, tell the owner to check project trust / hooks trust (Grok: `grok inspect --json`, `/hooks-trust`) and IDE Copilot trust — not “the tool was hard-blocked by exit code”
- [ ] SessionStart / Heal locators remain available when marketplace plugins are absent
- [ ] Exit-2 fidelity unit test remains green: `tests.scripts.test_chaos_engine_exit2_fidelity`

