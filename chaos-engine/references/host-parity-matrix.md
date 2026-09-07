# Host Parity Matrix v0

Living adapter-outcome matrix for ChaosEngine across Claude Code, Codex, Grok, Gemini, and GitHub Copilot.

Parity means the same workflow *outcomes* on all five hosts (not UI chrome parity).
Machine-checkable capability pins also live in `scripts/ci/agent_harness_parity.json`.

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
| Retrieval soft-degrade (Memory/MemPalace/Graphify) | P | P | P | P | P |
| Work-item to merge playbook | P | P | P | P | P |
| Learning Session | P | P | P | P | P |

## Measured gaps (severity)

| ID | Host(s) | Severity | Gap | Evidence / next |
| --- | --- | --- | --- | --- |
| GAP-EXIT2 | Grok, Copilot | high | `HostCapability.process_exit2_honored=False`; ChaosEngine still returns deny exit 2 + native deny payload (`decision`/`permissionDecision`). Owner doctor surfaces `blockingGap`. | Proven by `tests/scripts/test_chaos_engine_exit2_fidelity.py`; fields on HOST_CAPABILITIES (#5579). |
| GAP-SESSIONSTART | — (cleared) | info | ChaosEngine emits identical locator-only SessionStart context (`SESSION_START_MAX_BYTES`=4096) on all five hosts; residual risk is a host ignoring SessionStart output (companions still load via entrypoint). | Proven by `tests/scripts/test_chaos_engine_sessionstart_locator_parity.py` (#5580). |
| GAP-HOOK-TRUST | Grok | medium | Project hook trust (`/hooks-trust`, projectTrusted) can leave doctor recovery-required after install. | Host onboarding card + grok_runtime_status. |
| GAP-MARKETPLACE-CLI | Claude, Codex | low | Marketplace/plugin auto-activation needs host CLI on PATH; absent CLI still installs adapters but activation is manual. | Onboarding cards. |
| GAP-COPILOT-DETECT | Copilot | low | Detection is soft (`gh` / `code` / `cursor`); IDE/cloud hosting is outside install probes. | Onboarding card. |
| GAP-GEMINI-NODE | Gemini | low | Hook launcher needs Node.js; unsupported native events remain explicit capability gaps. | Onboarding card + launch.js. |


## How to refresh

1. Update rows when adapter contracts or Host Parity Wave B issues land.
2. Keep `scripts/ci/agent_harness_parity.json` as the machine-checked pin set.
3. Prefer outcome language (P/A/G) over host UI chrome comparisons.
4. Re-run the eval / parity fixture suite (`python3 scripts/ci/chaos_engine_eval_parity.py`;
   see [eval-parity-fixtures](eval-parity-fixtures.md)) after hook or adapter changes.
   Fixture failures ratchet into hooks, skills, or a documented matrix gap — never
   weaken the fixture to look green.
