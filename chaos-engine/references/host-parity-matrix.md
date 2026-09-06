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
| SessionStart locator-only / progressive disclosure | A | A | G | A | G |
| Skills discovery | A | A | A | A | A |
| Companions (Caveman + Ponytail) | P | P | P | P | P |
| Retrieval soft-degrade (Memory/MemPalace/Graphify) | P | P | P | P | P |
| Work-item to merge playbook | P | P | P | P | P |
| Learning Session | P | P | P | P | P |

## Measured gaps (severity)

| ID | Host(s) | Severity | Gap | Evidence / next |
| --- | --- | --- | --- | --- |
| GAP-EXIT2 | Grok, Copilot | high | Native exit-2 / hard-block semantics are thinner or host-gated vs Claude/Codex/Gemini; outcome relies on adapter + trust. | Tracked by #5579; kernel HOST_CAPABILITIES + hook configs. |
| GAP-SESSIONSTART | Grok, Copilot | medium | SessionStart locator-only / progressive disclosure is not proven equivalent; may inject more or less context. | Tracked by #5580. |
| GAP-HOOK-TRUST | Grok | medium | Project hook trust (`/hooks-trust`, projectTrusted) can leave doctor recovery-required after install. | Host onboarding card + grok_runtime_status. |
| GAP-MARKETPLACE-CLI | Claude, Codex | low | Marketplace/plugin auto-activation needs host CLI on PATH; absent CLI still installs adapters but activation is manual. | Onboarding cards. |
| GAP-COPILOT-DETECT | Copilot | low | Detection is soft (`gh` / `code` / `cursor`); IDE/cloud hosting is outside install probes. | Onboarding card. |
| GAP-GEMINI-NODE | Gemini | low | Hook launcher needs Node.js; unsupported native events remain explicit capability gaps. | Onboarding card + launch.js. |


## How to refresh

1. Update rows when adapter contracts or Host Parity Wave B issues land.
2. Keep `scripts/ci/agent_harness_parity.json` as the machine-checked pin set.
3. Prefer outcome language (P/A/G) over host UI chrome comparisons.
