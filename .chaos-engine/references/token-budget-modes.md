# Token budget modes

Owner-selectable context budgets for ChaosEngine sessions. Set
`CHAOS_ENGINE_TOKEN_BUDGET` to one of `ultra-lean` (default / max), `balanced`, or
`deep` before starting a host session. SessionStart echoes the active mode as
one compact locator line; agents load this page only when the mode requires
deeper guidance. Always-on host instruction cards also name the mode because
Grok ignores SessionStart stdout.

| Mode | Intent | Read/search bound | When to use |
| --- | --- | --- | --- |
| `ultra-lean` | Minimum tokens that still preserve safety | Prefer ≤80-line excerpts; one discriminating search | Routine edits, doctor/status, single-file fixes |
| `balanced` | High-signal set | Prefer ≤200-line excerpts; narrow once after truncation | Opt-in when ultra-lean is too tight |
| `deep` | Broader evidence before deciding | Prefer ≤400-line excerpts; allow a second discriminating pass | Incidents, unfamiliar subsystems, adversarial review |

## Invariants (all modes)

- Safety, negation, attribution, and hard blocks never shrink with the budget.
- Prefer [script-first](script-first.md) and [context economy](context-economy.md) before spending the budget.
- SessionStart stays locator-only under `SESSION_START_MAX_BYTES` regardless of mode.
- Progressive disclosure: do not dump this whole page into the transcript; follow the active mode row.

## How hosts pick it up

1. Export `CHAOS_ENGINE_TOKEN_BUDGET=ultra-lean` (or `balanced` / `deep`).
2. Start or restart the host so SessionStart re-runs.
3. Confirm the SessionStart or always-on card names the mode; if unset, ChaosEngine uses `ultra-lean`.

Machine-readable labels live in `hooks/lifecycle.py` (`TOKEN_BUDGET_MODES`).




## Triage → token budget defaults (#5621)

When `CHAOS_ENGINE_TOKEN_BUDGET` is **unset**, pick the default from triage
(blast radius). Env override always wins.

| Triage (worse of blast radius / reversibility) | Default budget |
| --- | --- |
| One file, reversible | `ultra-lean` (Headroom `agent-90`) |
| One module, reversible | `ultra-lean` |
| Public contract, many callers, or hard to reverse | `ultra-lean` (set `deep` only via env) |

Machine map: `hooks/lifecycle.py` → `TRIAGE_TO_TOKEN_BUDGET` /
`triage_token_budget()`.

## Headroom profile map

| Token budget | `HEADROOM_SAVINGS_PROFILE` |
| --- | --- |
| `ultra-lean` | `agent-90` (CE max savings) |
| `balanced` | `balanced` |
| `deep` | `coding` / passthrough |

See [headroom.md](headroom.md). Beacon stays `off`; memory injection `disabled`.
