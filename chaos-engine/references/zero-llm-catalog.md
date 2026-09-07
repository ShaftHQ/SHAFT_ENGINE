# Zero-LLM / script-first catalog

Deterministic ChaosEngine paths that never require an LLM. Prefer these before
opening a host chat. Companion guidance: [script-first](script-first.md).

| Path | Command / entry | Proves / repairs |
| --- | --- | --- |
| Install / upgrade | One-liners in [INSTALL.md](../INSTALL.md) | Fresh or upgraded payload + healthy doctor |
| Human doctor | `python3 .chaos-engine/install.py doctor --project .` | Component health + fix-next lines |
| Fix-next only | `python3 .chaos-engine/install.py doctor --project . --fix-next-only` | Prints only actionable repair lines (exit 0 if none) |
| Component repair | `python3 .chaos-engine/install.py repair --project . --component <id>` | Targeted quarantine/republish without full wipe (#5620/#5621) |
| Heal route | [`heal-route.md`](heal-route.md) | File-path Heal surface even if marketplace plugin absent |
| Level-1 catalog | [`level-1-catalog.md`](level-1-catalog.md) | Progressive-disclosure secondary surfaces |
| JSON doctor | `... doctor --project . --json` | Schema v2 machine contract |
| Status (passive) | `... status --project .` | Receipt-bound status without active probes |
| Empty-project smoke | `python3 scripts/ci/chaos_engine_empty_project_smoke.py` | Install→doctor budget evidence |
| ChaosGauge digests | `python3 scripts/ci/chaos_gauge/validate_experiment.py --write ...` | Harness digest refresh after CE changes |
| README inventory | `python3 scripts/ci/validate_chaos_engine_readme.py` | Stdlib / surface inventory drift |
| Kernel contract | `python3 -m unittest tests.scripts.test_chaos_engine_kernel -v` | Host capability + adapt_hook_output |
| Exit-2 fidelity | `python3 -m unittest tests.scripts.test_chaos_engine_exit2_fidelity -v` | Deny exit code + native payloads |
| SessionStart budget | `python3 -m unittest tests.scripts.test_chaos_engine_sessionstart_locator_parity -v` | Locator-only ≤4096 bytes |
| Token budget modes | `python3 -m unittest tests.scripts.test_chaos_engine_token_budget_modes -v` | ultra-lean < balanced < deep |
| Cache status/purge | `... cache status|purge --component maven-tools-mcp ...` | Shared MCP cache without chat |
| Rollback / uninstall | `... rollback|uninstall --project .` | Deterministic recovery |
| Phase ledger | [`phase_ledger.py`](../phase_ledger.py) `record` / `show` / `summary` | Triage-scaled research gate + doctor `phaseLedger` |
| Retrieve orchestrator | [`retrieve.py`](../retrieve.py) via `tool.py retrieve` | One store + used / skipped / degraded receipt |
| Wake pack | [`wake_pack.py`](../wake_pack.py) + `.chaos-engine-state/wake-pack.md` | Owner-curated ≤~120 tokens; draft-only MemPalace |
| Portable Learning Session | [`learning_session.py`](../learning_session.py) `finalize` | Issues-first; **no** auto draft PRs |
| Research preflight marker | `python3 .chaos-engine/hooks/reflection.py research-preflight --session-id <id>` | Unblocks opt-in research-before-mutation gate |
| Eval / parity fixtures | `python3 scripts/ci/chaos_engine_eval_parity.py` | Cross-host CE policy fixture suite (#5584) |

## Notes

`--fix-next-only` (#5582) lets scripts scrape repair actions without parsing the
full human doctor essay or invoking a model. The eval runner (#5584) exercises
fixture tasks under simulated hook runners; failures ratchet into hooks/skills
per [eval-parity-fixtures](eval-parity-fixtures.md).

