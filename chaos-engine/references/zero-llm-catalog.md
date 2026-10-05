---
description: Use when install, doctor, or repair can run as a script before any host chat. Prefer these deterministic paths.
---

# Zero-LLM / script-first catalog

Deterministic ChaosEngine paths that never require an LLM. Prefer these before
opening a host chat. Companion guidance: [script-first](script-first.md).

| Path | Command / entry | Proves / repairs |
| --- | --- | --- |
| Install / upgrade | One-liners in INSTALL.md (repo-only `chaos-engine/INSTALL.md`) | Fresh or upgraded payload + healthy doctor |
| Human doctor | `python3 .chaos-engine/install.py doctor --project .` | Component health + fix-next lines |
| Fix-next only | `python3 .chaos-engine/install.py doctor --project . --fix-next-only` | Prints only actionable repair lines (exit 0 if none) |
| Component repair | `python3 .chaos-engine/install.py repair --project . --component <id>` | Targeted quarantine/republish without full wipe |
| Heal route | [`heal-route.md`](heal-route.md) | File-path Heal surface even if marketplace plugin absent |
| Level-1 catalog | [`level-1-catalog.md`](level-1-catalog.md) | Progressive-disclosure secondary surfaces |
| JSON doctor | `... doctor --project . --json` | Schema v2 machine contract |
| Status (passive) | `... status --project .` | Receipt-bound status without active probes |
| Empty-project smoke | `python3 scripts/ci/chaos_engine_empty_project_smoke.py` | Install→doctor budget evidence (repo-only) |
| ChaosGauge digests | `python3 scripts/ci/chaos_gauge/validate_experiment.py --write ...` | Harness digest refresh after CE changes (repo-only) |
| README inventory | `python3 scripts/ci/validate_chaos_engine_readme.py` | Stdlib / surface inventory drift (repo-only) |
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
| Eval / parity fixtures | `python3 scripts/ci/chaos_engine_eval_parity.py` | Cross-host CE policy fixture suite (repo-only) |
| Harness eval suite | `python3 scripts/ci/chaos_engine_harness_eval_suite.py` (repo-only) · [harness-eval-suite](harness-eval-suite.md) | Capability + regression pass@k gate (issue 6519) |
| Learning metrics | [`learning.py`](../learning.py) `metrics` + [`learning_counters.py`](../learning_counters.py) (+ `doctor --json.learningMetrics`) | Queued→submitted rates, SessionStart bytes, denials, digests |
| CE brief | [`ce_brief.py`](../ce_brief.py) | Locator-only byte-capped system brief for local-agency design turns |
| Design-turn gate | citation+schema zero-LLM gate for local design/spec turns | [`../skills/local-agency/scripts/design_turn_gate.py`](../skills/local-agency/scripts/design_turn_gate.py) |
| Dispatch CE brief | [`skills/local-agency/scripts/dispatch.py`](../skills/local-agency/scripts/dispatch.py) `brief` / `--with-ce-brief` | Attach locator-only brief to OpenCode/chat paths |
| CE brief eval fixtures | [`../evals/ce-brief-unit-fixtures.json`](../evals/ce-brief-unit-fixtures.json) | Locator-only / byte-cap / no-secret unit contracts |
| Silent verify | [`silent_verify.py`](../silent_verify.py) / `finalize --silent` | Success silent exit 0; failure one stderr line |
| Heuristics retrieve | [`retrieve.py`](../retrieve.py) `heuristics --top 3` | Once-per-task ERL heuristics; SessionStart locator only |
| Heuristics CLI | [`heuristics.py`](../heuristics.py) `locator|retrieve|add` | Privacy-safe heuristic store under `.chaos-engine-state/heuristics/` |
| Memory provenance | [`memory_provenance.py`](../memory_provenance.py) `summary\|stamp\|verify` (repo-only) · [memory-provenance](memory-provenance.md) | Origin + trust; quarantine filter for retrieve/promotion (issue 6520) |
| Evolving playbook | [`heuristics.py`](../heuristics.py) `feedback\|delta\|retrieve` (repo-only) · [evolving-playbook](evolving-playbook.md) | Helpful/harmful counters + delta text; ranked retrieve (issue 6532) |
| Insight extract | [`insight_extract.py`](../insight_extract.py) `experience\|pair\|distill-failure\|operator\|retrieve` (repo-only) · [insight-extract](insight-extract.md) | Success/failure pairs + ExpeL votes + failure distill (issue 6544) |
| Workflow induction | [`workflow_induce.py`](../workflow_induce.py) `trajectory\|induce\|retrieve\|materialize\|summary` (repo-only) · [workflow-induction](workflow-induction.md) | Repeated successful windows; materialize behind the eval suite (issue 6518) |
| External reflection | [`external_reflection.py`](../external_reflection.py) `record\|retrieve\|summary` (repo-only) · [external-reflection](external-reflection.md) | Tests, CI, or doctor only; self-report refused (issue 6518) |
| Prompt evolution | [`prompt_evolve.py`](../prompt_evolve.py) `propose\|score\|accept\|summary` (repo-only) · [prompt-evolution](prompt-evolution.md) | Accept only a suite report that passes this task (issue 6518) |
| Self-modify | [`self_modify.py`](../self_modify.py) `propose\|score\|apply\|rollback\|summary` (repo-only) · [self-modify](self-modify.md) | Apply only behind the eval suite; archive the previous body (issue 6518) |
| Context rot | [`context_rot.py`](../context_rot.py) `record\|check\|compact` (repo-only) · [context-rot](context-rot.md) | Budget check; compaction keeps marked text and must shrink (issue 6518) |
| GenAI spans | [`genai_spans.py`](../genai_spans.py) `emit\|summary` (repo-only) · [genai-spans](genai-spans.md) | Local invoke_agent and execute_tool spans behind the eval suite (issue 6518) |
| Significance capture | [`significance.py`](../significance.py) `mark|list|drain|locator` | Soft fail/deny marks → Learning Session drain; no Task Observer |
| Skill compress audit | [`skill_compress_audit.py`](../skill_compress_audit.py) `audit` | Propose-only SKILL.md filler/bloat report; never auto-apply |
| Javadoc `@param` arity | `scripts/ci/check_javadoc_param_arity.py` (repo-only) | Fail fast when `@param` names do not match method parameters in engine interaction packages |
| Meta-optimize review | [`meta_optimize.py`](../meta_optimize.py) `review`/`cadence` | Periodic aggregation of significance + learning metrics + compress proposals; not continuous |
| Draft skill PR gate | [`draft_skill_pr.py`](../draft_skill_pr.py) `status`/`prepare`/`open` | Opt-in draft PRs only; default OFF; never auto-merge |
| Learn-traces collect | [`learn_traces.py`](../learn_traces.py) `collect --out <dir>` | Portable session-trace run dir |
| Learn-traces portable /learn | [`learn_traces.py`](../learn_traces.py) + [`learn_traces_mrv.py`](../learn_traces_mrv.py) `learn --out <dir>` | Map-reduce-verify file contract → `report.md` + git-tracked `actions.json`; never `~/.grok/skills` |
| Deep-research scaffold | [`deep_research.py`](../deep_research.py) `init --query … --out <dir>` | Portable research run dir; phases are host agents |

## Notes

`--fix-next-only` lets scripts scrape repair actions without parsing the
full human doctor essay or invoking a model. The eval runner exercises
fixture tasks under simulated hook runners; failures ratchet into hooks/skills
per [eval-parity-fixtures](eval-parity-fixtures.md).


## CLI-over-MCP iron law

Prefer training-data CLIs over MCP schema tax when both can do the job:

| Job | Prefer CLI | Avoid when CLI exists |
| --- | --- | --- |
| GitHub issues/PRs/checks | `gh` | GitHub MCP for the same call. Default install never publishes GitHub MCP. If a host already has GitHub MCP, leave it; still prefer `gh`. |
| Learning queue/metrics | `learning.py` / `learning_session.py` | MCP wrappers around the same files |
| Install health / repair | `install.py doctor|repair` / `--fix-next-only` | Chat discovery of doctor |
| Delivery phase ledger | `phase_ledger.py` | MCP phase bookkeeping |
| Store retrieve | `tool.py retrieve` / `retrieve.py` | Extra MCP hop for Memory/Graphify/MemPalace when CLI works |
| Heuristics once/task | `retrieve.py heuristics` | Re-injecting heuristic prose every Pre/PostToolUse |

MCP remains valid when **no** equivalent CLI exists. Encode lasting preference
in this catalog + SessionStart locator, never agent-only memory.
