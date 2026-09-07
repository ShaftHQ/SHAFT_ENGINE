# Eval / parity fixture suite

Minimal regression harness for ChaosEngine policy outcomes across the five
hosts (Claude Code, Codex, Grok, Gemini, GitHub Copilot). Parent epic #5569 /
issue #5584.

Parity here means **same policy outcomes** under simulated hook runners — not
UI chrome parity and not driving hosts as child processes.

## Corpus and runner

| Piece | Path |
| --- | --- |
| Fixture corpus | [`evals/parity-fixtures.json`](../evals/parity-fixtures.json) |
| Documented runner | `python3 scripts/ci/chaos_engine_eval_parity.py` |
| Machine report | `python3 scripts/ci/chaos_engine_eval_parity.py --json` |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_eval_parity_fixtures -v` |

Each fixture declares:

- `event` — one provider-neutral lifecycle payload
- `expect` — exit code, allow/deny, reason substrings, SessionStart budget
- `ratchet` — where a failure must land (`hooks`, `skills`, or `matrix`)

The runner loads `hooks/guard.py` through `lifecycle.run_hook_protocol` with
`adapt_hook_output` for every host in the corpus. That is the simulated hook
runner: identical CE policy, host-native deny/context payloads.

## Fixture set (v0)

| ID | Policy | Expected |
| --- | --- | --- |
| `deny-catastrophic-rm-rf-root` | catastrophic deny | exit 2 + native deny on all hosts |
| `allow-read-tool` | non-mutation allow | exit 0, no deny payload |
| `allow-safe-echo` | non-mutation allow | exit 0 for `echo hello` |
| `sessionstart-locator-budget` | SessionStart locator | ≤4096 bytes, companion paths, identical context |
| `research-before-mutation-enforced` | research receipt gate | deny until preflight when env enforced |
| `research-triage-public-contract-hard` | triage-scaled research | deny for public-contract without preflight |
| `research-triage-one-file-soft` | triage-scaled research | allow one-file even when env enforced |
| `sessionstart-no-memory-prose` | SessionStart locator | wake-pack locator; no Memory prose dump |
| `learning-session-portable-finalize-allow` | portable Learning Session | PostToolUse allow for `.chaos-engine/learning_session.py` |

## Failure ratchet (hooks / skills / matrix)

Do **not** weaken a fixture to look green ([ethical-conduct](ethical-conduct.md)).

When a fixture fails:

1. **`ratchet: hooks`** — fix `hooks/guard.py`, `hooks/kernel.py`, or
   `hooks/lifecycle.py` so the guarantee holds on every host adapter.
2. **`ratchet: skills`** — if the gap is judgment (thoroughness, playbook
   wording), update the routed skill / reference and keep the hook contract
   unchanged.
3. **`ratchet: matrix`** — if a host cannot honor the outcome, document a
   severity-ranked gap in [host-parity-matrix](host-parity-matrix.md) and keep
   the fixture asserting ChaosEngine's emitted contract (exit 2 + native deny).

CI executes the suite through the Agent Guidance Gate
(`eval-parity-contract` in `scripts/ci/harness_pr_gate.py`) and the scheduled
exhaustive harness in `.github/workflows/agent-plugin-acceptance.yml`.

## Related

- [Host Parity Matrix](host-parity-matrix.md)
- [Lifecycle hooks](lifecycle-hooks.md)
- [Delivery phase gates](delivery-phase-gates.md)
- [Zero-LLM catalog](zero-llm-catalog.md)


## Wave C expansions (#5625)

Additional fixtures cover triage-scaled research, SessionStart wake-pack
(no Memory prose), and portable Learning Session finalize. Plugin activation /
status⊆doctor / memory degrade remain covered by doctor/status unit tests and
the Wave A health-truth suite; Learning Session Stop continues to require a
terminal finalize (portable or monorepo) with **issues-first** disposition and
**no** auto draft PRs.
