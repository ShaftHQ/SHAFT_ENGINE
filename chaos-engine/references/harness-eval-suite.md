# Harness eval suite

Outcome-graded ChaosEngine harness evals for capability and regression
coverage (issue 6519). This is the safety floor for harness self-improvement: every
change under `chaos-engine/` runs the suite through the PR Gate before merge.

## Corpus and runner

| Piece | Path |
| --- | --- |
| Manifest | [`evals/harness-suite/manifest.json`](../evals/harness-suite/manifest.json) |
| Runner | `python3 scripts/ci/chaos_engine_harness_eval_suite.py` (repo-only) |
| Machine report | `python3 scripts/ci/chaos_engine_harness_eval_suite.py --json` (repo-only) |
| Validate only | `python3 scripts/ci/chaos_engine_harness_eval_suite.py --validate-only` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_harness_eval_suite -v` |
| PR Gate check | `harness-eval-suite-contract` (surface `harness-eval`, paths `chaos-engine/**`) |

## Sets

- **capability** — current outcome contracts (installer golden path, doctor /
  entry locators, hooks, retrieve justification). These prove the harness can
  still do the job.
- **regression** — closed CE failures pinned by issue number. These prove a
  fixed bug stays fixed.

Tasks grade **outcomes** (unittest exit status), not process traces. Each task
declares `k` (attempts). The runner reports per-task `pass@k` and the suite
mean `pass_at_k`. The PR gate threshold is `pass_at_k == 1.0` with `default_k
== 1` until nondeterministic tasks exist.

## Failure ratchet

Do **not** weaken a task to look green ([ethical-conduct](ethical-conduct.md)).

1. Fix the harness so the outcome holds.
2. If the task itself is wrong, replace it with a stricter outcome check and
   cite the source issue in the PR.
3. Slow live-installer jobs stay out of this suite; keep PR tasks fast and
   deterministic.

## Related

- Hook policy fixtures: [eval-parity-fixtures](eval-parity-fixtures.md)
- Script-first catalog: [zero-llm-catalog](zero-llm-catalog.md)
