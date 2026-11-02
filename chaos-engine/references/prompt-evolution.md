# Prompt and skill evolution (eval-scored)

Phase-3 harness learning (issue 6518; GEPA 2507.19457). Candidates are
`prompt` or `skill` text. A candidate is accepted only after a harness eval
suite report shows task `reg-prompt-evolution-6518` at pass@k 1.0 inside a
suite that itself passed. There is no model score. Accepted files stay under
`.chaos-engine-state/prompt-evolution/accepted/` and do not replace shipped
`SKILL.md` files.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`prompt_evolve.py`](../prompt_evolve.py) |
| CLI | `python3 chaos-engine/prompt_evolve.py propose\|score\|accept\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_prompt_evolution_6518 -v` (repo-only) |
| PR Gate check | `prompt-evolution-contract` (surface `prompt-evolution`) |
| Suite task | `reg-prompt-evolution-6518` |

## Rules

1. `propose` stores privacy-safe prompt or skill text and leaves it unaccepted.
2. `score` reads a suite report. It refuses a failing suite, a missing pass@k, or a report that did not pass this task.
3. `accept` refuses a candidate that has no passing score, and refuses both commands when the suite manifest drops this task.
4. Candidate text is refused when it holds a secret, an absolute path, a URL, or a backtick.

## Related

- External grounds stay in [external reflection](external-reflection.md).
- Report shape comes from [harness eval suite](harness-eval-suite.md).
