# SkillOpt-style SKILL.md compression audit (#5659 / Top 10 #6)


Module: [`skill_compress_audit.py`](../skill_compress_audit.py).
Offline/script audit that **proposes** bounded compress actions for skill
bodies. **Never** auto-applies or auto-merges skill patches.

## Policy

| Do | Don't |
| --- | --- |
| Propose filler/bloat + refs-split hints | Write `SKILL.md` from the audit CLI |
| Gate future apply on eval-parity + unit tests | Weekly auto-mutate live skills |
| Keep L2 discipline (&lt;500 lines) measurable | Treat propose diffs as merged |

## CLI (zero-LLM)

```bash
python3 .chaos-engine/skill_compress_audit.py audit
python3 .chaos-engine/skill_compress_audit.py audit --skill self-improve --diff
python3 .chaos-engine/skill_compress_audit.py audit --strict   # exit 1 if over L2 budget
```

`--diff` embeds bounded unified-diff **stubs** in JSON only — files stay unchanged.

## Apply gate (future / S4 #8)

Any apply path must remain **opt-in** and pass:

1. `python3 scripts/ci/chaos_engine_eval_parity.py` (fixtures green)
2. Unit tests for the skill / compress invariants
3. Human issues-first PR — never auto-merge

Until then: propose-only.
