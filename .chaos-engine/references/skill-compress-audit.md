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

## Apply gate (S4 #8 / #5665) — opt-in draft PRs

Propose-only remains the default. Draft PRs are available **only** via
[`draft-skill-pr.md`](draft-skill-pr.md) / [`draft_skill_pr.py`](../draft_skill_pr.py):

1. Explicit `--opt-in` **and** `CHAOS_ENGINE_DRAFT_SKILL_PRS=1` (default OFF)
2. `python3 scripts/ci/chaos_engine_eval_parity.py` green
3. Focused unit tests green
4. `gh pr create --draft` only (`--execute`); **never** auto-merge; never apply without gate

```bash
python3 .chaos-engine/draft_skill_pr.py status
CHAOS_ENGINE_DRAFT_SKILL_PRS=1 python3 .chaos-engine/draft_skill_pr.py open --opt-in
```
