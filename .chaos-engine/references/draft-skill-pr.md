# Eval-gated draft skill PRs — opt-in phase 2 (#5665 / Top 10 #8)

Module: [`draft_skill_pr.py`](../draft_skill_pr.py).

Proposed skill patches from [`skill_compress_audit`](skill-compress-audit.md) /
[`meta_optimize`](meta-optimize.md) may become **draft PRs only** when every
gate below passes. **Default OFF** for all hosts.

## Gates (all required)

1. Explicit CLI `--opt-in`
2. Env `CHAOS_ENGINE_DRAFT_SKILL_PRS=1`
3. eval-parity fixtures green (`python3 scripts/ci/chaos_engine_eval_parity.py`)
4. Focused unit tests green (`test_chaos_engine_s2_self_improve`,
   `test_chaos_engine_s4_self_improve`)

## Policy

| Do | Don't |
| --- | --- |
| Open **draft** PRs after gates | Auto-merge skill mutations |
| Keep default OFF | Apply SKILL.md without gate |
| Document overlay for all hosts | Always-on Observer / SessionStart mutate |

## CLI

```bash
# Default OFF — status shows enabled=false
python3 .chaos-engine/draft_skill_pr.py status

# Prepare propose-only artifact (never mutates SKILL.md)
python3 .chaos-engine/draft_skill_pr.py prepare --skill self-improve

# Dry-run open (still requires opt-in + env); default does not call gh
CHAOS_ENGINE_DRAFT_SKILL_PRS=1 python3 .chaos-engine/draft_skill_pr.py open --opt-in

# Execute draft PR create (human/operator only after gates)
CHAOS_ENGINE_DRAFT_SKILL_PRS=1 python3 .chaos-engine/draft_skill_pr.py open --opt-in --execute
```

Without `--opt-in` **or** without the env var, `open` exits non-zero and creates
no PR. `--execute` is required to call `gh pr create --draft`; dry-run is the
safe default. Auto-merge is never enabled (never auto-merge).

## Reject

Auto-merge skill mutations; apply without gate; continuous observation.
