# Periodic meta-optimize over shared logs (#5664 / Top 10 #9)

Module: [`meta_optimize.py`](../meta_optimize.py).

Offline/periodic script that aggregates significance marks + learning queue
metrics + `skill_compress_audit` proposals into a **bounded review summary**
and optional **issue candidates**.

## Policy

| Do | Don't |
| --- | --- |
| Run periodically (weekly / post Learning Session burst) | Wire into SessionStart |
| Script-first aggregation (zero/low LLM) | Continuous Task Observer |
| Emit propose-only issue candidates | Auto-open issues or mutate skills |
| Leave skill apply to opt-in draft-PR gate (#5665) | Always-on observation |

## CLI

```bash
python3 .chaos-engine/meta_optimize.py cadence
python3 .chaos-engine/meta_optimize.py review
python3 .chaos-engine/meta_optimize.py review --write   # .chaos-engine-state/meta-optimize/last-review.json
```

## Operator cadence

1. After a delivery / Learning Session burst, or on a weekly operator schedule.
2. Run `review` and triage `issueCandidates` with `gh issue create` (human).
3. Skill compress proposals stay propose-only until the **opt-in** draft-skill-PR
   gate (#5665 / Top 10 #8) passes eval-parity + unit tests.
4. **Never** SessionStart / always-on Task Observer.

## Reject

Always-on Task Observer; continuous SessionStart full scan; auto-merge skill
mutations.
