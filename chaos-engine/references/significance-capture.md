# Significance-filtered mid-session capture (#5658 / Top 10 #4)


Module: [`significance.py`](../significance.py).
Capture **only significant friction** — corrections, repeated failures, missing
skill coverage, denials, doctor recovery — without becoming Task Observer.

## Policy

| Do | Don't |
| --- | --- |
| Soft PostToolUse / deny marks → tiny state JSON | Always-on observation every turn |
| Learning Session **drain** as deferred heavy path | Load self-improve refs mid-turn |
| SessionStart **locator only** | Inject mark prose into SessionStart |
| Explicit `significance.py mark` for corrections | Continuous Task Observer protocol |

## State

`.chaos-engine-state/significance/marks.json` — bounded, privacy-safe notes.

## CLI (zero-LLM)

```bash
python3 .chaos-engine/significance.py mark --kind correction --note "Prefer silent verify"
python3 .chaos-engine/significance.py list
python3 .chaos-engine/significance.py drain
python3 .chaos-engine/significance.py locator
python3 .chaos-engine/significance.py filter-check --failed
```

Kinds: `correction`, `repeated-failure`, `missing-skill`, `denial`, `doctor-recovery`.

Soft hook path (guard): fail/deny only by default. Env
`CHAOS_ENGINE_SIGNIFICANCE_CAPTURE=1` does **not** enable always-on capture —
happy-path edits remain filtered out.

## Learning Session

`learning_session.py finalize` drains pending marks into the completion receipt
(`significanceDrained`, `significanceKinds`) for dual-track review.
