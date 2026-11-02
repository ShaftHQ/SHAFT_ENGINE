# SPEC: Repeat-failure PDCA

## Problem

Agents burn sessions chaining fix-after-fail on the same CI stage or local gate
without a written root cause, blowing the session/budget cap and hiding the
real inflation.

## Rule

Stop on: two consecutive failures of the same stage/gate; budget overrun; or
two distinct failure kinds on the same stage. Write root-cause note → plan →
one change → check → act. No chained fixes.

## Module

[`lessons_repeat_failure_pdca.py`](../rules/lessons_repeat_failure_pdca.py).
Tests: `tests/scripts/test_ce_lessons_repeat_failure_pdca.py`.
Always-loaded wiring: `references/bot-entry.md`, `skills/chaos-engine/SKILL.md`.
