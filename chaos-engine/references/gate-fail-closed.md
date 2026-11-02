# Gate fail-closed template

## Rule

Every new observation / receipt / inventory gate **FAIL**s when a required
input is missing, empty, unreadable, or the wrong JSON shape. Never PASS on
absent evidence. Prefer
[`gate_fail_closed.py`](../gate_fail_closed.py)
(`require_file`, `require_observations`, `fail_hits`).

## Why

Designer review caught gates that passed with zero observations or missing
files after adversarial review had signed off (prior designer-review miss class).

## Check

- Unit tests assert missing path / empty observations → FAIL hits.
- New gate PRs include a missing-input test before the happy path.

## Follow-up

[doc-claim review](doc-claim-review.md), design-skills Verify bullets.
