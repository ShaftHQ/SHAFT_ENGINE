# Doc-claim vs code (review checklist)

## Rule

Independent reviewers must verify every **Verify** bullet on a lesson card
maps to a real command, flag, default, or unit test in the same PR. If the
card says "fail closed" or "required", the code path and a missing-input test
must exist. Doc-only claims without code are a review FAIL.

## Checklist block (copy into review)

- [ ] Each Verify bullet names a command/flag that exists in this diff
- [ ] Missing/empty input has a FAIL test
- [ ] Defaults match the card (fail-closed, not opt-in)
- [ ] Personal names / client data absent from repo text

## Why

Adversarial review passed packages whose docs over-claimed behaviour; domain
review caught the gap.

## Follow-up

[gate-fail-closed](gate-fail-closed.md),
[design-review-video-checklist](design-review-video-checklist.md).
