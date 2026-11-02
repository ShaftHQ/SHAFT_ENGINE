---
name: viral-pacing-flag
description: Use when a viral short’s title claim lands late in the kept window; FLAG advisory, never hard-fail on pacing alone.
---

# L21 Viral pacing FLAG

## Use when

A `type=viral` Short has a long content window and the ear-locked title
claim (punchline) lands late. Extends [D13](../cutting-pacing.md) and
[L19](hard-cut-coherence.md); does not replace privacy or silence gates.

## Rules

1. **Advisory FLAG, not FAIL.** When `punchline_at / content_dur` exceeds
   the threshold (default **0.65**), record a pacing **FLAG**. Do not
   REJECT a privacy-clean viral on pacing alone until a hard gate ships.
2. **Viral only.** `type=qa` / other types PASS this check (N/A).
3. **Punchline = ear-locked title claim.** Use the absolute second where
   the spoken title claim starts; do not confuse sparse mid-SRT with
   silence — cross-check silence / SRT gaps separately.
4. **Schedule may demote.** A FLAG may go last in upload order; optional
   tighter contiguous cut is a later edit, not a SIGN block.

## Verify

- [ ] `design_qc.py viralpacing --content-dur D --punchline-at T --type viral`
      returns `status=flag` (exit 0) when `T/D > 0.65`.
- [ ] Same inputs with `T/D ≤ 0.65` return `status=pass`.
- [ ] `--type qa` returns `status=pass` even when `T/D` is late.
- [ ] Review JSON records `punchline_rel` and `content_dur`.

## Why

A long viral with a late payoff can still be publishable. Blocking on
pacing alone trades a clean privacy cut for a taste call. FLAG keeps the
signal without a hard reject.

## Sources

Field lesson from a content QA recut batch: long viral SIGN with pacing
FLAG when the title claim landed near the end of a continuous window.
