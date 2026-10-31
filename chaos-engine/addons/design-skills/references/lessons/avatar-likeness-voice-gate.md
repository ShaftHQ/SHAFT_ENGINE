---
name: avatar-likeness-voice-gate
description: >-
  Use when signing a digital-human / avatar / talking-head shot that claims to
  match a real subject; require blind likeness and voice acceptance by a fresh
  independent reviewer.
---

# L56 Digital-human likeness + voice acceptance

## Use when

A digital-human, avatar, or talking-head render claims to match a real
**subject / presenter** (face and voice). Extends [L13](perceptual-acceptance.md),
[L20](avatar-voice-first.md), [L39](video-signoff-viewer-frames.md), and
[L48](avatar-wardrobe-gate.md).

## Rules

1. **Fresh independent reviewer.** The builder (or anyone who generated or
   tuned the shot) cannot own the acceptance. A new reviewer who has not
   worked the render loop runs the check.
2. **Blind likeness side-by-side.** Before reading metrics or prior notes, the
   reviewer compares the render to **real reference photos and/or video** of
   the subject in a blind side-by-side. Identity drift, wrong face, or "not
   the subject" is a hard FAIL.
3. **Blind voice vs real reference audio.** Separately, compare the render
   audio to **real reference audio** of the subject (same script window when
   available). Wrong timbre, identity, or language rhythm is a hard FAIL —
   voice stays the first dealbreaker ([L20](avatar-voice-first.md)).
4. **Any doubt = FAIL.** Hesitation, "close enough", or unresolved disagreement
   fails. Only an unambiguous pass on **both** axes may proceed.
5. **No sign-off without both.** A SIGN that skips likeness, skips voice, or
   records only one axis is void. Gate: `design_qc.py likenessvoice` with a
   required observations JSON (independent + blind + likeness + voice).
6. **Personal names stay out of repo text.** Cards, gates, and receipts say
   "the subject" / "the presenter", never a personal name.

## Verify

- [ ] `design_qc.py likenessvoice out.mp4` without `--observations` fails
      (`missing-observations`).
- [ ] Observations missing `independent_reviewer: true`, `blind: true`,
      likeness refs, or voice refs → FAIL.
- [ ] `verdict` of `doubt` or `fail` on either axis → FAIL; both `pass` with
      side-by-side refs → PASS.
- [ ] L39 done-checklist includes the likeness + voice block; a SIGN without
      both receipts is void.

## Why

A coach-style sign-off that skipped blind likeness and voice accepted a shot
that was not the subject's face and not the subject's voice. Metric-green and
wardrobe-green packages still fail identity. Blind dual-axis acceptance by a
fresh reviewer is the only ship bit.

## Sources

Field retro 2026-10-31 (digital-human shot rejected: not the subject's face,
not the subject's voice; prior sign-off missed it). Companions: L13, L20, L39,
L48, L53.
