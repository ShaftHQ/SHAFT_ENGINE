---
name: visual-first-approach
description: Use when choosing or iterating a visual/media approach (avatar, lip-sync, talk video, stills) before burning multiple passes.
---

# L08 Visual-first approach

## Use when

A visual or media pipeline is being chosen or iterated (talking avatar,
lip-sync, drive-footage overlay, talk edit, generated stills), or numeric
QC is about to declare a pass ready for owner review.

## Rules

1. **Approach prototype first.** Before multi-pass work, ship one cheap
   visual prototype and get an **owner glance**. Do not lock an approach
   (for example mouth-inpaint on old drive footage) across five numeric
   passes. A dead-end approach fails early on looks, not after metrics.
2. **Visual / human gate first.** Numeric metrics (lip correlation,
   face-embedding similarity, speaker-embedding similarity, motion MAD,
   loudness, length) are **secondary**. They may pass while the picture
   is still a fail (owner ~2–4/10). Never call a render ready for
   handoff on metrics alone; require a human/visual gate with frame
   evidence (contact sheet or timed strips). For acceptance, blind A/B
   vs real reference, floors-only metrics, NaN fail-closed, and reviewer
   eye-check rules, see [L13 perceptual acceptance](perceptual-acceptance.md).
3. **AV lag sign by sweep.** Prove lag direction and magnitude with a
   delay sweep (grid of offsets, pick best by both metric and ear/eye).
   Do not guess the sign or a single delay from one trial.
4. **Checkpoint long media jobs.** Keep a durable `PROGRESS.md` (or the
   host checkpoint ledger) with a `Resume at:` line so box reboots do not
   drop job progress. Pair with [bot-entry](../../../../references/bot-entry.md)
   session checkpoints.

## Verify

- A written approach note names the prototype path, the owner glance
  result (pass / fail / change approach), and the date before pass 2+.
- Handoff packages include visual evidence (frames/strips) **and** numeric
  QC; numeric-only ready claims fail review.
- Lag fix notes cite the sweep table (offsets tried, metric + visual pick),
  not a single guessed delay.
- `PROGRESS.md` (or equivalent) updated this session with `Resume at:`.

## Sources

Field retro: multi-pass mouth-inpaint green on metrics, fail on visual;
lag sign errors; checkpoint survival across box reboots.
