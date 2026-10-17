---
name: motion-graphics-craft
description: Use when kinetic type, UI animation, or graphic scenes are built (with D08/D09).
---

# C07 Motion graphics (craft)

## Use when

Kinetic type, UI animation, or graphic scenes are built (with D08/D09). Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Land decelerates, leave accelerates: most travel in the first 4-8 f, long tail, no bounce.
2. Entries move (offset, scale, blur); opacity is a 4-6 f helper only.
3. Arrive from oversize; nothing grows from zero.
4. One system per film: one clock, one blur grammar, one accent meaning, at most one visible spring.

## Do

- Stagger siblings 2-3 f (CTA 8 f).
- Give each sibling a different offset and tail.
- Keep one living thing in a held frame (drift, caret, light).

## Don't

- Linear or one shared ease for everything (`easing`).
- Opacity-only fades between scenes (`fadeonly`).
- Page chrome: counters, timecodes, tracked caps labels (`chrome`).

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Default move | 10-12 f (0.33-0.4 s); median declared <= 0.6 s | motionmaxxing motion.md:27 (R1.1) | `motiondur` |
| Opacity helper | 4-6 f | motion.md:73 (R1.6) | `fadeonly` |
| Visible springs | <= 1 per film; overshoot ~8% peak late | motion.md:96-97 | `springs` |
| Word stagger | 2-12 f by mode | SKILL.md:55 | review |
| Motion band | mean 64x36 luma diff 0.5-6 | motion.md sec 1.7, calibrated on its comparison films | `motionband` |

## Anti-patterns

- Everything bounces.
- Letter-typed headlines with a caret.
- Giant cropped word as the default hook.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Linear fades, templated moves, HUD chrome.
- **3 (competent):** Correct eases and timing; checks pass; safe.
- **5 (expert):** Motion that carries meaning, one signature move spent once, a film a motion designer would screenshot.

## Verify

- `design_qc.py easing|fadeonly|springs|motiondur|chrome <html>`
- `design_qc.py motionband <film>`

## References

Twenty verified references with techniques: [research/motion-graphics-craft.md](../research/motion-graphics-craft.md).
