---
name: transitions
description: Use when beat A becomes beat B.
---

# C09 Transitions (craft)

## Use when

Beat A becomes beat B. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Shots hand off: the last frame of A is the first frame of B (a carried object, a matched shape, a shared element).
2. A hard cut means something; most joins are cuts on motion.
3. A transition starts in A and finishes in B.
4. Sound sells the join: a whoosh peaks on the cut, silence before the hit.

## Do

- For music-led Shorts, use the beat-sync vocabulary in [D13](../cutting-pacing.md)
  (hit, anticipation, backbeat hold, match, J/L, ramp).

- Match on shape, direction, or colour.
- Use shared-element (container transform) morphs for UI continuity.
- Motion-blur whip pans across the cut, matched on both sides.

## Don't

- Colour-flood or circle wipes between every beat.
- Crossfades as the default join.
- Transitions longer than 0.5 s in social cuts.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Transition length | 6-15 f; whips 7-8 f | motionmaxxing motion.md sec 2.1 | review |
| Crossfade share | <= 1 per film unless calm brand | motionmaxxing SKILL.md:50 | review |
| Flash | no more than 3 flashes per second | WCAG 2.3.1 | `flash` |

## Anti-patterns

- Template packs (spin, zoom, glitch) as style.
- A transformation chain as the whole idea.
- Fade to black at the end by default.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Default crossfades and wipes everywhere.
- **3 (competent):** Clean cuts and a few matched joins.
- **5 (expert):** Joins that feel inevitable; the viewer never notices the edit but feels the flow.

## Verify

- `design_qc.py flash|flatframes <film>`

## References

Twenty verified references with techniques: [research/transitions.md](../research/transitions.md).
