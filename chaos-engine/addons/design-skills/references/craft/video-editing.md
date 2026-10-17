---
name: video-editing
description: Use when footage, captures, and graphics are cut into a film.
---

# C04 Video editing (craft)

## Use when

Footage, captures, and graphics are cut into a film. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Murch's rule of six: emotion, story, rhythm before eye-trace and screen geometry.
2. Cut on motion, never on rest; start shots already moving.
3. Every shot buys one belief; cut the shot that buys nothing.
4. Rhythm varies: metronomic cuts read as a template.

## Do

- Use J and L cuts to carry sound across picture cuts.
- Trim heads and tails to the action; leave 2-4 f breath before a payoff.
- Watch at full speed, then muted, then at half size.

## Don't

- Crossfade between every scene.
- Shots that outstay their read (> 4 s in social, > 7 s in explainers without new information).
- Dead air over 0.7 s without intent.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Shot length, social | 0.3-4 s, varied | motionmaxxing SKILL.md:135; platform data (Wistia, TikTok) | `cutrhythm --max-shot 4` |
| Shot length, explainer | <= 7 s unless the picture changes | explainer practice (Vox, Kurzgesagt) | `static --max` |
| Dead air | <= 0.7 s | D13 cutting-pacing | `silence` |
| Hold before payoff | 0.6-1.5 s designed breath | motionmaxxing motion.md sec 1.7 | review |

## Anti-patterns

- Every cut on a beat with identical length.
- Jump cuts inside a capture that hide a lie.
- Transitions doing the job of a missing idea.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Template rhythm, long dead holds, cuts on rest.
- **3 (competent):** Clean continuity and pacing; checks pass.
- **5 (expert):** Rhythm that breathes and builds, cuts land on motion, every shot earns its time.

## Verify

- `design_qc.py cutrhythm|static|silence|blackfreeze <file>`

## References

Twenty verified references with techniques: [research/video-editing.md](../research/video-editing.md).
