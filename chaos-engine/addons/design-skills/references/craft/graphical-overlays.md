---
name: graphical-overlays
description: Use when lower thirds, callouts, captions, or data graphics sit on footage.
---

# C08 Graphical overlays (craft)

## Use when

Lower thirds, callouts, captions, or data graphics sit on footage. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Overlays annotate the picture; they never cover the proof.
2. Label directly on the data or UI; avoid legends.
3. Grey everything, highlight one thing.
4. Captions are content, not decoration: timed to speech, two lines max.

## Do

- Anchor callouts to the element with a leader or zoom.
- Keep a consistent overlay system (position, type, colour) across the film.
- Test overlays at half size.

## Don't

- Lower thirds styled as generic web cards with a status dot.
- Three callouts at once.
- Burned captions inside platform UI zones (`safezones`).

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Caption line | <= 42 chars, <= 2 lines | Netflix Timed Text Style Guide | `captions --max-chars 42` |
| Caption rate | <= 20 cps | Netflix TTSG | `captions --max-cps` |
| Caption duration | 0.83-7 s | Netflix TTSG | `captions` |
| Overlay text contrast | 4.5:1 on its plate | WCAG 1.4.3 | `contrast` |
| Concurrent callouts | 1 (max 2) | Storytelling with Data | review |

## Anti-patterns

- Data-ink waste: 3D bars, gridlines everywhere.
- Callouts that animate longer than they are read.
- Captions overlapping UI text.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Overlays cover the proof; inconsistent styles.
- **3 (competent):** Consistent, readable, timed; standard.
- **5 (expert):** Overlays that make the proof faster to understand than the footage alone.

## Verify

- `design_qc.py captions|contrast|safezones`

## References

Twenty verified references with techniques: [research/graphical-overlays.md](../research/graphical-overlays.md).
