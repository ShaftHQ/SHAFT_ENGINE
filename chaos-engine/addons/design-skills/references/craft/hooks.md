---
name: hooks
description: Use when the first 1-3 s, a thumbnail, poster frame, or README GIF is designed.
---

# C10 Hooks, thumbnails and posters (craft)

## Use when

The first 1-3 s, a thumbnail, poster frame, or README GIF is designed. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. The first frame is the payoff (or the problem) and works alone as a poster.
2. Promise something specific and deliver it within the piece.
3. Something changes within the first second.
4. Design for sound off and small size.

## Do

- Write five hook lines and three poster frames; pick by the squint test.
- Put the product outcome, not the logo, in frame 1.
- Brand within 3-5 s, not frame 1.

## Don't

- Black or logo-only first frame (`hook`).
- Clickbait the piece cannot deliver.
- Text over 7 words on a thumbnail.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| First frame | luma std >= 12 (not flat) | poster practice; Meta and Google creative guidance | `hook` |
| First change | within 1 s | TikTok/Meta first-3-s guidance | `hook --window 1` |
| Hook length | 3-6 s | motionmaxxing SKILL.md:135; Google ABCD | review |
| Thumbnail | 1280x720, <= 7 words, readable at 160px | YouTube Help | review |
| Headline size | 7-11% of frame height | motionmaxxing SKILL.md:134 | review |

## Anti-patterns

- Slow logo reveal opening.
- Generic stock-like hero shot.
- Thumbnail that reads only at full size.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Flat or logo-first opening; nothing happens in 1 s.
- **3 (competent):** Clear promise and quick change; generic.
- **5 (expert):** A first frame people would screenshot, a promise kept, curiosity that pulls the next 3 s.

Hook card over speech (verbatim, ends with the speech, script-aware gate): [L01](../lessons/hook-speech-match.md).

## Verify

- `design_qc.py hook|contrast <film|thumb>`

## References

Twenty verified references with techniques: [research/hooks.md](../research/hooks.md).
