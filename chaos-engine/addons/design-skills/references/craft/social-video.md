---
name: social-video
description: Use when a vertical or square cut is made for Shorts, Reels, TikTok, LinkedIn, or X.
---

# C11 Social media videos (craft)

## Use when

A vertical or square cut is made for Shorts, Reels, TikTok, LinkedIn, or X. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Native first: vertical, captioned, made for sound off and a thumb scrolling.
2. Hook, proof, payoff in 10-30 s; one idea per video.
3. Keep text and CTA inside platform safe zones.
4. A real person or real product beats polish.

## Do

- Burn word-level captions inside the safe zone.
- Loop the ending into the opening when it fits.
- Make 3 variants of the hook and test.

## Don't

- Landscape video letterboxed in vertical.
- Text in the bottom 35% of a Reel.
- Over 4 s with no new picture.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Aspect, resolution | 9:16, 1080x1920 | TikTok and Meta specs | `delivery` |
| Safe zone 9:16 | clear top 14%, bottom 35%, 6% sides | Meta Reels Ads Guide | `safezones --format 9x16` |
| Length | 10-30 s for a single idea | Wistia length data; LinkedIn and X specs | review |
| Shot length | <= 4 s | platform practice | `cutrhythm` |
| Loudness | -14 LUFS, -1 dBTP | platform normalisation (YouTube) | `loudness --target -14` |

## Anti-patterns

- Corporate intro card.
- Captions under platform UI.
- Ending on a static logo for 3 s.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Repurposed landscape ad with no hook.
- **3 (competent):** Native format, captioned, safe; generic idea.
- **5 (expert):** Feels made for the feed: instant hook, real proof, a loop people rewatch.

## Verify

- `design_qc.py safezones|cutrhythm|hook|loudness|captions`

## References

Twenty verified references with techniques: [research/social-video.md](../research/social-video.md).
