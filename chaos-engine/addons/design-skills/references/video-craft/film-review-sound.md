---
name: film-review-sound
description: Use when reviewing a finished cut or film, writing review notes, building review sheets, or placing SFX and music under voice.
---

# V08 Film review and sound

## Use when

Any film (Short, explainer, long video) is reviewed before delivery.
Extends [D20 delivery QC](../delivery-qc.md) and [D15 mix and loudness](../audio-mix-loudness.md).

## Rules

1. **Review notes are one line each:** `t=4.2s · where · problem -> exact
   change · what stays`, ranked P0/P1/P2; never a score out of 10. Fix
   text first, then timing, then detail; re-check only what changed.
2. **Measured flags** (a flag is a question: fix it, or keep it on purpose
   and say why): a line on screen shorter than its read time; more than
   3 s with nothing changing; text under 2.6% of the frame's short side;
   an empty first frame; a film over 15 s with no sound.
3. **Review sheets:** each scene's start and middle, a strip across every
   cut (-0.2 s / +0.2 s), and beat frames at 360 px phone width.
4. **Words-covered test:** the beat frames alone still tell the story.
5. **SFX:** every effect has a cause on screen; its peak lands one frame
   after the picture; hits sit 3-6 dB over the bed; rotate 2-3 takes of a
   repeated sound; one riser into an impact per film. Music under voice
   at about 0.14 gain; a bed whose energy sits mostly under 150 Hz
   vanishes on phones.
6. **Delivery note** says what was measured and what was only looked at
   ("sound measured, not heard").

## Verify

- [ ] `design_qc.py deadair film.mp4` (static picture with silent audio
      over 3 s; a film over 15 s with no audio stream)
- [ ] `design_qc.py firstframe film.mp4` and `design_qc.py textfloor boxes.json`
- [ ] Review notes use the one-line format with P0/P1/P2 ranks.
- [ ] The delivery note lists measured vs looked-at checks.

## Sources

- [heygen-com/hyperframes](https://github.com/heygen-com/hyperframes/tree/164d6c7a36eba26e9126b364beab98437e2eeec7/skills/hyperframes/references) @ `164d6c7a36eba26e9126b364beab98437e2eeec7`
  (Apache-2.0, adapted): `review.md`, `sound.md`.
