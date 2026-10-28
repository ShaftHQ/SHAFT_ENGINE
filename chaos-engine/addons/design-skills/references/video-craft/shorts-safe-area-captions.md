---
name: shorts-safe-area-captions
description: Use when laying out a 9:16 Short, grouping and timing burned captions, choosing a caption blend over footage, or timing cards over a talking head.
---

# V07 Shorts safe area and caption groups

## Use when

A vertical Short (1080x1920) gets text, captions or cards. Extends
[C11 social video](../craft/social-video.md), [D18 captions](../captions-subtitles.md)
and [L06](../lessons/caption-video-qc.md).

## Rules

1. **UI map in px:** nothing in the top 168 px; every word inside y
   168-1330; below 1330 px decoration only; no text under the right action
   rail (its width depends on the platform: measure it).
2. **Frame 0 is the thumbnail and the hook:** a face, the result or one
   bold line on screen; never black, never a logo. Captions start
   0.1-0.3 s in; a cue at exactly 0 s looks pre-existing.
3. **Portrait type** is larger than landscape (titles about x1.35);
   nothing meant to be read under 28 px; a claim line holds about 5
   English words: cut words, never shrink.
4. **Caption group size by energy:** 2-3 words high energy, 3-5
   conversational, 4-6 calm. Break at a sentence end, a pause of 150 ms or
   more, or the word limit. One group visible at a time.
5. **Hard kill per group:** fade 0.12 s, then hide it at its end, so no
   stale group stays on screen.
6. **Fade the group or its words, never both:** the opacities multiply and
   read as a pop.
7. **Blend by brightness behind the text (0-255):** under 60 `screen`,
   60-180 `overlay`, over 180 `normal` with opaque colour. Plain
   subtitles over 60 get a text-sized 30-40% scrim, never a full-width bar.
8. **ASR guards:** drop words that start inside a final silence
   (`silencedetect=noise=-35dB:d=0.6`); more than 20% `♪` or `�`
   tokens means the transcript failed: rerun a larger model. Match
   transcript words to caption words by position, not text. English-only
   models translate other languages: for Arabic pass a multilingual model
   and the language.
9. **Pre-placement scan:** a 1 fps contact sheet finds source footage that
   already carries burned-in subtitles.
10. **Text behind the speaker** reads best 30-55% covered; re-lay out when
    a content word is 80% covered or two adjacent words 50% each. Measure
    per cue window: heads move.
11. **Cards over a talking head:** in about 0.4 s ease-out, out about
    0.35 s ease-in; a static card dies after about 8 s, past 15 s it needs
    a staged reveal; about one card per 6-8 s under a minute. Reframe only
    in the gaps between cards.
12. **Clamp** the last cue and card end to the ffprobe media length, or
    the render ends in black.
13. **Sparse keyframes** (over 1 s apart) freeze on seek: re-encode with
    `-g <fps>` before editing.
14. **Frame-measured contrast gate:** CSS stroke intent ≠ WCAG on light
    wardrobe/walls. Measure white-on-(pill∘local-bg) per cue (mid +
    ~150 ms after in). Floor ≥ **4.5** WCAG; Shorts burned-caption
    target ≥ **7** (pilot-raised after navy pill + stroke cleared easily;
    stroke alone is not enough on collars). Persist `contrast_gate.json`.
    Scrim/box moves the ratio.
15. **Hook→caption chrome family:** keep the same lower-third
    stroke/scrim under the hook card and post-hook captions; prove the
    handoff with frames at ~0.8 / 1.5 / 1.7 s (see [L01](../lessons/hook-speech-match.md)).

## Verify

- [ ] `design_qc.py safezones boxes.json --px-map 9x16 [--rail <px>]`
- [ ] `design_qc.py textfloor boxes.json` (2.6% of the short side, 28 px
      on portrait)
- [ ] `design_qc.py firstframe short.mp4 --cues cues.srt`
- [ ] `design_qc.py capluma short.mp4 --cues cues.srt --box x,y,w,h [--plain]`
- [ ] `design_qc.py mediaclamp cues.srt short.mp4`
- [ ] Notes record the subtitle scan, the occlusion check and the group sizes.
- [ ] `contrast_gate.json` exists; every post-hook cue ≥ 4.5 (Shorts target ≥ 7).
- [ ] Hook→caption frames at ~0.8 / 1.5 / 1.7 s share chrome family.

## Sources

- [heygen-com/hyperframes](https://github.com/heygen-com/hyperframes/tree/1a238171ec55896a9cb8452c33e4e0fe1063a60c/skills/hyperframes/references) @ `1a238171ec55896a9cb8452c33e4e0fe1063a60c`
  (Apache-2.0, adapted): `types/vertical.md`, `captions.md`, `overlays.md`.
