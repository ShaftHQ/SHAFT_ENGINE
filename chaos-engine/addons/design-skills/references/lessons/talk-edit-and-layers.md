---
name: talk-edit-and-layers
description: Use when tightening a talking-head or Q&A edit, choosing cut points, or crossfading overlays and masking burned-in banners.
---

# L03 Talk edit and layers

## Use when

A talk or Q&A clip is being tightened, or overlays change over footage.
It extends [D13 cutting](../cutting-pacing.md) and
[C08 overlays](../craft/graphical-overlays.md).

## Rules

1. Pause tightening for talk: cut dead air longer than 0.8 s, with max 2 cuts, at pauses, never mid-word, 30 ms crossfade
   (an audio crossfade at each splice). Keep 0.2 s margins and start the card on a whole
   frame. Keep the 200-300 ms rule for fast edits only.
2. No mid-sentence jump cuts: cut only at pauses between thoughts.
3. Captions and word timestamps stay in pre-cut time and the renderer
   retimes them. Checks that measure a span need the pre-cut length.
4. End on a complete thought, with the cut on an RMS dip after the last
   word. Confirm it by listening to a short snippet.
5. ASR on code-switched audio: use 2 or more models plus the source text as
   evidence. A word that overlaps a cut takes its end time from RMS. ASR
   diffs are review evidence; waivers are human decisions recorded in the
   review file.
6. Crossfades between two overlays show what is under them at mid-fade.
   Make the persistent element (panel plus label) an opaque base and fade
   only what changes.
7. Burned-in source banners: mask (blur) the rows rather than re-cropping
   the frame.
8. End card: append it after the content span; it never overlaps speech.
   Fade the span audio out over 0.3 s and hold the panel through the fade.
9. Big content jumps (a splice that removes a long bridge, ≫ a pause)
   need a **soft transition**: keep the 30 ms audio crossfade and add a
   short zoom punch-in (~0.4–0.8 s after land) or another soft mask so
   the cut does not read as a glitch. Prefer keeping 1–2 bridge sentences
   when length allows. Gate: `design_qc.py softjump plan.json`.
10. Antecedent check before splice lock: scan the kept span for pronouns
    and epithets; ensure the referent noun/setup remains (or rewrite so
    the referent is self-contained). See also [L06](caption-video-qc.md).
11. **G17 per-splice land:** for each cut `a-b`, the first cue at/after `b`
    must not open on a dangling binder pronoun (`هو`/`هي`/`ده`/`دي`/…). A
    binder noun must sit in **kept-prior** speech (cues ending ≤ `a`).
    Same-cue Latin after `هو` does not bind. Prefer extending the cut or
    landing on a self-contained opener. Gate:
    `design_qc.py antecedent pre.srt --splice a-b,c-d --noun <binder>`
    (project binder lists stay out of CE; pass via `--noun`).

## Verify

- [ ] Every splice falls on a pause of more than 0.8 s, between sentences;
      there are no more than 2 per Short, none mid-word,
      each with a 30 ms audio crossfade.
- [ ] The last frame plays after the last word, on an RMS dip; checked by
      ear.
- [ ] A frame at the midpoint of each overlay crossfade shows no
      see-through panel.
- [ ] Per-frame fade check: the end card starts after the last word, the
      audio fades over 0.3 s, and the panel is held on every fade frame.
- [ ] `design_qc.py softjump plan.json` passes for every big jump (soft /
      punch-in / crossfade flagged when `removed_s` is large).
- [ ] Antecedent check passes on the kept span after every splice.
- [ ] `design_qc.py antecedent pre.srt --splice … --noun …` passes (G17;
      no dangling land opener).

## Sources

- [Walter Murch, *In the Blink of an Eye*](https://www.goodreads.com/book/show/77012.In_the_Blink_of_an_Eye)
- [auto-editor (silence-based cut lists)](https://github.com/WyattBlue/auto-editor)
- Field lessons from one talk-Shorts batch, verified on rendered frames.
