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

1. Pause tightening for talk: cut dead air longer than 0.8 s, with at most
   2 splices per Short. Keep 0.2 s margins and start the card on a whole
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

## Verify

- [ ] Every splice falls on a pause of more than 0.8 s, between sentences;
      there are no more than 2 per Short.
- [ ] The last frame plays after the last word, on an RMS dip; checked by
      ear.
- [ ] A frame at the midpoint of each overlay crossfade shows no
      see-through panel.

## Sources

- [Walter Murch, *In the Blink of an Eye*](https://www.goodreads.com/book/show/77012.In_the_Blink_of_an_Eye)
- [auto-editor (silence-based cut lists)](https://github.com/WyattBlue/auto-editor)
- Field lessons from one talk-Shorts batch, verified on rendered frames.
