---
name: cutting-pacing
description: Use when an edit needs rhythm: where to cut, how long shots last, dead-air removal, J and L cuts, and pacing per format.
---

# D13 Cutting and pacing

## Use when

A rough assembly exists and feels slow, jumpy, or confusing, or a short
vertical cut must be made from a long tutorial.

## Priorities when choosing a cut

Paraphrasing Murch's rule of six, in order: emotion, story, rhythm,
eye-trace, the two-dimensional plane of the screen, then spatial continuity.
Give up the lower ones first.

## Rules

- Tutorials: never cut away before the viewer can read a command's result;
  hold a result for at least 1.5 s after it appears.
- Remove dead air longer than 700 ms unless it is a deliberate beat recorded
  in `intentionalHolds`.
- Cut on action (a keypress, a page change, a cursor move) to hide the cut.
- J cut: the next scene's audio starts before its picture. L cut: the
  current audio continues over the next picture. Use them to carry narration
  across visual changes.
- Long waits (installs, builds) become labelled speed ramps or a cut with a
  time-skip card, never silent jumps. The badge names the speed and the REAL
  elapsed time ("sped up 24x, 16 s real").
- No visually static stretch over about 3 s, measured against the frame at
  the window start (`design_qc.py static`). A slow push-in does not read
  as motion unless it visibly changes the picture; compress the wait, or give a necessary hold VO-keyed callouts
  that move to the line being spoken.
- A hold reads alive only when real content moves. EDL holds last at most
  3 s, or 5.5 s with two or more stepping callouts or a real scroll;
  callouts alone read static past about 5 s.
- A card, stat or list slide that holds more than about 3 s after its last
  reveal gets a camera that visits each element as the narration names it,
  then pulls back for the summary line. The constant push-in can pass
  `static` and still read as a static slide to a viewer.
- Camera pushes run at a constant rate spread over the whole clip; encoders
  flatten eased or capped pushes into visible freezes.
- Pacing targets per format, written in the storyboard:
  tutorial 4 to 8 s average shot; short vertical cut 2 to 4 s with a visual
  change every 5 to 8 s; trailer 1 to 2 s.
- With music, align major cuts to beats within two frames; with no music,
  skip beat alignment.

## Workflow

1. Watch the assembly once without stopping; note where attention drops.
2. Apply removals and J/L cuts as EDL changes (D12); keep the diff.
3. Re-watch at full speed; compare shot lengths to the targets.

## Verify

- `design_qc.py silence master.mp4 --allow <holds>` passes (no unmarked
  silence over 700 ms).
- `design_qc.py static clean-master.mp4` passes (no stretch over 3 s where
  no 60 px cell changes; measure the caption-free picture, with
  `--crop` to the capture region when callouts move over it).
  A constant push-in can be flagged although it moves (the check misses
  sub-pixel motion): diff the span's first and last frames, and
  allow it (`--allow start-end`) only when a few percent of pixels change
  (3.5 to 9 % measured). Allowed spans are timestamps: re-measure them
  after every re-encode or rebuild, since they drift (1.2 s seen).
- `design_qc.py contenthold edl.json` passes.
- For every slide, the gap between its last reveal or camera key and its
  end is 3 s or less.
- Shot-length list from the EDL is inside the storyboard targets.
- With music, beat offsets (from an onset detector) are within two frames
  for marked beat cuts.
- The pacing pass is recorded as an EDL diff in the PR.

## Sources

Walter Murch, *In the Blink of an Eye* (copyrighted: ideas paraphrased, no
text imported).
