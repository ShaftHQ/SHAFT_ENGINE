---
name: caption-video-qc
description: Use when cutting a talk clip into a captioned short, checking caption timing at splices, sampling frames for review, or trusting ASR output.
---

# L06 Caption and video QC

## Use when

A captioned talk clip is cut, reframed and reviewed. It extends
[D18 captions](../captions-subtitles.md), [L02](mixed-script-captions.md)
and [L03](talk-edit-and-layers.md).

## Rules

1. Contact sheets sample cue midpoints: full-opacity mid-cue frames, never
   crossfade windows. Extra probe times snap into a cue, at least 0.25 s
   from its edges.
2. Self-review grabs a frame at every cue midpoint plus the hook, and
   reads the bidi order on each frame.
3. Splice joins have no caption gap: the previous cue extends to the join,
   so no frame at a cut is captionless.
4. Cutaway-in-crop: scan the span for the source's own scene cuts and flag
   any B-roll or graphic half-cropped by the face crop.
5. Text or graphic B-roll is reframed to fit, on frame-exact boundaries.
   Verify with frame-number tiles (`select=between(n,a,b)`); seek grabs
   are imprecise.
6. Fade vs last word: the span ends after the last spoken word plus a
   pause, with a short tail fade (0.1-0.15 s) so the last word is not
   dimmed. Check every fade frame.
7. ASR hallucinates on pauses and noise (outro phrases such as
   «شكرا للمشاهدة» or "thanks for watching"). Drop them from the words
   file, log them in the notes, and cross-check with 2 or more models plus
   a targeted probe of that span.
8. Hook = spoken words: hook text matches the speech under it, and the
   hook card ends when that speech ends.
9. Rendered-mix transcript diffs are noisy over music and end cards. Gate
   on cross-source agreement plus tagged misses; keep the rendered diff as
   evidence and waive only with a recorded reason.
10. A span starts on a shot cut and a clean sentence onset, never
    mid-phrase or on a cutaway.
11. Every cue edit re-runs the caption checks: removing a word can
    re-wrap a line and strand a bound prefix at its end. Split the cue on
    word timestamps instead.
12. Run heavy ASR models one at a time, detached. Parallel large models
    can be killed for memory without an error.

## Verify

- [ ] `design_qc.py cuemids cues.srt --probe <t>` lists the frames grabbed.
- [ ] `design_qc.py cuegaps cues.srt --join <t>` passes for every splice.
- [ ] `design_qc.py asrhall words.json` passes, or each hit is logged and
      dropped.
      Words files use `word|w`, `start|s`, `end|e`; phrases match across
      consecutive words.
- [ ] `design_qc.py boundprefix cues.srt` passes after every cue edit.
- [ ] `design_qc.py fadetail --words words.json --end <s> --fade <s>`
      passes.
- [ ] `design_qc.py bidi` passes on the captions and the hook/label text.
- [ ] Notes record the scene-cut scan, the reframe tile check and the
      shot-cut onset.

## Sources

- Field lessons from one talk-Shorts batch, verified on rendered frames.
- [FFmpeg select filter](https://ffmpeg.org/ffmpeg-filters.html#select_002c-aselect)
