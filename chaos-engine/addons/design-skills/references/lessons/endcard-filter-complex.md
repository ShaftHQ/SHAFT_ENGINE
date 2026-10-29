---
name: endcard-filter-complex
description: Use when concatenating a clean end card onto content; avoid concat demuxer and empty end-card seeks.
---

# L16 End-card filter_complex concat

## Use when

Appending a clean branded end card after a content segment with burned
bands. Extends [L10](endcard-band-concat.md) with the ffmpeg join and
seek rules that keep video and audio aligned.

## Rules

1. **Join with `filter_complex` concat, not the concat demuxer.** Demuxer
   concat has dropped end-card *video* while keeping end-card audio when
   the content file carries a non-zero video `start_time`. Use
   `[0:v][0:a][1:v][1:a]concat=n=2:v=1:a=1[v][a]`.
2. **Seek live end-card frames by video-stream duration.** Prefer
   `ffprobe -select_streams v:0 -show_entries stream=duration`. Format
   duration can outrun video when demuxer concat left audio-only tail —
   seeking there yields empty frames and a false clean gate.
3. **Force matching `pix_fmt` on still grabs.** When writing JPEG stills
   from the live mp4, pass `-update 1` and `-pix_fmt yuvj420p` (or the
   project target) so modern ffmpeg does not reject full-range MJPEG and
   the gate frame is non-empty.
4. **Fail closed on short video.** After concat, if format duration −
   video-stream duration > ~0.35 s, treat the end card as lost and
   rebuild.

## Verify

- [ ] `design_qc.py endcardlive final.mp4 --clean endcard.png` seeks with
      video-stream duration when present and writes a non-empty live frame.
- [ ] Pipeline docs / render script use `filter_complex` concat for
      content+endcard; concat demuxer is not the join path.
- [ ] Live end-card contact sheet shows brand plate, not black/empty.

## Why

A green loudness/captions suite still ships a black end card when video
was dropped at join or the gate seek landed past the last video packet.

## Sources

Field lesson from a content QA recut batch: demuxer concat dropped
end-card video; video-stream seek + `yuvj420p` made the live gate real.
