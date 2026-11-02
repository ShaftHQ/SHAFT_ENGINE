---
name: edit-assembly
description: Use when clips, graphics, narration, and music must be assembled into a frame-accurate edit described as data and rendered with ffmpeg.
---

# D12 Edit assembly

## Use when

Assembling captures (D11), graphics (D07, D09, D10), narration (D17), and
music into a master, plus a vertical reframe from the same decisions.

## Edit decision list (`edl.json`)

```json
{"fps": 30, "size": [1920, 1080],
 "clips": [{"id": "s01", "src": "capture/install.mp4", "in": 2.0, "out": 8.0,
            "speed": 1.0, "transition": {"type": "fade", "frames": 10}},
           {"id": "s02", "src": "gfx/title.mov", "in": 0, "out": 4.0,
            "overlay": {"x": 96, "y": 860}}],
 "audio": [{"src": "vo/s01.wav", "at": 0.5, "vo": true},
           {"src": "music/bed.wav", "at": 0, "duck": true}],
 "chapters": [{"t": 0, "title": "Install"}],
 "intentionalHolds": ["12.0-13.5"]}
```

## Rules

- The EDL is the source of truth; ffmpeg filtergraphs are generated from it
  and never hand-edited.
- Normalize every input first: constant frame rate (`fps`), size (`scale`,
  `setsar=1`), BT.709 color (D14), 48 kHz audio.
- Cuts are hard by default and land on frame boundaries; a transition is a
  deliberate EDL entry (`xfade` with an explicit frame count), never a dip
  through black at every cut.
- Every scene shows content on frame 0: its first element enters before
  the cut, never on an empty card.
- Narration never overlaps: each VO line starts at or after the previous
  line's end plus 0.25 s. Lines anchored to footage events slide later and
  the scene or hold extends to fit; assembly refuses an overlapping EDL.
- Speed ramps for long waits use `setpts` with an on-screen label (for
  example "sped up 8x").
- Vertical 9:16 comes from the same EDL with per-clip crop or reframe hints,
  not a separate edit; terminal and code clips reflow (D11), never crop.
- Chapters export as ffmetadata and as a text list for the description.

## Workflow

1. Write the EDL from the storyboard; keep clip ids equal to scene ids.
2. Generate and run the render; save the exact command in the build log.
3. Review; apply pacing changes (D13) as EDL diffs.
4. Hand the master to D14 (grade), D15 (mix), and D20 (delivery).

## Verify

- Output duration equals the EDL total within one frame (ffprobe).
- `design_qc.py vooverlap edl.json` passes (edit-list gap and per-line
  speech masks), right after the render.
- `design_qc.py blackfreeze master.mp4 --allow 12.0-13.5` passes outside
  intentional holds, and `design_qc.py flatframes master.mp4` finds no
  solid frame outside the first and last second.
- `design_qc.py claims claims.json` passes: the screen matches each
  narration claim at its word.
- Audio and video start offsets differ by at most one frame.
- Re-running the render gives the same frame hashes with the same ffmpeg
  build.

## Sources

Recipes re-expressed from bryanwhl/ffmpeg-video-editor (MIT); QA-before-
preview idea from naveedharri/video-editor (MIT); FFmpeg filter
documentation.
