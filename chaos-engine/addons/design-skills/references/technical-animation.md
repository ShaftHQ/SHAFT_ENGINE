---
name: technical-animation
description: Use when an architecture, data flow, install sequence, or algorithm should be explained with a programmatic animated diagram.
---

# D10 Technical animation

## Use when

The idea is structural: components and arrows, request flow, phases of an
installer, before and after states, or a code walkthrough with highlights.

## Tooling

Manim Community Edition (MIT) renders headless on CPU. Motion Canvas (MIT)
is the TypeScript alternative. Avoid a LaTeX dependency unless formulas are
on screen (use `Text`, not `Tex`).

## Rules

- One scene class per storyboard scene id, under 20 seconds each, so renders
  run in parallel and retakes stay cheap.
- Colors and fonts come from a generated constants module built from the
  brand tokens (D02); no literal hex values in scene code.
- Arrows and labels never cross text; keep a consistent stroke width and a
  grid for node placement.
- Reveal in reading order; one new element per beat; hold each beat per D08.
- Charts: bars start from a zero baseline, labels sit on the data, and each
  transition changes one thing (axis, then values, then order) in about
  1 s with slow-in and slow-out. Every number carries its source.
- Code on screen is real code from the repository at a pinned commit, with
  the file path shown.
- Glyph cap height at least 24 px at 1080p.

## Workflow

1. Sketch nodes and beats from the storyboard.
2. Write the scene with token constants; preview at low quality
   (`manim -ql`).
3. Render 1080p for review (`-qh`), then 2160p once (`-qk`); log render time.
4. Commit scene sources next to the storyboard; renders are build outputs.

## Verify

- Headless render exits 0 at 1080p and 2160p; render times recorded.
- Sampled frames show minimum glyph height of 24 px at 1080p.
- Palette check: only token colors in sampled frames.
- `design_qc.py flash` passes on the rendered scene.

## Sources

ManimCommunity/manim (MIT); motion-canvas (MIT); Heer and Robertson,
"Animated Transitions in Statistical Data Graphics" (InfoVis 2007).
