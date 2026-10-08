---
name: html-motion-graphics
description: Use when lower thirds, logo stings, kinetic type, or UI callouts should be built as HTML compositions and rendered locally to video.
---

# D09 HTML motion graphics

## Use when

A motion graphic is best expressed with HTML, CSS, and the Web Animations
API: lower thirds, title cards, logo stings, callouts over captures, kinetic
type. Timing rules come from D08.

## Tooling

HyperFrames (Apache-2.0) is the default renderer: HTML compositions,
`lint`, a snapshot contact sheet, and a deterministic headless render to MP4,
WebM, or alpha MOV. Pin the CLI version in the project; read upstream skill
notes at the SHA in INVENTORY.md. Do not run its skill auto-update command.
Remotion (custom licence, company use may need a paid licence) and Motion
Canvas (MIT) are documented alternatives, not defaults.

## Rules

- Brand tokens enter as CSS custom properties; no literal colors or curves in
  composition files.
- Fonts are local files referenced with `@font-face`; no network fetch at
  render time.
- Render locally with the network disabled. Cloud rendering services are
  out of scope.
- GSAP only when the owner accepts its licence in writing; WAAPI and CSS
  first.
- Kinetic type: 3 to 7 words per beat; a phrase lands within 3 frames of
  its spoken word and holds per D08. Move the frame rather than the
  letters; per-letter motion only for two or three key words. Watch it
  muted once.
- Every composition has a contact-sheet review before a full render.
- Transparent overlays render with alpha (ProRes 4444 or VP9 with alpha) and
  are composited in D12.

## Workflow

1. Scaffold a composition per storyboard scene id; wire tokens.
2. Lint, then snapshot the contact sheet; review holds and staging.
3. Render 1080p for review, then the delivery size once approved.
4. Render twice and compare frame hashes to prove determinism.
5. Fallback when the renderer is unavailable: D10 scenes or ffmpeg
   `drawtext` for simple titles.

## Verify

- Renderer lint and validate report 0 errors.
- Two renders give identical frame hashes
  (`ffmpeg -i out.mp4 -f framemd5 -`).
- `design_qc.py easing compositions/` passes.
- Output passes `design_qc.py delivery` or is an alpha intermediate listed in
  the manifest.

## Sources

heygen-com/hyperframes (Apache-2.0, referenced by pin, not vendored);
Remotion and Motion Canvas documentation.
