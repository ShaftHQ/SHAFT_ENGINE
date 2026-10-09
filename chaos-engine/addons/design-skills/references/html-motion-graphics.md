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
- A 2160p master keeps the 1080p layout and renders it at device scale
  factor 2 (Playwright or Puppeteer `deviceScaleFactor`), so text and vector
  art are native at 3840 x 2160. Raster images inside a scene need at
  least one image pixel per output pixel at their deepest camera zoom
  (`natural width / CSS width >= zoom x scale factor`): re-shoot
  HTML-sourced screenshots at a higher scale factor and pin the CSS width so
  camera keys keep working. Never upscale text or UI (D19).
- The last element revealed in a scene holds at least 1.5 s before the cut;
  key late reveals to an earlier word rather than the last one.
- Headlines and end-card lines never end on an orphan word:
  `text-wrap: balance` where the renderer supports it, and a no-break space
  between the last two words as the fallback.
- Transparent overlays render with alpha (ProRes 4444 or VP9 with alpha) and
  are composited in D12.

## Screenshot and code camera

- Real screenshots, code and terminal captures get a region camera: keys
  `[beat, x, y, w]` make the w px wide region at (x, y) fill the frame,
  easing about 900 ms into each key. Key each move to the narration word that
  names the region (beat = line start plus the word's character proportion
  of the line duration).
- One still on screen at a time; never stack or fan out several captures.
- Never pull back below reading size. To show "the whole file", scroll at
  reading size and state the size as a number (a line-count chip).
- 9:16: captures run near full-bleed (a side margin of 24 px plus the push-in growth keeps light
  captures off the frame edge, which `edgeclip` flags) with a tighter region
  than 16:9, and end above the D20 caption band.
- Before any full rebuild, render stills of the edited scene at the moments
  under review from the same seek-based page; rebuild only once they read.
- Generated scenes are linted for unrendered template placeholders (`{`
  followed by a helper call) before render.

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
- `design_qc.py revealhold timeline.json --html scenes/` passes (resolves
  `data-at="beat+offset"` reveals against each scene's beats).
- Stills at each camera key show the named region with text at reading
  size (D05), in 16:9 and 9:16.
- `rg -n '\{w\(|\{[a-z_]+\(' scenes/` finds no unrendered placeholders.
- Output passes `design_qc.py delivery` or is an alpha intermediate listed in
  the manifest.

## Sources

heygen-com/hyperframes (Apache-2.0, referenced by pin, not vendored);
Remotion and Motion Canvas documentation.
