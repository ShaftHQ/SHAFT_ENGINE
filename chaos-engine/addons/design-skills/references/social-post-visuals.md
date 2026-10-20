---
name: social-post-visuals
description: Use when a social post visual ships in several sizes, in RTL Arabic or mixed scripts, or with a call to action, and needs a measured self re-review before handoff.
---

# D23 Social post visuals

## Use when

A post image ships for a feed, a link share, and a story at once; the copy
is Arabic (RTL) or mixes scripts; or it carries a call to action (CTA).
Sizes and the render rule come from [D07](image-graphics.md).

## Build

- One HTML template per size, rendered with headless Chrome at the exact
  pixel size; one Python builder writes every size from one copy source, so
  a wording change lands in all sizes in one run.
- Fonts are local and licensed (D02); wait for `document.fonts.ready`
  before the screenshot.

## Sizes

| Size | Pixels | Adapt |
| --- | --- | --- |
| Feed square | 1200 x 1200 | full layout |
| Link share | 1200 x 627 | one row; shorten copy before shrinking type |
| Story | 1080 x 1920 | stack vertically; top ~250 px and bottom ~340 px hold no key content (profile bar, link sticker) |

## RTL and mixed scripts

- `dir="rtl"` on the root; an Arabic face that covers Arabic and Latin
  digits (for example Cairo).
- Wrap every Latin brand term, version, or code word in `<bdi>` so it stays
  whole and in order (`كله <bdi>AI-native</bdi>`); give it the Latin brand
  font. Never split a brand term across lines (`white-space: nowrap`).
- Step numbers and roadmaps run right to left; check reading order in the
  render, not the source.
- No colour emoji inside the visual: they bring off-palette colour. Use an
  outline icon set (Lucide SVG) recoloured by role.
- In RTL post copy (the caption, not the image), an emoji goes at the end
  of its line, never at the start, so bidi does not move it.

## Content truth

- Real code, real product names, official logos under their usage rules;
  never an invented output line or metric (router iron rule 4).
- People data: [technical-presentations hard rule 5](../../../skills/technical-presentations/SKILL.md#hard-rules)
  applies to post visuals; client names stay out of examples too.
- The CTA names the real channel ("message our page to register"), not a
  link that does not exist. When the channel changes, update the CTA in
  every size in the same pass and re-render all of them.
- A longer CTA rebalances the row (column widths, gaps, line split) before
  any text shrinks; nothing drops below the D05 minimum size.

## Self re-review (mandatory before handoff)

Measure, do not eyeball. Have the template dump element boxes
(`getBoundingClientRect`) to JSON and run `design_qc.py layout` on it:

- Optical centring: a block is centred on its ink, with a small allowance
  for Arabic descenders and Latin low descenders.
- Labels in one row share a baseline.
- A bracket spans exactly from the first to the last circle edge it groups.
- Gutters between sibling blocks are equal within 2 px.
- Every text keeps at least 20 px inner clearance from its box edge.
- Story key content stays out of the safe zones: `design_qc.py safezones
  boxes.json --format story` (boxes as 0-1 fractions of the canvas).

Then view each PNG at 100% and at phone width. If the review shows an
earlier claim of yours was wrong ("fits", "10 px inside"), fix it and say so
in the handoff.

## Verify

- `design_qc.py layout boxes.json` passes for every size. Shape:
  `{"elements": [{"id", "x", "y", "w", "h", "baseline"?, "descender"?}],
  "baselines": [[ids]], "brackets": [{"id", "spans": [ids]}],
  "gutters": [{"ids", "axis"}], "clearance": [{"text", "box"}],
  "centre": [{"id", "in", "axis"}]}`.
- `design_qc.py safezones story.json --format story` passes.
- `design_qc.py image out.png --size 1080x1920` (per size) passes.
- `design_qc.py contrast` passes for every text pair used.
- CTA string identical across sizes (diff the builder output).

## Sources

Lessons from a multi-size RTL post series; platform story safe-zone
guidance (Meta stories specs); W3C "Inline markup and bidirectional text in
HTML" for `<bdi>`.
