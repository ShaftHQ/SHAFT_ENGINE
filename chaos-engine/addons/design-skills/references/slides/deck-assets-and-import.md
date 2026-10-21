---
name: deck-assets-and-import
description: Use when adding icons, images, logo walls or humour to a training deck, or importing a .pptx into Google Slides.
---

# Deck assets and import

Companion to [C12 training decks](../craft/training-decks.md).

## Palette from the logo

Token format and contrast checks: [brand system](../brand-system.md).
- Measure logo colours by pixel count or k-means, never by eye; map them to
  roles: primary, signal/negative, neutral dark, neutral light.
- Add only what roles need: ink text (~#1F2328), muted caption grey
  (>= 4.5:1 on white), a dark surface (a primary shade) for code and
  dividers, light tints, a checkpoint accent with a dark-surface variant.
- Compute WCAG contrast for every allowed text/background pair; publish an
  allowed and a banned list. Dimmed text is never lighter than muted.
- Simulate colour-vision deficiency on chart colours; colour is never the
  only signal (pair it with a word or icon).
- Map roles to theme slots; if hidden originals depend on old theme fonts,
  set fonts explicitly on every visible run and QC for legacy font runs.

## Proof mocks

Author title, body, code panel and big stat slides as HTML at the canvas
size with the real font files and logo, render headless at device scale 2,
and review each at full size and as a contact sheet until all pass a glance
test.

## Font pairing

- Hosted fonts only (for Google Slides, check Font > More fonts); at most 2
  families plus 1 mono.
- Display face echoes the logo's letterform class; text face is a humanist
  sans that holds at 18-22 pt; mono has a tall x-height and clear 0/O, 1/l.
- Another script gets a companion family and its own paragraph direction.
  Use only glyphs the fonts cover, verified in the platform's PDF export.

## Icons, images, humour

- One outline icon set (open licence) at one stroke width, one colour.
  Symbols are icons, not glyphs: check, x and warning marks become icons
  at cap height, coloured by role, next to their word.
- Arrow glyphs are missing from many hosted fonts: check the cmap
  (fontTools `getBestCmap()`), set a fallback face for that glyph only,
  confirm in a real upload; diagrams use native arrow shapes.
- Images only for photos, logos, memes and screenshots, each with alt text.
- Logo walls: uniform grid, one optical height, equal-area sizing
  (`w = sqrt(A*aspect)`, `h = sqrt(A/aspect)`), padding trimmed.
- Photos on dark slides: a cut-out bleeding off the edge or a full-bleed
  panel with a gradient melt; never a light rectangle.
- Screenshots show today's tool and carry a version or date caption.
- Legacy decorations stay on kept slides and go on rebuilt ones.
- At most one designed humour aside per slide, never at a learner's expense.

## pptx to Google Slides traps

- Title placeholders import centred unless every placeholder sets `align`.
- `bullet:true` runs leave a default-font bullet glyph: use paragraph
  spacing or set the bullet font.
- Give each tracker label its own placeholder (spaced labels drift).
- Run the office validator with `--repair` before upload.
