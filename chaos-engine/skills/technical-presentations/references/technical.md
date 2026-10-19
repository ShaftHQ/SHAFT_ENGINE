# Technical content

## 7 Code slides

- Show code only when it earns its place: a short walk-through, code linked to
  behaviour, or a deliberate "look how verbose" moment.
- At most 8 lines, monospace at 24 pt or larger, real syntax highlighting,
  straight quotes. Elide boilerplate with `// …`.
- Progressive reveal: duplicate the slide per step, highlight the active line
  and dim the rest. Copies, not animations, so PDF and video match.
- Never a code screenshot too small to read: crop and zoom, or set it as text.

## 8 Data slides

- One message per chart; the takeaway is the title.
- Declutter: no 3D, borders, or heavy grid; direct labels, not legends;
  context grey, the key series in the accent colour.
- Pies only for 2-3 parts of a whole; otherwise a sorted bar chart.
- Every number has a source and date in the notes. An unverifiable claim is
  replaced, or moved to the notes as "Unverified, not shown".

## 9 Type and contrast

- Title 32-40 pt, body 18 pt or more (never below 14 pt), captions 12-14 pt
  muted, code 24 pt or more. One display font, one body font, one grid.
- WCAG 2.2: text contrast at least 4.5:1, large text (18 pt, or 14 pt bold)
  and chart marks or diagram strokes at least 3:1 (SC 1.4.3, 1.4.11).
- Never colour alone: add a label, shape, or pattern (SC 1.4.1). Red means
  fail or risk, green means pass; not decoration.
- Check every brand palette pair before use; some brand pairs fail.
- Alt text on meaningful images; empty alt on decorative ones.
- Theme font slots (heading, body) name the deck's fonts in every new copy, so
  new boxes never inherit Arial or Calibri.
- Glyphs: Google Slides falls back silently per character. ✓ ✗ ⚠ fall back to
  MS PGothic; → (U+2192) is missing from Google Fonts' Roboto Slab, Lato,
  Roboto, and Noto Sans, so it renders in Times or Arial. Use outline icons
  for marks and native arrow shapes in diagrams. Set a prose → alone in a
  family whose cmap has it (checked with fontTools `TTFont(path).getBestCmap()`),
  one weight heavier and +2 pt so it matches the line (for example Source
  Sans 3 Black next to a slab serif); keep the baseline.
