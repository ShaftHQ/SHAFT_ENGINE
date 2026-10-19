# Refactor and review

## 13 Refactor a deck

- Hard rule 1: work in a new file; the owner's source stays untouched.
- In the copy, put each redesigned slide where the original was. Delete the
  original only after its text is in the replacement's speaker notes,
  verbatim, with "Original: slide N of <source file>". No replacement: hide
  it, or collect it in a hidden "Reference" appendix.
- Editing the owner's own file (only on request): hide, never delete.
- Apply the brand hook (hard rule 4) through the theme or master, not per
  slide.

## 14 Review

Render every slide and look before calling it done. Reject on review:

- Topic title plus 6-12 small bullets.
- Text read aloud from the slide, or spoken sentences duplicated on it.
- Legends or labels far from their data; a text slide explaining the
  previous diagram.
- A new hue per bullet, a box behind every paragraph, random theme flips,
  a logo on every slide.
- Pies with 4+ slices, 3D charts, charts without a takeaway title.
- Unreadable code or settings screenshots.
- More than 2 consecutive slides with the same layout geometry, or the same
  concept drawn two ways on neighbouring slides.
- A tinted screenshot, a pasted vendor diagram, a colour logo in a wall, or a
  logo the presenter's CV does not back (card 6a).
- A visible fact with no source in the notes.
- An edited source file, deleted text, text baked into an image, or audience
  personal data (hard rules 1, 2, 3, 5).

## 15 Gotchas

- pptx `show="0"` survives the Google Slides import as Skip slide, but the
  PDF export still includes skipped slides; strip them from a PDF copy or
  say so.
- ⮑ (U+2B91) renders as tofu in Roboto Slab and Lato, and → (U+2192) or
  ✓ ✗ ⚠ fall back to Times, Arial, or MS PGothic (card 9). Check every
  non-ASCII glyph in a PDF export of a real Google Slides upload. A local
  LibreOffice render substitutes fonts and hides the fallback.
- With only the Drive connector, replacing a deck creates a new file id;
  update every link and share, and report the new id.
