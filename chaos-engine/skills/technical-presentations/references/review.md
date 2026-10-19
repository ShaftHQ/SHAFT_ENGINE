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
- An edited source file, deleted text, text baked into an image, or audience
  personal data (hard rules 1, 2, 3, 5).

## 15 Gotchas

- pptx `show="0"` survives the Google Slides import as Skip slide, but the
  PDF export still includes skipped slides; strip them from a PDF copy or
  say so.
- ⮑ (U+2B91) renders as tofu in Roboto Slab and Lato. Check every unusual
  glyph in a Google PDF export, not only in the editor.
- With only the Drive connector, replacing a deck creates a new file id;
  update every link and share, and report the new id.
