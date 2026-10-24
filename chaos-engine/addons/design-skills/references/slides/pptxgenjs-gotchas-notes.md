---
name: pptxgenjs-gotchas-notes
description: Use when generating a .pptx with pptxgenjs, filling a template's slots, setting deck palette dominance, or writing speaker notes.
---

# S04 pptxgenjs gotchas and notes

## Use when

A deck is generated in code or a template is filled. Companion to
[deck system](deck-system.md) and [S03 native RTL](arabic-pptx-native-bidi.md).

## Rules

1. **Set the layout first** (16:9 = 10 × 5.625 in); coordinates are not
   clamped, so check every shape is on the canvas.
2. **Hex without `#`, never 8-digit;** never reuse an options object
   across calls; shadow offset 0 or more.
3. **`bullet: true`,** never a typed `•`; `margin: 0` when aligning text
   to shapes; notes via `addNotes`, never a text box.
4. **Template slots are not source items:** delete the whole unused
   group, then check for orphans.
5. **Palette dominance:** 60-70% one colour, 1-2 support colours, 1 accent.
6. **Speaker notes:** 150-300 words a slide, spoken register, bold cue words.

## Verify

- [ ] `design_qc.py pptxlint build.js` and `design_qc.py pptxlint deck.pptx`
      (hex, bullets, shadow offset, off-canvas shapes)
- [ ] Opened in PowerPoint or Google Slides; no repair prompt.

## Sources

- anthropics/skills pptx @ `dbd4588f9e1033efb41dad4bef2f7947c8993d44`:
  proprietary terms, so rules are re-stated in our words; no text or code copied.
- [lewislulu/html-ppt-skill](https://github.com/lewislulu/html-ppt-skill/tree/fd1629067909ff55b36b905476de4e5f26062a1f)
  @ `fd1629067909ff55b36b905476de4e5f26062a1f` (MIT, inspiration; scripts not run).
