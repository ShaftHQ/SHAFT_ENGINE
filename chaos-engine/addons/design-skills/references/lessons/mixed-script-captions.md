---
name: mixed-script-captions
description: Use when captions, hook lines, or small labels mix an RTL script (Arabic) with Latin words, punctuation, or code terms.
---

# L02 Mixed-script captions

## Use when

Burned-in captions, hook lines or labels mix RTL and Latin. It extends
[D18 captions](../captions-subtitles.md) and the RTL rules in
[D23](../social-post-visuals.md).

## Rules

1. Isolate every Latin run (LRI ... PDI, or `<bdi>` in HTML), with the
   punctuation outside the isolate. Latin punctuation next to Arabic text
   outside an isolate is a fail. Match question marks to the script:
   Latin `?`, Arabic `؟`.
2. Apply no-break glue (article plus word, Latin runs of 36 characters or
   fewer) before any wrapping markup such as highlight spans. If the markup
   goes in first, it silently breaks the glue and strands the article at
   the end of a line.
3. Hook lines are layout units: one line each, `nowrap`, shrink-to-fit,
   2 lines at most. Refuse a third.
4. Test invisible characters by rendering them, never by reading the spec.
   A hair space (U+200A) can render at 0 px in headless Chrome. For a
   visible gap after a glued prefix, use WJ + thin space + WJ.
5. Check glyph confusables at the smallest size used (labels), not at
   caption size. A bare capital I makes «AI» read as «A|». Use a face with
   distinct I, l and 1 for small Latin text.
6. A Latin phrase split by an RTL prefix renders scrambled. Keep it as one
   Latin run with the prefix outside it, and never leave a Latin function
   word dangling before RTL text.
7. Fix the source text (SRT or VTT), not only the render, and assert on the
   file that ships.

## Verify

- [ ] Script check: every Latin run is isolated, punctuation sits outside
      it, and question marks match their script.
- [ ] Zoom-crop each mixed line from rendered frames (not the HTML): no
      stranded article, no 0 px gap, no scrambled run.
- [ ] No hook has more than 2 lines; the smallest label passes the I/l/1
      check.
- [ ] The shipped SRT, not only the render, carries the fixes.

## Sources

- [Unicode UAX #9 bidi algorithm](https://www.unicode.org/reports/tr9/)
- [W3C: inline markup and bidirectional text](https://www.w3.org/International/articles/inline-bidi-markup/)
- Field lessons from one talk-Shorts batch, verified on rendered frames.
