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

1. Isolate every Latin run (LRI ... PDI, or `<bdi>` in HTML). Keep Latin
   trailing `.` `!` `?` **inside** the isolate (outside paints as a leading
   `.` under `dir=rtl`, e.g. `.AI`). Arabic `، ؟` stay outside. Other Latin
   punctuation next to Arabic outside an isolate is a fail. Match question
   marks to the script: Latin `?`, Arabic `؟`.
2. Apply no-break glue (article plus word, Latin runs of 36 characters or
   fewer) before any wrapping markup such as highlight spans. If the markup
   goes in first, it silently breaks the glue and strands the article at
   the end of a line. Bind a bound article plus its following Latin into
   one RTL isolate so the article paints at the **right** of its noun.
3. Hook lines are layout units: one line each, `nowrap`, shrink-to-fit,
   2 lines at most. Refuse a third.
4. Test invisible characters by rendering them, never by reading the spec.
   A hair space (U+200A) can render at 0 px in headless Chrome. For a
   visible gap after a glued prefix, use WJ + thin space + WJ.
5. Check glyph confusables at the smallest size used (labels), not at
   caption size. A bare capital I makes «AI» read as «A|». Use a face with
   distinct I, l and 1 for small Latin text, for example
   Atkinson Hyperlegible (distinct I/l/1) for small Latin text.
6. A Latin phrase split by an RTL prefix renders scrambled. Keep it as one
   Latin run with the prefix outside it, and never leave a Latin function
   word dangling before RTL text.
7. Fix the source text (SRT or VTT), not only the render, and assert on the
   file that ships. If speech includes a bound article (`الـ` / `و`+…),
   the SRT must carry it; a missing article fails meaning even when the
   Latin noun is present.
8. Caption lint: put an Arabic comma «،», not a bare «و», between two
   Latin terms, especially in bold headings. Don't end a cue on a
   trailing filler word. Don't end a line on a bound prefix
   (و/ب/ل/ك/ف/الـ), not just a stranded article. Keep a narrow
   placeholder whitelist (فلان/كذا) so the lint does not flag them.
9. Neutral characters break isolates: ’ ‘ " - -- : touching a Latin run
   in an RTL line reorder it («It’s» shows as «s'It», «--help» as
   «help--»). Use ASCII ' inside a word, or wrap the whole run, neutrals
   included, in LRI U+2066 ... PDI U+2069. The rule covers hook and
   label text, not only SRT. Scope: isolates are for burned captions and
   HTML only, never PPTX; decks use native `rtl="1"` ([S03](../slides/arabic-pptx-native-bidi.md)).
10. «70%» showing as «%70» in an Arabic line is normal RTL order: warn,
    never fail.
11. Render CSS: `unicode-bidi: plaintext` **ignores** `dir=rtl` and takes
    base direction from the first strong character (Latin-leading lines
    go LTR). Use `unicode-bidi: isolate` + explicit `direction: rtl` + a
    leading RLM on every caption line. Line-breakers must honor author
    hard newlines; rejoining with `' '.join(t.split())` undoes wraps and
    can put verb+article+noun on one line (visual middle-article).
12. Gate bidi on cropped rendered frames at cue midpoints, never from the
    SRT string alone. False positive: reading LTR visual order of a
    correct RTL paint as “wrong.” Article **right** of Latin = correct
    RTL (read the article first).
13. Tech-term lexicon (ASR Arabic-script misspellings): Whisper often
    paints English tech terms as Arabic-script transliterations. Prefer
    Latin with the bound article outside the isolate (rule 2 / 11). Gate
    with `design_qc.py termlexicon` on the shipped SRT; extend the
    placeholder lexicon with `--term AR=LATIN` when a new miss appears —
    do not waive by tagging alone.

14. Trailing Latin sentence punct (`.!?`) belongs inside the LTR isolate
    with the Latin run. Gate: `design_qc.py bidi` flags
    `trailing-latin-punct-outside-isolate` (PDI then `.!?`, or Latin+`.!?`
    with no isolate). Prove on hook crops: period after the last Latin
    glyph, never a leading `.AI`.

## Verify

- [ ] Script check: every Latin run is isolated; Latin trailing `.!?` sits
      inside the isolate; Arabic `، ؟` stay outside; question marks match
      their script.
- [ ] Zoom-crop each mixed line from rendered frames (not the HTML): no
      stranded article, no 0 px gap, no scrambled run; article right of
      its Latin noun when RTL.
- [ ] No hook has more than 2 lines; the smallest label passes the I/l/1
      check.
- [ ] Lint passes: «،» between Latin terms, no cue ending on a filler,
      no line ending on a bound prefix, only whitelisted placeholders.
- [ ] The shipped SRT, not only the render, carries the fixes (including
      spoken bound articles).
- [ ] Caption CSS is `isolate` + `direction: rtl` + leading RLM; no
      `plaintext`; author newlines preserved.
- [ ] `design_qc.py bidi <captions|hooks.json|labels>` passes (percent
      warnings allowed); rendered frames show no tofu.
- [ ] `design_qc.py termlexicon cues.srt` passes (no leftover Arabic-script
      tech transliterations; `--term` for project extensions).

## Sources

- [Unicode UAX #9 bidi algorithm](https://www.unicode.org/reports/tr9/)
- [W3C: inline markup and bidirectional text](https://www.w3.org/International/articles/inline-bidi-markup/)
- Field lessons from one talk-Shorts batch, verified on rendered frames.
