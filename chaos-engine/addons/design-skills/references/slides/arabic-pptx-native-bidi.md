---
name: arabic-pptx-native-bidi
description: Use when a .pptx or Google Slides deck contains Arabic or mixed Arabic and English text, or Arabic renders out of order in a deck.
---

# S03 PPTX native RTL

## Use when

An Arabic or bilingual deck is built or repaired as editable PPTX.
Companion to [C12 training decks](../craft/training-decks.md) and
[deck system](deck-system.md). Scope note: [L02](../lessons/mixed-script-captions.md)
LRI/PDI isolates are for burned captions and HTML only, never PPTX.

## Rules

1. **No Unicode bidi controls in PPTX text** (LRI/PDI, RLI, LRE/RLE/PDF,
   LRM, RLM, ALM). Use native direction instead: `<a:pPr rtl="1">` on
   every Arabic paragraph, `<a:bodyPr rtlCol="1">` on its text body,
   explicit right alignment, run language `ar-SA`, and the complex-script
   font slot (`<a:cs>`) when a typeface is set.
2. **One base direction per paragraph.** URLs, code, long English and
   citations go in separate LTR text boxes.
3. **Native bullets only:** no typed `•` at a paragraph start. Tables:
   RTL cell paragraphs; mirror column order only when the semantic
   reading order is Arabic.
4. **Renderer authority:** LibreOffice and preview renderers are not the
   authority for Arabic order. Report Structural, Layout, PowerPoint
   visual, Google Slides compatibility and Font portability as separate
   PASS or NOT RUN lines.

## Verify

- [ ] `design_qc.py pptxbidi deck.pptx` passes (no bidi controls, every
      Arabic paragraph `rtl="1"`, no typed bullets; missing `lang="ar-*"`
      warns).
- [ ] The five report lines are present, each PASS or NOT RUN.

## Sources

- Rules only, re-expressed; its repair script is never fetched or run:
  [sultanalsafran/agent-skills arabic-presentations](https://github.com/sultanalsafran/agent-skills/tree/fc05be36316e5e5ea2ee0b5ec51519b411d54665/arabic-presentations)
  @ `fc05be36316e5e5ea2ee0b5ec51519b411d54665` (MIT).
