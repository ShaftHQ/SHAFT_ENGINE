---
name: technical-presentations
description: >-
  Use when planning, building, refactoring, or reviewing technical talks or
  training decks: story arc, claim titles, visual evidence, code slides,
  checkpoints. On demand only.
license: MIT
---

# Technical presentations

Judgement cards for decks that teach or explain technical work. Mechanics
(pptx editing, rendering, upload) belong to the host's slide tools; this skill
owns pedagogy and design. Load ONE card file below, not all.

## Hard rules

1. **Never remove content from an existing deck.** Only add or update.
   Unwanted slides are hidden (Google Slides: Slide › Skip slide; pptx
   `show="0"`). Unwanted on-slide text moves verbatim into the speaker notes
   or a hidden "Reference" appendix slide. The notes of a redesigned slide
   point to the original ("Original slide: hidden slide N").
2. **Brand hook.** Use the project's brand palette file if present (for
   example `palette.md`, `brand.md`, or a theme file the owner names); check
   its pairs against card 9. No palette file: neutral theme plus one accent.
   Never invent brand colours.
3. No learner or audience personal data on slides, notes, or recordings;
   aggregate it.

## First: retrieve

1. `python3 .chaos-engine/tool.py retrieve --store mempalace "<deck> slides decision feedback"`:
   prior owner rules, feedback, rejected styles.
2. `python3 .chaos-engine/tool.py retrieve --store graphify "<topic> code examples"`:
   the real code and names the slides must match.
3. Record `retrieve: used` or `skipped(<reason>)`.

## Card index

| # | Card | Rule | File |
| --- | --- | --- | --- |
| 1 | Audience and outcome | One audience, one "by the end you can" per section. | [plan](references/plan.md#1-audience-and-outcome) |
| 2 | Story arc | ABT per section; alternate what is / what could be. | [plan](references/plan.md#2-story-arc) |
| 3 | Storyboard | Layout per slide first, then copy to fit. | [plan](references/plan.md#3-storyboard) |
| 4 | One idea | One idea per slide; passes the 3-second glance test. | [slide](references/slide.md#4-one-idea) |
| 5 | Assertion-evidence | Claim title (sentence, ≤2 lines) over visual evidence. | [slide](references/slide.md#5-assertion-evidence) |
| 6 | Multimedia | Mayer: coherence, signaling, redundancy, contiguity. | [slide](references/slide.md#6-multimedia-principles) |
| 7 | Code slides | ≤8 lines, ≥24 pt mono, highlight active line, reveal by copies. | [technical](references/technical.md#7-code-slides) |
| 8 | Data slides | One message per chart; takeaway in the title. | [technical](references/technical.md#8-data-slides) |
| 9 | Type and contrast | WCAG 4.5:1 text, 3:1 large text and marks; never colour alone. | [technical](references/technical.md#9-type-and-contrast) |
| 10 | Retrieval checks | Recall at start; checkpoint every 7-10 slides. | [teach](references/teach.md#10-retrieval-checks) |
| 11 | Worked examples | Worked, then faded, then solo. | [teach](references/teach.md#11-worked-examples) |
| 12 | Voice and demos | Humour tied to the point; demos rehearsed with a reset. | [teach](references/teach.md#12-voice-and-demos) |
| 13 | Refactor a deck | Redesign beside the original; hide, never delete. | [review](references/review.md#13-refactor-a-deck) |
| 14 | Review | Reject the anti-pattern list; render and look. | [review](references/review.md#14-review) |

Evidence behind the cards: [sources](references/sources.md).
