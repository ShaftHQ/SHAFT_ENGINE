---
name: launch-video
description: Use when someone wants a short launch or showcase video of a project or website they built, with a poster frame and share copy.
---

# D22 Launch video

## Use when

"Make a launch video", "show off what we built", a release teaser, or a
showcase clip after delivery. Longer tutorials use the technical-video
pipeline instead; persuasive explainers use [D21](explainer-arc.md).

## Inputs

The source: a project directory (read the code, styles, README, routes) or a
website URL (render JavaScript sites in a headless browser, dismiss overlays,
scroll section by section, see [D11](screen-capture.md)). Optional: tone,
format (16:9, 9:16, 1:1), duration, and one feature or version to focus on.

## Rules

- Short: 15-25 s, 18-22 s is the sweet spot. Shape: hook (2-3 s), reveal
  (2-4 s), two or three highlights, punchline or call to action (2-4 s).
- Plan the hook first. A stranger must know what it is, who it is for, and
  how to get it after one viewing.
- Show the real thing: reuse the product's own components, CSS, fonts,
  copy, captures, and animations; rebuild only what cannot be reused. The
  working product in use (entry, key action, result) beats a page that
  describes it.
- Specific, not generic: the project's own claims and wording. No invented
  numbers or testimonials, no stock "streamline your workflow" lines.
- Public-asset hygiene: never carry secrets, tokens, internal hosts or URLs,
  real customer or user names, or email addresses into the plan, frames, or
  copy. Swap in fictional stand-ins and say so in the plan.
- Readable: a line meant to be read stays settled about 0.3 s per word,
  counted from when the whole line is visible. Pace comes from cuts and
  motion, not from pulling text early.
- Alive: staged reveals, simulated clicks, typing, and swipes beat static
  slides. Every frozen frame should be worth posting.
- Tone presets set pacing: `default` (4-5 scenes, soft transitions),
  `polished` (3-4, long holds, fades), `deadpan` (3-4, empty space, slow
  fades), `cinematic` (4-5, big type, wipes), `app-store` (4-6 feature
  cards, slides). Freeform direction overrides a preset.
- Music and effects are one mix: effects in key, under the music
  ([D15](audio-mix-loudness.md)).

## Workflow

1. Answer in the plan: what it is (one sentence), audience, differentiator,
   strongest claim with evidence, visual hook, the real flow to show, tone,
   one-line caption.
2. Storyboard with [D01](design-brief-storyboard.md); durations sum to target.
3. Build with [D09](html-motion-graphics.md) or [D12](edit-assembly.md).
   Check stills of every scene and mid-transition ([D08](motion-principles.md)).
4. Deliver per [D20](delivery-qc.md): poster as frame 0, plus
   `share-copy.txt` (1-3 postable, specific sentences in the tone; no
   "excited to share").

## Verify

- `design_qc.py brief storyboard.json` and `design_qc.py holds storyboard.json` pass.
- `design_qc.py unslop` passes on the build source.
- `design_qc.py delivery` passes on the final file; duration 15-25 s.
- Manual: frame grep for secrets, emails, and internal hosts is clean.

## Sources

Ideas re-expressed from latent-spaces/brag (MIT, see
[INVENTORY](../INVENTORY.md)); no prompts, code, music, or effects copied.
