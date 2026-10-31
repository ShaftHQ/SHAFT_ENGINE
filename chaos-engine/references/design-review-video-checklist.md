# Design review / video worker checklist

Default checklist every design or video worker (and independent reviewer)
loads. Role-neutral: implementer, reviewer, orchestrator SIGN. Does **not**
re-implement sibling video sign-off gates — call those commands; do not fork them.

Lesson card ids (L02, L38, L40–L43, chrome-follows-title) live under the
design-skills add-on lesson index; load them on demand from that add-on.

## Rule

### Flow
1. **Plan** first (options, riskiest assumption + check). Live/external: think-first.
2. **Do** — one implementer.
3. **Self-check** against the gates below.
4. **Independent adversarial review** (not the implementer).
5. **Orchestrator SIGN** after indep PASS + gate JSON. PDCA ≤ ~5 rounds then escalate.
6. **Publish / schedule** only after SIGN — upload owners only; design lane never uploads.
7. **Retro** defects → ChaosEngine gate + review-brief block.

### Video / caption (build + review + SIGN)
- Ear-lock title to kept speech; spoken-lock keeps **scope words** (design L41);
  run `title_scope_gate`.
- Title polarity / claim ⊆ kept spans; near-title similarity when titles must differ.
- Long vs Short dedupe (design L43): titles differ; overlap only if long is proper ≥3×
  superset.
- Chrome / title plate = locked title; no production meta (`TODO`, `DEBUG`, brief lines).
- Mixed AR/EN: wrap Latin with **LRI…PDI** (or `<bdi>`); **never strip** isolates in the
  ASS writer (design L02).
- Exactly **one** caption layer; cue gap ≥150 ms; snap to words; fontsize fits band.
- Body-only overlays by **frame index `n`**; endcard clean (design L42 chrome-follows-title).
  Retitle → re-render all burned-in chrome.
- Mute / privacy / phonetic dumps per design L38.
- Loudness ≤ −1 dBTP; no mid-word cuts.

### Automated gates (attach JSON stdout)
```text
design_qc.py bidisolate <cues|ass|srt|hooks>
design_qc.py bidi <cues|ass|srt|hooks>
design_qc.py captionlayers <ass|mp4|manifest.json>
design_qc.py asscapfit <file.ass> --band-h <px>
design_qc.py nonviewertext <chrome.txt|frames-dir|ass>
design_qc.py renderfresh <out.mp4> --prior-sha256 <prev>   # L46; REUSE_MP4 fails
design_qc.py frozentail <avatar.mp4>                        # L47 last 2s + after speech
design_qc.py capsync <cues.srt> --onsets <asr.json> [--snapper-words <snap.json>]
title_scope_gate.py check --title T --srt answer.srt
```
Sibling video sign-off lesson owns viewer-frame SIGN (contact sheet ≥1/5s +
first/last 3s; zero non-viewer text). Metric/hash/title-crop-only SIGN = **FAIL**.

### Upload language order
Design L40: video language (not Auto) → schedule → title/description language →
captions; auto-dub leave alone; desc L1 = title; series playlist not Shorts.

### Never
- Never strip LRI/PDI after applying.
- Never bake production/debug strings into viewer chrome.
- Never PASS without viewer-frame gate JSON + human look at the contact sheet.
- Never ship metadata-only retitle without chrome re-render.
- Never let overlays bleed onto the endcard.
- Never upload or schedule from the design implementer role.

## Why

Hash-only or crop-only SIGN shipped stacked captions and non-viewer chrome.
Fragmented briefs omitted mandatory gate commands. One checklist every fresh
worker loads closes that gap without duplicating gate code.

## Check

Indep review brief includes the automated gate block above. Orchestrator SIGN
void without attached JSON + contact-sheet look. `worker_ce_entry.py lint` PASS
on the worker brief (`ce_load: required` + `tool.py entry`).

## Follow-up

[sequential WIP](sequential-wip.md), [multi-agent PDCA](multi-agent-pdca.md),
design-skills lesson index (+ index-extension for L40–L43), sibling video sign-off lesson.

### Doc-claim vs code (required on every lesson/gate PR)
- [ ] Each Verify bullet maps to a real command/flag/test in this diff
- [ ] Missing/empty input has a FAIL test (see [gate-fail-closed](gate-fail-closed.md))
- [ ] Defaults match the card (fail-closed, not silent opt-in)
- [ ] See [doc-claim-review](doc-claim-review.md)
