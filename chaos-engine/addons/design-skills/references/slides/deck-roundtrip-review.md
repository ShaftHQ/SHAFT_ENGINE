---
name: deck-roundtrip-review
description: Use when a deck is re-uploaded or exported through a cloud slide tool, or when two agents review a deck set across rounds.
---

# Deck round-trip and review (S06)

Companion to [deck system](deck-system.md) and
[S05 measurable QC](deck-qc-measurable.md). The multi-agent loop and sign-off
rules are in the harness reference `multi-agent-pdca.md`.

## Re-upload and export risks

- A re-upload can change deck IDs, which breaks every link and id file.
  Keep an ids file with one entry per round (`replaced_<round>`) and confirm
  the folder lists exactly the expected files.
- After every re-upload check: native slide type, slide count, speaker
  notes (text diff per slide against the previous export), hyperlinks
  (count and target per slide), fonts (PDF embedded fonts and pptx
  typefaces), text frames, folder contents, "Next:" and breadcrumb strings.
  `design_qc.py pptxparity new.pptx --before old.pptx` covers notes, body
  text, links, typefaces, slide count and the size cap.
- **Export size cap**: the tool's pptx export has a cap (10 MB). A deck at
  1 % under it loses its export on the next image. Track the size per
  round, keep a headroom (default 5 %), compress before adding images.
- **Race with the implementer**: an export can predate a later edit.
  Compare the file's modified time with your export time and re-list
  before signing; if newer, re-export. Re-export from the cloud copy,
  never from a cached file.
- Upload with an explicit target mimeType
  (`application/vnd.google-apps.presentation`) or the file stays a pptx.
- **API edit traps**: moving a line by API detaches or diagonalises the
  connector; delete and redraw it glued. Images cannot be resized by API;
  re-insert at size. A slide re-import gives new slide and object IDs:
  re-resolve every ID, re-check notes and links on that slide.
- **Renumbering** (quiz 17 -> 18) touches footers and numbers only, never
  body text; `pptxparity` diffs body text (footer and slide-number fields
  ignored) and matches slides by content when counts differ.
- Prefer editing in place (same ID, partial slide re-import) over a full
  re-upload; the changelog names exactly which slides changed.

## Review process

- Keep roles separate; the reviewer is read-only on the shared folder. A
  self-reviewed delivery still drew ten rejects from a separate pixel and
  geometry reviewer.
- A reject is deck, slide, issue (measured), expected (measured). A vague
  reject costs a round.
- Review the delta each round: re-run everything on fresh exports, pixel-diff
  against the previous round (`design_qc.py slidediff`) to bound the review
  to the changed slides, then read those slides at full size.
- The implementer's changelog is a list of claims; verify each with data. A
  claimed padding fix is where regressions hide ([S05](deck-qc-measurable.md)).
- Review artefacts carry no client names, learner records or prices.
- Commit explicit paths only (no `git add -A`, no caches); report a blocked
  or failed push instead of working around it.

## Long reviews and resume

A usage cap or context compaction can cut any long review; persist state as
files, not memory.

- Keep an ids file, the implementer's changelog, a progress file, per-round
  folders (source, renders, data, tools) and the committed analyzer scripts;
  write the review file as you go and commit it once it is true.
- Number each step in a README and make it idempotent (export, extract,
  spans, render, analyzers, merge, diffs). A resumed agent reruns from step
  1 on fresh exports instead of trusting half-finished data.
- Keep a baseline export per round so notes, links, fonts and pixel diffs
  can be recomputed.
- On resume, re-check external state first: modified times, the folder
  listing, branch and HEAD against the last pushed commit.
- Decode large tool results through a script saved to disk so the step
  survives a resume.
