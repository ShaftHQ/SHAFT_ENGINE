---
name: pre-done-self-review
description: >-
  Use before claiming Done, signing off, or handing off: reconcile every item
  with all standing rules, cite rule and alternative for each gap, check views.
  On demand only.
license: MIT
permissions:
  - read
---

# Pre-done self-review

Green tests are not Done. Before you say "Done", sign off as owner or
reviewer, or write a handoff note, run this review on the status report (PR
body, handoff note, or status doc). Run
`python3 .chaos-engine/tool.py retrieve --store graphify "<task> standing rules"` and
`python3 .chaos-engine/tool.py retrieve --store mempalace "<task> standing rules"` first.

## Steps

1. **Rules reconcile.** List every standing rule that applies: user memory,
   project and harness skills, the overlay, and the item's Definition of
   Done. Check every tracked item against each rule, not only against tests.
2. **Gaps cite a rule and an alternative.** Every `Blocked` or `Skipped` row
   carries `rule: <rule that permits it>` and `alt: <mock, fixture, stub,
   fake, simulator, or recording tried>`. If a rule says a gap needs a
   substitute (for example blocked-on-data needs mock coverage), build the
   substitute; it is not a gap you can report.
3. **Derived views match the source.** Diagrams (mermaid), summaries, and
   counts are views of the status table. Regenerate them, or edit them in
   the same change as the table.
4. **Adversarial pass.** Write a `## What's missing` section that answers
   the user's next question, "are we missing any?": items not tracked,
   rules not checked, and evidence not opened this turn.
5. **Machine check.** Run the gate ([`pre_done_review.py`](../../pre_done_review.py)), fix every finding, then re-run it:

   ```bash
   python3 .chaos-engine/tool.py pre-done report.md          # 0 pass, 1 findings, 2 usage
   python3 .chaos-engine/tool.py pre-done report.md --json
   ```

## What the gate enforces

- A status table exists (first markdown table with a `Status` column; the ID
  is in the first column).
- Mermaid drift: a node `ID[status]` or `class ID status` that disagrees with
  the row's status fails.
- Blocked/Skipped rows without `rule:` or without `alt:` naming a substitute fail.
- `alt:` must point at an existing covering test file (`path` or `path::name`); "no", "not yet",
  "candidate", "TODO" or "pending" before the first path fail.
- Non-data blocks (human-only step, platform-only, feature disabled) may write
  `alt: none-possible (<reason>) verified: <how>`; data-blocked rows (test data, fixture, mock,
  seed, account) still need an existing covering test.
- A "nothing"/"none" item under `## What's missing` fails without a `verified: <how>` note.
- Declared counts in prose (`Blocked: 3`) that differ from the table fail.
- A missing or empty `## What's missing` section fails.

The gate cannot judge step 1 or the quality of step 4. Those stay manual and
belong in the reviewer's PDCA Check.
