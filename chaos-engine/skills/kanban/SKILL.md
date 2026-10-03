---
name: kanban
description: >-
  Use when work has several deliverables, tickets, or delegates: board, WIP 1
  writer + 2 review, pull rule, Definition of Done, findings fixed or filed.
license: MIT
---

# Kanban task flow

Use when work has more than one deliverable, more than one ticket, or any
delegate. Flow beats busyness: finish before starting.

## Board

Keep one board per task as JSON (models corrupt JSON less than prose) in the
session state directory, never in tracked files:

```json
{"wip": {"doing": 1, "review": 2},
 "cards": [{"id": "T1", "title": "...", "column": "ready",
            "acceptance": ["..."], "proof": "command that proves it",
            "evidence": null, "pr": null}]}
```

Columns: `backlog` -> `ready` -> `doing` -> `review` -> `done`.

- `ready` requires acceptance criteria and a proof command. No criteria, no pull.
- `doing` WIP **1 writer** per repository: one card being edited at a time.
- `review` WIP **2**: when two cards wait for review or CI, stop pulling and help
  finish them.
- `done` only when the Definition of Done holds.

## Pull rule

Pull the highest-priority `ready` card only when the downstream column has
capacity. Never push work forward; never start a card to fill idle time while
review is full. Group related cards into the fewest safe pull requests.

## Definition of Done

A card is done when all hold:

1. Every acceptance criterion is met and its proof command output is attached.
2. Tests for new behavior were red first, then green.
3. One fresh-context review is clean; a second round only for blocker findings.
4. Required checks are green on the exact head after one blocking wait per push
   (`gh pr checks --watch --fail-fast` or `gh run watch`); retry once, then stop
   and report. Never re-verify what already passed.
5. Every finding raised while working is closed (see below).
6. Merged or handed off as instructed; worktree cleaned.
7. After any CE harness change merges, every agent and delegate reloads the
   latest harness from main before continuing. After each completed delivery
   run `python3 .chaos-engine/tool.py maintain` (fast-forward with local edits
   kept, reinstall, doctor, stale-store refresh, reload).
8. Ship to-deliver items in the fewest pull requests; group related issues.

Check items 2 and 4 plus labels and links with `python3 .chaos-engine/tool.py dod <PR>`
(table; exit 1 names each gap).

## Findings policy

Every finding ends as exactly one of:

- **fixed** in the current delivery, or
- **filed** as an issue, allowed only when it is both outside the task's scope
  and not critical, blocker, or high severity.

Nothing stays only in chat, a report, or a local file. The final report lists
each finding as fixed (with PR) or filed (with issue).

## Waste checklist

Before each pull, drop anything that is partially done work, extra process,
extra features, task switching, waiting, handoffs without context, or defects
passed downstream (Poppendieck's seven wastes).

## Status

Every status report is a Markdown table, one row per card
(`ID | Issue(s) | PR | Status | Proof or blocker`), then Risks. Never prose or
bullet lists. No separate status rituals.
