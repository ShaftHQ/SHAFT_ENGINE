# Design / spec turn contract (local openai-compat writer)

## When

`--mode design` or `--with-ce-brief` on `dispatch.py` chat/config/argv for a
bounded design or GitHub-issue spec draft. Host remains the acceptor.

## Required writer closing

After the deliverable, the local writer MUST end with exactly one line:

```text
CE_BRIEF_LOCATORS: <path1> | <path2> | …
```

Rules:

- Paths MUST be copied verbatim from the system brief text (locator-only lines).
- Do **not** invent product labels or umbrella repo names as locator paths.
- If the brief is empty/skipped, write `CE_BRIEF_LOCATORS: none`.

## Spec schema (GitHub issue body)

When asked for a ticket spec, emit Markdown with these sections only:

1. `## Goal`
2. `## Context` (current plugin/CE surfaces that exist — name real paths)
3. `## Acceptance` (checkbox list, testable)
4. `## Out of scope`
5. `## First slice` (smallest PR-shaped change)
6. `## Validation` (unit/UI/manual)
7. `## Risks`
8. Closing `CE_BRIEF_LOCATORS:` line

## Host coach gates (accept / reject)

Reject and re-coach when any gate fails:

| Gate | Fail signal |
| --- | --- |
| Brief citation | Missing/wrong `CE_BRIEF_LOCATORS` vs injected brief |
| Reality | Named classes/files do not exist and were not marked `NEW` |
| Schema | Missing required sections |
| Scope | Multi-epic sprawl in one ticket |
| Fluff | Generic IDE ideas with no product/CE capability tie |

On reject: paste the failing gate + evidence (missing path, wrong locator list,
schema diff). Do not praise partial free-form.


## Default first ask

The first design request is a skeleton that already contains the exact
`CE_BRIEF_LOCATORS` line. The writer must leave that line unchanged. The host
accepts the draft only after `design_turn_gate.py` citation passes.

## Related

- [coach-loop.md](coach-loop.md) section D
- `ce_brief.py` (#6067)
- Dispatch modes (#6073)
