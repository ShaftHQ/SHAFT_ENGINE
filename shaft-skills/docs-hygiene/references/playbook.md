# Docs-hygiene playbook

## Single sources of truth (FR-3)

Prefer a small SoT set for journey-based mobile automation:

1. **Plan** — progress tracking and coverage / hours calculations.
2. **Journey map** — full details of each journey, updated only after it is actually implemented.
3. **Issue tracker** — defects, categorized at least into **locator / missing accessibility** vs **product inconsistencies**.

Delete stale delivery reports, retros, and duplicate gap catalogues once durable content is merged into those SoTs (or GitHub issues). Push back only when something is still referenced by skills, CI sync scripts, or evidence links.

## Status vocabulary (FR-1)

Use exactly three statuses in plan / journey / scenario tables:

- `Done`
- `In progress`
- `Not started`

Do **not** invent status cells such as `Done (with comments)`, `Blocked`, `Planned`, or `Out of scope`. Remaining notes, bugs, out-of-scope reasons, and SHAFT upstream links belong in a **Comments / Remarks** column. Scrub retired words from legends, mermaid classDefs, sync color maps, and guard scripts.

## Hours math (FR-2)

- For every **Done** row: **Completed hours = Planned / Effort hours**.
- Bottom totals use live SUM formulas (or regenerated sync), not stale hardcoded numbers.
- Document when a workbook total includes a framework row outside the coding budget so Summary % cannot disagree with the plan prose.
- Overall completion % = completed / planned from the same totals row the Summary tab points at.

## Adversarial review (FR-4)

Independent review catches mechanical failures self-review misses:

- Broken or duplicate journey-map links after mechanical rewrites.
- Lost content when merging catalogues.
- Secret-bearing curl examples (use `sed` from gitignored keys).
- Status / hours / coverage drift between markdown, workbook, and Summary narrative.
- Evidence paths that break when folders move outside git.

Rules:

- After an independent **PASS**, any new commit on that HEAD must **invalidate or re-queue** review before merge (see #7709).
- Review against the owner's latest vocabulary rules, not the previous round's vocabulary.
- Prefer `sync --check` / journey-guards / secret_guard dry-runs before claiming clean.

## Delete-guard upstream branch (FR-5)

Some repos refuse path deletes until the branch exists **upstream**. Push an empty or docs-start branch first, then delete in a follow-up commit. That forced second push is not a "push once" violation when the guard requires it.

## Boundary

- English-only; no client names or secrets in public docs or this skill.
