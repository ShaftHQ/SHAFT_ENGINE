---
name: ui-delivery
description: Use when a change touches user-visible UI, layout, styling, or themes: red-then-green e2e, measured geometry, viewport x theme matrix.
---

# UI delivery

Load when the deliverable changes what a user sees: layout, styling,
components, pages, themes, or responsive behavior. Pair with
[deep research](deep-research.md) when the issue asks for best practices.
Return to the router after delivery.

## Plan

- Before the first edit, name the surface, every theme (for example dark and
  light), and the matrix below.
- Best-practice asks need cited sources (NN/g, WCAG 2.2, Baymard, web.dev,
  MDN). Map each practice to a concrete change or to existing compliance in a
  PR-body checklist. An uncited practice is a guess.
- Find which PR workflow runs the UI/e2e suite:
  `rg -n "pull_request|playwright|e2e" .github/workflows`. A suite that runs
  only on push or deploy is a CI coverage gap. Record it now, not after green.

## Red, then green

Run every new or edited UI/e2e test against a build of the base branch first.
It must fail for the stated reason (element missing, nav font fixed at every
width, measured empty gap). Put the failing assertions in the PR. A test never
seen red is not evidence.

## Measure, do not eyeball

Assert geometry in e2e tests: `scrollWidth <= innerWidth`, gap and grid-cell
sizes and empty-cell counts from `getBoundingClientRect()`, and computed font
sizes per viewport. Screenshots support review. They never replace an
assertion.

## Wide tables

- Scroll the table, not the page: wrap the real `<table>` in `<div
  role="region" tabindex="0" aria-labelledby="<caption or heading id>">` with
  `overflow-x: auto` and a 3:1 `:focus-visible` outline. WCAG 2.2 1.4.10
  exempts only the table, so `scrollWidth <= innerWidth` still holds at 320;
  2.1.1 needs the tab stop.
- Keep table semantics: a `<caption>` or labelled heading, `<th scope>`. No
  `display` override on table parts, no `role="grid"`, no card stacking.
- Restructure instead when cells hold paragraphs or lists, rows differ, it is
  layout, or there are too many columns: split, use prose or lists, or swap axes.
- Verify: axe `scrollable-region-focusable` passes, Tab reaches the named
  region, ArrowRight scrolls it, in every theme. Sources: Roselli, Pickering,
  GOV.UK, USWDS, MDN.

## Visual matrix

Capture Playwright screenshots before and after, in every theme, at 390x844,
768x1024, 1440x900, 2560x1440, and 3840x2160. Sweep widths from 320 to 3840
for horizontal scroll. Inspect each shot for large empty regions, crowding,
overlap, and nav readability. 2560 and 3840 are required: designers and
editors use them, and they are the widths most often skipped.

## Test-contract changes

When an existing guard blocks a legitimate change, narrow it to the real
invariant ("portrait videos are never cropped", not "no CSS targets portrait
videos"). Never delete it (iron law 4). Prove the narrowed guard still fails on
the original bad case: reapply that case, run, read the failure, revert. List
the change under `Test contract changes` in the PR body.

## Review

Terminal adversarial review is mandatory when a UI PR changes a test contract
or PR CI does not run the changed surface. Time pressure is not a skip reason.
Keep it cheap: one reviewer, one round, given the diff, the red-then-green log,
and the matrix screenshots.

## PR body and report

- `CI coverage`: when PR CI does not run the changed surface, write "PR CI
  green does not cover <surface>" and attach the local run (command, counts,
  screenshots).
- `Post-merge deploy`: when deploy is manual (`workflow_dispatch`, for
  example GitHub Pages), name the workflow and the command to run after merge
  (`gh workflow run <file> --ref <default>`).
- In a consumer repository, deliver from a separate worktree
  ([task isolation](task-isolation.md)) so overlay files never reach the diff.
