---
description: Use when a hot-spot dispatch change must treat a static-analysis Complexity ACTION_REQUIRED as a unit failure.
---

# Complexity gate

Static-analysis **Complexity / NPath** findings often block merge after
functional CI is already green. The usual pattern: a hot-spot dispatcher grows
as one fat method, then a second pass has to split it.

## Iron rule

Complexity is one category of the wider
[Static-analysis ACTION_REQUIRED gate](static-analysis-gate.md): any
≥medium `ACTION_REQUIRED` finding blocks merge the same way.

Treat a static-analysis **Complexity** `ACTION_REQUIRED` as a first-class red
gate **equal to a failing unit test**. Do not wait for unit jobs to finish when
Complexity already failed: extract helpers and re-push. The repository CI
watcher classifies `ACTION_REQUIRED` as RED; triage it with the same urgency as
unit red.

## Checklist (before opening or pushing hot-spot PRs)

- [ ] New kind branches land as **kind-family helpers** or first-match rule
      tables, not more sequential `if`/`return` arms on one dispatch method
- [ ] Prefer `Set` / `Map` buckets plus a loop over rules (NPath multiplies
      across sequential branches even when each guard is trivial)
- [ ] Static-analysis Complexity `ACTION_REQUIRED` == unit red: fix before
      waiting on other checks
- [ ] Local proof includes the touched dispatcher unit tests and any tests that
      share those kinds

## Hook

A portable PreToolUse soft reminder (non-blocking `additionalContext`) fires
when a mutation targets a hot spot that the active profile declares in its
`profile.json` `complexityHint` (`pathMarkers`, `nameMarkers`). See
[`hooks/guard.py`](../hooks/guard.py). Hosts that ignore soft context still owe
this checklist via the Level-1 catalog and router row.

## Boundaries

- The soft reminder never denies a tool call; Complexity red is still a
  delivery gate.
- Do not weaken static-analysis findings or delete tests to clear the gate.
- The portable overlay owns the policy; host adapters stay thin pointers.
