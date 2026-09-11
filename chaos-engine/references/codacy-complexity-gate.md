# Codacy Complexity gate

Learning from issue #5747 (Waves D/E of epic #5732): Codacy **Complexity /
NPath** repeatedly blocked merge after functional CI was already green.
Pattern: element-classifier growth lands as one fat `classify*` method, then a
second pass splits it.

## Iron rule

Treat Codacy **Complexity** `ACTION_REQUIRED` as a first-class red gate **equal
to a failing unit test**. Do not wait for GitHub unit jobs to finish when Codacy
Complexity already failed — extract helpers and re-push.

`scripts/agents/watch_pr_checks.py` already classifies `ACTION_REQUIRED` as
RED (Codacy and similar apps). Agent behavior must match that: triage Complexity
red immediately, same urgency as unit red.

## Checklist (before opening or pushing interaction / classifier PRs)

- [ ] New mobile / desktop / web kind branches land as **kind-family helpers**
      or first-match rule tables — not more sequential `if`/`return` arms on a
      single `classify*` method
- [ ] Prefer `Set` / `Map` buckets plus a loop over rules (NPath multiplies
      across sequential branches even when each guard is trivial)
- [ ] Codacy Complexity `ACTION_REQUIRED` == unit red: fix before waiting on
      other checks
- [ ] Local proof includes the touched classifier unit tests (and any
      interaction strategy tests that share those kinds)

## Hook

Portable PreToolUse soft reminder (non-blocking `additionalContext`) when a
mutation targets an `ElementClassifier` path or interaction `classify*` surface.
See [`hooks/guard.py`](../hooks/guard.py). Hosts that ignore soft context still
owe this checklist via the Level-1 catalog and router row.

## Boundaries

- Soft reminder never denies a tool call; Complexity red is still a delivery gate.
- Do not weaken Codacy findings or delete tests to clear the gate.
- Portable overlay owns the policy; host adapters stay thin pointers.
