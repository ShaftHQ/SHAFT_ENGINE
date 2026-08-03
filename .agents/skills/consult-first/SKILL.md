---
name: consult-first
description: >-
  Deliberation gate. Use at the start of every task before discovery or edits to
  size the work and choose an approach, and use again before any hard-to-reverse
  step or when an approach stops holding.
---

# Consult

You are here because the entrypoint's triage selected a depth beyond the
trivial row. Decide what good looks like and how the change will be proved
before touching anything. No edits yet.

Triage is unconditional and lives in the entrypoint, so a trivial task never
loads this file. Arrive already knowing which points you owe. If triage and the
user's framing disagree about size, say so in one line and work to the larger.

## Full pass

1. **Deliverable and proof of done.** Name the artifact the user gets and the
   single observation that proves it works. If you cannot name the observation,
   you do not yet understand the request.
2. **Unknowns.** List what you do not know that could invalidate the approach.
   Rank by how cheaply each can be settled, and settle the cheapest fatal one
   first.
3. **Invariants.** Name what must not break: public API, persisted or wire
   format, security and trust boundaries, accessibility, error handling,
   performance, and behavior users already rely on.
4. **Two rival approaches.** Write both, then steelman the one you intend to
   reject — argue its strongest case in its own terms. If the steelman wins,
   switch. One approach is not a decision; it is the first idea.
5. **Choose by removability.** When both hold, take the one that is easier to
   delete later. Prefer the smallest change that fixes the root owner of the
   invariant rather than each symptom.
6. **The RED test.** State the focused check that fails today for the right
   reason, where it lives, and the exact command that runs it. If no check can
   observe the change, say what evidence replaces it.
7. **Delegation shape.** Decide what stays on the main thread and what is
   bounded enough to assign. Compare file scopes before running anything
   concurrently; isolate independent writers.
8. **Completion gate.** State what you will run, read, and show to close this
   out, and who reviews it.

## Lifecycle

Work runs in this order, and each phase ends before the next begins:

analyze -> plan -> design -> RED -> GREEN -> refactor -> commit ->
pull request -> babysit to green -> merge.

Every phase that changes behavior ends with the independent adversarial review
defined in [delegation](../act-as-mohab/references/delegation.md), at the depth
this task's triage set.

## Output

Report the triage result, the depth you took, and only the points that depth
included. A partial pass reports partially; do not pad it with points you were
told to skip.
