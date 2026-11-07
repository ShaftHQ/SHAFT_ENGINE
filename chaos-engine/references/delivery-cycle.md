# Delivery cycle

Standard order for ChaosEngine work. A one-file reversible change still uses
this order, with a one-line bigger-picture note. The cap is not a quota.

## Order

1. Architect. Bigger picture first, then the architect tier for the blast
   radius. A public contract records the premise, the options, the choice,
   and the non-goals.
2. Run at most 3 PDCA iterations. Each iteration is Plan, Do, Check, Act. Check
   is the tests and evidence for that iteration, including the in-session
   review phase. Stop when Check accepts. Do not start a fourth iteration.
3. Run two independent adversarial reviews of that accepted head, after required checks are green on that head.
   The author is neither reviewer. The
   reviewers are separate instances, both prompted to refute, and both must
   pass on the same head SHA. A blocking finding is Act. If an iteration
   remains, fix it, Check again, and repeat both reviews on the new SHA. If
   none remains, stop and report. Do not review an unchanged head a third time.
   A new commit invalidates both passes.

## Not a quota

"At most 3" does not mean three iterations are required. Zero extra
iterations is the normal case when the first Check accepts.

## Adoption

The core card, kanban Definition of Done, router contract, and delegation
point here. They do not keep a one-review exception. Investigation close
rules: [investigate lessons](investigate-lessons.md).

## Decision

Chosen over a single fresh review, with a second pass only when a blocker
appears, because one pass was signing public-contract changes, and an
uncapped loop was repeating review on heads whose checks were not green.
Dual review starts only on a green head. The cap stops the loop.
