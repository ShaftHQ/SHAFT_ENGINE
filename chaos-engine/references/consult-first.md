# Consult

You are here because the entrypoint's triage selected a depth beyond the
trivial row. Decide what good looks like and how the change will be proved
before touching anything. No edits yet.

Triage is unconditional and lives in the entrypoint, so a trivial task never
loads this file. Arrive already knowing which points you owe. If triage and the
user's framing disagree about size, say so in one line and work to the larger.
During planning, keep asking material follow-ups until the plan is
decision-ready. After owner approval, execution is unattended; dispatch a
consultant agent for implementation ambiguity rather than asking the user.

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
9. **Reflection boundary.** Resolve failure fingerprints, diagnostic/recovery
   actions, receipt proof, and the one-hour terminal state in the matrices when
   repeated failure or a long session is plausible.

## Executable specification for consequential work

For cross-cutting or hard-to-reverse work, the three matrices below are mandatory.
Every required cell must be resolved. Blank cells, TODO, TBD, placeholders,
and guesses are invalid, even when qualified by explanatory text.
Any unresolved cell blocks RED/GREEN.
Every acceptance criterion and invariant must map to positive and negative proof.
At least one sibling/caller omission mutation must fail.
Record the completed matrices on the target GitHub issue comment before the first
implementing commit.

Use exactly these names and columns, in this order. Add one resolved data row
for every caller/site, every state/transition/failure mode, and every acceptance
criterion/invariant.

### Resolved caller matrix

| Site | Effective cwd/path | Runtime/version/platform | Permissions/trust | Configuration precedence | Input existence |
| --- | --- | --- | --- | --- | --- |

### State/failure matrix

| State | Immutable ownership | Preflight | Mutation order | Mixed state | Atomicity | Concurrency | Idempotency | Recovery | Fail-closed |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

### Acceptance-to-proof map

| Criterion or invariant | Positive proof | Negative or mutation proof | Command |
| --- | --- | --- | --- |

Use #4649 and #4650 as mandatory regression prompts when relevant. Cover each applicable scenario:

- Effective working-directory/path resolution.
- Interpreter/version/conditional dependency marker.
- Mixed owned+unknown preflight.
- Immutable ownership.
- Atomic backup/concurrent replacement.
- Post-migration adapter/link resolution.

## Plan artifact destination

Choose the repository-safe destination before any imported design or planning
workflow writes a file. For issue-backed work, persist the approved design and
implementation plan on the target GitHub issue. For non-issue work, keep them
transient unless the owner explicitly approves an existing operational-guidance
location. Never create or write `docs/superpowers/**` in this repository; this
rule overrides an imported skill's default path.

## Lifecycle

Work runs in this order, and each phase ends before the next begins:

analyze -> plan -> design -> RED -> GREEN -> refactor -> commit ->
pull request -> babysit (fix review comments and failed tests) to green -> merge ->
terminal reflection when owed -> one root Learning Session -> final report.

Behavior changes receive only the planning-approved terminal adversarial review
defined in [delegation](delegation.md), after the final scope commit and automated
finding batch. Never force review between phases or actions.

## Output

Report the triage result, the depth you took, and only the points that depth
included. A partial pass reports partially; do not pad it with points you were
told to skip.
