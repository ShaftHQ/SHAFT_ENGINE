# Playbook: promotion and orchestrated runtime

Moved out of [work-github-playbook.md](../work-github-playbook.md) so that file
can take another lesson without deleting this guidance.

For a meaningful event, record an evidence-consistent `signal`, then `assess`
it into a quarantined candidate using one distinct `--tracking-issue-url` per
incident. Behavior changes stay quarantined while
`evaluate` records a strict improvement comparison on the frozen adherence
corpus with zero unmeasured rules and no regression. The record is a
consistency summary, not proof that commands ran or reviewers are authentic.
Independently derive the live diff, rerun tests, and verify review artifacts.
Use `promote` to record intent only for the exact evaluated commit after the
selected terminal assurance; normal GitHub workflow must still perform
and verify the merge. Kernel-tier changes require
two independent reviewer keys, correctness/reproduction/safety lenses, and two
independent runs on the same commit and corpus. If a promoted change regresses,
use `repair-or-revert` to record one repair requirement; recurrence freezes the
candidate and records a revert requirement. The normal git/GitHub workflow
performs and verifies the repair or revert.

For an orchestrated runtime, the root first runs `create-runtime`. Every
dispatch then runs `register-participant` atomically before launching its
delegate. Before finalization, record schema-v2 evidence-bearing disposition
receipts (`fixed-now` with changed files plus passing proof, `existing`/`new`
with a tracking URL, or `blocked` with a privacy-safe queued learning payload).
`finalize-runtime` automatically harvests root and delegate receipts, live
session ledgers (failures, guard blocks, retries), and sibling incident
sources, closes membership, and rejects callers that omit a registered
participant or supply enum-only dispositions without evidence.
After closure no caller can replace, omit, or add participants. Every
registered participant must contribute incident dispositions, a structured
no-learning attestation, or, for an unreachable delegate only, an explicit
`attest-unavailable` record. Finalization reads membership from the frozen
closed registry rather than accepting a caller-supplied participant list. It
writes one immutable root-owned schema-v2 completion. Stop hooks validate that
artifact and never credit finalize command prose alone. Completions omit
transcripts, private routes, model identities, credentials, and local paths.
