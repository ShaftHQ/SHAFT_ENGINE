# Reflection checkpoints

Reflection is a task-scoped circuit breaker, not a diary. It extends the
existing append-only session ledger with bounded structured outcomes and
receipts. Never store prompts, transcripts, raw logs or errors, credentials,
source excerpts, or user-specific absolute paths.

## Triggers and depth

- After two attempted failures with different bounded fingerprints, stop for
  task reflection.
- After two attempted failures with the same fingerprint, stop for deep
  reflection. A third same-symptom fix attempt without a receipt is forbidden.
- Also stop after a third fix attempt, two local/CI disagreements, repeated
  review or user corrections, repeated guard blocks, a safety incident, scope
  expansion, an invalidated premise, or an unchanged rerun.
- A diagnostic or capability probe recorded as `non_attempt` does not advance
  the counter, but it also does not clear a pending checkpoint.
- When a session exceeds one hour, complete terminal reflection after delivery
  and before the final Stop. `stop_hook_active` never bypasses this rule.

The failure fingerprint is a digest of low-cardinality fields only: phase,
target, failure class, platform, invariant, and head. Raw host error text is
never fingerprint input or ledger content.

## Checkpoint workflow

While reflection is pending, read-only diagnosis, planning/tracker updates, a
changed diagnostic experiment, and receipt creation remain available.
Implementation mutation and unchanged test reruns are blocked.

1. Reconstruct the bounded fingerprint and classify the failure.
2. State the failed assumption and challenge it.
3. Compare at least two approaches.
4. Choose one changed diagnostic experiment and improve observability if needed.
5. Run the experiment, record its outcome, route any durable learning, and
  append a validated receipt with `scripts/agents/reflection.py`.
6. Resume only after the receipt matches the exact pending fingerprint set.

The receipt schema requires `schemaVersion`, task/session identity, a trigger
enum (`second-failure`, `repeated-fingerprint`, `third-fix`,
`platform-disagreement`, `review-repeat`, `user-correction`, `guard-repeat`,
`safety-incident`, `scope-expansion`, `premise-invalidated`, or
`long-session-completion`), `failureFingerprints`, `failedAssumption`, `approachesCompared`,
`chosenExperiment`, `changedApproach`, `proofCommandOrCheck`, `proofOutcome`,
and a `durableDisposition` enum (`guidance-fixed`, `issue-filed`,
`knowledge-recorded`, `nothing-durable`, or `degraded`). An optional `issue`
must be a GitHub issue URL.
Stores and GitHub are optional: an unavailable service never prevents the
local receipt or resumption, and hooks never create or update issues.

For semantic events the hook cannot infer safely, record the trigger explicitly
with `py -3 scripts/agents/reflection.py trigger --session-id {id} --trigger
{enum}`. Append a receipt without creating a blocked intermediate file using
`py -3 scripts/agents/reflection.py receipt --session-id {id} --session-token
{token} --json {receipt-json}`. Mark only a proved setup, syntax, or capability
probe by exact ID with the `non-attempt` subcommand. Portable installs use the
same subcommands through `.chaos-engine/hooks/reflection.py`.

For a terminal receipt, the final user-facing summary must label the elapsed
estimate, main time consumer, repeated failures or corrections, changed
assumption or approach, successful proof, remaining risk or follow-up, and
Learning Session disposition.
