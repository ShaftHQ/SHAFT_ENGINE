# Reflection checkpoints

Reflection is optional task-scoped evidence, not a mutation or completion gate.
It extends the existing append-only session ledger with bounded structured
outcomes and receipts. Never store prompts, transcripts, raw logs or errors,
credentials, source excerpts, or user-specific absolute paths.

## Triggers and depth

- Reflect after multiple failures in the same area when it will change the next
  diagnostic experiment. One-off failures do not trigger reflection work.
- Other semantic triggers may be recorded when useful, but never interrupt an
  implementation mutation, diagnostic rerun, delivery action, or Stop.
- A diagnostic or capability probe recorded as `non_attempt` does not advance
  the counter, but it also does not clear a pending checkpoint.
- At the end of a long session, a terminal receipt is optional and never delays
  delivery or the final Stop.

The failure fingerprint is a digest of low-cardinality fields only: phase,
target, failure class, platform, invariant, and head. Raw host error text is
never fingerprint input or ledger content.

## Checkpoint workflow

Pending reflection state is advisory. Every tool remains available, including
implementation mutation, reruns, delivery, and Stop.

1. Reconstruct the bounded fingerprint and classify the failure.
2. State the failed assumption and challenge it.
3. Compare at least two approaches.
4. Choose one changed diagnostic experiment and improve observability if needed.
5. Run the experiment, record its outcome, route any durable learning, and
  append a validated receipt with `scripts/agents/reflection.py`.
6. Continue regardless of whether a receipt is appended.

The receipt schema requires `schemaVersion`, task/session identity, a trigger
enum (`second-failure`, `repeated-fingerprint`, `third-fix`,
`platform-disagreement`, `review-repeat`, `user-correction`, `guard-repeat`,
`safety-incident`, `scope-expansion`, `premise-invalidated`, or
`long-session-completion`), `failureFingerprints`, `failedAssumption`, `approachesCompared`,
`chosenExperiment`, `changedApproach`, `proofCommandOrCheck`, `proofOutcome`,
and a `durableDisposition` enum (`guidance-fixed`, `issue-filed`,
`knowledge-recorded`, `nothing-durable`, or `degraded`). When supplied, `issue`
must be a canonical GitHub issue URL. For `long-session-completion`, it is
required: search open and closed issues for duplicates, create a standalone
issue when the optimization is actionable, read it back, and only then append
the receipt with that issue URL. A session or local-only reference is invalid.
A `long-session-completion` receipt also requires `tokenConsumer` and
`nextSessionOptimization`: identify what consumed the most tokens, then name a
concrete harness or workflow change that improves accuracy and token use next
session. The terminal summary labels these `Main token consumer:` and
`Next-session optimization:` alongside the existing time analysis.
Stores and GitHub are optional: an unavailable service never prevents the
local receipt or resumption, and hooks never create or update issues.

For semantic events the hook cannot infer safely, record the trigger explicitly
with `py -3 scripts/agents/reflection.py trigger --session-id <id> --trigger
<enum>`. Append a receipt without creating a blocked intermediate file using
`py -3 scripts/agents/reflection.py receipt --session-id <id> --session-token
<token> --json <receipt-json>`. Mark only a proved setup, syntax, or capability
probe by exact ID with the `non-attempt` subcommand. Portable installs use the
same subcommands through `.chaos-engine/hooks/reflection.py`.

For a terminal receipt, the final user-facing summary must label the elapsed
estimate, main time consumer, repeated failures or corrections, changed
assumption or approach, successful proof, remaining risk or follow-up, and
Learning Loop disposition.
