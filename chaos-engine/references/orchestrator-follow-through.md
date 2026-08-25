# Orchestrator follow-through

Orchestrators inspect live writers and required checks, consult, remove
impediments, and continue delivery. Assignment alone is not progress.

## Cadence and inspection

While any writer or required check is live, inspect at most every five minutes.
No live writers or required checks: no scheduler.

Each inspection must produce a decision, solved subproblem, re-spec, consult
queue, upgrade, or explicit keep. Never a heartbeat. Validate errors and
progress before the next handoff; bound retries and escalate instead of waiting
indefinitely.

## Consult and impediments

Ask what is blocked and what would unblock. Do not rewrite a healthy writer's
task. Remove tooling, access, and environment impediments;
coach the owner through how-to-work impediments; escalate owner-only decisions.

## Delivery and escalation

Opening a PR does not complete follow-through. Continue until the in-scope
delivery condition is met.

Escalate using portable capability levels: mechanical, default, and most
intelligent. Host dispatch prompts may map those levels to their own capability
labels; portable policy never binds them to a vendor.
