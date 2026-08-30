# Orchestrator follow-through

Orchestrators inspect live writers and required checks, consult, remove
impediments, and continue delivery. Assignment alone is not progress.

## Cadence and inspection

While any writer or required check is live, including a live subagent,
automatically inspect on a five-minute cadence. Floor: inspect at least every
five minutes, without the owner asking. Ceiling: do not inspect more often than
every five minutes except on dispatch, completion, failure, or owner/delegate
interrupt. First inspect as soon as dispatch yields a live handle. Use the host
timer or scheduler when it has one; otherwise the next main-thread wake still
owes the inspection.
No live writers or required checks: no scheduler.

Each inspection is a Scrum-master duty: establish blocked or unblocked state and evidence
of progress, then keep / re-spec / upgrade / kill. Never a heartbeat. Validate
errors and progress before the next handoff; bound retries and escalate instead
of waiting indefinitely.

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
