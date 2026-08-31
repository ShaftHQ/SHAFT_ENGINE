# Orchestrator follow-through

Orchestrators inspect live writers and required checks, consult, remove
impediments, and continue delivery. Assignment alone is not progress.

## Cadence and inspection

While any writer or required check is live, including a live subagent, choose
one cadence from task scope: 5 minutes for short or high-risk work, 10 minutes
for normal work, or 15 minutes for long stable jobs. Inspect automatically,
without the owner asking. Do not inspect more often than the selected cadence
except on dispatch, completion, failure, or owner/delegate interrupt. First
inspect as soon as dispatch yields a live handle. Use the host timer or
scheduler when available; otherwise the next main-thread wake still owes the
inspection.
No live writers or required checks: no scheduler.

Each inspection is a Scrum-master duty: establish blocked or unblocked state,
report status with evidence, apply pressure when progress stalls, consult on
ambiguity, and remove blockers within granted authority. Then keep, re-spec,
upgrade, or kill. Never send a heartbeat. Validate errors and progress before
the next handoff; bound retries and escalate instead of waiting indefinitely.

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
