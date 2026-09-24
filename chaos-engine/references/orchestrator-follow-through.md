# Orchestrator follow-through

Orchestrators inspect live writers and required checks, consult, remove
impediments, and continue delivery. Assignment alone is not progress. Enforce
[process-owner](process-owner-scrum-master.md) MUST gates by
link; do not restate those invariants here.

## Cadence and inspection

While any writer or required check is live, including a live subagent, choose
one cadence from task scope: 5 minutes for short or high-risk work, 10 minutes
for normal work, or 15 minutes for long stable jobs. Inspect automatically,
without the owner asking. Do not inspect more often than the selected cadence
except on dispatch, completion, failure, or owner/delegate interrupt. First
inspect as soon as dispatch yields a live handle. Use the host timer or
scheduler when available; otherwise the next main-thread wake still owes the
inspection.
No live writers or required checks: no scheduler — **unless** the owner asked
for unattended babysitting or adaptive follow-ups; then follow
[process-owner adaptive follow-up](process-owner-scrum-master.md#adaptive-follow-up-orchestrator-duty)
and retune **x** each cycle from evidence (tighten under active CI/downloads,
stretch when quiet, stop when terminal).

Each inspection is a process-owner duty: establish blocked or unblocked state,
report status with evidence, apply pressure when progress stalls, consult on
ambiguity, and remove blockers within granted authority. Then keep, re-spec,
upgrade, or kill. Never send a heartbeat. Validate errors and progress before
the next handoff; bound retries and escalate instead of waiting indefinitely.

## Consult and impediments

Ask what is blocked and what would unblock. Do not rewrite a healthy writer's
task. Remove tooling, access, and environment impediments;
coach the owner through how-to-work impediments; escalate owner-only decisions.

## Delivery and escalation

Implementers write code directly. Offload to a local writer when
[when-to-use-local](../skills/local-agency/references/when-to-use-local.md)
favors it (#6171).

Opening a PR does not complete follow-through. Continue until the in-scope
delivery condition is met. Unattended watch is one blocking
`python3 scripts/agents/watch_pr_checks.py --pr <n> --until-merged`: one line (repo-only)
on GREEN, RED, or MERGED. No second poll, no heartbeat, no status table.
A host "you will be notified" line does not end the turn. Ending the turn,
a completion summary, or a worker_completed record while that watch is pending
is a failed delivery. If the host backgrounds the command, resume attaches to
that same task id and does not start another poller. Resume text is
`scripts/agents/unattended_delivery.py`.

A scheduled status routine, a babysit poll, and parent narration about the same PR are overlapping status channels and a contract failure; the live watch lease owns status ([one status channel](process-owner-scrum-master.md#one-status-channel-6163)).

When the owner asks to **deliver**, **babysit**, or keep the work going, that
is the goal: finalize the in-scope outcome in the best honest way. Keep
iterating after red CI, a merged partial slice, or a stalled delegate. Re-spec,
expedite, and support writers. Compaction is not stop. Compaction restores the
watch task id, pull request number, repository, head SHA, and the
not-done-until-merged bit. The resumed turn waits on that task. It does not
re-read the router before MERGED or RED. A quiet interval with no
live writer is not permission to end babysit until the delivery condition is
met or a named HALT applies.

Escalate using portable capability levels: mechanical, default, and most
intelligent. Host dispatch prompts may map those levels to their own capability
labels; portable policy never binds them to a vendor.
