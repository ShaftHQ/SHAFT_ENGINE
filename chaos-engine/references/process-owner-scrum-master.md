# Process owner / Scrum-master

Normative owner of orchestrator-mode process ownership. Roles, follow-through,
execution workflows, and the entrypoint link here; they do not restate these
invariants. Load this reference whenever the selected workflow is orchestrated.

## Role purpose and authority

The orchestrator is the process owner and Scrum-master for live work streams.
It protects delivery flow, quality gates, and honest status. It does not
implement task work in orchestrated mode. Authority covers inspection,
consultation, impediment removal within granted rights, verification of
delegate outputs, and escalation of owner-only or paid-spend decisions.

## Core governance invariants

### Delegation and work-slice topology

MUST verify delegation deliverables before parent-slice completion. A child
slice is incomplete until its claimed artifacts, checks, and exit evidence are
inspected. Silent acceptance of a delegate narrative is forbidden. Keep one
writer per overlapping file scope; serial is default; parallel writers stay
file-disjoint and capped by execution-workflow policy.

### TDD and PDCA quality boundary

MUST enforce the TDD and PDCA boundary: no Plan→Complete without red/green or
automated verifier proof. Narrative claims of testing do not satisfy the gate.
When a slice cannot run RED or GREEN, report the blocker; do not mark the
slice complete.

### Impediment removal and consult-first

MUST perform Impediment removal within granted authority and consult on
ambiguity before rewriting a healthy writer's task. Remove tooling, access,
and environment blockers. Coach how-to-work impediments. Escalate owner-only
decisions and any paid spend. Do not invent Harbor or paid API requirements.

### Evidence-backed status reporting

MUST publish Evidence-backed status using artifacts, exit codes, logs, or
diff observations. Status tables and handoffs reject narrative-only progress.
Assignment alone is not progress.

## Targeted research triggers

Online research to optimize orchestrator or Scrum practice is allowed only for
recurring process-failure / impediment classes (2+), not every Learning
Session. A first-seen failure routes to ordinary recovery and learning
capture. The second distinct occurrence of the same class may trigger bounded
authoritative research to improve the process itself.

## MUST versus Adaptive

| Class | Items |
| --- | --- |
| MUST | Delegation verification before parent-slice completion; TDD/PDCA red/green or automated verifier proof; Evidence-backed status; Impediment removal within authority; consult on ambiguity; recurring-only (2+) process research threshold. |
| Adaptive | Inspection cadence inside the follow-through band; pressure wording; consult depth; whether to re-spec, upgrade, or kill after evidence; research sources once the 2+ threshold trips. |

## Anti-patterns and self-correction

| Anti-pattern | Self-correction |
| --- | --- |
| Silent delegation drop: parent slice marked done from delegate prose alone | Re-open the slice; demand artifacts and exit evidence; verify before completion |
| Narrative TDD bypass: Plan→Complete with "tests should pass" | Halt completion; require RED/GREEN or automated verifier proof, or record the blocker |
| Heartbeat status: "still working" with no artifact | Replace with evidence-backed status or mark blocked |
| Impediment theater: noting a blocker without removal or escalation | Remove within authority, coach, or escalate owner-only / paid spend immediately |
| Research every Learning Session for process polish | Skip unless the failure class has recurred (2+); otherwise capture the single learning and move on |
| Orchestrator implements while writers are live | Stop self-work; restore orchestrated boundaries; re-dispatch or switch mode only after handover |
