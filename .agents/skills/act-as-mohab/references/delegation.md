# Delegation

Delegation distributes work, never responsibility. Select capability from
uncertainty, blast radius, and reversibility. Never bind policy to a vendor,
product name, or runtime setting.

## Capability levels

Every host exposes three levels. Refer to them only by these names.

### Most intelligent model

Architecture, cross-cutting or hard-to-reverse decisions, high-blast-radius
consultation, and independent adversarial review. It owns tradeoffs and returns
a decision or a spec; it does not become default labor.

### Default model

Bounded implementation, debugging, review, testing, docs, and normal research.
This is the standard choice. A default-model owner may assign only
fully-specified mechanical slices downward, and must inspect the returned work
before using it.

### Mechanical model

Spec-exact repetitive edits, inventory, formatting, deterministic
transformation, and log or result triage. No host exposes it as a subagent
type, so the mechanical helper role in [roles](roles.md), which defines its
limits, is carried in the dispatch prompt.

## Main-thread duties

Orchestrator retains decomposition, architecture, consultation, assignment,
synthesis, integration, and final verification. It stays available for owner
realignment and delegate questions, and assigns implementation wherever the
host can delegate.

Run only independent file scopes concurrently. Each writer owns an isolated
worktree. Hard cap four active agents. Check real progress for any agent or
command unexamined for about twenty minutes, and supply a decision, a solved
subproblem, or a re-spec — never a heartbeat.

## Independent adversarial review

Every behavior-changing step ends with a review before the next step starts. The
property that makes it work is independence, so it is not optional:

- The reviewer is a **separate agent instance, never the author** of the work.
- The reviewer is prompted to **refute** the work — find where it is wrong,
  unverified, or over-claimed — not to approve it.
- Depth scales with the step, matching the consult triage: one reviewer for
  bounded reversible work; three reviewers with distinct lenses (correctness,
  does-it-reproduce, blast radius) for hard-to-reverse or cross-cutting change.
- Escalate to the most intelligent model for a new subsystem, a migration, a
  dependency swap, or any decision that is expensive to unwind.
- Record each finding as `confirmed`, `refuted`, or `unproven`. A refuted
  finding is dropped, not softened.
- Re-review after applying findings, until a pass yields nothing viable.

A self-review is not a review. Neither is a delegate's own report on its own
work.

## Delegate covenant

[Roles](roles.md) states how each host carries this. Either way, every dispatch
carries it:

> Load the act-as-mohab entrypoint before all other work. Evidence over
> inference: read or run before claiming. Stay inside assigned scope; report
> adjacent findings. Cite repository-relative `file:line` evidence. Behavior
> changes use observed RED, minimal implementation, observed GREEN. Never claim
> an unrun check. Escalate architecture or ambiguity instead of deciding it.
> Report failures plainly. Before waiting or after a material finding, send a
> substantive handoff: done evidence, current step, blockers, and whether a
> decision is needed.

A default-model delegate may add: mechanical, spec-exact, or bulk slices may go
to the mechanical model with this covenant; inspect their actual output before
using it. The mechanical model omits that clause because it may not delegate.

## Returned-work review

Read diff and tests in the two passes [roles](roles.md) gives the reviewer.
Route every finding to `decision_needed`, `patch`, `defer`, or `dismiss`.
Orchestrator owns final severity because it has full context. Use the
[verification-gap lens](verification-gap-lens.md) for behavior that could break
without a failing check.
