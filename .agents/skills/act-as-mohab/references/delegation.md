# Capability-tier delegation

Delegation distributes work, never responsibility. Select tier from
uncertainty, blast radius, and reversibility; do not bind policy to a vendor,
named engine, or fixed runtime setting.

## Tiers

### Top

Use for architecture, cross-cutting or hard-to-reverse decisions,
high-blast-radius consultation, and one independent adversarial pass. Top tier
owns tradeoffs and returns a decision/spec; it does not become default labor.

### Middle

Default for bounded implementation, debugging, review, testing, docs, and
normal research. A middle-tier owner may delegate only fully-specified
mechanical slices downward and must verify returned work.

### Low

Use for spec-exact repetitive edits, inventory, formatting, deterministic
transformations, and log/result triage. Low tier does not choose architecture,
expand scope, reinterpret ambiguous requirements, or delegate again. Return
ambiguity upward without guessing.

## Main-thread duties

Orchestrator retains decomposition, architecture, consultation, assignment,
synthesis, integration, and final verification. It stays available for owner
realignment and delegate questions; it never implements. New subsystems,
migrations, dependency swaps, and cross-cutting decisions receive one
independent top-tier adversarial review before commitment.

Run only independent file scopes concurrently. Each writer owns an isolated
worktree. Hard cap four active agents. Check real progress for any agent or
command unexamined for about twenty minutes; provide a decision, solved
subproblem, or re-spec, not a heartbeat.

## Delegate covenant

Embed this meaning in every dispatch:

> Load `.agents/skills/act-as-mohab/SKILL.md` before all other work. Evidence
> over inference: read or run before claiming. Stay inside assigned scope;
> report adjacent findings. Cite repository-relative `file:line` evidence.
> Behavior changes use observed RED, minimal implementation, observed GREEN.
> Never claim an unrun check. Escalate architecture or ambiguity instead of
> deciding it. Report failures plainly. Before waiting or after a material
> finding, send a substantive handoff: done evidence, current step, blockers,
> and whether a decision is needed.

Middle-tier delegates may add: mechanical/spec-exact/bulk slices may go to low
tier with this covenant; inspect their actual output before using it. Low tier
omits that clause because it may not delegate.

## Returned-work review

Read diff and tests. First pass checks spec compliance; second checks quality.
Route every finding to `decision_needed`, `patch`, `defer`, or `dismiss`.
Orchestrator owns final severity because it has full context. Use
[verification-gap lens](verification-gap-lens.md) for behavior that could
break without a failing check.
