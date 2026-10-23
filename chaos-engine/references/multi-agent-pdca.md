# Multi-agent PDCA loop

Standing owner rule: whenever two or more bots or agents work together and one
holds the orchestrator role, they deliver through this loop. It complements the
single-session phase order in [TDD/PDCA](tdd.md#workflow) and the
[orchestrator follow-through](orchestrator-follow-through.md) cadence.

## Roles

- **Orchestrator:** Plan and Do, then an adversarial self-Check of its own
  diff or artifact before handing it over.
- **Reviewer:** a separate agent that Checks the actual diff, rendered output,
  and evidence; never a self-report.

## Loop

1. Plan: spec, acceptance, risks.
2. Do: smallest change that meets the spec.
3. Check: orchestrator self-check, then the reviewer's independent check for
   logical and visual issues.
4. Act: fix every finding, then return to Check.

Repeat until neither agent finds a logical or visual issue. Both sign-offs
together count as the user's approval for the in-scope work; the user hears
only the final Done, with evidence.

## Guards

- Sign-offs never authorize a destructive, external-send, or out-of-scope
  action the user did not already ask for; those still need the user.
- Cap: 5 rounds without convergence. Stop and escalate to the user with the
  open findings and each agent's position.

## After each delivery

Every agent runs its own self-improvement round: a lessons pass and a parity
check, per the [router contract](router-contract.md#learning-session).
