# Multi-agent PDCA loop

Standing owner rule: whenever two or more bots or agents work together and one
holds the orchestrator role, they deliver through this loop. It complements the
single-session phase order in [TDD/PDCA](tdd.md#workflow) and the
[orchestrator follow-through](orchestrator-follow-through.md) cadence.
Across items, the [iterative flow](iterative-flow.md) rule applies: WIP 1,
Done = both sign-offs and shipped, lessons become preventive rules.

## Roles

- **Orchestrator:** Plan and Do, then an adversarial self-Check of its own
  diff or artifact before handing it over. Does **not** implement media
  generation; delegates that to the design-capable implementer (Designer).
- **Reviewer:** a separate agent that Checks the actual diff, rendered output,
  and evidence; never a self-report.

## Loop

1. Plan: spec, acceptance, risks.
2. Do: smallest change that meets the spec.
3. Check: orchestrator self-check, then the reviewer's independent check for
   logical and visual issues.
   Both run the [pre-done self-review](../skills/pre-done-self-review/SKILL.md)
   (`tool.py pre-done <report.md>` exits 0) before any sign-off.
4. Act: fix every finding, then return to Check.

Repeat until neither agent finds a logical or visual issue. Both sign-offs
together count as the user's approval for the in-scope work; the user hears
only the final Done, with evidence.

## Guards

- Sign-offs never authorize a destructive, external-send, or out-of-scope
  action the user did not already ask for; those still need the user.
- A sign-off names the full head SHA it reviewed. Before merging, check it
  equals the PR head; on a mismatch or no SHA, ask for a re-sign-off.
- Reviewer evidence: re-run every check on a fresh copy of the artifact; the
  implementer's own tool output is never evidence. A reject names the
  location, the measured value and the expected value.
- A sign-off on an artifact outside git (document, deck, design file) names
  the version it reviewed (revision id, modified time or hash). Before
  accepting it, compare with the current version; if the artifact changed
  after the review copy was taken, re-review.
- Cap: 5 rounds without convergence. Stop and escalate to the user with the
  open findings and each agent's position.

## After each delivery

Every agent runs its own self-improvement round: a lessons pass and a parity
check, per the [router contract](router-contract.md#learning-session).
