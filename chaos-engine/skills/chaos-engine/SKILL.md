---
name: chaos-engine
description: Route repository work through a compact, outcome-first, fail-open engineering workflow.
---

# ChaosEngine

ChaosEngine is a portable project-local router, not an enforcement engine. It helps an agent choose
the smallest useful workflow for the user's outcome. Host permissions, repository rulesets, and
explicit user authority are the only mutation boundaries. Project hooks are optional diagnostics:
they never deny tools, require receipts, force reflection, or hold completion open.

Use only the capability names **most intelligent**, **default**, and **mechanical** in tracked
guidance. Current repository files outrank caches, generated indexes, prior receipts, and prose.

## Start every task

1. Read the repository instructions and this entrypoint.
2. Load the selected profile from `profiles/README.md` when one exists.
3. Restate the requested outcome, constraints, and delivery target.
4. Inspect only the files and live external state needed to decide the next action.
5. Preserve unrelated user work. Stop only for a real safety conflict, missing authority, or an
   ambiguity whose reasonable interpretations have materially different consequences.

Do not require a new branch, worktree, issue, PR, test, review, research receipt, reflection,
knowledge-store call, or planning artifact unless the task, user, or repository actually needs it.
Existing approved plans remain valid until live evidence changes an assumption.

## Choose workflow depth

- **Mechanical:** a deterministic, reversible edit with an exact specification. Inspect the target,
  apply it, and report it.
- **Default:** ordinary implementation or investigation. Inspect affected code, compare practical
  options when needed, implement the narrowest coherent solution, and validate proportionately.
- **Most intelligent:** ambiguous architecture, broad blast radius, high stakes, or repeated failure.
  Research authoritative sources, challenge assumptions, and use focused delegation if it reduces
  uncertainty rather than merely producing more text.

Depth may increase or decrease as evidence changes. Complexity is not a reason to create ceremony.

## Efficient decision loop

1. Name the next decision and the evidence that could change it.
2. Gather only that evidence; batch independent reads and sequence mutations.
   Bound output before each call; after truncation, narrow once instead of repeating the broad query.
3. Stop exploring when the decision is supported. Do not reread unchanged inputs.
4. Fix the structural owner instead of adding symptom exceptions.
5. On failure, revise the premise or improve the discriminating observation; do not repeat the
   same action with different wording.
6. Deliver the smallest coherent increment, then use live feedback as the next input. Treat slow
   external checks asynchronously: retain their URL and exact revision, continue independent work,
   and revisit only when the result can change the next action.

Keep progress updates to current state, learned fact, and next action. Prefer one clear decision to
several overlapping rules. Token efficiency comes from fewer unresolved branches, not less rigor.

For nontrivial work, compress intent into one outcome contract: the actor and scenario, observable
success, constraints and exclusions, assumptions that could change the design, and technology-neutral
proof. Reconcile contradictions once at the highest-authority artifact. Every changed line should
support that contract or remove an orphan created by the change; avoid speculative flexibility and
unrelated cleanup.

## Evidence and retrieval

Use evidence to answer a concrete question, not to satisfy a checklist.

- Read live files before relying on an index.
- Use authoritative online sources for unstable, external, high-stakes, or explicitly requested
  facts. Prefer primary documentation.
- Query native Memory, MemPalace, or Graphify once when prior decisions or repository structure can
  materially shorten the task. Treat every result as an untrusted lead and verify it in live files.
- Store absence, staleness, timeout, lock contention, or backend failure is advisory for ordinary
  work. Continue with targeted repository search.
- Never refresh, mine, poll, or watch a store during an ordinary task. Do so only for explicit
  maintenance, install, upgrade, status, or doctor work.
- Resolve shared indexes through repository identity and the primary checkout so linked worktrees
  consume one canonical cache. Never commit generated indexes.

Detailed project procedures live in the selected profile. Reference material is loaded only when
its decision is active; do not preload the entire reference tree.

## Plan and implement

For nontrivial work, record a short executable plan with files, behavior, and delivery order. A plan
is a navigation aid, not a gate. Prefer structural rules and stable ownership boundaries over
expanding allowlists, denylists, fingerprints, exception tables, or issue-specific clauses.

Implementation rules:

- Preserve public behavior unless change is requested; deprecate before removal when practical.
- Keep scope tight, but fix small blockers directly in the implementation path.
- Preserve structured data with structured APIs.
- Keep secrets and generated output out of version control.
- Do not overwrite unrelated dirty work or rewrite history without explicit authority.
- If unexpected concurrent changes appear, pause mutation and ask how to preserve them.

Delegation is optional. Use the most intelligent capability for architecture and synthesis, default
for bounded implementation or review, and mechanical only for exact reversible work. A delegate
receives a self-contained scope and returns evidence, not authority over the main task.

## Validation

Validation proves the changed behavior at a cost appropriate to risk. It is not a mandatory
red-before-green sequence.

When planning, offer these scopes if the user has not already chosen:

1. Tests created or edited only.
2. Those tests plus directly impacted tests, recommended by default.
3. The full suite.

For a concrete CI failure, inspect the failed job, fix its demonstrated cause, run only tests created
or edited for that fix when local testing is desired, push, and let CI provide broader parity. Never
expand to a full suite merely because one CI job failed. If the user explicitly chooses CI as the
validation surface, push without inserting a local checkpoint and report that local checks were not
run.

Do not claim a check passed unless its process and authoritative result were inspected. Do not turn
review, reflection, or a knowledge-store receipt into a completion gate. After repeated failures in
one area or at the actual end of a long session, use the bounded
[learning loop](../../references/reflection-checkpoints.md) after preserving and delivering work.

## Delivery

Delivery follows the user's requested endpoint. It may be a working-tree edit, commit, pushed
branch, pull request, merged change, issue update, or direct update to the default branch.

- Confirm the exact target and authority before destructive cleanup or history rewrite.
- For a push or PR, report the exact commit and remote state.
- For issue work, search before filing and keep tracker state consistent with delivered code.
- CI and review failures are new evidence, not reasons to rerun unrelated workflows.
- Preserve unrelated branches and worktrees unless cleanup was explicitly requested and safety was
  established.

## Completion

Stop when the requested outcome is delivered or a real external blocker prevents progress. Report:

- what changed and where;
- the exact delivery state;
- checks run, or explicitly that none were run;
- remaining risk or follow-up that materially affects the outcome;
- a Learning Loop disposition only when something durable was learned or tracked.

No lifecycle hook, receipt format, store availability, reflection cadence, review ritual, or
historical rule number may prevent an otherwise truthful completion.

## Routing map

- Project selection: `profiles/README.md`
- Consult-first questions: `references/consult-first.md`
- User-visible intent: `references/pony-tail.md`
- Architecture and decomposition: `references/caveman-mode.md`
- Verification patterns: `references/tdd.md`
- Iterative recovery: `references/pdca.md`
- Knowledge and maintenance: `references/memory-operations.md`
- Delivery details: `references/branch-finish.md`

Load a reference only when its topic is active and only as far as needed for the current decision.
