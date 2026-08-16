# Work GitHub — playbook

A session-shaped method for taking an issue from filed to merged. Load
the canonical ChaosEngine entrypoint alongside this playbook. Start with [planning and tracking](work-github-planning.md), then return here for delivery.

## Before the first file change: publish the plan

Task isolation creates the branch and linked worktree. Before the first task
file mutation, make one clean `git commit --allow-empty` planning checkpoint,
push that exact head, and open its pull request as a draft with explicit base
and head. The PR must still introduce zero changed files, and its visible body
must contain substantive `## Plan`, `## Scope`, and `## Proof` sections. Verify
the exact head, draft state, and zero-file count from GitHub before proceeding.

This is the second durable task step, immediately after isolation. It does not
replace issue-backed executable specifications or research: prepare those as
transient evidence, place the approved plan in the draft, then start file work.
If the branch already contains an implementation diff, preserve its history;
the retained-checkpoint gate below owns that continuation. GitHub unavailability
blocks the first mutation rather than turning unknown state into permission.

## 4. Push each iteration, then review and run CI in parallel

Rule every open design choice on the issue before the first implementing commit;
a ticket that reaches code still choosing between candidate fixes converts that
choice into review rounds. This is measured rather than asserted, and the
practice already works: on #4554, #4545 and #4547 each carried a two-option
design question, each was ruled on its issue before any code, and each cost zero
rounds, while #4536 carried "Trigger points worth considering" into code unruled
and cost four.

A subagent's report describes intent, not necessarily its actual work. Before reviewing or shipping any nontrivial diff, query Graphify for the touched symbols, read the actual diff, and inspect changed tests for real assertions. Before committing any subagent's work, verify empirical claims rather than trusting a report. Scan the report/diff, and once opened, the PR body for deferred/out-of-scope/adjacent-finding/follow-up language; file every real finding before treating the item as done.

Commit one locally validated, coherent iteration using the repository's normal
message convention, including its issue number, and push it to the existing
draft PR immediately. Keep one active channel and the smallest coherent increment for rapid iteration.
For each pushed iteration, run CI first; when it fails, fix the exact cause, run only the focused checks required
by that fix, commit, and push the next iteration promptly. Once CI is green,
run independent review against the latest exact head. Repair any blockers,
validate, and push again. Repeat until CI is green and independent review
reports zero blockers on the same head. Only then may the PR be armed.

### Every retained checkpoint: make delivery visible immediately

After the first locally validated implementation commit succeeds and remains at `HEAD`,
stop behavior work and make that exact checkpoint visible before another
behavior change or commit. Repeat this gate after every later retained commit:

1. Resolve repository identity from the active worktree, then bind the full
   `HEAD` SHA and implementation branch. Never infer an issue number from a
   branch name.
2. Push the branch and create or discover its open draft or ready PR. PR
   creation always names `--base` explicitly; stacked and dependent work uses
   its intended branch base, never an assumed default branch.
3. Require the PR to cover the exact checkpoint SHA. Persist its canonical
   `baseRefName`, PR identity, and `closingIssuesReferences`; those closing
   references, not branch text, supply the issue mapping. GitHub ignores
   closing keywords when a PR targets a non-default stacked base. Only in that
   state, an explicit, unambiguous same-repository closing keyword in the PR
   body supplies the fallback issue mapping until the stack reaches the
   default branch; titles, branch names, ordinary references, and malformed or
   cross-repository clauses never do.
4. Update the PR body for that exact head with nonempty `## Summary`, `## Checks`,
   and `## Continuation` sections. Visible Continuation text states the full
   current `Head:`, plus meaningful `State:`, `Blockers:`, and `Next action:`
   fields so a new owner can resume without reconstructing the session. Hidden
   comments and code blocks do not count. Keep the linked tracker current as
   later commits land.

Read-only work, failed commit attempts, and sessions with no retained
implementation commit owe no PR. For an already-running session whose first
checkpoint predates this rule, perform steps 1–3 before resuming behavior. An
older-head PR does not repair the state: push/update it until its head is exact.
If the exact PR lacks a closing issue reference, add one. If GitHub is
unavailable, report that concrete blocker and retry; do not treat unknown as no
PR or continue accumulating commits. The guard leaves the inspection, push,
explicit-base PR creation/update, and narrowly defined checkpoint-repair paths
available while this duty is pending.

## 5. Docs, catalog, and screenshots — only where real

Update user documentation and the feature catalog only for shipped behavior.
Regenerate screenshots only for changed panels. Externally documented behavior
changes in the companion documentation repository require their own PR.

## 6. Learning Loop before wrapping up

Route durable findings through the entrypoint's Learning Loop before the final
push; a later push loses the context needed to record it accurately.
When reflection is required, put the changed approach and focused proof on the
tracker before resuming. The hook never writes issues. After delivery, a
session over one hour records its terminal receipt and labeled user summary
before final Stop and Learning Loop reporting.

### Learned-lessons workflow

1. Collect durable costs, decisions, structural changes, procedure gaps, and
   deliberately deferred work.
2. Classify each knowledge result with the entrypoint's Learning-loop table and
   use exactly one knowledge destination.
3. Separately classify actionability. For every problem, follow-up action, or
   potential improvement needing work, search for duplicates and then open one
   new standalone GitHub issue for that action. A duplicate hit informs the new
   issue; it does not replace it.
4. Link the receipt ID and incident evidence in the issue, then bind that issue's
   canonical URL during `assess`. A receipt, Memory entry, Graphify flag, or old
   issue comment is evidence only and never replaces the action ticket.
5. Write the knowledge result, or explicitly record that nothing durable or
   actionable surfaced. Do not manufacture an issue for a genuinely no-action result.

For a meaningful event, record an evidence-consistent `signal`, then `assess`
it into a quarantined candidate using one distinct `--tracking-issue-url` per
incident. Behavior changes stay quarantined while
`evaluate` records a strict improvement comparison on the frozen adherence
corpus with zero unmeasured rules and no regression. The record is a
consistency summary, not proof that commands ran or reviewers are authentic.
Independently derive the live diff, rerun tests, and verify review artifacts.
Use `promote` to record intent only for the exact evaluated commit after the
normal pull-request review gate; the normal GitHub workflow must still perform
and verify the merge. Kernel-tier changes require
two independent reviewer keys, correctness/reproduction/safety lenses, and two
independent runs on the same commit and corpus. If a promoted change regresses,
use `repair-or-revert` to record one repair requirement; recurrence freezes the
candidate and records a revert requirement. The normal git/GitHub workflow
performs and verifies the repair or revert.

## 7. Push, PR, green, merge, compact

- For work not already covered by the first-checkpoint rule, push the branch and
  open the agreed PR shape with a
  description that lists each sub-item and its commit. Keep that description
  current as later commits land.
- Verify `closingIssuesReferences` after opening: GitHub matches closing words
  even inside negated or illustrative prose, so partial work says `Related to
  #N`, never a closing keyword adjacent to an issue number. If it lists an issue
  this PR does not fully resolve, unlink it from the PR's Development sidebar.
- Merge only within granted authority. A companion PR in another publishing
  repository needs its own authority.
- Compact after a confirmed merge if the host supports it.

### PR-merger workflow: arm, watch, fix, confirm

The entrypoint makes this a duty. The terminal states are merged, red,
conflicting, and stale; a watcher observes only green and red.

This repository uses merge commits so a delivered branch remains identifiable
by ancestry. Squash and rebase merging are disabled; do not substitute them.

1. **Clear every GitHub comment before arming.** Read and address every open
   review thread, inline review comment, conversation comment, check annotation,
   and bot finding (including code-quality and security bots). A green check is
   not evidence that its comments were handled. Reply or resolve only after the
   finding is fixed, ruled non-applicable with evidence, or filed as explicitly
   approved follow-up work. Re-query GitHub after the final push and require zero
   unhandled comments before continuing.
2. **Arm** after the review and comment gates: `gh pr merge <n> --auto --merge`.
3. **Watch** from the target repository with
   `gh pr checks <n> --watch --fail-fast`. Pass `--repo` for an explicit
   cross-repository target.
4. **Ask for unseen states** with `gh pr view <n> --json
   mergeStateStatus,mergedAt`; `DIRTY` conflicts and `BEHIND` stale heads need
   action even when no event fires.
5. **Fix** red checks on the branch, review comments, or bot findings on the
   branch, or merge the fetched configured upstream default branch for a conflict or
   stale head, then return to watch. Never force-push away owner-visible history.
   Any new push restarts the comment gate before auto-merge may remain armed.
6. **Confirm** remotely that `mergedAt` is non-null; armed is not merged.

## 8. Report

Report what shipped, what was deferred and why, what remains open, and any
surprise worth flagging. State only verified facts.
