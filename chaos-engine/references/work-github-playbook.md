# Work GitHub — playbook

A session-shaped method for taking an issue from filed to merged. Load
the canonical ChaosEngine entrypoint alongside this playbook. Start with [planning and tracking](work-github-planning.md), then return here for delivery.

## 4. Implement, check, review, deliver

Rule every open design choice on the issue before the first implementing commit;
a ticket that reaches code still choosing between candidate fixes converts that
choice into review rounds. This is measured rather than asserted, and the
practice already works: on #4554, #4545 and #4547 each carried a two-option
design question, each was ruled on its issue before any code, and each cost zero
rounds, while #4536 carried "Trigger points worth considering" into code unruled
and cost four.

A subagent's report describes intent, not necessarily its actual work. Before reviewing or shipping any nontrivial diff, query Graphify for the touched symbols, read the actual diff, and inspect changed tests for real assertions. Before committing any subagent's work, verify empirical claims rather than trusting a report. Scan the report/diff, and once opened, the PR body for deferred/out-of-scope/adjacent-finding/follow-up language; file every real finding before treating the item as done.

Complete the approved implementation before local validation. Run its checks as
one consolidated Check phase, patch observed blockers as one Act phase, then
perform one independent pull-request review. Commit and push coherent batches;
do not stop behavior work for per-step commits, reviews, PR-body updates, or
delivery receipts.

After the implementation batch is ready, resolve repository identity from the
active worktree, bind the full `HEAD` SHA and branch, push, and create or update
the PR with an explicit base. Persist its `baseRefName`, PR identity, and
`closingIssuesReferences`. Keep nonempty `## Summary`, `## Checks`, and
`## Continuation` sections current for the delivered head.

## 5. Docs, catalog, and screenshots — only where real

Update user documentation and the feature catalog only for shipped behavior.
Regenerate screenshots only for changed panels. Externally documented behavior
changes in the companion documentation repository require their own PR.

## 6. Terminal Learning Session

Collect durable findings during work, but route them through exactly one root-owned
Learning Session only after confirmed delivery and immediately before the final report.
Delegates and intermediate pushes never start another session.
When reflection is required, put the changed approach and focused proof on the
tracker before resuming. The hook never writes issues. After delivery, a
session over one hour records its terminal reflection receipt before the Learning Session.

### Learned-lessons workflow

1. Collect durable costs, decisions, structural changes, procedure gaps, and
   deliberately deferred work.
2. Classify each knowledge result with the entrypoint's Learning Session table and
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
   Non-private Memory objects and relations are source-controlled: commit them on
   the task branch. Secrets never enter `.memory/memory`; they stay in
   `.memory/private/` (gitignored).

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

- After the implementation and Check/Act batch, push the branch and open or
  update the agreed PR shape with a
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
5. **Fix** red checks, failed tests, review comments, and bot findings on the
   branch, or merge the fetched configured upstream default branch for a
   conflict or stale head, then return to watch. Never force-push away
   owner-visible history. Any new push restarts the comment gate before
   auto-merge may remain armed.
6. **Confirm** remotely that `mergedAt` is non-null; armed is not merged.

## 8. Report

Report what shipped, what was deferred and why, what remains open, and any
surprise worth flagging. State only verified facts.
