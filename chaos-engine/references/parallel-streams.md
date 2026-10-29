# Parallel streams (orchestrator fan-out)

On-demand protocol when an orchestrator runs more than one implementer or
reviewer at once on a shared box. Complements [iterative flow](iterative-flow.md)
(WIP 1 **per stream**), [multi-agent PDCA](multi-agent-pdca.md),
[delegation](delegation.md), and [cleanup scopes](cleanup-scopes.md).

## When to fan out

Fan out only for **file-disjoint** work. Same subsystem or shared always-loaded
card → serialize or [stack](bot-entry.md#delivery-ledger) on the previous
branch. Research URLs are canonicalized before a second research dispatch.

## Stream-ownership manifest

Before the first dispatch, write a stream table (checkpoint items or a review
scratch file) with: stream id, repository, owned path globs, issues, implementer
id, reviewer id, worktree path, branch, disk budget (MB), status.

- Two active streams must not share a **writable** path glob.
- Read-only shared paths are allowed.
- Epic children inherit the parent's locks until released.

## Conflict detection (before dispatch)

1. Collect paths from open agentic PRs and dirty files in live worktrees.
2. Intersect with the proposed owned globs.
3. Non-empty intersection → do not dispatch; re-scope, stack, or wait.

## Brief template (implementer)

Hand every implementer: goal, acceptance, out of scope, exclusive owned paths,
riskiest assumption with a **named** test (red on base / green on head),
false-positive and live-behaviour cases the gate must pass, harness entry
(`doctor`, `entry`, `retrieve`, worktree overlay materialize), push rule
(`overlay_pre_push` then push; **no force-push**), disk budget and allowed
scratch paths, and the short report contract (head SHA, files, checks, leftovers).

## Reviewer independence

Separate agent instance; no author reasoning history; confirm the head SHA
before findings. Re-run the named tests; when the PR claims a live gate, exercise
**live behaviour** (real URL, real build, real HTML) — unit green alone is not
evidence. Sign-off names the full head SHA; mismatch → re-review. Cap: five PDCA
rounds, then escalate ([multi-agent PDCA](multi-agent-pdca.md)).

## Merge and rebase

Never force-push a branch under open review. Prefer append-only fix commits. If a
rebase is required, the orchestrator pauses reviewers, rebases, then requests a
fresh sign-off on the new SHA. Merge with head-SHA match; confirm auto-merge.

## WIP and system cap

- Inside a stream: WIP limit 1 until Done (both sign-offs and shipped).
- Across streams: default cap three implementers and three reviewers, or fewer
  when free disk is under the [disk gate](#disk-gate) or the review queue is
  deeper than one round (Theory of Constraints: cap in front of review / disk /
  attention, not coding).

## Disk gate

Shared box, before creating a worktree or clone:

1. Refuse when free space on the workspace filesystem is under **20 GiB**.
2. Prefer one `git clone --filter=blob:none` (or bare / `--reference`) primary
   per repository and `git worktree add` for every worker — never N full clones
   when a primary already exists ([bot-entry](bot-entry.md)).
3. Optional cone sparse-checkout for path-scoped tasks.
4. Share host language caches (`~/.m2`, Gradle caches, yarn/npm/pip) across
   worktrees; do not duplicate them per worker.
5. Default working-tree budget **500 MB** for harness-only streams unless the
   brief raises it.
6. After verified push or merge: inventory scratch paths on the stream row as
   `scratch_pending_cleanup`. **Do not delete** without
   [delete-check](delete-check-discipline.md) / owner code. List candidates with
   verified-pushed status for the owner ([cleanup scopes](cleanup-scopes.md)).

## Checkpoint

Session-capped hosts: `tool.py resume` / `checkpoint` every step; on budget exit
push WIP and hand `--next` to the parent. Parent re-dispatches from resume
output, never from a raw transcript ([bot-entry session cap](bot-entry.md)).
