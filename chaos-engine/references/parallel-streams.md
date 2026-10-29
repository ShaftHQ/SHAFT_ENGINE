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

1. Collect **peer owned path globs** from every other **active** row in the
   ownership manifest (and any stacked / in-flight brief reservations).
2. Collect paths from open agentic PRs and dirty files in live worktrees.
3. Intersect the proposed stream's writable globs with (1) and (2).
4. Non-empty intersection → do not dispatch; re-scope, stack, or wait.

Executable check: `scripts/ci/parallel_stream_conflict.py`
(`refuse_dispatch_on_ownership_overlap` / `conflicting_peer_stream_ids`).

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

## Post-merge own cleanup

After the stream's PR is **MERGED** (GitHub state MERGED and the merge commit /
head OID is on the remote), the **owning worker** (or orchestrator on its behalf)
cleans **only** artifacts recorded on that stream's ownership row:

1. Verify the local branch tip is contained in the remote merge (ancestor of the
   configured upstream tip, or PR head OID matches the merged receipt) and the
   worktree has **no unpushed commits** and **no untracked / dirty work**.
2. From outside the target worktree, run
   `scripts/ci/worktree_hygiene.py --cleanup-task-worktree …` with the schema-v1
   ownership manifest, owned branch, expected HEAD, and diff SHA-256
   ([cleanup scopes](cleanup-scopes.md) bounded task cleanup).
3. Then `--cleanup-task-local-branch` (merged-only `git branch -d`) once no
   worktree still holds that branch.
4. Then `--cleanup-task-remote-head` if `origin/<branch>` still exists (repo did
   not auto-delete).
5. Then `--prune-after-task-cleanup` (`git worktree prune` + `git fetch --prune`).

Never remove another stream's worktree, branch, or scratch. Never touch
[delete-guard](delete-guard.md) never-touch paths (`ROG*`, `yt`, Chrome/Chromium
profiles); those refuse even with an owner code. Hygiene refuses
`never-touch-path` without weakening the guard.

### Delete-guard interaction

- `git worktree remove` / `git branch -d` / remote head delete via hygiene proofs
  are **outside** delete-guard's parse list; the sanctioned path is MERGED +
  ownership manifest + clean/ancestor proofs above — **do not** weaken
  delete-guard to special-case them.
- Filesystem deletes of scratch under the stream row still need
  `tool.py delete-check` ([delete-check discipline](delete-check-discipline.md)).
  Clean + pushed targets may pass; dirty / unpushed / outside allow-list still
  need the owner's one-time code. Never route around a refusal.

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
   `scratch_pending_cleanup`, then run [post-merge own cleanup](#post-merge-own-cleanup)
   for that stream's recorded artifacts. Listing candidates is always allowed;
   foreign or never-touch paths stay listed only — never deleted without the
   proofs above / [delete-check](delete-check-discipline.md).

## Checkpoint

Session-capped hosts: `tool.py resume` / `checkpoint` every step; on budget exit
push WIP and hand `--next` to the parent. Parent re-dispatches from resume
output, never from a raw transcript ([bot-entry session cap](bot-entry.md)).
