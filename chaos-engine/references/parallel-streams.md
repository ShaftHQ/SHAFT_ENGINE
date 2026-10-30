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
scratch file) with: stream id, repository, owned path globs, semantic **kind**
(when design work: `video`, `podcast`, `image`, or `website`), issues,
implementer id, reviewer id, worktree path, branch, disk budget (MB), status,
and (for design kinds) `first_pass_signoff_streak` for that kind.

- Two active streams must not share a **writable** path glob.
- Read-only shared paths are allowed.
- Epic children inherit the parent's locks until released.
- Two active **design** streams of the **same kind** are a conflict under the
  kind WIP cap (see [Design-kind WIP](#design-kind-wip-same-kind-batch--conflict)),
  even when path globs are disjoint — one shared template/gate defect otherwise
  multiplies across the batch.

## Conflict detection (before dispatch)

1. Collect **peer owned path globs** from every other **active** row in the
   ownership manifest (and any stacked / in-flight brief reservations).
2. Collect paths from open agentic PRs and dirty files in live worktrees.
3. Intersect the proposed stream's writable globs with (1) and (2).
4. Non-empty intersection → do not dispatch; re-scope, stack, or wait.
5. If the proposal carries a design **kind**, collect active peers of that kind.
   When their count is at or above the kind WIP limit (1, or 2 after two
   consecutive first-pass sign-offs for that kind), treat it as a conflict —
   same as path overlap. Different kinds may run in parallel.

Executable check: `scripts/ci/parallel_stream_conflict.py`
(`refuse_dispatch_on_ownership_overlap` / `conflicting_peer_stream_ids` /
`refuse_dispatch` with workspace for disk + worktree caps, and `refuse_dispatch_on_same_kind_wip` / `conflicting_same_kind_stream_ids`).

## Brief template (implementer)

First lines of every brief:

```
ce_load: required
Run: python3 .chaos-engine/tool.py entry
```

Lint with `worker_ce_entry.py lint`. Hand every implementer: goal, acceptance, out of scope, exclusive owned paths,
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

**Definition of Done (mandatory, not advisory):** after the stream's PR is
**MERGED** (GitHub state MERGED and the merge commit / head OID is on the
remote), the **owning worker** (or orchestrator on its behalf) **must** clean
**only** artifacts recorded on that stream's ownership row before the next
item in that stream. Hookless bots run this themselves; CLI Stop hooks cannot
substitute on hosts without hooks. Skipping cleanup is a DoD miss.

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
profiles); those refuse even with an owner code. Hygiene refuses `never-touch-path` without weakening the guard, and fails **closed** (`never-touch-check-unavailable`) if delete-guard cannot be imported — cleanup does not proceed open.

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

## Design-kind WIP (same-kind batch = conflict)

A design **conflict** is not only file overlap. Batching several items of the
**same kind** (video, podcast chapter, image, website change) while one shared
defect is still unknown multiplies rejects: one bad caption/template/gate rule
fails the whole batch (wasted tokens and wall time). Treat that like a path
conflict.

Rules:

1. **WIP = 1 per design kind.** Parallel across different kinds is allowed when
   path globs are still disjoint.
2. Each item must finish **build → independent review → orchestrator sign-off →
   lessons captured** before the next same-kind item starts.
3. After **two consecutive first-pass sign-offs** for that kind, the kind may
   rise to **WIP = 2**. Any reject drops that kind back to WIP = 1 (streak
   resets to 0).
4. Ownership-manifest rows for design work set `kind` and keep
   `first_pass_signoff_streak` current so
   `refuse_dispatch_on_same_kind_wip` can enforce the cap before dispatch.

## Disk gate

Shared box, before creating a worktree or clone. **Executable** check:
`scripts/ci/box_disk_guard.py` (`disk_level` / `refuse_new_worker` /
`new_worker_blockers`) and `scripts/ci/parallel_stream_conflict.py`
(`refuse_dispatch` with a workspace path).

1. **Warn** when free space is under **15 GiB**. **Block** a new worker,
   worktree, or clone when free space is under **10 GiB**
   (`disk-free-below-10giB`).
2. **Block** when linked worktrees on the primary already reach the cap
   (**24**, `linked-worktree-cap`); reclaim via [post-merge own cleanup](#post-merge-own-cleanup)
   before adding another.
3. Prefer one `git clone --filter=blob:none` (or bare / `--reference`) primary
   per repository and `git worktree add` for every worker — never N full clones
   when a primary already exists ([bot-entry](bot-entry.md)).
4. Optional cone sparse-checkout for path-scoped tasks.
5. Share host language caches across worktrees; do not duplicate them per
   worker. Enforce [cache caps](#cache-caps); prune with tool-native commands
   only (`box_disk_guard.prune_cache`) — never raw `rm` on caches.
6. Default working-tree budget **500 MB** for harness-only streams unless the
   brief raises it.
7. After verified push or merge: inventory scratch paths on the stream row as
   `scratch_pending_cleanup`, then run [post-merge own cleanup](#post-merge-own-cleanup)
   for that stream's recorded artifacts. Listing candidates is always allowed;
   foreign or never-touch paths stay listed only — never deleted without the
   proofs above / [delete-check](delete-check-discipline.md).

## Cache caps

Shared host caches have size caps in `box_disk_guard.CACHE_SPECS` (pip, yarn,
npm, uv, gradle, m2, ChaosEngine cache/share). When a cache is over cap:

- If a **tool-native** prune exists (`pip cache purge`, `uv cache prune`,
  `yarn cache clean`, `npm cache clean --force`), run `prune_cache(name)`.
- If no native prune exists (gradle / m2 / CE share), **report only** and route
  regenerable reclaim through [delete-check](delete-check-discipline.md) — never
  invent `rm -rf` inside the guard.
- Never-touch paths refuse prune even when over cap.

## Own scratch TTL

Own-created scratch under stream-allowed roots may be listed as TTL candidates
after **24 hours** when they carry the owner marker
`.chaos-engine-owned-scratch` (`own_scratch_ttl_candidates`). False positives
refused: no marker (`not-own-scratch`), within TTL (`within-ttl`), never-touch /
delete-guard unavailable. Listing is always safe; filesystem delete of scratch
still needs `tool.py delete-check`.

## Checkpoint

Session-capped hosts: `tool.py resume` / `checkpoint` every step; on budget exit
push WIP and hand `--next` to the parent. Parent re-dispatches from resume
output, never from a raw transcript ([bot-entry session cap](bot-entry.md)).
