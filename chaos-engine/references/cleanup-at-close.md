# Cleanup at item close

## Rule

1. **Clean at item close** only after the stream's delivery is pushed and
   verified (merged PR tip on origin, or upload + watch-page receipt for media).
   Never delete the only copy of unpushed work. Remove only paths the agent
   itself created and recorded in the ownership manifest.
2. **Ordered actions after verify:**
   1. **Finals hold** — keep final `mp4` / `png` / signed `json` until upload
      and watch-page verify; archive to the shared backups directory only if the
      owner asks.
   2. **Intermediates delete** — task-owned `cut.mp4`, `work/` frames,
      `*.wav` scratch, proof frame dirs, `_chrome_*` probes, local
      `.venv-*` created for the task (use delete-check; never-touch paths
      refuse).
   3. **Worktree remove** — `worktree_hygiene` proofs (own, merged, clean);
      non-force `git worktree remove`, then prune; local branch `-d` only
      when merged. Tip-on-origin linked worktrees older than 24h are
      cleanup candidates (`box_disk_guard.stale_tip_on_origin_candidates`).
   4. **Cache trim** — tool-native prune only when free disk is under the
      warn tier or on weekly maintain; never delete HuggingFace / whisper /
      playwright while those tools stay in the active toolset.
   5. **Chrome forks** — delete only `Cache`, `Code Cache`, `GPUCache`,
      `Service Worker/CacheStorage`; never Login Data / Cookies /
      Local State / Preferences.
3. **Disk budget:** `box_disk_guard` free-GiB tiers — **warn** under
   **15 GiB** free; **block** new workers under **10 GiB** free. Also run
   `df` before heavy work; when **used percent ≥ 80%**, set
   **BLOCKED-INFRA** and stop starting new heavy workers until space
   recovers (`blocked_infra_reasons`). Steady-state target: ≥20 GiB free.
4. **Shared venvs** live outside per-task trees (for example
   `~/.cache/chaos-engine/venvs/`); never leave a task `.venv` inside a
   closed worktree.
5. **Scoped worker dirs:** each worker owns one worktree / scratch dir; the
   lead owns cleanup of that stream after DoD. Foreign worktrees (other
   agent mtime <24h with an active process) are refused.
6. **Outdated YouTube uploads:** cleanup-at-close is always OK and encouraged.
   After a **replacement** is verified live (watch page OK **and** schedule
   OK), delete the outdated upload. Log every deleted video ID (append-only
   receipt via `box_disk_guard.log_deleted_youtube_ids`). Never delete the
   only live copy; never delete before the replacement verify.
7. **Live status before board writes:** before writing any status or board
   row, verify the item's live state with `gh` at write time
   (`box_disk_guard.verify_live_item_state_before_status`). Never write
   "merge queue" for a PR that has already merged.
8. **Doctor/repair dirt:** if CE entry doctor/repair modifies tracked files,
   commit that through the normal PR path or revert it before close. Do not
   leave a dirty tree. Session logs under the shared workspace root must be removed or
   moved into a scoped temp dir at close (record paths in the session
   created-paths manifest).
9. **Close gate:** `item_close_blockers` requires a clean `git status` plus an
   accounted list of created files (session manifest) before the item can
   close.
10. **Self-created cleanup:** workers record every path they create in a
    per-session manifest under a scoped temp dir
    (`CHAOS_SESSION_CREATED_MANIFEST`). `delete-check` allows deleting exactly
    those manifest paths, or reverting CE-managed files to the primary SHA,
    without an owner code. Never-touch and all other paths still need the
    code (or stay locked). A delete-check refusal is final — never bypass
    with raw `rm`.
11. **Large media** goes to a release artifact or Git LFS — not unbounded box
   scratch. Protected folders and the delete-guard (including its code
   requirement) stay intact.

## Why

Shared boxes fill from orphaned worktrees (~1.5 GiB each), in-tree render
intermediates, and unbounded caches. Cleanup before push loses unique
commits; cleanup after verify keeps the box reusable. Free-GiB tiers refuse
new workers; the 80% used alert catches fill even when free GiB still looks
comfortable on a large volume.

## Check

Post-merge: worktree hygiene proofs (own, merged, clean); intermediates gone;
finals retained until upload verified. Pre-heavy: `disk_level` → ok / warn /
block and `blocked_infra_reasons` (includes `disk-used-pct-ge-80`).
BLOCKED-INFRA board row when warn, block, or ≥80% used. Close gate: clean git status + accounted created-files (`item_close_blockers`). Live board writes call `verify_live_item_state_before_status`. Unit:
`tests/scripts/test_box_disk_guard.py` classifiers, percent gate, close blockers, live-status gate.

## Follow-up

[cleanup scopes](cleanup-scopes.md), [delete-check discipline](delete-check-discipline.md),
`scripts/ci/box_disk_guard.py`, `scripts/ci/worktree_hygiene.py`.
