---
name: profile-slow-installs
description: Use when ChaosEngine install or upgrade-session is slow across many worktrees; profile per-dir timing and prune stale dirs.
---

# Profile slow installs

## Rule

1. When install / `upgrade-session` exceeds ~25 s per directory under normal
   load (or 40–80 s outliers), record **per-dir elapsed** from the JSON
   receipt (`elapsed_s`) or `tool.py` / installer logs under
   `.chaos-engine-state/logs/`.
2. Compare dirs: a stale linked worktree or half-applied overlay often dominates.
3. Prune **own, merged, clean** worktrees via hygiene proofs; never foreign or
   never-touch paths. Re-run the profile after prune.

## Helper

Run [`profile_installs.py`](../profile_installs.py):

`python3 chaos-engine/profile_installs.py --log <upgrade-session.jsonl-or-dir>`
summarises `elapsed_s` by session path when receipts are available.

## Why

Parallel load pushed install/doctor from ~7 s to ~25 s and some dirs to 40–83 s
without a per-dir breakdown, so the slow outlier stayed invisible.
