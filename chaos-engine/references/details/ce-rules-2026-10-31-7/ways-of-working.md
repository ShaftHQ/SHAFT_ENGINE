# Ways of working (WoW) — enforced

Source brief: owner msg 2026-10-31 (Cairo). Applies to every bot and worker that
loads ChaosEngine.

## 1. Pushback and flags

Pushback is encouraged. Disagree with the orchestrator or peers when you see a
better way; say why; propose an alternative. Overloaded? Flag the orchestrator
**early** with: what is overloaded, why, and 1–3 concrete ideas (delegate,
reorder, split scope, new member).

## 2. Delegation

Any bot may delegate to sub-workers when the worker loads ChaosEngine and shares
memories (shared + owner private via CE abstractions). Independent adversarial
reviews are **always** a fresh worker — never self-review in the same context.

## 3. Parallelism limits

Parallelize when it helps, under two hard rules:

1. **One task of a kind** at a time per owner (one open PR touching the same
   build config class; one avatar likeness iteration; one render pipeline per
   asset type).
2. **No conflicting work.** No two workers edit the same file at once. Before
   starting, claim the file set (`file_claim_lock` (repo `scripts/ci/file_claim_lock.py`)); overlapping
   claims wait. Complements stream ownership in
   [parallel-streams.md](../../parallel-streams.md) /
   `scripts/ci/parallel_stream_conflict.py`.

Different kinds with disjoint file sets may run in parallel. After each task:
lessons → ChaosEngine before the next task of that kind.

Anti-patterns: many PRs conflicting on the project build file; many avatar videos before one
passes likeness.

## 4. Memory

Back up all team memories to `/workspace` on a cadence; abstract into CE.
See [memory-full-dump.md](memory-full-dump.md).

## 5. Implicit by default

Sections 1–4 are explicit, enforced (guards/preflight where possible), and
implicit for future parallel / full-speed work.

## Check

- [ ] `file_claim_lock.refuse_claim` / `refuse_same_kind` before dispatch
- [ ] Board ≤1 in-progress per kind per owner
- [ ] Memory dump receipt `count_ok=true` on cadence
- [ ] Adversarial review is a fresh worker id
