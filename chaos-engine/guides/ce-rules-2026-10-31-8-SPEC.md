# CE 2026.10.31.8 — L78–L81 + install perf + trainee fact + dump/install hardening

## Scope

- `chaos-engine/` (gates, references, lessons, VERSION, worktree_overlay fast-path,
  memory_full_dump shard/CLI, bootstrap SSL retry + local-clone fallback,
  tool.py git-fetch retry)
- `scripts/ci/` (`review_scratch.py`)
- `tests/scripts/` (contracts + L74 proven-red)
- `chaos-engine/task_ledger.py` (L80 per-task budget override)

## Cards

| Id | Card | Module |
| --- | --- | --- |
| L78 | [review-scratch-cleanup](../references/details/ce-rules-2026-10-31-8/review-scratch-cleanup.md) | `scripts/ci/review_scratch.py` |
| L79 | [cms-block-identity](../references/details/ce-rules-2026-10-31-8/cms-block-identity.md) | [`cms_block_identity.py`](../rules/cms_block_identity.py) |
| L80 | [test-run-budget](../references/details/ce-rules-2026-10-31-8/test-run-budget.md) | [`task_ledger.py`](../task_ledger.py) |
| L81 | [no-self-clock](../references/details/ce-rules-2026-10-31-8/no-self-clock.md) | [`motion_graphics_gate.py`](../addons/design-skills/scripts/motion_graphics_gate.py) |
| PERF | [install-perf-30s](../references/details/ce-rules-2026-10-31-8/install-perf-30s.md) | [`worktree_overlay.py`](../worktree_overlay.py) |
| FACT | [trainee-count-600](../references/details/ce-rules-2026-10-31-8/trainee-count-600.md) | [`public_facts.py`](../rules/public_facts.py) |
| DUMP | [memory-dump-own-shard](../references/details/ce-rules-2026-10-31-8/memory-dump-own-shard.md) | [`memory_full_dump.py`](../rules/memory_full_dump.py) |
| INST | [install-download-retry](../references/details/ce-rules-2026-10-31-8/install-download-retry.md) | [`bootstrap.py`](../bootstrap.py) / [`tool.py`](../tool.py) |
| L74 | proven-red on every new/changed gate | [`gate_proven_red.py`](../rules/gate_proven_red.py) |

## Remotes (identity)

ChaosEngine identity only for the origin engine remotes (see commit_identity).
