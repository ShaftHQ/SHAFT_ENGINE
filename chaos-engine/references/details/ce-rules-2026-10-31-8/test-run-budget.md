# L80 TEST-RUN-BUDGET

Per-task budget override for `kind=test-run` (default 35 min is too tight for
sequential long suites, e.g. J11 ~14 methods). Checkpoint `--kind test-run`
raises the session budget to 90 minutes unless `--minutes` / `--budget` set an
explicit value. Module: [`task_ledger.py`](../../../task_ledger.py).
