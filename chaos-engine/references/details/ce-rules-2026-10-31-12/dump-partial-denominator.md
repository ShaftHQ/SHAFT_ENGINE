# L85 DUMP-PARTIAL-DENOMINATOR

RecallMemory / memory dump has a ~4 KB per-call cap. `count_ok` is never true
without a known host `own_shard` id set. Until full-entry export exists, report
partial with an explicit denominator.

Enforced by [`lessons_l82_l92.py`](../../../rules/lessons_l82_l92.py) (`evaluate_dump_partial`) with proven-red unit tests.
