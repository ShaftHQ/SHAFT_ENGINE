# Repeat-failure PDCA

When the **same stage or gate fails twice**, a task **overruns its budget**, or
**different failures hit the same stage**, STOP.

1. Write a root-cause note under `.chaos-engine-state/repeat-failure-pdca/`
   (JSON): `stage`, `failures`, `hypothesis`, `evidence`, `plan`.
2. Make **one** change from that plan.
3. Check it (targeted test or the same CI stage).
4. Only then act further. **No chained fix-after-fail.**
5. After a second consecutive failure of the same stage, the next push waits
   for that RCA entry in the progress file (`push-without-rca`). This sits
   beside the verified-findings round cap.

Enforced by [`lessons_repeat_failure_pdca.py`](../../../rules/lessons_repeat_failure_pdca.py)
(`evaluate_repeat_failure_pdca`). Always-loaded: [bot-entry](../../bot-entry.md#repeat-failure-pdca)
and ChaosEngine skill Check and deliver.
