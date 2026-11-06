L7718 STOP-SCOPE-GUARD

Default targeted stop; broadcast needs reason + warning + peer ack; claimed-done needs live verify verdict.
After a cancel that followed a live write, re-check the live surface before the next status.

Enforced by [`lessons_2026_11_02.py`](../../../rules/lessons_2026_11_02.py) (`evaluate_stop_scope_guard`, `evaluate_claimed_done_verify`). Extends E1/E2.
