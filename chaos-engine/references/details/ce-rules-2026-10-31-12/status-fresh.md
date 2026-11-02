# L83 STATUS-FRESH

Status reports to Cortex (or the owner) start with a fresh fetch of live state
plus a dropped-item check across bots. Never paste a stale checkpoint as
"current".

Enforced by [`lessons_l82_l92.py`](../../../rules/lessons_l82_l92.py) (`evaluate_status_fresh`) with proven-red unit tests.
