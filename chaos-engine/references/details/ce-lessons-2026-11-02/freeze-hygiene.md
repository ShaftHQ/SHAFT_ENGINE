#7716 FREEZE-HYGIENE

Commit before dispatching freezable workers; stop only own workers; record frozen HEAD + next step.

Enforced by [`lessons_2026_11_02.py`](../../../rules/lessons_2026_11_02.py) (`evaluate_freeze_hygiene`).
