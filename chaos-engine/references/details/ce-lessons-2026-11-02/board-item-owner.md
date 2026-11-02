#7698 BOARD-ITEM-OWNER

Verifier must own the board item (or borrowed_by); drop-verify receipt before redo.

Enforced by [`lessons_2026_11_02.py`](../../../rules/lessons_2026_11_02.py) (`evaluate_board_item_owner`, `evaluate_drop_verify_before_redo`). Extends verify_dropped.
