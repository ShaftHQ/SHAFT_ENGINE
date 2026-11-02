# L86 VERIFY-DROPPED

Before redoing any task marked dropped, cancelled, or "someone else will do it",
check live state first (PRs, issues, posts, files, releases). Check the **board
owner** first (C4): do not claim or redo another owner's dropped item without
naming that owner in the receipt.

If the work already exists: report `already exists` with the link. Never
duplicate. Applies to every bot.

## Check

A redo without a live-state probe this turn is a process FAIL. A dropped /
`VERIFY-DROPPED` brief without a board-owner marker is also a FAIL. The
[`verify_dropped`](../../../rules/verify_dropped.py) helper returns hits when
a brief claims "redo X" without an evidence URL / `already exists` receipt, or
mentions dropped work without `board owner` / `board_owner`.
