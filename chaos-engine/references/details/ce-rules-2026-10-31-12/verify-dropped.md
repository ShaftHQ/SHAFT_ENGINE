# L86 VERIFY-DROPPED

Before redoing any task marked dropped, cancelled, or "someone else will do it",
check live state first (PRs, issues, posts, files, releases).

If the work already exists: report `already exists` with the link. Never
duplicate. Applies to every bot.

## Check

A redo without a live-state probe this turn is a process FAIL. The
[`verify_dropped`](../../../rules/verify_dropped.py) helper returns hits when
a brief claims "redo X" without an evidence URL / `already exists` receipt.
