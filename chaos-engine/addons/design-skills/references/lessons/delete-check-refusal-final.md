# L45 — Delete-check refusal is final

## Rule

1. A `tool.py delete-check` (or PreToolUse `block_reason`) refusal is **final**.
   Do not bypass it with raw `rm`, `unlink`, `pathlib` deletes, encoded argv,
   wrappers, or a second shell. Re-route: commit/push, back up, add an
   allow-list scratch pattern, or request an owner code for the **same** paths.
2. Self-created paths recorded in the per-session created-paths manifest
   (`CHAOS_SESSION_CREATED_MANIFEST`, scoped temp) may be deleted without a
   code. CE-managed doctor/repair dirt may be reverted to the primary SHA
   without a code. **Never-touch** paths stay locked even with a code.
3. Rematerialize / `worktree_overlay materialize` must clean its own stale
   leftovers (manifest-recorded overlay files the primary no longer has).

## Why

Bypassing the guard deletes the only copy of unpushed work. Sharing the
refusal with a raw `rm` is how session logs and CE repair dirt used to stick
around — or worse, wipe protected trees. Manifest-scoped self-delete restores
cleanup-at-close without weakening the guard for anything else.

## Check

Unit: `tests/scripts/test_chaos_engine_delete_guard_7251.py` (refusal final /
no raw-rm bypass) and session-manifest allow tests. Process: cleanup-at-close
close gate requires accounted created files.

## Sources

ChaosEngine delete-guard , cleanup-at-close gate, session
created-paths manifest.
