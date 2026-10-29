# Delete-check discipline (D5)

Field lesson: never delete outside ChaosEngine's delete-check. Generic CE rule;
project-specific allowlists stay in the delete-guard implementation and its
dedicated reference.

## Rule

Before every `rm` / `unlink` / `git clean` / `find -delete` / `rmtree` / moving
data out of a workspace:

1. Install ChaosEngine in **that** worktree (not only a sibling checkout).
2. Run `python3 .chaos-engine/install.py doctor --project . --agent-summary`
   until it reports `pass` (including `delete-guard: pass`).
3. Run `python3 .chaos-engine/tool.py delete-check <paths>` (or
   `--command "…"`).
4. **Stop on refusal.** Do not route around with a variable, `xargs`, a wrapper
   script, or a renamed command.
5. Prefer commit+push first, or `cp -a` to a backups directory outside the worktree and get owner
   OK. Owner override is only `--request-code` → the owner pastes the one-use
   code in chat. An agent-relayed code is not approval.

Hookless bots load this via [bot entry](bot-entry.md) and the
`chaosengine-bot-entry` skill. Implementation detail:
[delete guard](delete-guard.md).

## Why

Cleanup that used a bare `rm -rf` in a worktree with no CE overlay bypassed the
guard entirely. Doctor + delete-check in the **same** tree is the only safe
path.

## Check

- Doctor row `delete-guard: pass`.
- Adversarial: attempt delete of a modified, uncommitted path → refusal.
- Never-touch paths refuse even with a code.

## Follow-up (owned elsewhere)

Verified-upload unlocks, allowlist hardening, and `delete_guard.py` changes are
owned by the dedicated delete-guard PR stream. Do **not** edit
`delete_guard.py`, `references/delete-guard.md`, or
`tests/scripts/test_chaos_engine_delete_guard_7251.py` from a lessons PR.
