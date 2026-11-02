# Mobile client automation (portable)

On-demand card for mobile UI automation against a real backend. Generalized
from field retros; **no client, host, or product names**.

## Rule

1. **Read-only vs production:** never tap final submit / pay / delete / cancel;
   never fire state-changing APIs. Assert the final control is present and
   enabled; do not activate it. Never tap in-app force-update; dismiss-only.
2. **Evidence:** compose framework results **with** attachment files; never
   tip-only merge in scratch temp. Secret-scan the composed report before
   attach; redact session tokens from screenshots/videos.
3. **Secrets:** live credentials only in gitignored property files. Never print
   tokens, passwords, keys, session IDs, or JWTs to chat, logs, or commits.
   Secret-scan every commit in the push range; never `--no-verify`.
4. **Branches / PRs:** no force-push to the default branch. Do not push or open
   a client-repo PR without the owner's explicit OK when that charter applies.
5. **Session helpers:** if renewal fails, report HTTP status + response **keys
   only** (no values).
6. **Device farm / emulator:** bound memory; install one heavy artifact at a
   time; prefer module-scoped runs.
7. **Assertions must be able to fail** — no tautologies. Upstream engine gaps
   become tracked issues with a minimal repro; do not paper over them in the
   client suite.
8. Pair with [mobile-retro harness](mobile-retro-harness.md) (green freeze,
   backend→Blocked, primary time-cap, tracked NO_PUSH, evidence pin,
   NamedBlock by gap id).

## Why

Mobile suites that mutate production or leak session material create real
incidents. Tip-only results merges lose attachments; unscanned HTML embeds keys.

## Check

Doctor/secret-guard pass before push. Dry-run: final submit control asserted
but not clicked. Green run freezes results; mismatched build pin rejects
evidence. Attachment-count check on composed HTML.

## Follow-up

`mobile_retro.py`, `scripts/ci/no_push_guard.py`, delete-check before cleanup.
