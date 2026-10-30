# ChaosEngine upgrade on release

## Rule

Every bot installs the latest ChaosEngine into its working directories, then
loads and uses it for **all** tasks. Fresh workers inherit that install
implicitly (`ce_load: required` + `tool.py entry`).

1. **Version identifier:** `chaos-engine/VERSION` holds the broadcastable
   calver (for example `2026.10.30.1`). Read it with
   `scripts/ci/ce_version.py` or `Path("chaos-engine/VERSION").read_text()`.
2. **Upgrade-on-release:** when a batch of harness improvements lands, the
   releasing lead broadcasts `CE upgrade <version>` to all bots. Each bot
   reinstalls ChaosEngine in its working dirs (primary + active session
   worktrees via `worktree_overlay.py upgrade-session` / `materialize`)
   and confirms `doctor --agent-summary` is pass. Prefer
   `upgrade-session` over `CHAOS_ENGINE_ALLOW_LINKED_WORKTREE=1` +
   full `install.py install` (~227s across 17 worktrees). Timing
   budget: under ~5s per worktree when VERSION matches (hash skip);
   under ~15s when overlay files upgrade without a plugin marketplace
   round-trip (~60s). `materialize` upgrades stale overlay files in
   place (hash/manifest); local edits refuse with a clear message.
3. **Worker CE-entry gate:** a fresh worker must load ChaosEngine before any
   repo work. Briefs carry `ce_load: required` and an entry instruction;
   lint with `worker_ce_entry.py`. Prefer the post-upgrade VERSION.
4. **Upgrade issues** (including slowness or delays) go back to the harness
   owner — do not silently skip the upgrade or invent a local fork.
5. Role-neutral: no bot-specific exceptions; ownership of the harness is
   the harness owner role, not a named agent.

## Why

A bot on a stale overlay misses gates, lessons, and delete-guard changes
that already landed. Implicit inheritance only works when every working dir
is on the broadcast VERSION.

## Check

`chaos-engine/VERSION` exists and matches `^\d{4}\.\d{2}\.\d{2}(\.\d+)?$`.
Broadcast string embeds that exact value. Worker briefs still pass
`worker_ce_entry.py lint`. Unit: `tests/scripts/test_ce_version.py` and
process-card needles in the memory-abstraction suite.

## Follow-up

[bot entry](bot-entry.md), [harness maintenance](harness-maintenance.md),
`chaos-engine/skills/local-agency/scripts/worker_ce_entry.py`,
`scripts/ci/ce_version.py`.
