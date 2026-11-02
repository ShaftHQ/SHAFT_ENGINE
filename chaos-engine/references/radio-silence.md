# Radio silence (send guard)

## Rule

When radio silence is armed (`CHAOS_RADIO_SILENCE=1` / `true` / `full`, or
project file `.chaos-engine-state/radio-silence`), **do not** send user-facing
status, progress, or routine NEEDS-HUMAN chatter to the owner chat. Route
status to the orchestrator contact only. Owner-visible sends are allowed only
when the orchestrator or owner explicitly requests them, or for a true
human-only approval that cannot be relayed.

`chaos-engine/radio_silence.py` → `user_send_blocked()`.

## Why

FULL radio silence was breached by progress narratives in owner-visible
threads. The orchestrator is the status plane.

## Check

- Env or flag file set → `user_send_blocked()` is True; status channel =
  `orchestrator-only`.
- Bot-entry entry card mentions the guard when armed.

## Follow-up

[bot-entry](bot-entry.md), [sequential WIP](sequential-wip.md),
[multi-agent PDCA](multi-agent-pdca.md).
