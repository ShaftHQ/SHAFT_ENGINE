# OmniRoute living lessons

Append dated bullets here (and optional `.memory` gotchas) as deliveries learn
more. Seed list absorbed from #5864 (2026-09-16 process-owner wave with
#5858/#5455). Related enforcement: #5863 /
[proof-of-dispatch.md](proof-of-dispatch.md).

## 2026-09-16 — agent machine vs user machine (ROG)

- **Box ≠ ROG loopback.** Cursor / remote box executors see
  `127.0.0.1:20128` as connection refused. The user's dashboard and
  `omniroute serve` traffic live on the ROG (or whichever host runs serve).
  OmniRoute-first delivery must Shell on that user machine via Local Execution
  / the correct `machineId`, against the ROG checkout path — not the agent
  box loopback.
- **Probes ≠ coding.** CredentialHealth / catalog refresh create call_logs
  (`model=connection-test`, often `tokens=0`) and leave `/home` looking idle
  for coding sessions. Operators watching the dashboard correctly expect
  `omniroute run` traffic.
- **`auto/coding` + Claude Code.** `omniroute run --model auto/coding claude`
  can emit `[claude-code:unrecognized_model]` for session-title generation.
  Prefer a concrete coding id from live `candidates --task coding` (for
  example `antigravity/claude-sonnet-4.6`) or document the supported auto path
  per target after smoke.
- **Quota exhaustion.** Antigravity (and peers) can 429 with short reset
  windows. Skip the exhausted identity (existing backoff), try the next
  candidate, and surface the skip — never claim success.
- **Retired model warnings.** Claude Sonnet 4 retirement warnings can appear
  even when requesting Sonnet 4.6. Prefer live catalog native ids; investigate
  mapping before pinning retired display names.
- **Dismiss OmniRoute when thrashing.** After repeated
  unrecognized_model / 429 / catalog-miss loops with no productive
  `omniroute run`, record an explicit OmniRoute blocker or
  `RUNTIME_EXHAUSTED` and continue on a qualified native path when the adopter
  allows fallback. Do not infinite-probe.

## How to append

1. Date the bullet (`YYYY-MM-DD`).
2. State the failure mode and the durable rule in one or two lines.
3. Link the issue/PR when useful.
4. Keep secrets, account emails, and raw tokens out of this file.
