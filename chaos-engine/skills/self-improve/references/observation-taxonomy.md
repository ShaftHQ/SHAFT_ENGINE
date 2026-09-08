# Observation taxonomy (CE self-improve)

Load on demand when classifying a finding.

## Tracks

| Track | `learning.py` category hint | Examples |
| --- | --- | --- |
| harness/skills | `guidance` | Missing rule, friction in SKILL.md, bad progressive disclosure |
| harness/hooks | `reliability` | Exit codes, SessionStart payload size, false blocks |
| harness/memory | `tooling` | MemPalace/Graphify/Headroom retrieval misses, doctor fix-next |
| harness/ux | `portability` | Installer/doctor dead-ends, host parity gaps |
| product | `guidance` or `tooling` | Product enhancement candidates for the app under development |
| security | `security` | Only privacy-safe, non-secret observations |

## Privacy invariants (hard)

Use `chaos-engine/learning.py` `queue` path only. Never put secrets, absolute
paths, URLs (except after submit), emails, raw prompts, transcripts, or code
fences into queued fields. Respect category allow-list and field length limits.

## Dual-track checklist

Every Learning Session should attempt:

1. At least one **harness** observation (or explicit "nothing durable").
2. At least one **product** enhancement candidate when product work occurred
   (or explicit "nothing durable").

Product filing playbook: [product-track.md](product-track.md).
