# Activation

## Always-on (cheap)

SessionStart injects a single locator line pointing at this skill. Do **not**
load reference files or scan observation directories at SessionStart.

## Heavy path (right lifecycle point)

Activate the full protocol:

1. **Post-delivery / Learning Session** — after confirmed delivery, before the
   final report (see router skill Learning Session section).
2. **Explicit user ask** — "self-improve", "learning session", "observe skills".
3. **Optional** — after a delivery-phase gate completes with mutations.

## Protocol (heavy)

1. Load this SKILL.md (already loaded) + `observation-taxonomy.md` if classifying.
2. Scan the session for harness friction and product enhancement candidates.
3. For each durable lesson, build a privacy-safe candidate and
   `python3 .chaos-engine/learning.py queue ...` (or the host-equivalent wrapper).
4. Do **not** auto-edit skills/hooks; propose via queued `proposedChange`.
5. Report counts: harness queued / product queued / nothing durable.
