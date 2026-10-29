# Activation

## Always-on (cheap)

SessionStart injects a single locator line pointing at this skill. Do **not**
load reference files or scan observation directories at SessionStart.

## Heavy path (right lifecycle point)

Activate the full protocol:

1. **Post-delivery / Learning Session** — after confirmed delivery, before the
   final report. Portable Stop / delivery-complete hooks own this duty on every
   supported host (see router skill Learning Session section and
   [lifecycle-hooks](../../../references/lifecycle-hooks.md)).
2. **Explicit user ask** — "self-improve", "learning session", "observe skills".
3. **Optional** — after a delivery-phase gate completes with mutations.

## Non-skips (hard)

- **Unchanged `chaos-engine/` files are not a valid skip.** Product-only
  deliveries still owe one Learning Session.
- Agent-local routines or single-host memory are not substitutes. Harness parity
  requires lasting policy to live in the portable ChaosEngine overlay (hooks,
  skills, installer/doctor, host guidance adapters) so Codex, Claude, Grok,
  Gemini, Copilot, and Grok Bot share the same outcomes.
- Prefer lean self-improve on the delivery Stop path over always-on Task
  Observer style scanning (token cost).

## Protocol (heavy)

1. Load this SKILL.md (already loaded) + `observation-taxonomy.md` if classifying.
2. Scan the session for harness friction and product enhancement candidates.
3. For each durable lesson, build a privacy-safe candidate and
   `python3 .chaos-engine/learning.py queue ...` (or the host-equivalent wrapper).
4. Submit confirmed candidates with `learning.py submit` (GitHub issues, not
   queue-only).
5. Do **not** auto-edit skills/hooks; propose via queued `proposedChange`.
6. Report counts: harness queued N / product queued N / nothing durable.
