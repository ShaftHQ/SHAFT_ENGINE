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
3. A ChaosEngine harness lesson, finding, or potential enhancement is a GitHub
   issue only. Do not call `learning.py queue --track harness`. Do not write
   that lesson into chat.
4. A product lesson may use `python3 .chaos-engine/learning.py queue --track product`.
   Submit confirmed product candidates with `learning.py submit`.
5. Persist durable Memory knowledge with `memory save --stdin` only — never
   hand-edit `.memory/**` sidecars (#5852). If a body changed outside
   `memory save`, run `python3 .chaos-engine/skills/local-agency/scripts/tip_preflight.py --rehash <sidecar>`
   before push ([tip-churn preflight](../../../references/tip-churn-preflight.md), #6169).
6. Do **not** auto-edit skills/hooks. Propose a harness change as a GitHub issue.
7. Do not report harness lesson text. Product counts may be `product queued N`
   or `nothing durable`.
8. The reflection and this learning action include all ten elements: intended
   versus actual result, cause of the result, what to repeat, what to change,
   external proof, lesson for the next attempt, bounded retry, committed next
   action, durable carry-forward, and token consumption optimization. Token
   consumption optimization names the least-token path for the same kind of
   task next time and one further cut so consumption keeps falling.
