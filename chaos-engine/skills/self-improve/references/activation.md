# Activation

## Always-on (cheap)

SessionStart injects a single locator line pointing at this skill. Do **not**
load reference files or scan observation directories at SessionStart.

## Heavy path (right lifecycle point)

Activate the full protocol:

1. **Post-delivery / Learning Session** — Learning Session after every final delivery
   (owner approval, publish done, final PR merged), automatic, before the final report. Portable Stop / delivery-complete hooks own this duty on every
   supported host (see router skill Learning Session section and
   [lifecycle-hooks](../../../references/lifecycle-hooks.md)).
2. **Explicit user ask** — "self-improve", "learning session", "observe skills".
3. **Optional** — after a delivery-phase gate completes with mutations.

## Non-skips (hard)

- **Every final delivery owes a session.** A product-only delivery owes one
  too; untouched `chaos-engine/` files do not matter. Nothing new after the
  lessons diff is one report line, not a skip.
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
   hand-edit `.memory/**` sidecars. If a body changed outside
   `memory save`, run `python3 .chaos-engine/skills/local-agency/scripts/tip_preflight.py --rehash <sidecar>`
   before push ([tip-churn preflight](../../../references/tip-churn-preflight.md)).
6. Do **not** auto-edit skills/hooks. Propose a harness change as a GitHub issue.
7. Do not report harness lesson text. Product counts may be `product queued N`
   or `nothing durable`.
8. Close with one retrospective: what happened, why it happened, continue
   doing, stop doing, start doing, and one next action. File every actionable
   line before the note. A ChaosEngine change is a GitHub issue on the
   configured upstream repository. A project-only change is a GitHub issue
   on the repository of the project that uses ChaosEngine. Cite each issue
   URL. Write `nothing actionable` only when no line needs an issue. Do not
   add a token or cost estimate.
