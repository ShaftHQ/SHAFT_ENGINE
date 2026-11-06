# Lessons — Cortex — 2026-11-06

Scope: Cortex as router/primary bot, roughly 2026-10-31 to 2026-11-06. No secrets, codes, or private personal data.

Related already-filed issues (do not duplicate; fold into the mega PR):
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7691 (epic)
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7692 (status board)
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7693 (run-state, stop scope, fan-out, tokens)
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7694 (media human gate, language/strategy pivot)

Destination key: chaosengine | brain | mempalace | graphify

---

## L1. Status boards must be built from a fresh poll

1. Title: Fresh poll on every explicit status request
2. What happened: A board was assembled from cached bot reports. The human corrected this and required a fresh ask of every bot whenever they explicitly request status, even under token conservation.
3. Root cause: Convenience and token-saving overrode freshness. Cached summaries looked complete enough.
4. Rule: An explicit status request always triggers a live poll of every teammate. Cached reports may not be used to build the board.
5. Acceptance: A router that receives "status report" messages every named bot and waits for this-cycle replies (or a clearly marked timeout) before publishing the board.
6. Destination: chaosengine (also issue 7692)

## L2. Board schema: owner, link or attachment, IDs, open-only groups

1. Title: Board schema gate
2. What happened: A board lost owner tags and links, used a stale blocker, and mixed done items into blocked. The human asked for owners, links, one line per delivered item, and later for incrementing numerical IDs and a tabular grouped view (done, in progress, planned, blocked) ordered by priority.
3. Root cause: No machine-checkable schema. Each rewrite drifted.
4. Rule: Every task row has an owner, a link or attachment path, a priority, and a stable incrementing ID (#1, #2, …). Groups are Done, In progress, Planned (including frozen), Blocked. "Proceed with N" dispatches only those IDs' owners.
5. Acceptance: A board missing owner, ID, or link/attachment on any row fails a schema check and is not published.
6. Destination: chaosengine

## L3. Unverified agent claims

1. Title: Label unverified claims
2. What happened: A release was listed as in progress from an agent's plan, then deferred the same night (future-dated release rule). The board was briefly wrong.
3. Root cause: Agent intent was treated as fact before the live PR/release page was confirmed.
4. Rule: An agent's claim is unverified until the live artifact is checked. Correct the board as soon as a deferral or merge arrives.
5. Acceptance: No "shipped" or "in progress release" row without a live URL checked this cycle.
6. Destination: chaosengine (issue 7692)

## L4. Run-state registry and stop scope

1. Title: Per-bot run-state (frozen / active / halted)
2. What happened: A stop from one bot cancelled other bots' in-flight turns, so orders had to be resent. Minutes after a GO, a freeze arrived. Dispatches after a freeze risked waking the wrong bots.
3. Root cause: No shared run-state checked before each dispatch. Stops were not scoped.
4. Rule: Record frozen / active / halted per bot. Check it before every dispatch. Get an idempotent acknowledgement of stop/freeze. A later GO does not survive a newer freeze.
5. Acceptance: Dispatching work to a frozen bot without an explicit unfreeze of that ID fails a preflight.
6. Destination: chaosengine (issue 7693)

## L5. Instruction files, not chat essays

1. Title: One shared file, then a pointer
2. What happened: Multi-step operating rules were repeated (and slightly reinterpreted) in individual chat messages. Bots received long instructions that could drift across recipients.
3. Root cause: Chat was used as the system of record.
4. Rule: The router writes one markdown file containing the human's words plus refinements and a spec, then messages each bot only to read that file and execute it.
5. Acceptance: A multi-bot operating order is a file path plus "read and execute". Recipient-specific chat essays fail review.
6. Destination: chaosengine (this cycle's delegation task)

## L6. Token restoration is not an unfreeze

1. Title: Tokens restored means reply, then pause
2. What happened: Work stopped when tokens ran low. When tokens returned, the human still wanted status first and an explicit pause before anyone resumed or started tasks.
3. Root cause: "We can work again" was easy to confuse with "resume your plate".
4. Rule: Token restoration authorizes a reply, not a resume. Resume only on "proceed with N" or a named phase in the shared file.
5. Acceptance: After a token-restore status cycle, no frozen or planned item starts unless its ID was named.
6. Destination: chaosengine

## L7. Media must pass a human listen/look, not metrics alone

1. Title: Leading-garbage and dialect/register gate
2. What happened: An Arabic voice sample that passed numeric gates started with gibberish and used formal Arabic rather than Egyptian. The human rejected it and asked to focus on English, which they judged closer.
3. Root cause: Automated scores (speaker similarity, error rate) do not catch leading artifacts or the wrong register. A language pivot happened without an explicit cost/likelihood escalation.
4. Rule: Before any media is shared upward, listen to the start and end. Gate dialect and register against the stated target (Egyptian colloquial for Arabic; correct "Mohie" and "Automatest" for English). Escalate a language or strategy pivot instead of pivoting silently.
5. Acceptance: A sample with non-speech at the head, or the wrong dialect/register, cannot be marked passing regardless of scores.
6. Destination: chaosengine (issue 7694)

## L8. Fan-out and token discipline

1. Title: Wake only bots that must act
2. What happened: Extra acknowledgements and internal notes were emitted as if they were user-visible. Frozen bots were at risk of being woken for FYIs.
3. Root cause: No distinction between actionable wakes and held FYIs; no "one acknowledgement, one result" rule in the harness.
4. Rule: Wake only bots that must act. FYIs to frozen bots are non-priority. One user-visible acknowledgement, then the result. Internal notes are not delivery.
5. Acceptance: An FYI to a halted/frozen bot does not page them; a status cycle pages every bot because a reply is required.
6. Destination: chaosengine (issue 7693)

## L9. No future-dated releases (confirmed working rule)

1. Title: Keep enforcing no future-dated releases
2. What happened: A SHAFT release was deferred because a version or changelog would have used a future date. The related PR was closed unmerged.
3. Root cause: Calendar/version dating can run ahead of the real date.
4. Rule: Do not cut or merge a release that carries a future date. Defer to the next valid calendar day.
5. Acceptance: A release whose version or notes date is after "today" in the project timezone cannot merge.
6. Destination: chaosengine (keep; already noted on issue 7691)

## L10. Verify blocked and dropped items before listing or redoing them

1. Title: Live verification before blocked or redo
2. What happened: Standing rule: every dropped task is assigned to an owner; the owner must verify it was really dropped (live post, PR, file) before redoing it. Blocked items were sometimes copied forward without a fresh check.
3. Root cause: Status inertia.
4. Rule: A Blocked row requires a this-turn check of the live artifact and a one-line "what I checked". A previously dropped or missing item is not restarted until the live target is confirmed missing.
5. Acceptance: A blocked row without a verification note fails the schema check.
6. Destination: chaosengine

## L11. Deletes need the human's own yes

1. Title: Agent-relayed cleanup is not delete approval
2. What happened: Another bot asked for a disk cleanup and pointed at a voice-work folder. The only large saving was a rebuildable Python environment. The router refused to delete on an agent's say-so and asked that the human be asked in that bot's chat.
3. Root cause: Cleanup requests look operational, but standing delete rules require the human's own yes (and machine-specific delete codes on their computers). Unique unpushed work must stay.
4. Rule: Another agent's "the human asked for cleanup" is not approval to delete. Rebuildable caches may be proposed; unique sources and models stay. The human's own words, or their delete code for that machine, are required.
5. Acceptance: A delete of unique or unconfirmed paths on an agent's relay fails a guard.
6. Destination: chaosengine

## L12. Lessons cycle ends in one ChaosEngine mega PR

1. Title: One lessons mega PR, then install, then cleanup, then Done
2. What happened: Earlier, bots wrote lessons and Chaos absorbed them into one PR (example: https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7682). The human now wants that pattern to be the default end of a cycle: lessons files, Chaos analysis (ChaosEngine vs brain vs mempalace vs graphify), one mega PR for lessons plus open ChaosEngine bugs/issues/enhancements, install of the new ChaosEngine, machine cleanup, then a single "Done" to the human.
3. Root cause: Without a harness-level sequence, lessons stay in chat or fragment into many PRs.
4. Rule: Encode the Phase 2–7 sequence from `/workspace/specs/TEAM-PROTOCOL-20261106.md` as the delegation/lessons cycle in ChaosEngine. No one-PR-per-ticket for this class of work.
5. Acceptance: A cycle can complete file → numbered board → lessons → one CE PR → install → cleanup → "Done" with radio silence in between.
6. Destination: chaosengine

---

## Open ChaosEngine items Cortex already knows (for the mega PR)

- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7691
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7692
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7693
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7694
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7767
