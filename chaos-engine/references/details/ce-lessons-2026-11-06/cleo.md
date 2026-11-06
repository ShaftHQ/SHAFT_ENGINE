# Lessons — Cleo — 2026-11-06
Scope: growth/delivery work from about 2026-10-31 through 2026-11-06 (SEO/AEO/GEO, social and YouTube metadata, CV and profile facts, status reporting, Learning Session filing). Facts are abstracted; no secrets, tokens, delete codes, or private personal data.

Public facts that belong in brain / mempalace (do not paraphrase, and do not copy the values into this tree):
- Title: `<profile.title>` (reject the banned variants listed in the private profile)
- Trainee count: `<profile.trainee_count>` (reject a retired count and any qualifier tied to that retired count)
- Organization founding year: `<profile.founding_year>` (reject a different year copied from a third-party profile)

## Already filed ChaosEngine issues (verified 2026-11-06: all CLOSED)
These were opened as the 2026-11-02 Learning Session tracker. They are closed now; treat them as already submitted, not as new tickets.
- Umbrella: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7717
- Stop-scope, post-cancel live recheck, claimed-done verify: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7718
- Publish-metadata lint: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7719
- BLOCKED-INFRA for apt 5xx / unsigned lists: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7720
- foundingDate policy, scheduler timezone proof, admin URL shape: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7721

Note: #7720 being closed does not mean apt updates work. On 2026-11-06 `sudo apt-get update` still returned HTTP 500 and unsigned lists through the local apt proxy, while a direct HTTPS fetch of the same suite's InRelease returned 200. The issue was about a harness gate (do not force), not about repairing the mirror.

---

### 1. Stop-all is not a local worker stop
1. Title: Scoped stops, not process-wide cancels
2. What happened: A "stop everything I started" action also cancelled other bots' in-flight turns.
3. Root cause: The stop primitive is process-wide (all child workers and, in this host, peer turns), but the caller treated it as "stop my helper."
4. Enforceable rule: Default to a targeted stop of the caller's own workers. A host-wide stop requires an explicit "all peers" intent and should warn about collateral impact.
5. Acceptance: A test where bot A stops only its worker leaves bot B's turn running. A documented all-stop path states that peers will be cancelled.
6. Suggested destination: `chaosengine`

### 2. A cancel can land after a live write has already saved
1. Title: Recheck live state after any cancel or halt
2. What happened: A live profile/settings edit had already saved when a cancel arrived. Treating the task as aborted would have hidden a real production change.
3. Root cause: UI saves and chat-level cancels are not a single transaction.
4. Enforceable rule: After any cancel, halt, or interrupted publish, re-read the live page or file. Keep the change only if it matches approved facts; then run the post-check. Never assume "cancelled" means "not applied."
5. Acceptance: A fixture where the live value already matches the intended edit after a cancel is reported as applied-and-needs-check, not as aborted.
6. Suggested destination: `chaosengine`

### 3. Verify claimed-done or dropped items live before redoing them
1. Title: Live verify before replay
2. What happened: Items reported as dropped were already live (profile sections, retitles, scheduled posts, a documentation SEO PR). Redoing them would have duplicated work.
3. Root cause: Status memory goes stale; other owners may finish the item.
4. Enforceable rule: Before starting a "missing" or "dropped" task, open the live PR, issue, post, page, or file. If it exists, record it as done with the link and do not redo it.
5. Acceptance: Given a live merged PR or published page that matches the task, the bot files "already done" with the URL and makes no write.
6. Suggested destination: `chaosengine`

### 4. Never probe writes on live source-of-truth files
1. Title: No write probes on live documents
2. What happened: A capability probe that wrote to a live CV file corrupted that file and forced a restore-from-version recovery.
3. Root cause: "Can I write here?" was tested against the production object instead of a copy.
4. Enforceable rule: Probe writes only on a throwaway copy. Live documents are updated only as a new version of known-good content. Confirm the live file identity before uploading.
5. Acceptance: Any write-capability check against a production document is rejected; a copy-based probe is allowed.
6. Suggested destination: `chaosengine`

### 5. Unsigned or 5xx package lists are blocked infrastructure, not a force-upgrade
1. Title: Do not force package upgrades when the index is 5xx or unsigned
2. What happened: A machine update via the package manager failed with HTTP 500 and unsigned repository lists. Forcing it would have installed from unauthenticated indexes. The failure path (HTTP through a local proxy) can disagree with a direct HTTPS fetch of the same host.
3. Root cause: The upgrade was treated like an ordinary shell step instead of an integrity-gated infra operation.
4. Enforceable rule: On apt/index 5xx or "not signed," mark BLOCKED-INFRA, do not pass `--allow-unauthenticated` or otherwise force, and escalate to the human machine-update control. Recheck the live symptom before re-listing the block.
5. Acceptance: A simulated 500/unsigned index produces a blocked report and zero upgrade commands with force flags.
6. Suggested destination: `chaosengine` (already tracked in closed #7720; keep the gate)

### 6. Publish metadata must pass a lint before upload
1. Title: YouTube and social metadata lint
2. What happened: Chapter lists built from a script's beat table can include spans under the platform minimum (chapters must start at 0:00 and each run at least 10 seconds). Titles, tags, and bio facts can drift when drafted by hand.
3. Root cause: No mechanical check before handoff.
4. Enforceable rule: Lint before handoff: first chapter at 0:00, every chapter ≥10s, title length within platform limits, tags under the platform cap, and bio facts equal `<profile.title>`, `<profile.trainee_count>`, and `<profile.founding_year>`. If a deep-link URL is unconfirmed, use the site root and flag the exact page as missing.
5. Acceptance: A sample description with a 6-second chapter or paraphrased trainee wording fails the lint; a merged ≥10s chapter list with locked facts passes.
6. Suggested destination: `chaosengine` (already tracked in closed #7719; keep the gate)

### 7. Locked public facts must be copied, not paraphrased
1. Title: Fact lock for identity and counts
2. What happened: Draft copy paraphrased the trainee claim and older packs still used a retired count. Public pages, CVs, and profiles drifted until each surface was updated separately.
3. Root cause: The fact lived in chat memory, not in a single enforced lexicon consulted at write time.
4. Enforceable rule: When writing public or paste-ready copy, consult the locked lexicon in the private profile. Reject banned title variants, a retired count, and a founding year other than `<profile.founding_year>`. Do not paraphrase `<profile.trainee_count>`.
5. Acceptance: A linter or review bot fails any draft whose title, count, or founding year disagrees with the private profile.
6. Suggested destination: `brain` (facts) and `chaosengine` (gate)

### 8. Status reports need a template and a this-turn verify
1. Title: Fresh status is verified, not remembered
2. What happened: Repeated status pings reused remembered "open" items. A later live check showed a documentation SEO PR had already merged and several page titles already matched the agreed copy. Cached "blocked" items must also be re-checked; some stay blocked, some do not.
3. Root cause: Status was treated as durable memory rather than a live poll.
4. Enforceable rule: A status request is a fresh poll. Every blocked or previously dropped row must be checked live this turn. Unopened claims are labeled unverified. Use a fixed table (in progress, planned including frozen, blocked, done) with a link or attachment path on every row. After the first full report, later pings may send a delta against that file, but the delta still requires live checks for blocked and claimed-done items.
5. Acceptance: Given a merged PR that matches a planned row, the new status file lists it under Done with the URL and not under Planned.
6. Suggested destination: `chaosengine`

### 9. Public issue text needs an independent leak review
1. Title: Adversarial review of anything filed to a public tracker
2. What happened: A first draft of Learning Session issues included product UI names, a local timezone, and duplicate rules already filed elsewhere. An independent review caught them before create.
3. Root cause: Internal runbooks were pasted toward a public repository without a leak pass.
4. Enforceable rule: Before opening or commenting on a public issue, an independent reviewer (not the author) checks for secrets, tokens, private IDs, product-internal names, and exact duplicate tickets. Search open issues first; comment on a duplicate instead of opening another.
5. Acceptance: A draft containing an internal product UI name or a secret-shaped string is rejected; the filed text does not include them.
6. Suggested destination: `chaosengine`

### 10. Coach (quality/identity) acceptance before any live publish
1. Title: Pre-publish acceptance is the gate; post-publish checks are extra
2. What happened: Work that looked schema-valid or "ready to paste" was not done until identity, titles, facts, and quality were accepted. Post-publish review cannot replace that gate.
3. Root cause: Delivery pressure skips the owner who knows the biographical facts.
4. Enforceable rule: No live web, social, or video-metadata publish without a passing pre-check for facts, titles, identity, and quality. Visual web publishes also need mixed-direction text isolation (no raw URLs in RTL, no stray characters), WCAG AA contrast (4.5:1), alignment checks, and independent 390px plus desktop renders.
5. Acceptance: A publish attempt without a recorded passing pre-check is rejected by the harness.
6. Suggested destination: `chaosengine`

### 11. Social and admin surfaces often need numeric or documented URL shapes
1. Title: Record the working admin URL shape
2. What happened: Company admin pages were reachable through a numeric company id and failed through the vanity slug. Scheduling UIs also need proof of the timezone they will fire in, not only a clock time.
3. Root cause: Platform URLs and schedulers are not interchangeable with public vanity URLs or naive local-time stamps.
4. Enforceable rule: Persist the working admin URL shape (for example a numeric company id) in project memory. For scheduled publishes, store timezone-named times and a proof of the scheduler's zone.
5. Acceptance: A runbook that uses only a vanity slug when the numeric admin URL is the working one is flagged. A schedule without a timezone label fails the metadata lint.
6. Suggested destination: `chaosengine` and `mempalace` (already tracked in closed #7721)

### 12. Re-download live documents after someone else may have edited them
1. Title: Do not trust last week's version number
2. What happened: A delivered CV later showed a newer modification time on the PDF than on the DOCX. The content still matched, but only because it was downloaded and read again.
3. Root cause: Shared drives are mutable; "we uploaded vN" is not a live fact.
4. Enforceable rule: When citing a shared document as current, fetch it this turn and check the locked facts. Record the modification time you saw.
5. Acceptance: A status row that cites a shared file includes a this-turn content check or is marked unverified.
6. Suggested destination: `chaosengine`

### 13. One writer per file; one task of a kind at a time
1. Title: No conflicting editors
2. What happened: Parallel workers on the same live document or the same kind of task created overwrite risk (the live-file probe was the severe case).
3. Root cause: Parallelism without file ownership.
4. Enforceable rule: Only one worker edits a given file at a time. Parallelize different kinds of tasks, not two edits of the same artifact.
5. Acceptance: A second writer assigned to a file that already has an active writer is rejected or queued.
6. Suggested destination: `chaosengine`

### 14. Unconfirmed deep links stay at the site root
1. Title: Do not invent a docs URL
2. What happened: A video description needed a specific guide page whose exact URL was not confirmed. Shipping a guessed path would have created a broken citation.
3. Root cause: Completeness pressure beat verification.
4. Enforceable rule: If the exact page URL is unconfirmed, link the documented site root and flag the missing path. Do not guess slug-shaped URLs.
5. Acceptance: A draft that contains an unverified deep link fails metadata lint; a root link plus a "exact URL TBD" note passes.
6. Suggested destination: `chaosengine`

## Suggested ChaosEngine enhancements still worth merging (even if tickets closed)
- E1. Stop-scope guard (lesson 1)
- E2. Post-cancel live-state recheck (lesson 2)
- E3. Claimed-done / dropped-item live verify (lesson 3)
- E4. Publish-metadata lint (lesson 6, 7, 14)
- E5. BLOCKED-INFRA for unsigned or 5xx package indexes (lesson 5)
- E6. Fresh-status template with this-turn verification and delta mode (lesson 8)
- E7. Public-tracker leak review before issue create (lesson 9)
- E8. foundingDate / fact lexicon plus scheduler timezone proof and admin URL shape (lessons 7, 11)

These belong in the single ChaosEngine mega PR described in the team protocol, not as one PR per ticket.
