# Lessons — Carl — 2026-11-06

Scope: work since about 2026-10-31 (voice/TTS, explainer, gates, freeze/resume, disk cleanup) plus still-relevant earlier design/media patterns. Abstracted: no secrets, tokens, delete codes, trainee data, client-confidential details, or private biometric payloads.

Related write-ups already on disk: `/workspace/dm-v5/retro/RETRO-20261101.md`, `/workspace/dm-v5/retro/RETRO-20261102.md`.

---

## L1 — Speaker-only clone gates pass unintelligible audio

1. Title: Voice-clone acceptance must require speaker match AND intelligibility
2. What happened: A fine-tuned voice scored as the target speaker while speech recognition heard the wrong words, or (on an earlier path) nearly unintelligible output. The sample was treated as a candidate because identity metrics looked good.
3. Root cause: Speaker embeddings measure timbre, not content. A wrong tokenizer (English-only vocabulary on non-English text) trained the model on unknown tokens. Checkpoint pick used speaker score alone.
4. Enforceable rule: Gate = speaker (two embeddings vs held-out real audio, both ≥ threshold) AND ASR intelligibility (English WER ≤ 15%, Arabic CER ≤ 15%). Refuse training/inference if tokenizer unknown-token rate > 5%. Pick checkpoints on the joint score. Pass the correct language id into multilingual models. Proper nouns in the script must appear correctly in the ASR transcript.
5. Acceptance: Known-bad fixtures (wrong-language tokenizer, speaker-ok/ASR-fail sample) must FAIL. A pass file must clear both speaker thresholds and the language-appropriate ASR cap, and the transcript must contain required proper nouns verbatim.
6. Destination: `chaosengine`

## L2 — Metrics can miss a garbled start

1. Title: Clean-start gate for spoken media
2. What happened: A sample that passed speaker and character-error metrics was rejected on listening: it began with gibberish before the intended words.
3. Root cause: The gate scored the whole clip and never inspected the onset. No human or automatic check of the first few hundred milliseconds against the script’s first word.
4. Enforceable rule: Before sharing spoken media, decode the file’s own audio and require that the leading window is actual speech whose first word matches the script. Fail silence, noise, or non-speech voiced junk before onset (suggested: first ~300 ms, first-word ASR match). A person (or the producing bot) must listen to the start and the end; metrics alone are not enough.
5. Acceptance: A fixture with a junk prefix plus a clean tail must FAIL even if whole-clip CER/WER would pass. A clean-onset fixture with the same tail must PASS the clean-start check.
6. Destination: `chaosengine`

## L3 — Intelligible Arabic can still be the wrong dialect

1. Title: Target-dialect acceptance for spoken media
2. What happened: An Arabic sample cleared character-error limits but was too formal (modern standard) rather than the requested regional vernacular.
3. Root cause: The gate measured character overlap with the script, not register/dialect. The script and the synthesis path did not name or constrain dialect.
4. Enforceable rule: Acceptance names the target dialect or register (for example Egyptian vernacular vs formal standard). A clear register mismatch is a FAIL even when CER/WER pass. Scripts meant for a vernacular must be written and reviewed in that vernacular before synthesis.
5. Acceptance: A formal-standard reading of a vernacular script fails dialect check. A vernacular reading of the same meaning passes if intelligibility and speaker gates also pass.
6. Destination: `chaosengine`

## L4 — Spelling tricks can game an ASR gate

1. Title: ASR alias audit must be fail-closed
2. What happened: To fix proper-noun pronunciation, synthesis text was respelt. That can make recognition look better without the real words being spoken, or hide errors by changing what is compared.
3. Root cause: Unbounded alias lists turn the reference into whatever the system already said.
4. Enforceable rule: Aliases may only map a Latin technical term to its documented transliteration (or the reverse). The ASR transcript of a shipped sample must contain the real words the audience should hear (names, product names). An independent audit compares transcript to the canonical spelling, not to the respelling used for synthesis.
5. Acceptance: A sample whose transcript says a look-alike of a required name fails even if a private alias table would have scored it as a match.
6. Destination: `chaosengine`

## L5 — An accepted script that later changes needs a fresh ack

1. Title: Script-delta re-acknowledgement
2. What happened: After narration copy was accepted, lines were edited to help synthesis or timing. The original accepter was not asked to re-acknowledge the new words.
3. Root cause: Acceptance was treated as sticky on a moving document.
4. Enforceable rule: Any edit to an accepted narration script, including pronunciation respellings that change visible/spoken words, requires a re-ack from the original accepter before more media is generated or shared.
5. Acceptance: A hook or checklist fails a media job when `script` differs from the accepted hash and no re-ack receipt is present.
6. Destination: `chaosengine`

## L6 — Long foreground media jobs die mid-session

1. Title: Media work must be short, committed, and resumable
2. What happened: Voice and explainer jobs running in a single long session were cut around the executor time cap, losing progress or forcing expensive reruns.
3. Root cause: Foreground steps longer than the session budget, with no commit/lease between stages.
4. Enforceable rule: Media pipelines run as detached durable jobs. Each step stays under about 30 minutes, commits its outputs, and the next step resumes from git or a lease, not from chat memory.
5. Acceptance: Killing the parent session after a completed step leaves enough on disk/git for a new worker to continue without repeating that step.
6. Destination: `chaosengine`

## L7 — A broadcast stop cancels teammates

1. Title: Freeze hygiene — commit first, stop only your own workers
2. What happened: A global stop intended to halt one bot’s workers also cancelled peer agents who were in the middle of their own turns.
3. Root cause: A host-wide “stop all” reaches children and, in this environment, peer deliveries.
4. Enforceable rule: Commit workspace state before dispatching a worker. On freeze, stop only that bot’s own workers. Do not issue a host-wide stop-all when peers must keep running. Record frozen/active/halted per bot and check it before every dispatch.
5. Acceptance: Freezing bot A leaves bot B’s in-flight work running. A frozen bot does not start a new worker until an explicit proceed names its task id.
6. Destination: `chaosengine`

## L8 — A gate is untrusted until it fails a known defect

1. Title: Proven-red before proven-green
2. What happened: Gates were adjusted until a favorite sample passed, without first showing that the same gate rejects the known-bad sample that motivated the rule.
3. Root cause: Thresholds and aliases were tuned on the candidate under review.
4. Enforceable rule: Before trusting a new or changed media gate, run it on a stored defect fixture and require FAIL. Only then run it on the new candidate.
5. Acceptance: CI or a skill self-check includes a red fixture per gate (junk prefix, wrong dialect, speaker-ok/ASR-fail, alias-gamed name) and fails the suite if any red fixture passes.
6. Destination: `chaosengine`

## L9 — Fix loops need a cap and a frozen method

1. Title: Two fix rounds, then stop and adjudicate
2. What happened: Caption-sync and voice-pronunciation loops burned many rounds because each review invented a slightly different bar.
3. Root cause: The measurement method and the pass bar were not frozen before the loop started.
4. Enforceable rule: Freeze the acceptance standard and how it is measured before review. Cap automated fix rounds at two. Then stop and let the designated accepter adjudicate rather than starting a third silent retry.
5. Acceptance: A third automatic regeneration without an adjudication record is a process FAIL, even if the file later looks better.
6. Destination: `chaosengine` (already tracked historically as the verified-findings / round-cap work)

## L10 — Token restoration is not a resume order

1. Title: Reply, then pause, unless the shared file names the next phase
2. What happened: After tokens ran out, a later restore or an older “go” sitting in chat could be read as permission to restart frozen media work.
3. Root cause: Resume was coupled to “I can talk again” instead of to an explicit task id.
4. Enforceable rule: When tokens return, reply to the current protocol (status, lessons) and do not unfreeze planned work. Resume only when the router says proceed with a numbered board id, or when the shared instruction file names the next phase (lessons, install, cleanup).
5. Acceptance: A bot that writes Phase 1 status does not spawn a media worker in the same cycle unless the shared file explicitly schedules that work.
6. Destination: `chaosengine` (delegation / run-state)

## L11 — Deletes of unique local work are irreversible

1. Title: Cleanup may not drop unpushed unique files
2. What happened: Disk pressure led to a request to remove large local folders. Many design and video worktrees had never been pushed (hundreds of local-only commits, dirty trees). Deleting them would have dropped unique history. After an explicit human OK, finished video worktrees, a sample motion-graphics tree, and a parked cartoon-pilot folder were removed.
3. Root cause: Local worktrees were treated as disposable caches even though they held unpushed commits and review dirt.
4. Enforceable rule: Before delete: `git status` and unpushed-commit check. Unique unpushed work is forbidden to delete (backup-to-remote first). Rebuildable caches and virtualenvs may go if sources and trained models remain. Deletes on the human’s personal machines still need that person’s delete code. Shared-box deletes of someone else’s trees wait for that owner’s keep/drop list.
5. Acceptance: A cleanup report lists each removed path, whether it had unpushed commits, and the approval that allowed removal. A dry-run refuses any dirty or unpushed unique tree.
6. Destination: `chaosengine`

## L12 — Live publish still needs an independent quality and identity check

1. Title: Accepter sign-off before anything goes live
2. What happened: Web, posts, videos, and metadata were at risk of shipping on schema-or-owner-say-so alone. Mixed-direction text, contrast, and caption timing have failed reviews after looking “done.”
3. Root cause: Valid structure was treated as done. The person who built the item also graded it.
4. Enforceable rule: An independent accepter checks facts, titles, identity, and quality before live publish. Post-publish checks are extra, never a substitute. For Arabic/English UI: bidirectional isolation, no raw URLs in RTL, WCAG AA contrast, and both a narrow-mobile and a desktop render reviewed by someone who did not make the change.
5. Acceptance: A publish checklist fails closed without the accepter’s receipt. A page with mixed-direction errors or contrast below 4.5:1 cannot be marked done.
6. Destination: `chaosengine` and `brain`

## L13 — Identity facts must stay consistent everywhere

1. Title: Canonical public identity facts
2. What happened: Public copy drifted on title, trainee count, and founding year.
3. Root cause: Facts lived in many documents without a single enforced source.
4. Enforceable rule: Title is Founder and CEO (not Co-Founder, not title-only Founder). Trainee count is “600+ trainees” (Arabic: أكثر من 600 متدرب), never a lower retired figure. Organization founding year is 2014; a later year on a third-party profile is not copied into owned surfaces.
5. Acceptance: A facts checker fails any draft that uses the banned title, the retired count, or a founding year other than 2014.
6. Destination: `brain` (also `mempalace` / `graphify` so retrieval returns the same facts)

## L14 — Do not ask the subject for new recordings when archives exist

1. Title: Adapt scripts to existing speech, do not request new voice or video capture
2. What happened: Voice work stalled when a clone could not say certain names, and the temptation was to ask for a fresh recording.
3. Root cause: The pipeline assumed it could demand new takes.
4. Enforceable rule: No new voice or video recordings from the subject. Use existing podcasts and published video only. If a line cannot be said from that material, change the script to words the subject has actually said (and re-ack the script).
5. Acceptance: A media plan that includes a “please record this line” step is a process FAIL unless the human independently offers a recording.
6. Destination: `brain`

## L15 — Motion graphics means animation, not a zoomed still

1. Title: Reject slideshow-style “animation”
2. What happened: Still images with shake or zoom were presented as motion graphics and were rejected.
3. Root cause: The word “animation” was used for camera moves on a static frame.
4. Enforceable rule: Motion graphics require real animated graphics (type, diagrams, scenes changing over time). Ken Burns / shake-on-still does not count.
5. Acceptance: A reviewer watching the file can describe graphic elements that change independently of a global pan or zoom. If the only motion is a transform on one still, FAIL.
6. Destination: `chaosengine`

## L16 — Shared instruction files beat chat delegation

1. Title: Router writes one file; bots read and execute it
2. What happened: Status, freeze, resume, and lesson orders were repeated in chat with drift (who is frozen, what “go” meant, which sample to share).
3. Root cause: Detailed instructions lived in messages instead of one versioned file.
4. Enforceable rule: The router writes one shared instruction file (human words plus refinements). Other bots are messaged only to read that file and execute it. An explicit status request triggers a fresh poll; no board is built from cached reports. “Proceed with N” unfreezes only that numbered item’s owner.
5. Acceptance: A future router can run status → numbered board → lessons → one ChaosEngine PR → install → cleanup → a single human-facing “Done” without extra chatter to the human.
6. Destination: `chaosengine` (this is the delegation protocol the Phase 3 mega PR must implement)

## L17 — Designer generates; an independent worker reviews

1. Title: No self-review of media or design
2. What happened: When the same worker built and scored a clip or page, known defects were missed (caption timing, face “digital” look, leading junk).
3. Root cause: Incentive and context blindness.
4. Enforceable rule: Workers implement. A fresh independent worker does adversarial review against a frozen checklist. The designer may direct, but does not certify their own output. Claim a file set before writing so two workers never edit the same files.
5. Acceptance: A review signed by the same worker id that produced the artifact is invalid.
6. Destination: `chaosengine`

---

## Already filed (do not re-open as new work unless implementation is still missing)

Verified 2026-11-06: these GitHub issues are CLOSED.

| Issue | Title |
| --- | --- |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7705 | Epic: ChaosEngine lessons from voice/TTS media session (2026-11-01..02) |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7708 | voice clone gate (speaker ∧ ASR, tokenizer preflight, joint checkpoint) |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7710 | clean-start leading-audio gate |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7711 | target-dialect acceptance |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7713 | ASR alias audit (fail-closed) |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7714 | script-delta re-ack |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7715 | media pipeline durable-job playbook |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7716 | multi-agent freeze hygiene |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/6685 | verified-findings review gate with a round cap |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/6526 | atomic .part outputs and resume cleanup |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7506 | digital-avatar v1 retro lessons |
| https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7352 | caption/video QC lessons (MERGED) |

If any CLOSED lesson issue was closed without the gate actually landing in ChaosEngine, the Phase 3 mega PR should implement the gap rather than open a duplicate ticket.

## Still needed in the Phase 3 mega PR

- Delegation protocol from `/workspace/specs/TEAM-PROTOCOL-20261106.md` section 4 (file an issue if none exists, then implement in the one mega PR): shared instruction file, fresh status poll, numbered board, proceed-by-id, radio silence, token-restore ≠ resume, one lessons PR, install then cleanup, run-state checked before dispatch.
- Human listen-to-start-and-end step as part of the clean-start / media gate (L2), not metrics-only.
- Cleanup guard against deleting dirty or unpushed unique worktrees (L11).
- Canonical identity facts in brain / mempalace / graphify (L13).
