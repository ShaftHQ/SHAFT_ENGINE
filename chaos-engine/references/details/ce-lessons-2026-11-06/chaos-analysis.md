# Phase 3 analysis — learned lessons → ChaosEngine mega PR (Chaos, ID #1)

- **Written:** 2026-11-06 12:35 Africa/Cairo; updated 12:45 Africa/Cairo to add coach.md
- **Spec:** `/workspace/specs/TEAM-PROCEED-20261106.md` (Chaos #1) and `/workspace/specs/TEAM-PROTOCOL-20261106.md` (Phase 3, §4)
- **Lesson files read:** carl.md (17 lessons), chaos.md (19), cleo.md (14), coach.md (20, written 12:21 Cairo, a first-class source), cortex.md (12), hrsd.md (20). That is 102 lessons in total.
- **CE source checked:** read-only `git grep` of `origin/main` in `/workspace/mq-fix`. origin/main is at `5c0ce78f36` (2026-11-04 11:08 Cairo), which matches the live `gh api commits/main`. No git writes.
- **retrieve:** skipped (read-only verification via `git grep` of live origin/main plus `gh issue view`. No checkout entry/doctor was run because nothing was edited this turn. The PR worker must run entry and retrieve.)

## 1. Per-source summary and classification

Key: **CE** = upcoming CE mega PR · **CE-landed** = already in CE, no work needed · **brain / mempalace / graphify** = memory only · **OUT** = other ID or not CE

### Chaos (L1–L19)
| Lesson | Class | Note |
|---|---|---|
| L1 watcher exits and wakes on any failure/timeout | CE | No shared watcher template in CE (`ci-status-economy.md` covers polling cost only) |
| L2 hourly watchdog; lossy wakes | CE + brain | Watchdog text exists (11 files), but there is no rule that tasks longer than 30 minutes register one |
| L3 PR Gate rerun reuses payload → empty commit | CE + brain | `ci-status-economy` and `tip-churn-preflight` mention reruns, but no "empty commit, not rerun" rule and no PR-body receipt lint |
| L4 BLOCKED with green checks → unresolved threads | CE | Thread wording appears in 2 files, but there is no merge-ready helper that lists threads by author |
| L5 merge with `--match-head-commit` and confirm the queue; stuck-head detector | CE (generic watcher/detector only) | `match-head-commit` appears 0 times in CE. Dependabot grouping and SHAFT auto-merge config → OUT (ID #28) |
| L6 full suite: background, one copy at a time | CE | No suite lock found |
| L7 pins latest+LTS, fallback plus ticket | CE-landed (#7749, #7752) | Keep the rule, no new work |
| L8 cleanup at close | CE-landed (#7756 `delivery_close_blockers`) + CE gap | Gap: register artifacts at creation, and refuse to delete dirty/unpushed worktrees (merges Carl L11) |
| L9 headless, no Allure auto-open | CE (small) | Allure auto-open/serve wording exists in 3 files. Add an explicit bot test-run default and a leftover check |
| L10 mega PRs for related tickets | CE | Fold into §4 delegation reference |
| L11 PDCA after two failures | CE | PDCA text exists (23 hits), but no push guard |
| L12 independent review when external CLI fails | CE | Record the fallback method and both verdicts at the head SHA. The SHA-bound part landed (`adversarial-pass-sha.md`) |
| L13 real IDE e2e for plugin work | CE (small reference) | Evidence pattern landed in #7757. Add a one-line rule for plugin deliveries |
| L14 no personal data in skills | CE-landed (#7764, #7765) + mempalace | `check_abstraction` exists. Applicant profile stays in the private store |
| L15 delegation cycle | CE — **#7767** | `delegation protocol` appears 0 times in CE |
| L16 external-contribution trackers | CE (rule) + brain | Implementing #7758 and #4307 is OUT |
| L17 delete codes, no sudo, no force-push, no tokens | CE-landed (delete-check) + CE gap | Add a refusal for relayed codes and a pre-push `--force` check, if missing |
| L18 self-provisioned toolchains | CE — **#7766** | `hosts.py` emits the "no Python 3.11+" block. The installer must provision or guide Python before the lifecycle prompt (Windows `install.ps1` path) |
| L19 CE version never future-dated | CE | No future-date lint found (0 hits). Duplicates Cortex L9 |

### Cortex (L1–L12)
| Lesson | Class | Note |
|---|---|---|
| L1, L2, L3 fresh poll, board schema, unverified claims | CE-landed (#7692 → `status-board.md`, `evaluate_status_board`) | Extend for numbered IDs and "proceed with N" → #7767 |
| L4, L8 run-state, stop scope, fan-out | CE-landed (#7693 → `run-state.md`, `evaluate_run_state`) | Check before every dispatch → #7767 |
| L5, L12 instruction files; lessons → one CE PR | CE — #7767 | |
| L6 token restoration ≠ unfreeze | CE — #7767 | Also Carl L10 |
| L7 media human gate | CE-landed (#7694 → `owner-media-gate.md`) | See Carl L2 gap |
| L9 no future-dated releases | CE gap (same as Chaos L19) | |
| L10 verify blocked/dropped live | CE-landed (#7718 `evaluate_claimed_done_verify`) | |
| L11 deletes need the human's own yes | CE-landed + gap (Chaos L17) | |

### Carl (L1–L17)
| Lesson | Class | Note |
|---|---|---|
| L1, L3, L4 voice clone, dialect, ASR alias | CE-landed (#7708, #7711, #7713 → `voice-clone.md`, `target-dialect.md`, `alias-audit-fail-closed.md`) | |
| L2 garbled start | CE-landed (`clean-start.md`) + gap | Add an explicit human listen step at start AND end, not metrics only |
| L5 script delta re-ack | CE-landed (`script-delta-sha.md`) | |
| L6 long foreground media jobs | CE-landed (`media-durable-job.md`, #7715) | |
| L7 broadcast stop | CE-landed (`stop-scope-guard.md`, #7716/#7718) | |
| L8 gate untrusted until it fails a known defect | CE | Add a "seeded-defect proof" requirement to the gate-authoring reference |
| L9 fix-loop cap | CE-landed (#6685 round cap) | Merge with Chaos L11 PDCA guard |
| L10 token restore | CE — #7767 | |
| L11 deletes of unique work | CE gap | Merged with Chaos L8 |
| L12 independent quality/identity check before publish | CE-landed (`coach-pre-publish.md`, `avatar-publish.md`) + brain | |
| L13 identity facts | brain / mempalace / graphify | `founded-year.md` exists in CE. Canonical facts go to memory, not the PR |
| L14 use archive recordings | brain | |
| L15 motion graphics = animation | CE-landed (design-skills motion refs) | |
| L16 shared instruction files | CE — #7767 | |
| L17 designer generates, an independent worker reviews | CE (merge with Chaos L12) | |

### Cleo (1–14, E1–E8)
| Lesson | Class | Note |
|---|---|---|
| 1 stop-all ≠ local stop (E1) | CE-landed (#7718) | |
| 2 cancel after a live write (E2) | CE gap | No post-cancel live recheck found (0 hits for post-cancel). Add it to `stop-scope-guard` |
| 3 verify claimed-done (E3) | CE-landed | |
| 4 never probe writes on live source-of-truth files | CE | Add to `live-change-diff.md` |
| 5 unsigned/5xx apt = BLOCKED-INFRA (E5) | CE-landed (`blocked-infra-apt.md`, #7720) | Infrastructure is still broken, but the gate exists |
| 6, 7, 14 publish-metadata lint, locked facts, root deep links (E4) | CE-landed (`publish-metadata.md`, `founded-year.md`) + brain | Verify that unconfirmed deep links fall back to the site root, and add it if absent |
| 8 status template + this-turn verify (E6) | CE-landed (`status-template.md`, `status-fresh.md`) | Delta mode → #7767 |
| 9 leak review before public issue text (E7) | CE gap | Only coach-pre-publish mentions public issue text. Add a leak-review check before `gh issue create` |
| 10 Coach acceptance before publish | CE-landed | |
| 11 numeric/admin URL shapes (E8) | CE-landed (`platform-scheduler.md`, #7721) + mempalace | |
| 12 re-download live docs before editing | CE | Add to `live-change-diff.md` |
| 13 one writer per file | CE (WAYS-OF-WORKING §3) | Fold the claim/lock into #7767 if not enforced |

### Coach (L1–L20)
Checked live against `chaos-engine/skills/job-apply`, `chaos-engine/skills/job-search`, the rules and the references on origin/main.
| Lesson | Class | Note |
|---|---|---|
| L1 fact-gate (Coach accept) before live publish | CE-landed (`coach-pre-publish.md`, `rules/coach_pre_publish.py`) + gap | Gap: the delivery close must reject a live publish with no recorded accept, and the accept record must name the facts checked |
| L2 canonical identity strings; cover letter has no founder/CEO title | CE-landed (`coach_pre_publish.py` banned titles, `founded-year.md`) + gap; strings → brain | Gap: a cover-letter lint for founder/CEO wording and client names (merged with L14) |
| L3 bidi, contrast, 390px plus desktop renders | CE-landed (design-skills `color-contrast`, `captions-subtitles`, `web-design`) + gap | Gap: the publish package must carry both renders and a contrast note, or the close fails |
| L4 motion graphics = real animation; no new recordings | CE-landed (`motion-graphics-gate.md`, 11 files) + brain | No new recordings is a brain rule (also TEAM-PROTOCOL §5) |
| L5 verify live before redo; search Sent before a resend | CE-landed (#7718 claimed-done verify) + gap | Gap: a Sent-folder search before any email resend (`job-apply/gate.md` mentions Sent, but not as a pre-resend check) |
| L6 done needs a link or file | CE-landed (#7692 board schema; Cleo 8) | Fold an explicit "no link = not Done" check into #7767 |
| L7 radio silence and router | CE — #7767 | Merges with Chaos L15, Cortex L5 and Carl L16 |
| L8 reflection travels as full markdown | CE-landed (#7765 `references/reflection-checkpoints.md`, `job-search/references/reflection.md`) | Apply to this mega PR: commit the six lesson files and this analysis, abstracted |
| L9 short links are feed posts; save verbatim post text | CE-landed (#7764 job-apply `SKILL.md`/`lessons.md`, short link + verbatim) | No gap found |
| L10 job-page work mode beats the listing card; unverified | CE-landed (#7765 job-search `SKILL.md`/`gate.md`, `unverified`, remote) | No gap found |
| L11 permalinks from Activity; End/Page Down; one worker | CE-landed (#7765 job-search, End key in browser.md) | No gap found |
| L12 "share your profile" is a write | CE-landed (#7765 `approval-gate.md`) | No gap found |
| L13 standing order outranks ask-first; distinct contacts = separate applications | CE gap (job-apply) + brain | "standing" appears only for identity in job-apply; "distinct" has 0 hits. The order's scope stays private in brain |
| L14 never invent qualifications; no client names in emails | CE-landed in part (job-apply gap check, `invent`) + gap | Gap: the lint (shared with L2) |
| L15 two independent reviewers, send, Sent read-back, single signature | CE gap (job-apply) + brain | 0 hits for signature or two reviewers in job-apply. The ranked lead order goes to brain |
| L16 public-by-link is a browser share-dialog step | CE gap | 0 hits |
| L17 ATS verification error: stop after 2, use the next channel | CE gap (job-apply) | No fallback rule. Ties to the PDCA guard (Chaos L11) |
| L18 tracker/ledger hygiene; Applied only after read-back | CE-landed in part (job-search `tracker.md`) + gap; sheet location → brain | Gap: ledger-is-cache and read-back after every write |
| L19 composer identity; retype after hashtag helper; search Studio for duplicates | CE-landed in part (`platform-scheduler.md` identity) + gap | Gap: duplicate search before upload, and retype after hashtag autocomplete |
| L20 no secrets in chat, remotes, status or lessons | CE gap (merges with Chaos L17 guards) | Add a secrets scan of status and lessons files and a refusal to add remotes to private working copies |

### HRSD (1–20)
| Lesson | Class | Note |
|---|---|---|
| 1–5, 7 docs-hygiene (source-of-truth files, status vocabulary, hours math, journey maps, partial journeys, delete-guard upstream) | **CE gap: skill `docs-hygiene` (#7726) is missing on main** | #7726 is closed as completed, but there is no `skills/docs-hygiene/` and no matches on main. Implement it in the PR |
| 4 journey consistency | + graphify | |
| 6 SHA-bound adversarial review | CE-landed (#7709, `adversarial-pass-sha.md`) | |
| 8, 9 secrets in gitignored props; never touch prod | CE-landed (`shaft-browserstack-native`) + brain | |
| 10 tap helper at element centre | brain | |
| 11, 13, 14, 15 retest hygiene, drag fallback, map triage, picker probe | CE-landed (`shaft-mobile-retest-hygiene`) | |
| 12, 18 evidence hygiene; plan workbook updates | brain | |
| 16 split mobile toolchain | CE-landed (`shaft-mobile-env`) | |
| 17 ABI/platform match | CE-landed (`shaft-ios-ipa-acceptance`, `shaft-android-apk-select`, `shaft-browserstack-native`) | |
| 19 verify dropped artifact | CE-landed (#7692/#7718) | |
| 20 abstracted upstream issues, Chaos merges | CE — #7767 | |

## 2. Already landed (verified this turn)

All checked with `gh issue view` (CLOSED/COMPLETED, mostly 2026-11-02 17:18–17:19 Cairo, landing via PR #7731) and present on CE main:
- #7691, #7692, #7693, #7694: `rules/lessons_2026_11_02.py` and `references/details/ce-lessons-2026-11-02/{status-board,run-state,owner-media-gate}.md`
- #7705 epic, #7707 (`avatar-publish.md`), #7708 (`voice-clone.md`), #7709 (`adversarial-pass-sha.md`), #7710 (`clean-start.md`), #7711 (`target-dialect.md`), #7712 (`founded-year.md`), #7713 (`alias-audit-fail-closed.md`), #7714 (`script-delta-sha.md`), #7715 (`media-durable-job.md`), #7716 (`freeze-hygiene.md`)
- #7717 epic, #7718 (`stop-scope-guard.md`), #7719 (`publish-metadata.md`), #7720 (`blocked-infra-apt.md`), #7721 (`platform-scheduler.md`)
- #7722–#7725 and #7727 skills present: `shaft-mobile-env`, `shaft-browserstack-native`, `shaft-ios-ipa-acceptance`, `shaft-android-apk-select`, `shaft-mobile-retest-hygiene`
- #6685 (round cap) and #6526 (atomic .part): closed earlier
- #7749 and #7752 pins/LTS, #7756 cleanup-at-close, #7757 IDE e2e evidence, #7764 and #7765 abstraction skills
- #7706 is SHAFT engine iOS observability, closed. It is not a CE item.

- Coach-cited #7764/#7765 checked on main: L8 (full-markdown reflection), L9 (short link + verbatim text), L10 (job-page work mode / unverified), L11 (Activity permalinks, End key) and L12 (share-profile approval gate) have all landed. The gaps are only L13, L15, L16, L17 and part of L18 (checklist item 21).

**Closed but not landed (gap, implement without refiling):** #7726 `docs-hygiene` skill. The #7728 epic claims 6/6, but only 5 skills exist on main.

## 3. Checklist for the upcoming CE mega PR

PR body closing keywords: `Closes #7766`, `Closes #7767`. Reference `#7726` (closed, gap fixed), `#7718` and `#7692` (extended).

1. **#7767 delegation protocol** (new `chaos-engine/references/delegation-cycle.md`, plus a rule in `rules/` and a bot-entry pointer). Cover:
   - the shared instruction file
   - a fresh poll on every status request
   - the status template and numbered IDs
   - "proceed with N" applying only to that ID's owner
   - radio silence
   - token restore ≠ resume
   - the lessons → one CE PR → install → cleanup → Done cycle
   - checking run-state before every dispatch
   - mega-PR grouping and the one-writer-per-file claim
   
   Extend `evaluate_status_board` and `evaluate_run_state` rather than duplicating them. Sources: Chaos L10/L15, Cortex L5/L6/L12, Carl L10/L16, Cleo 8/13, HRSD 20.
2. **#7766 Python 3.11+ on fresh install:**
   - `install.ps1`/`install.py` provision or locate Python 3.11+ before the lifecycle prompt.
   - Improve the `hosts.py` `HOOK_LAUNCH_PYTHON_BLOCK` message to say what is missing, why, and the one fix.
   - Add a test for a Windows install with no Python. Same pattern as JDK/Maven self-provisioning (#7676). (Chaos L18)
3. **Shared watcher template** (`scripts/` or `tool.py watch`):
   - bounded deadline
   - exit non-zero and wake on any failed, cancelled or timed-out check, on BLOCKED/DIRTY, on dequeue, and on its own deadline
   - logs queue position after enqueue
   - stuck-queue-head detector (no merge_group gate run after N minutes)
   - preflight warns on unbounded loops
   
   Generic only. Dependabot.yml grouping and SHAFT auto-merge config stay in ID #28. (Chaos L1/L5)
4. **Watchdog rule:** tasks longer than 30 minutes register an hourly watchdog plus a progress file, and remove it at close. (Chaos L2)
5. **PR Gate rerun reference + PR-body lint:** "push an empty commit, don't rerun" after a body fix, and a receipt-format lint (`retrieve: skipped(<reason>)`) before `gh pr create`. (Chaos L3)
6. **Merge-ready helper:** list unresolved review threads by author (including code-quality bots) before reporting "waiting CI", and enforce `--match-head-commit` in documented merge commands. (Chaos L4/L5)
7. **Suite lock:** the full-suite runner refuses a second concurrent run and warns when run in the foreground. (Chaos L6)
8. **PDCA push guard:** after a second consecutive failure of the same stage, require an RCA entry in the progress file before the next push. Ties into the #6685 round cap. (Chaos L11, Carl L9)
9. **CE version future-date lint:** reject a `0.4.YYYYMMDD.n` with a date later than today. (Chaos L19, Cortex L9)
10. **Independent review fallback:** two verdicts from independent methods or workers at the head SHA, with the fallback reason recorded when the external CLI fails, and designer-generates/independent-reviews. Extends `adversarial-pass-sha.md`. (Chaos L12, Carl L17)
11. **Plugin/IDE e2e rule:** real IDE under a headless display, with an evidence folder. (Chaos L13)
12. **Bot test defaults:** headless, no Allure auto-open, no `allure serve`; leftover browsers and report servers are close blockers. (Chaos L9)
13. **Cleanup safety:** register artifacts at creation, and `delivery_close_blockers` and cleanup refuse dirty or unpushed unique worktrees. (Chaos L8, Carl L11)
14. **Delete/force/token guards:** refuse agent-relayed delete codes, check pre-push for `--force` without approval, and secret-scan commits and output. (Chaos L17, Cortex L11)
15. **Media human gate step:** human listen/look at the start AND end, not metrics only. Extends `clean-start.md`/`owner-media-gate.md`. (Carl L2)
16. **Gate proof:** a new gate must fail a seeded known defect before it is trusted. (Carl L8)
17. **Cleo gaps:**
   - post-cancel live recheck → `stop-scope-guard`
   - no write probes on live source-of-truth files, and re-download before editing → `live-change-diff.md`
   - leak review before public issue text
   - unconfirmed deep links fall back to the site root, if absent
18. **`docs-hygiene` skill (#7726 gap):** `chaos-engine/skills/docs-hygiene/SKILL.md` covering:
   - three source-of-truth files
   - status vocabulary (Done / In progress / Not started)
   - done hours = planned hours with live formulas
   - journey-map agreement
   - no partial journey merged as the full one
   - delete-guard upstream-branch prerequisite
   - SHA-bound review link
   
   (HRSD 1–5, 7)
19. **External-contribution tracker rule:**
   - close only after upstream merge, release and SHAFT adoption
   - a stale/CHANGES_REQUESTED check
   - fork label ask
   - English-only upstream text
   
   (Chaos L16)
20. **Coach publish/identity gaps:**
    - the delivery close rejects a live publish with no recorded Coach accept that names the facts checked (L1)
    - a cover-letter/email lint for founder/CEO titles, client names and unsourced tool claims (L2/L14)
    - the publish package needs 390px and desktop renders plus a contrast note (L3)
    - Sent-folder search before any resend (L5)
    - an explicit "no link or file = not Done" check in the board schema (L6, folded with #7767)
21. **job-apply skill update on top of #7764 (Coach L13, L15, L16, L17, L18):**
    - a recorded standing order changes which drafts are produced inside `<profile.standing_order_scope>`; it does not authorize a send
    - distinct contacts or channels are separate drafts
    - the remote ask goes in the subject and body
    - two independent PASS reviews, excluding the writer, then the human sends; Sent read-back and a single signature are evidence, not agent authority
    - public-by-link happens through the share dialog and is confirmed this turn
    - after two identical ATS verification errors, stop and use the next channel
    - the ledger is a cache and the tracker is read back after every write
    
    Abstract only. Standing-order scope, signature text and sheet location stay in brain.
22. **Social/video playbook (Coach L19):** search the live Studio list for a duplicate before upload, and retype the description after a hashtag helper. Extends `platform-scheduler.md`.
23. **Secrets hygiene (Coach L20):** secrets-scan status and lessons files, and refuse to add a remote to private working copies. Merge into item 14.
24. **Lessons ledger:** link this analysis and the six lesson files from a `ce-lessons-2026-11-06` details folder, bump the CE version (`0.4.20261106.n`, not future-dated), and run the harness-index regen, abstraction check and entry/retrieve receipt.

**Count:** 24 checklist items covering 2 open issues (#7766, #7767), 1 closed-not-landed gap (#7726) and 21 lesson-derived gates/references (17 from the first five files, plus 4 Coach groups: items 20–23).

## 4. Memory only (not in PR)
- **brain:** identity/public facts (Carl L13, Coach L2 strings), the owner of the fact-gate (Coach L1), the no-new-recording rule (Coach L4), standing-order scope, signature and lead order (Coach L13/L15), the tracker sheet location (Coach L18), archive recordings over new ones (Carl L14), tap helper (HRSD 10), evidence hygiene and workbook updates (HRSD 12/18), lossy wakes (Chaos L2), PR Gate payload fact (Chaos L3), contribution facts (Chaos L16)
- **mempalace:** private applicant profile (Chaos L14), admin URL shapes (Cleo 11), canonical identity facts
- **graphify:** journey-map consistency (HRSD 4), plugin e2e evidence location (Chaos L13)

## 5. Out of scope for the CE mega PR
| Item | Reason |
|---|---|
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7758 allure-java 3.1.1 | SHAFT mega PR, ID #13 |
| https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7753 Flutter CI Java 25 | SHAFT mega PR, ID #14 |
| https://github.com/SeleniumHQ/selenium/pull/18254 / tracker https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4307 | ID #9, an upstream-only change |
| Dependabot PRs #7759–#7763, dependabot.yml grouping, SHAFT auto-merge stuck fix | ID #28 Dependabot mega PR (only the generic CE watcher/detector stays in the CE PR) |
| Coverage #7077–#7081 | SHAFT product work, not on the proceed list |
| #7423 json-schema-validator 3.x | SHAFT product issue |
| #7680/#7681 release + HRSD drag fallback removal | Needs a SHAFT release, and there is no release unless Mohab asks |
| #7706 iOS observability | SHAFT engine, closed |
| apt proxy 5xx (Cleo 5) | Infrastructure. The gate already exists, so this is not a CE code change |

## 6. Blocked
- https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7751 managed Python 3.15: blocked because onnxruntime has no cp315 wheels (verified in the Phase 1 status). Do not implement.

## 7. Privacy
This file holds only issue/PR numbers, file paths, public repo names and abstract rules. It has no emails, phone numbers, tokens, passwords, delete codes, employer names or applicant data.
