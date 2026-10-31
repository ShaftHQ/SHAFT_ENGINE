# ChaosEngine 2026.10.31.6 — consolidated retro SPEC

**Date:** 2026-10-31 (Africa/Cairo)  
**Ask:** Mohab t182u — refine, dedupe, spec, implement all-bot retro in harness.  
**Inputs:** `RETRO-CONSOLIDATED.md`, `chaos.md`, `carl.md`, `ce-retro-queue.md`  
**Baseline CE:** 2026.10.31.5 (L48–L59 shipped in issue 7652 / design-skills issue 7651)  
**retrieve:** used (graphify; store stale vs HEAD — advisory)

Personal names and client data stay out of repo text ("the subject" / "the presenter").

---

## Already shipped (DUP — do not re-implement)

| ID | Topic | Where | Reason |
| --- | --- | --- | --- |
| L48 | Wardrobe / per-button | design-skills | issue 7651 |
| L49 | Multi-model sync consensus | design-skills | issue 7651 |
| L50 | Render meta rev/sha | design-skills | issue 7652 fold |
| L51 | Extend-feasibility before slot | design-skills | issue 7651 |
| L52 | Per-cue sync adjudication | design-skills | issue 7652 |
| L53 | Freeze METHOD + fixed sample | design-skills | issue 7652 |
| L54 | Checkpoint generated assets | design-skills | issue 7652 |
| L55 | Signed script header | design-skills | issue 7652 |
| L56 | Blind likeness+voice (basic) | design-skills | issue 7652 — **harden provenance this PR** |
| L57 | Studio pre-upload dup | design-skills | issue 7652 |
| L58 | Inventory yt_id backfill | design-skills | issue 7652 |
| L59 | Retain mp4 until public URL | design-skills | issue 7652 |
| L46 | Render freshness sha256 | design-skills | prior |
| L47 | Avatar frozen-tail | design-skills | prior |
| L43 | Long vs Short dedupe | design-skills | prior |
| L44 | Min-cue / single-model sync | design-skills | prior |
| H2 | upgrade-session transactional + doctor post-verify | install | .4/.5 |
| H-GEMINI | Gemini quota-wait card | references/gemini-quota-wait.md | .5 card |
| askaraoke | Raw ASR karaoke fail | craft_qc askaraoke | L35 — **extend for human-frag** |
| cleanup-at-close.md | Policy doc + session manifest | references + delete_guard | partial — **harden ownership + signed-renders** |
| blocked_infra_reasons | Disk free/used → BLOCKED-INFRA | box_disk_guard | partial — **add swap/mirror** |

---

## Implement this PR (enforceable first)

### H-VERSION — VERSION-bump gate
- **Sources:** chaos.md L2; issue 7651 merged without VERSION bump
- **Problem:** Consumer-facing `chaos-engine/` changes can merge with unchanged VERSION → no broadcast.
- **Rule:** Any diff under `chaos-engine/` that is not exclusively docs-only waiver paths requires `chaos-engine/VERSION` to change **or** an explicit `ce-no-version-bump` label / PR body marker `<!-- chaos-engine:no-version-bump -->` with justification.
- **Enforcement:** `scripts/ci/overlay_pre_push.py` → `ce_version_bump_failures(root, changed)`; unit test.
- **Acceptance:** Changing `chaos-engine/references/bot-entry.md` without VERSION → fail; with VERSION bump → pass; with no-version-bump marker → pass.

### H-FAIL-MISSING — Fail-on-missing-input template
- **Sources:** chaos.md L4; Carl issue 7651 M1–M3 (gates passed on empty input)
- **Problem:** New gates defaulted open when inputs absent.
- **Rule:** Every new observation/receipt gate FAIL when required input missing/empty. Shared helper + test scaffold.
- **Enforcement:** `chaos-engine/gate_fail_closed.py` (`require_file`, `require_observations`, `fail_hits`); docs in `references/gate-fail-closed.md`; scaffold test module.
- **Acceptance:** Helper returns FAIL hits for missing path / empty observations; template test fails open-gate stub.

### H-DOC-CLAIM — Doc-claim vs code in review checklist
- **Sources:** chaos.md L4; Carl designer review
- **Problem:** Docs claimed multi-model / fail-closed behaviour code lacked; adversarial missed it.
- **Rule:** Independent review checklist must verify every Verify bullet maps to a real command/flag/test.
- **Enforcement:** Extend `design-review-video-checklist.md` + new `references/doc-claim-review.md`; bot-entry Definition of Done points at it.
- **Acceptance:** Checklist contains explicit "doc-claim vs code" block; test asserts file contains required phrases.

### H-OWNERSHIP — Worker ownership manifest + cleanup-at-close
- **Sources:** chaos.md L10; Coach cleanup-at-close-spec; Cortex/HRSD H9
- **Problem:** Leftover WTs; delete-check blocks cleanup without ownership record.
- **Rule:** Workers register created dirs/paths in ownership manifest at creation; cleanup-at-close deletes **only** manifest entries (plus tip-on-origin proofs).
- **Enforcement:** Harden `references/cleanup-at-close.md`; add `scripts/ci/worker_ownership.py` (record/list/verify); wire note in bot-entry DoD; tests.
- **Acceptance:** `record` then `verify` passes; cleanup helper refuses path not in manifest.

### H-SKILLSPECTOR — Baseline keyed by semantic id
- **Sources:** chaos.md L7
- **Problem:** Baseline fingerprints are content hashes; skill trims churn every entry.
- **Rule:** CE refresh tool keys suppressions by `semantic_id = rule_id + ":" + file` (optional `:+message_slug`); on refresh, update hash in place when semantic_id matches, keep reason.
- **Enforcement:** `scripts/ci/skillspector/semantic_baseline.py` + tests; doc in skillspector_gate.py header.
- **Acceptance:** Refresh with changed hash / same rule_id+file keeps reason and updates hash; new semantic_id adds entry.

### H-RADIO — Radio-silence send guard
- **Sources:** chaos.md L3; Coach L13; HRSD H1; Cortex
- **Problem:** Bots SendToUser under FULL radio silence.
- **Rule:** When `CHAOS_RADIO_SILENCE=1` (or project flag file `.chaos-engine-state/radio-silence`), bot-entry forbids user-facing status sends; status → orchestrator only.
- **Enforcement:** `references/radio-silence.md` + bot-entry section; `chaos-engine/radio_silence.py` (`user_send_blocked()`); entry prints guard; tests.
- **Acceptance:** Env set → blocked True; unset → False; bot-entry links the card.

### H-INFRA — Infra preflight → BLOCKED-INFRA
- **Sources:** chaos.md L9; HRSD L11–L12
- **Problem:** Apt mirror 500 / lost swap burned session budget.
- **Rule:** Preflight reports BLOCKED-INFRA for disk tiers (existing), **swap absent**, and optional **apt mirror probe failure** (best-effort, non-fatal probe).
- **Enforcement:** Extend `box_disk_guard.blocked_infra_reasons` (+ `swap_missing`, optional mirror); bot-entry durability checklist; tests with injected values.
- **Acceptance:** `swap_total_kib=0` → reason `swap-missing`; disk reasons unchanged.

### H-PROGRESS — Workers append progress log every step
- **Sources:** chaos.md L5; Carl §7
- **Problem:** Session interrupts lost "what's next."
- **Rule:** After every stage boundary, append to the task progress log (`checkpoints/<task>-progress.md` or `CHAOS_PROGRESS_LOG`) **and** `tool.py checkpoint`.
- **Enforcement:** `references/progress-log.md` + bot-entry session-cap cross-link; tiny `chaos-engine/progress_log.py` append helper; tests.
- **Acceptance:** append creates file with step line; second append grows file.

### H-PIN — Moving-channel version pin policy
- **Sources:** ce-retro-queue issue 7658; managed Android emulator 37.2.12 vs stable 37.2.13
- **Problem:** Exact pin of a moving upstream channel broke CI when channel advanced.
- **Rule:** For moving vendor channels: pin as **minimum** within major (`>= pinned`) **or** resolve from channel at plan time; add nightly drift check note. Exact equality only for immutable artifacts (commit sha, tarball digest).
- **Enforcement:** `references/moving-channel-pin.md`; cite from bot-entry / ci-status-economy companion; unit test on policy helper `channel_pin_allows(installed, pinned, *, mode)`.
- **Acceptance:** `mode=min` allows 37.2.13 when pinned 37.2.12; rejects 36.x; `mode=exact` requires equality.

### L60 — Likeness+voice real-ref provenance (harden L56)
- **Sources:** Carl rank 1; Coach L3
- **Problem:** L56 accepted AI still / AI speech as refs (circular oracle).
- **Rule:** Each ref must carry provenance `{kind: real_photo|yt_frame|real_audio, sha256, source_url|source_path}`. `kind` in `{ai_still, ai_speech, prior_generation}` → FAIL. Missing provenance → FAIL.
- **Enforcement:** Extend `_likenessvoice_axis_hits` / observations schema; update L56 card + L39 checklist; tests (AI-ref-only → FAIL; real provenance → PASS path).
- **Acceptance:** AI kind fails; missing kind fails; real_* with sha passes axis provenance check.

### L61 — Dup check vs live inventory before rebuild/schedule
- **Sources:** Carl rank 4; Coach L4
- **Problem:** Rebuild/schedule asked before title+window dup vs live inventory.
- **Rule:** Before rebuild or schedule ask: run inventory dup gate (title normalized + source window) against live inventory JSON; matches without `allow_replace` → FAIL.
- **Enforcement:** `design_qc.py scheduledup` + lesson `schedule-dup-before-rebuild.md` (L61); L39 checklist bullet; tests.
- **Acceptance:** Duplicate title+window → FAIL; clean inventory → PASS; missing inventory → FAIL.

### L62 — Protected signed-renders store
- **Sources:** Carl rank 3; Coach L5
- **Problem:** Signed mp4s under gitignored `renders/` deleted in cleanup.
- **Rule:** On SIGN, copy finals to `/workspace/signed-renders/<UNIT>/` with `SIGNED.json`. Path segment `signed-renders` is **never-touch**.
- **Enforcement:** Add `signed-renders` to delete_guard never-touch names; lesson `signed-renders-store.md` (L62); L59 companion; tests for never_touch hit.
- **Acceptance:** `never_touch(.../signed-renders/X)` returns never-touch; card Verify bullets present.

### L63 — No karaoke fragments in human cues
- **Sources:** Carl rank 6
- **Problem:** Continuous ASS karaoke fragment `<0.8s` inside a `human=true` cue passed.
- **Rule:** Any cue marked human (ASS override / JSON `human:true` / Style) that still contains a karaoke `\k`/`\K`/`\kf` fragment span with duration `< 0.8s` → FAIL.
- **Enforcement:** `craft_qc.py karaokehumanfrag` (or extend askaraoke); lesson card; tests.
- **Acceptance:** Human cue with short `\k` frag → FAIL; human cue phrase-only → PASS; missing file → FAIL.

---


### H-IDENTITY — Pre-commit ChaosEngine authorship guard
- **Sources:** Carl; designer-workspace had 6 ChaosEngine-authored commits outside allow-list
- **Problem:** ChaosEngine `<chaosengine@mail.grokbot.com>` authorship leaked into non-engine remotes.
- **Rule:** Repo→identity map: ChaosEngine email ONLY for `ShaftHQ/SHAFT_ENGINE` and `ShaftHQ/shafthq.github.io` (named in card Rule prose). Every other repo must use the owner identity. Optional host `CE_OWNER_IDENTITY` / `.chaos-engine-state/owner-identity` (untracked) binds the expected owner — no personal name/email in the portable tree. Pre-commit + PreToolUse **block** otherwise.
- **Enforcement:** `commit_identity.pre_commit_identity_failures` / `python commit_identity.py --pre-commit`; wired in `git_hooks.py` pre-commit and `hooks/guard.py` `commit_identity_pre_commit_reason`; card `references/commit-identity.md`.
- **Acceptance:** ChaosEngine identity + designer-workspace origin → FAIL; same identity on SHAFT_ENGINE → PASS; owner identity anywhere → PASS.

### L64 — Uncaptioned speech coverage gate
- **Sources:** Carl designer-workspace `lib/uncaptioned_speech.py`
- **Problem:** ASR heard speech ≥300ms with <50% human-cue cover shipped past phrase karaoke checks.
- **Rule:** Each ASR speech span ≥300ms needs ≥50% coverage by cues with JSON `human: true` (non-hall text without the flag does not count); missing cues/asr → FAIL. Shared `human_cue` helper documents L63 ASS vs L64 JSON predicates.
- **Enforcement:** Portable `chaos-engine/addons/design-skills/scripts/uncaptioned_speech.py` + `craft_qc.py uncaptionedspeech`; lesson `uncaptioned-speech.md`; L39 checklist bullet; tests.
- **Acceptance:** Uncovered 500ms span → FAIL; ≥50% cover → PASS; missing input → FAIL.

### H-PR-SCOPE — Pre-merge PR diff vs stated scope
- **Sources:** Carl scope-discipline retro (PR-path half)
- **Problem:** PR body claimed a narrow gate while the diff wandered; reviewers missed churn.
- **Rule:** Diff paths must match stated scope (`## Scope` bullets or `<!-- chaos-engine:scope ... -->`); else stop. Missing declaration → FAIL.
- **Enforcement:** `scripts/ci/pr_scope_check.py` + `references/pr-scope-check.md`; wired into `overlay_pre_push.pr_scope_failures` and `harness_pr_gate.pr_scope_gate_failures` for agentic PRs; unit tests.
- **Acceptance:** Path outside prefixes → FAIL; all paths under prefixes → PASS; no scope block → FAIL; pre-push path calls `pr_scope_failures`.

### H-SCOPE — Spoken-lock + one-task / path-claim
- **Sources:** Carl H-SCOPE brief (spoken-lock scope words + one-task-of-a-kind / no two workers same file)
- **Problem:** PR reused H-SCOPE for the PR-diff gate; L41 + sequential-wip + parallel_stream_conflict were not linked under Carl's id.
- **Rule:** Spoken-lock titles keep scope nouns (L41 / `title_scope_gate`); one In progress per bot (`sequential-wip`); exclusive writable path claims (`parallel-streams` + `parallel_stream_conflict.py`).
- **Enforcement:** `references/h-scope.md` card linking those surfaces; checklist keeps L41 under spoken-lock.
- **Acceptance:** Card names L41 + sequential-wip + parallel_stream_conflict; checklist does not equate H-SCOPE with `pr_scope_check` only.

### L65 — Motion-graphics gate
- **Sources:** Carl MG r1 (Coach found 5 misses after indep PASS)
- **Problem:** OCR/bidi garble, static kinetic text, late captions, unverified transcript, pop-in diagrams shipped.
- **Rule:** OCR frames vs transcript with U+2066/U+2069 Latin isolates; kinetic static >3s or >6 words FAIL; caption onset ≤150ms; transcript manually verified; diagrams draw-on; caption lines ≤32 chars / 1–2 phrase-split lines; share every sample with owner before acceptance; reviewer briefs list checks.
- **Enforcement:** `motion_graphics_gate.py` + `craft_qc.py mggate`; lesson + checklist; tests.
- **Acceptance:** Each miss class has a FAIL test; missing observations → FAIL.


## Dropped / deferred (with reason)

| Item | Reason |
| --- | --- |
| Caption FA-default / CPS≤20 / title-collision OCR suite | Partial overlap L49/L52/L53; large craft change → next design-skills PR (Carl reviews separately) |
| UG1167 yarn a11y:contract | User-guide repo; zero open issues there per plan — out of this CE PR |
| Main-red-reaction gh `--jq` quoting | SHAFT CI workflow bug, not CE harness |
| Dependabot merge_queue re-enqueue lesson | Ops note only; already in queue docs |
| HRSD journey push-once / tracker-paths / BrowserStack force-update | Consumer pack / product — not CE core this batch |
| WAITING-HUMAN-ARTIFACT ledger | P2 process; doc-only deferred |
| `/tmp` ban CI linter | Partial (allure-compose already refuses); full CI hook deferred |
| Install UX path/docs polish (Cortex/Coach queue) | Many already in INSTALL.md .31.x; residual doc nits → follow-up |
| backup-remote consumer warn | Incomplete issue 7643; separate bugfix |
| Wardrobe fail-closed default (Carl rank 7) | Flagged on issue 7651; verify already fail-closed in current design_qc — DUP if tests green |
| Whisper int8 AMBIG rubric / skip end-snap on human | METHOD card polish — deferred with FA suite |
| Titlelock adaptive OCR pill | Craft polish — deferred |
| Gemini UI grey-out 5h probe | Extends existing gemini-quota-wait card — doc note only this PR |

---

## Lesson counts (target)

| Bucket | Count |
| --- | --- |
| Implement (H-* + L60–L65 + H-IDENTITY/H-SCOPE/H-PR-SCOPE) | 18 |
| Already shipped DUP | 20+ |
| Dropped/deferred | 14 |

## Riskiest assumption

Observation/receipt gates + provenance JSON are honest. **Named test:** AI-ref-only likeness observations must FAIL (`test_ai_ref_provenance_fails`).

## PR constraints

- ONE CE PR on `ShaftHQ/SHAFT_ENGINE` from latest main
- Identity: ChaosEngine `<chaosengine@mail.grokbot.com>`
- Bump `chaos-engine/VERSION` → `2026.10.31.6`
- Tests for every gate; SkillSpector pass; lint; `overlay_pre_push && git push`
- PR body: agentic + release-note label, ## Plan, riskiest assumption naming a test, link SPEC summary
- Independent adversarial review → `/workspace/reviews/pr-ce-retro2-review.md`; fix findings
- **DO NOT merge** — Carl reviews (design lessons)
