---
name: job-search
description: >-
  Use when searching for jobs: two-channel discovery, eligibility-first filter,
  verify work mode on the job page, End-key scroll, share-profile approval gate,
  and tracker hygiene. On demand only.
license: MIT
permissions:
  - bash
  - read
  - write
  - env
---

# Job search

Load this skill when the deliverable is **find job openings** (guest jobs API,
recruiter posts, external apply URLs) before any apply/draft work. Pair with
[job-apply](../job-apply/SKILL.md) once a role is ready to draft or submit.

**Applicant / search profile (private):** base country, remote rule, seniority
targets, excluded regions, currency preferences, and tracker paths live in a
**private per-user store**. Never copy them into this skill tree.

Full evidence for this skill: [RETRO-job-search.md](references/RETRO-job-search.md)
(attach the full detailed markdown to every ChaosEngine improvement PR that
comes from a reflection — see [reflection](references/reflection.md)).

Before searching, run:

```bash
python3 .chaos-engine/tool.py retrieve --store graphify "job search guest API recruiter posts"
python3 .chaos-engine/tool.py retrieve --store mempalace "job search guest API recruiter posts"
```

Record `retrieve: used|skipped(<reason>)` in the task receipt. Then run the
abstraction self-check
([check_abstraction.sh](scripts/check_abstraction.sh) →
[check_abstraction.py](scripts/check_abstraction.py)):

```bash
bash chaos-engine/skills/job-search/scripts/check_abstraction.sh
```

## Process

1. **Two-channel discovery**
   - **Channel A — guest jobs API seed:** LinkedIn guest endpoint
     `jobs-guest/jobs/api/seeMoreJobPostings/search` with `f_WT=2` (remote)
     works without login. Treat it as a **seed list only**. The remote flag
     usually means remote *within the posting's country*; combine with the
     candidate's "outside home country" rule and filter early on **stated
     eligible countries**, not the remote flag alone.
   - **Channel B — recruiter posts:** email-apply and external-apply roles
     often live in hiring posts. Posts search results rarely expose stable
     permalinks. Open the poster's profile → **Activity → Posts**, then use
     **Copy link to post** for a `posts/<slug>-share-<id>` permalink. Store
     that permalink as the primary tracker URL; short links (for example
     lnkd.in) are secondary references only.
2. **Eligibility-first filter.** Extract nationality / residency / country-list
   lines **verbatim** into Fit Flags. Do not drop a role silently; mark it.
   Before adding a row, check the tracker for the same agency + contact + client
   to avoid double-apply on repeat postings.
3. **Verify work mode on the job page.** Listing cards and snippets can disagree
   with the current job page (for example "fully remote" vs "Hybrid, city").
   The job detail page — and ideally the employer's external apply page — is the
   source of truth. Record work mode as `unverified` when sources disagree; never
   claim remote eligibility in the tracker or a later cover letter until the
   employer page confirms it.
4. **Browser scroll: End / Page Down, never wheel loops.** On infinite feeds and
   search results, repeated mouse-wheel scroll hangs the remote desktop browser
   (heavy DOM + lazy load). Prefer the `End` key or `Page Down`, wait briefly,
   extract in batches. See [browser](references/browser.md).
5. **Share-profile apply dialog = write step.** Clicking Apply on some
   external-apply jobs opens a LinkedIn dialog offering to share the profile
   with the employer. Confirming shares data with a third party — treat it as
   an **irreversible / externally visible** action that needs **explicit user
   approval** before Confirm. To read the external URL without applying: dismiss
   the dialog, or read the URL from job-page markup / guest API. See
   [approval-gate](references/approval-gate.md).
6. **Tracker hygiene.** One row per opening: stable ID | status
   (Found / Applied / Not applied / ...) | source permalink | contact | apply
   method | fit flags. Add rows as Found at discovery; move to Applied only after
   an actual send/submit with read-back verification. **Read back after every
   write** to the tracker.
7. **Hand off to job-apply** only for roles that pass eligibility + verified
   work mode and that the user wants drafted or submitted.

Detail cards: [channels](references/channels.md),
[browser](references/browser.md), [approval-gate](references/approval-gate.md),
[tracker](references/tracker.md), [gate](references/gate.md),
[lessons](references/lessons.md), [reflection](references/reflection.md),
[RETRO-job-search.md](references/RETRO-job-search.md).

## Hard rules

- Never confirm a LinkedIn share-profile dialog without explicit user approval.
- Never wheel-scroll-loop LinkedIn feeds; use End / Page Down.
- Never invent eligibility or remote status; quote Fit Flags; mark unverified.
- Never put applicant identity, contacts, employers, or personal stats into this
  skill tree; private per-user store only.
- Run [check_abstraction.sh](scripts/check_abstraction.sh) (wraps [check_abstraction.py](scripts/check_abstraction.py)) before claiming the skill content is clean.
