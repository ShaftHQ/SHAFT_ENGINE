# Retrospective: LinkedIn job search for senior test-automation roles (remote, outside home country)

Date: 2026-11-04. Scope: finding ~10 senior/lead IC roles in test automation / AI test automation that are remote (or remote-optional), outside the candidate's home country, applied via email or external site (not Easy Apply), preferably paid in a hard currency. All details below are safe to share (no personal data, no credentials).

## 1. The guest jobs API with `f_WT=2` yields few truly eligible matches
- The LinkedIn guest jobs endpoint (`jobs-guest/jobs/api/seeMoreJobPostings/search`) with `f_WT=2` (remote) works without login and is the cheapest first pass.
- But "remote" there usually means remote *within the posting's country*. Combined with "outside candidate's home country", eligible results were very sparse; most hits were region-locked (US-only, EU-only, specific country lists).
- Lesson: treat the guest API as a seed list only. Budget for a second channel (recruiter posts search) from the start, and filter early on stated eligible countries rather than the `remote` flag.

## 2. "Hybrid" vs "Remote" mismatch between listing card and job page
- At least one role showed "fully remote in eligible locations" in an earlier listing/snippet but "Hybrid, {city}" on the current job page.
- Lesson: the job detail page (and ideally the employer's external apply page) is the source of truth. Record work mode as "unverified" when sources disagree, and never claim remote eligibility in a tracker or cover letter until the employer page confirms it.

## 3. Posts search needs permalinks via the Activity tab
- Recruiter posts ("hiring, send CV to ...") are where email-apply roles live, but the posts search results do not expose stable permalinks directly.
- Lesson: open the poster's profile -> Activity -> Posts, then use the post's "Copy link to post" to get the `posts/{slug}-share-{id}` permalink. Store that permalink in the tracker; `lnkd.in` short links are fine as secondary references.

## 4. Mouse-wheel scrolling hangs the browser; use the End key
- In the remote desktop browser, repeated mouse-wheel scroll on LinkedIn feeds/search caused the browser to hang (infinite-scroll + heavy DOM).
- Lesson: use the `End` key (or `Page Down`) to trigger lazy loading, wait briefly, and extract in batches. Avoid long wheel-scroll loops.

## 5. External apply behind a "share your profile" dialog is a write step
- Clicking "Apply" on some external-apply jobs first shows a LinkedIn dialog offering to share the profile with the employer. Confirming it shares data with a third party.
- Lesson: treat that dialog as an irreversible/externally visible action requiring explicit user approval. To read the external URL without applying, dismiss the dialog or read the URL from the job page markup/guest API instead.

## 6. Recruiter posts are often region-locked
- Many agency/recruiter posts specify nationality or residency (e.g. "Brazil only", "candidates from India/Egypt/Pakistan", "UAE-local preferred", specific EU country lists).
- Lesson: extract eligibility lines verbatim into a Fit Flags column; do not drop a role silently, but mark it. Watch for repeat postings from the same agency for the same client so you don't double-apply (check the tracker by agency + contact email + client before adding).

## 7. Tracker hygiene
- One row per opening with a stable ID, status vocabulary (Found / Applied / Not applied / ...), source permalink, contact, apply method, and fit flags. Add rows as "Found" at discovery time; only move to "Applied" after an actual send/submit with read-back verification.
- Read back after every write to the tracker.

## 8. Rule: lessons must travel with ChaosEngine PRs
After any reflection/retrospective session, an agent that opens a PR to enhance ChaosEngine **must attach the full, detailed markdown lessons file** (all safe-to-share details) to that PR — in the PR description or as a committed file referenced by it. A compressed "caveman" summary is not acceptable; reviewers need the full reasoning, evidence, and the concrete rule changes it motivates. Strip personal data, credentials, client names and prices before attaching.

## Suggested ChaosEngine changes
- Job-search skill: add a two-channel strategy (guest API seed + recruiter posts via Activity permalinks), an eligibility-first filter, and a "verify work mode on job page" gate.
- Browser guidance: prefer `End`/`Page Down` over wheel scroll on infinite feeds.
- Approval gates: classify LinkedIn "share profile" apply dialogs as write steps.
- Reflection workflow: enforce rule 8 (full lessons MD attached to every improvement PR).
