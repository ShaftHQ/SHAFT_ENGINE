# Lessons — Coach — 2026-11-06 (Phase 2 retrospective)

Scope: work since about 2026-10-31, plus job-search and job-apply work on 2026-11-04 that is still the current playbook. Sources: `/workspace/status/20261106/coach.md`, `/workspace/checkpoints/job-apply-20261104/JOB-APPLICATION-LESSONS-v2.md`, `/workspace/checkpoints/job-apply-20261104/JOB-APPLICATION-PLAYBOOK.md`, `/workspace/checkpoints/job-search-20261104/RETRO-job-search.md`, and live checks that [SHAFT_ENGINE#7764](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7764) and [#7765](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7765) had already merged. All lessons are abstracted. They contain no secrets, codes, tokens, contact details, client names, or prices.

---

## L1. Fact-gate before any live publish
- **What happened:** Identity errors (wrong title, wrong trainee count, wrong founding year) and quality misses reached or nearly reached live pages, posts, and video metadata. A post-publish check cannot unsay a public mistake.
- **Root cause:** Delivery was treated as done when schema validated or when an implementer said it was ready. The person who knows the human's facts was not a required gate before the write went live.
- **Rule/gate:** Coach acceptance (facts about the human, titles, identity, and quality) must pass before anything is approved or published live (web, posts, videos, metadata). Post-publish checks are extra, never a substitute. The implementer does not self-accept.
- **Acceptance:** A live publish without a recorded Coach accept is rejected by the delivery close. The accept record names what was checked (title, trainee count, founding year, claimed figures, and any bilingual text).
- **Destination:** chaosengine (gate), brain (who owns the fact-gate)

## L2. Canonical identity facts stay in one private store
- **What happened:** Public copy drifted: a company page said the wrong founding year; copy used a lower trainee count than the approved figure; titles were shortened or prefixed with a variant the private profile bans.
- **Root cause:** Figures and titles were copied from stale pages instead of from the private source of truth, and there was no lint against the approved strings.
- **Rule/gate:** Public copy uses only the approved identity strings stored in the private applicant/brain profile. Trainee count, title, and founding year are never inferred from a third-party page. Cover letters omit founder/CEO titles even when forms may use the full title. Claimed product figures (users, stars, releases) come from a live official source opened this turn, not from memory.
- **Acceptance:** A fact-gate fixture fails if copy contains a banned title variant, a stale trainee count, or a founding year that does not match the private profile. A cover-letter lint fails if it contains founder/CEO wording.
- **Destination:** brain (the strings), chaosengine (the lint)

## L3. Visual and bilingual publish checks are part of "done"
- **What happened:** Mixed Arabic and English text broke alignment and direction; low-contrast text passed a schema check; mobile layout problems were invisible on desktop. Valid structured data alone was treated as done.
- **Root cause:** "Done" meant the document parsed, not that a person would read it on a phone.
- **Rule/gate:** Before any live web publish, check mixed Arabic/English with bidi isolation, no raw URLs in RTL runs, no stray characters, WCAG AA contrast (4.5:1) on all text, and alignment. An independent visual reviewer checks a 390px mobile render and a desktop render.
- **Acceptance:** A publish package includes both renders and a contrast note. Missing either render fails the close.
- **Destination:** chaosengine

## L4. Motion graphics means real animation
- **What happened:** Slideshow-style output (stills with shake or zoom) and reused raw voice clips were offered as "animation" and rejected.
- **Root cause:** The word "motion" was treated as any movement, including Ken Burns on a still.
- **Rule/gate:** Motion graphics means original animated graphics, not stills with shake or zoom. Do not ask the human for new voice or video recordings; use existing public talks and videos as the source for any cloned voice or avatar work. Avatar look changes still need the human's own sign-off.
- **Acceptance:** A sample described as motion graphics is rejected if a frame-diff shows only pan/zoom/shake on a still. A request for a new recording from the human is treated as out of policy.
- **Destination:** chaosengine (definition), brain (no-new-recording rule)

## L5. Verify live before redoing anything marked dropped or missing
- **What happened:** Work was nearly duplicated because an earlier status said something was dropped or missing, when a live post, scheduled video, or sent email already existed.
- **Root cause:** Status files went stale. Agents trusted the last note instead of opening the live artifact.
- **Rule/gate:** Before redoing a dropped or missing item, open the live post, PR, email Sent folder, or scheduled item. If it already exists, record it as Done with the link and do not redo it. The same rule applies to email: search Sent for that recipient and subject before sending again.
- **Acceptance:** A "redo" task records what live URL or Sent thread was checked and when. A resend without a Sent search fails the playbook.
- **Destination:** chaosengine

## L6. Every item reported as done needs a link or a file
- **What happened:** Status updates described work as done without a URL or an attachment, so nobody could verify it.
- **Root cause:** "Done" was treated as a sentence rather than as evidence.
- **Rule/gate:** Every delivered item includes a link to the live artifact, or, if it is a still or video, the file path that can be attached. No link and no file means it is not Done.
- **Acceptance:** Status rows without a link or attachment path are rejected by the board builder.
- **Destination:** chaosengine

## L7. Radio silence and the router
- **What happened:** Progress chatter, WIP samples, and questions sent to the human burned attention and tokens. The human then required absolute radio silence and a single "Done" through the router.
- **Root cause:** Helpers optimized for keeping the human in the loop instead of for finishing the mandate.
- **Rule/gate:** When radio silence is on, do not message the human with progress, samples, questions, or status. Report only to the router. If something truly needs the human (a delete, an avatar sign-off, a production push the standing rules still reserve), mark it Blocked with what was already verified and stop. Do not ping the human. Standing sign-off from named lieutenants replaces the human except where a rule still reserves the human (currently the digital avatar).
- **Acceptance:** A helper message to the human during radio silence, other than the router's final Done, fails the protocol. Blocked items name the reserved decision and the live check already done.
- **Destination:** chaosengine (delegation protocol), brain

## L8. Reflection output travels as full markdown, not a summary
- **What happened:** Compressed one-line retros hid the evidence and the rule change they were meant to produce. Reviewers could not enforce the lesson.
- **Root cause:** Token-saving habits were applied to the one artifact that must stay complete.
- **Rule/gate:** After a reflection session, any PR that improves ChaosEngine must attach the full detailed markdown lessons file (safe-to-share details only) in the PR body or as a committed file the PR references. A compressed summary is not acceptable. Strip secrets, credentials, client names, and prices first. This rule is already filed in [SHAFT_ENGINE#7765](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7765); keep it and apply it to every later lessons PR.
- **Acceptance:** A ChaosEngine improvement PR without a linked full lessons file fails review.
- **Destination:** chaosengine (already partially landed in #7765)

## L9. Short recruiter links are usually feed posts
- **What happened:** Short professional-network links opened as recruiter feed posts, not job pages: no Easy Apply, no ATS, sometimes only an email in the text.
- **Root cause:** The helper assumed a jobs URL shape.
- **Rule/gate:** Open each pasted short link in a new tab, read-only (no Apply, no message, no save, no reaction). Capture role title, location and remote stance (quote it; "not mentioned" counts as "no remote"), apply route, poster domain, openings, years, must-haves, and the verbatim post text into the run folder so later drafting never re-opens the page.
- **Acceptance:** A draft written without a saved verbatim post text in the run folder fails the playbook.
- **Destination:** chaosengine (job-apply skill; confirm this is complete in #7764)

## L10. Job-page work mode beats the listing card
- **What happened:** A listing card said fully remote in eligible locations; the current job page said hybrid in a named city. A guest "remote" filter also included roles that were remote only inside another country.
- **Root cause:** Aggregated listing metadata and the `remote` filter describe the posting's own country more often than worldwide eligibility.
- **Rule/gate:** Treat a guest remote search as a seed list only. The job detail page, and ideally the employer's apply page, is the source of truth. Record work mode as "unverified" when sources disagree. Never claim remote eligibility in a tracker or cover letter until the employer page confirms it. Filter early on stated eligible countries, not on the remote flag alone.
- **Acceptance:** A tracker row whose work-mode sources disagree is marked unverified. A cover letter that claims "fully remote" against a hybrid job page fails the fact-gate.
- **Destination:** chaosengine (job-search skill; confirm this is complete in #7765)

## L11. Recruiter-post permalinks come from the Activity tab
- **What happened:** Posts search results did not expose stable permalinks. Mouse-wheel scrolling on those feeds also hung the remote browser.
- **Root cause:** Infinite-scroll feeds are heavy, and result cards omit the share URL.
- **Rule/gate:** Open the poster's profile, then Activity, then Posts, and use "Copy link to post" to store the stable permalink in the tracker. In the remote browser, use the End key or Page Down to load more results; do not loop on mouse-wheel scroll. One browser worker at a time.
- **Acceptance:** Tracker source URLs for feed posts are permalinks, not only short links. Browser playbooks mention End/Page Down and a single worker.
- **Destination:** chaosengine

## L12. "Share your profile" on an external apply is a write step
- **What happened:** Clicking Apply on some external-apply jobs first offered to share the profile with the employer. Confirming that dialog sends data to a third party.
- **Root cause:** The dialog looks like navigation. It is a share.
- **Rule/gate:** Treat that dialog as an externally visible write. To read the external URL without applying, dismiss the dialog or read the URL from page markup / the guest API. Confirming the share requires the same authority as submitting the application.
- **Acceptance:** A read-only job lookup that confirms the share dialog fails the playbook.
- **Destination:** chaosengine

## L13. Standing apply rules change which drafts are produced
- **What happened:** Early playbooks skipped a second contact for what looked like the same role, and they ignored a recorded standing order whose text lives only in the private profile (`<profile.standing_order_scope>`).
- **Root cause:** Generic job-search skills still tell the helper to shortlist and wait before drafting. That conflicts with an explicit standing order about which drafts to produce. It does not authorize the helper to send.
- **Rule/gate:** A recorded standing order changes which drafts are produced, inside `<profile.standing_order_scope>`. The scope text stays in the private store. Distinct contacts and distinct channels are separate drafts. When that scope includes a remote ask, the ask goes in the subject and the body. Do not invent a contact field the profile does not contain. Optional demographic questions stay with the human unless a recorded answer already exists. The helper still does not send.
- **Acceptance:** A second distinct contact for the same-looking role is drafted, not skipped. A helper that sends mail or submits a form on the strength of the standing order fails the playbook.
- **Destination:** chaosengine (job-apply skill update on top of #7764), brain (standing orders stay private)

## L14. Never invent qualifications or form answers
- **What happened:** Drafts were tempted to claim tools, client names, or years the CV and project READMEs did not support. Vague numbers ("thousands of users") appeared until they were replaced with verified figures.
- **Root cause:** Helpers filled gaps to sound stronger.
- **Rule/gate:** Diff the job description against the CV and each project's own README. List gaps. Never claim a skill, year-count, or client that is not in those sources. Disclose a weaker skill as weaker (for example basic scripting rather than professional fluency). Automated content never includes trainee personal data, client names, or prices; the CV and the professional profile are the exemption for client names, but job emails still must not name clients.
- **Acceptance:** A cover-letter lint fails on client names and on any tool claim that is not in the allowed source list. Each quantified figure cites the live source checked this turn.
- **Destination:** chaosengine, brain (private applicant profile; never copy personal contact data into a shared skill)

## L15. Two independent reviewers, then the human sends
- **What happened:** Unreviewed drafts leaked a client name, overstated a skill, or duplicated a signature.
- **Root cause:** The writer self-reviewed, and the mail client's default signature stacked on the body signature.
- **Rule/gate:** Two independent reviewers each record PASS on the current draft, and neither reviewer is the writer. The body keeps a single signature (`<profile.signature>` stays in the private store). Lead with the priorities stored in the private profile (`<profile.lead_priorities>`). The human sends from their own mailbox. The helper reads the Sent folder back only after that human send. The helper does not send.
- **Acceptance:** A handoff without two PASS reviews, or with a writer reviewing their own draft, or without a single signature, fails the playbook. Sent-folder read-back is stored in the tracker after the human sends.
- **Destination:** chaosengine (job-apply), brain (signature and lead-with order)

## L16. Public file links are a browser share step, not a files-API default
- **What happened:** The files connector could not create an "anyone with the link" share (it failed without the extra scope). A later recruiter message needed that public link because the network's composer would not attach a file.
- **Root cause:** "Share" in the connector and "anyone with the link" in the product UI are different permissions.
- **Rule/gate:** When a public-by-link URL is required, set it in the signed-in share dialog (Anyone with the link, Viewer) and then reuse that URL. Do not claim a link is public until that setting is confirmed. Do not put the file's private sharing state in a lesson or chat beyond "public-by-link was confirmed."
- **Acceptance:** A playbook step that needs a public CV link records that the share dialog was checked this turn.
- **Destination:** chaosengine

## L17. Some ATS hosts reject automated submits; use the next real channel
- **What happened:** An employer form accepted every required field and a human-check widget, then returned a verification error and reset the form on repeat submits. The fallback that reached a person was a recruiter message on the professional network, with the public CV link.
- **Root cause:** Some applicant-tracking hosts treat automated browsers as unverified even after a solved widget. Retrying the same submit does not fix it.
- **Rule/gate:** After two failed submits with the same verification error, stop retrying that form. Draft the next distinct channel `<profile.standing_order_scope>` allows and record both the form failure and the fallback in the tracker. Do not invent answers to get past a form. The helper does not submit the fallback.
- **Acceptance:** A third identical failed submit to the same form fails the playbook. The tracker row names the fallback channel. The helper has not sent it.
- **Destination:** chaosengine (job-apply)

## L18. Tracker and ledger hygiene
- **What happened:** A private ledger lagged the live tracker (missing later applications). Status words were also applied too early ("Applied" before Sent read-back, or "Not applied" after a later standing order should have reopened the row).
- **Root cause:** Two stores, updated at different times, with no read-back.
- **Rule/gate:** One tracker row per opening, with a stable ID, source permalink, contact, apply method, and fit flags. Add rows as Found at discovery. Move to Applied only after an actual send or submit with read-back. Read the tracker back after every write. The private ledger is a cache; the sheet is the source of truth for metrics.
- **Acceptance:** After a send, the sheet row shows Applied, a date, and a thread or application id. A private ledger that disagrees with the sheet is marked stale until updated.
- **Destination:** chaosengine (job-apply / job-search), brain (where the sheet URL lives; do not duplicate personal rows into shared skills)

## L19. Studio and social composers have silent defaults
- **What happened:** A group composer posted as the brand page instead of the person. A video description was garbled by a hashtag dropdown. A second upload created a duplicate because the schedule sheet was trusted instead of the live Studio list.
- **Root cause:** Composer identity, autocomplete, and "already scheduled" state are not visible unless checked on the live screen.
- **Rule/gate:** Before posting to a group, confirm the composer identity. Retype descriptions after any hashtag helper runs, then eye-check. Before uploading, search Studio for an existing scheduled item with that title or file. Delete of a public video still needs the human's own yes.
- **Acceptance:** A social publish record names which identity posted. A scheduling record names the Studio search that ruled out a duplicate.
- **Destination:** chaosengine (social/video playbooks)

## L20. No secrets in chat, git remotes, status, or lessons
- **What happened:** Tokens, passwords, and delete codes are one paste away from landing in a status file or a commit.
- **Root cause:** Helpers treated "the team already knows" as permission to write the value down again.
- **Rule/gate:** Never write tokens, passwords, or delete codes into chat, git remotes, status files, or lessons. The private brain repo stays private. Do not add a remote to a local coach working copy that must stay unshared. Do not run a blanket `git add -A` there. Do not touch machines the standing rules mark out of scope.
- **Acceptance:** A secrets scan of status and lessons files is clean. A commit that adds a remote or a secret fails review.
- **Destination:** chaosengine

---

## Known ChaosEngine items

Already merged, do not redo:
- [SHAFT_ENGINE#7764](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7764) — job-apply skill (ChaosEngine 0.4.20261104.1)
- [SHAFT_ENGINE#7765](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7765) — job-search skill and the reflection-attachment rule (ChaosEngine 0.4.20261104.2)

Open items named in the 2026-11-06 protocol (Chaos owns the mega PR):
- [SHAFT_ENGINE#7766](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7766) — open ChaosEngine bug
- [SHAFT_ENGINE#7767](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7767) — delegation protocol

Suggested fold-ins for the ChaosEngine mega PR, if they are not already complete in #7764 / #7765:
- L13 standing-order overrides (every distinct channel; onsite/hybrid with an explicit remote ask; auto-submit when the human mandated it)
- L15 two-reviewer then send, with Sent read-back and a single body signature
- L16 public-by-link share as a browser step
- L17 ATS verification-error fallback to the next real channel
- L5 Sent-folder check before any resend
- L1 fact-gate before live publish, including cover-letter lints in L2 and L14

Private personal details, contact numbers, and the applicant profile itself belong in brain, not in ChaosEngine.
