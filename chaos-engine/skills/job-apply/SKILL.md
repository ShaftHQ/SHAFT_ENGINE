---
name: job-apply
description: >-
  Use when anyone wants to apply for a job: classify the posting, gather inputs
  once, draft (never send) a researched cover email with a verified CV, and pass
  the gate checklist. On demand only.
license: MIT
permissions:
  - bash
  - read
  - write
  - env
---

# Job apply

Load this skill when the deliverable is **apply for a job** (email a recruiter,
ATS form, Easy Apply, or similar). Draft only; the human presses Send.

**Applicant profile (private):** identity, education, experience counts, form
defaults, cover-letter priorities, title exclusions, base country, and remote
rule live in a **private per-user store** (vault / memory / run folder owned by
that user). Never copy them into this skill tree or its references. Read the
profile at apply time; write updates back to the private store only.

Never store applicant identity, contacts, employer names, or personal stats in
this skill or its references.

Before drafting, run:

```bash
python3 .chaos-engine/tool.py retrieve --store graphify "job apply cover letter posting"
python3 .chaos-engine/tool.py retrieve --store mempalace "job apply cover letter posting"
```

Record `retrieve: used|skipped(<reason>)` in the task receipt. Then run the
abstraction self-check on this skill directory
([check_abstraction.sh](scripts/check_abstraction.sh) →
[check_abstraction.py](scripts/check_abstraction.py)):

```bash
bash chaos-engine/skills/job-apply/scripts/check_abstraction.sh
```

## Process

1. **Open each link in a NEW tab**, read-only (no Apply, message, save, or
   reaction). Resolve short links (for example lnkd.in — often a feed post, not
   a job page). Save **verbatim post text** plus location/remote stance (quote
   it; "not mentioned" means no remote), apply route, poster domain, openings,
   years, and must-haves into the run folder so drafting never re-opens the page.
2. **Classify and triage fit** before any drafting: feed post / job page / form /
   ATS, and apply route (email / form URL / ATS / Easy Apply). Report
   `fit: OK | STRETCH (<reasons>) | NO-GO (<reason>)` and wait when any of these
   hold: onsite outside the candidate's base country; prefers local candidates /
   existing work permit / in-country domain experience; remote limited to another
   country. Offer to draft anyway after the user decides.
3. **Extract** role title, hiring/payroll company vs end client, location
   options, contract type/length, experience range, openings, must-haves, the
   apply route, and required application fields (location preference, notice
   period, expected salary, and similar).
4. **Gap check** JD skills vs CV + project READMEs; list gaps to the user; never
   claim them. Flag other constraints (mandatory domain experience, on-site city,
   contract length, salary omitted).
5. **Fill known fields** from the private applicant profile + CV first. Ask
   **all remaining required unknowns in one multi-step question card**
   (allowCustom on; label estimates as such). Offer safe defaults such as
   "Negotiable" / "Leave it out".
6. **Pick the CV**: newest verified version; check it against standing identity
   facts in the private profile before attaching. Lead later copy with
   user-ranked priorities from that profile; omit titles the user excluded and
   flag CV conflicts.
7. **Draft, never send** (email path). An email that needs an attachment becomes
   a draft in the user's own mailbox with the PDF attached. The user presses
   Send. For non-home-country roles, state base country and that they seek a
   remote role near the top of the body.
7b. **Forms path:** fill known fields → screenshot the filled form → get an
    explicit "yes" → submit exactly once → capture confirmation text + screenshot
    + any application ID. Duplicate required fields: reuse the first value only
    if required; else leave blank. Missing checkbox option: tick Other and type
    the value. Single-page forms may have no Next; trust viewport capture and
    scroll-check.
8. **Iterate on feedback**: replace the draft so exactly one draft remains;
   report the draft id.
9. **Fact gate on every claim**: verify each number/claim against a live source
   (merged PRs, repo stars, project README) or the CV; replace vague figures
   with a verified number + source; tie projects only to README-stated features;
   drop unsupported wording; flag anything unverified.
10. **Respect exclusions** the user states (titles to omit, topics such as
    domain gaps, prices, client names). Note when the attached CV still contains
    excluded items.
11. **Deliver** the final body in chat plus 1–3 "check before sending" notes
    (for example payroll vendor vs client wording, soft claims).
12. **Update the application ledger** after every state change (one row per
    application: role | source link | route | fit flags | status
    read/flagged/drafted/submitted | draft id or app ID | date). Report it at
    the end.
13. **One browser worker at a time.** Queue posting reads and form fills
    serially; leave a half-filled form tab open (draft auto-saved) while reading
    other postings; acknowledge new links immediately and add them to the queue.

Detail cards: [research](references/research.md),
[template](references/template.md), [gate](references/gate.md),
[lessons](references/lessons.md).

## Hard rules

- A recorded standing order outranks confirm-before-send inside the scope that order names. The scope text stays in the private store.
- Distinct contacts or channels are separate applications.
- The remote ask goes in the subject and in the body.
- Two independent reviewers, then send from the human's mailbox connector. Read the Sent folder back. One signature. The signature text stays in the private store.
- Public-by-link happens through the browser share dialog and is confirmed this turn.
- After two identical ATS verification errors, stop and use the next channel.
- The ledger is a cache. Read the tracker back after every write. The sheet location stays in the private store.
- Search the Sent folder before any resend.
- Cover letters and emails do not use founder or CEO titles, client names, or tool claims that are not in the CV or a project README.
- Never send the application; drafts only (forms: submit only after explicit yes). The standing order above is the exception inside its named scope, and the human still owns the connector send.
- Never invent achievements, features, or metrics.
- Never put applicant names, emails, phones, employers, or personal stats into
  this skill tree; keep them in the private per-user applicant profile / live
  draft / vault only.
- Payroll vendor vs end client: use "with" (not "at") when the vendor only runs
  payroll.
- Include salary only when the posting asks; otherwise omit or mark Negotiable.
- Run [check_abstraction.sh](scripts/check_abstraction.sh) (wraps [check_abstraction.py](scripts/check_abstraction.py)) before claiming the skill content is clean.
