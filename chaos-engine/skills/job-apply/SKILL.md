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
ATS form, Easy Apply, or similar). Draft only; the human presses Send. Never
store applicant identity, contacts, employer names, or personal stats in this
skill or its references — those live in the user's vault / CV / chat answers.

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

1. **Open the exact link** the user pasted (resolve short links such as
   lnkd.in). Read-only: no Apply click, no messages, nothing saved to the
   posting site.
2. **Classify the posting.** Feed post vs job page vs ATS. Note whether apply
   means email CV, Easy Apply, or an external form.
3. **Extract** role title, hiring/payroll company vs end client, location
   options, contract type/length, experience range, openings, must-haves, the
   apply route, and required application fields (location preference, notice
   period, expected salary, and similar).
4. **Flag constraints** that change the decision (mandatory domain experience,
   on-site city, contract length, salary omitted).
5. **Ask the user once** (one multi-step question card) for only what the post
   requires and only the user knows. Offer safe defaults such as "Negotiable"
   / "Leave it out".
6. **Pick the CV**: newest verified version; check it against standing identity
   facts before attaching.
7. **Draft, never send.** An email that needs an attachment becomes a draft in
   the user's own mailbox with the PDF attached. The user presses Send.
8. **Iterate on feedback**: replace the draft so exactly one draft remains;
   report the draft id.
9. **Fact gate on every claim**: verify each number/claim against a live source
   (merged PRs, repo stars, project README) or the CV; drop wording a source
   does not support; report a source per claim; flag anything unverified.
10. **Respect exclusions** the user states (titles to omit, topics such as
    domain gaps, prices, client names). Note when the attached CV still contains
    excluded items.
11. **Deliver** the final body in chat plus 1–3 "check before sending" notes
    (for example payroll vendor vs client wording, soft claims).

Detail cards: [research](references/research.md),
[template](references/template.md), [gate](references/gate.md),
[lessons](references/lessons.md).

## Hard rules

- Never send the application; drafts only.
- Never invent achievements, features, or metrics.
- Never put applicant names, emails, phones, employers, or personal stats into
  this skill tree; keep them in the live draft / vault only.
- Payroll vendor vs end client: use "with" (not "at") when the vendor only runs
  payroll.
- Include salary only when the posting asks; otherwise omit or mark Negotiable.
- Run [check_abstraction.sh](scripts/check_abstraction.sh) (wraps [check_abstraction.py](scripts/check_abstraction.py)) before claiming the skill content is clean.
