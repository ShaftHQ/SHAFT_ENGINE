---
name: job-applications
description: "Use when applying to jobs for the user by email, LinkedIn, or an ATS form."
license: MIT
permissions:
  - bash
  - read
  - write
  - env
---

# Job applications, all channels

Apply end to end through whichever route each posting offers. Email, LinkedIn and web forms are all in scope; never drop a fitting role just because it has no email.

## 1. Inputs (read first)

- Target criteria, form answers, CV rules and tracker location from memory (RecallMemory "job-form answers", "job application").
- The tracker sheet: read every tab and dedupe on company + role + channel before picking roles.
- The current application CV (a clean version with no client names or titles the user excluded). If none exists, build one from the latest CV, keeping every real fact and inventing nothing.

## 2. Find and rank

- Search remote job boards, company career pages, and LinkedIn job search. Verify each posting URL is live, read the full description, and note the posted date, location eligibility and apply route.
- Drop roles that are locked to a country the user can't work in, and ghost or evergreen listings (flag them instead of counting them). Rank by fit and take the day's target count.
- Tag each role with its route: `email`, `linkedin` (Easy Apply or a recruiter or hiring-manager message), or `form` (Workable, Ashby, Greenhouse, Lever, Recruitee, Workday, a custom page).

## 3. Tailor

- Write a cover note per role from its top criteria. Run two separate adversarial review passes (hiring manager, then skeptical recruiter) and revise. Say honestly when the user lacks a stated requirement (a degree, a language); never claim it.
- Never use AI-written answers where the posting forbids them. Ask the user for their own words.

## 4. Apply by route

- **Email:** send from the user's mail connector with the CV attached, to an address printed on the official posting or careers page only. If no standing send permission covers it, save drafts with the attachment and get one yes for the batch.
- **LinkedIn:** prefer Easy Apply. Read the resume file name and date LinkedIn will attach and make sure it's the current CV. For a recruiter or hiring-manager message or connection note, use the reviewed text word for word.
- **Web form:** open the posting in the browser the user chose (the user's own desktop browser when they named it, otherwise your own browser). Fill fields from memory's form answers and upload the CV. Answer salary and demographic questions only from facts the user gave (for example, "negotiable"). Leave optional demographic fields at "prefer not to say" or blank unless the user chose an answer. Before you press submit, take a screenshot of the filled form and check every field against your plan. Then submit and capture the confirmation page.
  - Web forms use the browser the user named, or the host's own browser tools. Do not prescribe an OS input API. A run stays on local tools unless the user approves a named cloud agent or an extra machine. Never restart the user's browser; open a new tab.
  - A sign-in, captcha, one-time code, or new ATS account that needs the user goes to them as one step. Do the rest of the batch and come back.

## 5. Log and report

- Add one tracker row per application: id, company, role, remote terms, route, recipient or URL, date and time in the user's zone, source URL, message id or confirmation text, and status.
- Keep a dated checkpoint file with what went out, the shortfall, and blocked items.
- Report to the user: each application (company, role, route, link, proof), the tracker rows, the shortfall, and anything waiting on them.

## Guardrails

- Never invent qualifications, postings, salaries or deadlines. Never put secrets, client names, prices or trainee data in outbound text.
- Don't re-apply through a channel already on the tracker. A different channel for the same role is fine when the user wants every channel used.
- One submit per form. If a submit looks like it failed, check for a confirmation or email before you retry.
