---
name: public-landing
description: Use when a public calling card or product landing page must convince a human, rank in search, and orient an agent without becoming a second policy.
---

# Public landing

Load when the page is a public calling card or a product landing page.
Application UI stays on [ui-delivery](ui-delivery.md). Working policy stays
in the policy owner this page points at. This page does not restate it.

## Fold

- The first screen names what this is and one next step. No greeting and no
  biography dump.
- Proof is a short achievement row. A number is copied from a source you
  opened, or it is a live badge. Never invent one. When sources disagree, the
  newest primary source wins. If you cannot read it, omit the number.
- Leave out phone numbers, national IDs, street addresses, and compensation.
- Machine-local notes go behind a disclosure, not on the fold.

## Trust widgets

- Prefer live functional badges (build, release, license) over a wall of
  decorative logos.
- Probe every external image and link before shipping. Do not embed a host
  that fails, times out, or returns a 4xx or 5xx status.
- Remaining images stay readable in light and dark themes and have alt text.
  Use a `<picture>` pair when one asset cannot serve both themes.
- A generated animation lives in the repository, or on a host you just probed.

## Three readers

- Humans get one call to action, featured work that says what it is and why
  it matters, and contact or docs links that resolve.
- Search gets a first paragraph that names the thing, who it is for, and the
  outcome, plus a short FAQ of real questions. Do not stuff keywords.
- Agents get a short orientation block that points at the policy owner. The
  landing page is not a second policy.

## Lookalikes

Link only the project you mean. A similarly named repository is a different
product. Say what this one is in one sentence so the name cannot be confused.

## Verify

- Fetch or render the published page.
- Every http(s) link on the page returns success.
- Re-read every number against the source note.
- The policy pointer still names the policy owner, and this page does not
  replace that owner.
