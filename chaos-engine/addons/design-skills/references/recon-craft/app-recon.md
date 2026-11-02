---
name: app-recon
description: Use when mapping screens and flows, scoring feature gaps, or mining evidence-linked user complaints for a product you own or may improve — never to copy another product's code, logo, copy, or trade dress.
---

# R01 App recon (craft)

## Use when

You need a structured map of screens and flows, a feature-parity / gap matrix,
or complaint themes backed by real reviews — before design QC, CMS embed work,
or product feedback. Method only. Do not install or vendor third-party clone
skill packs.

## Principles

1. **Behavior, not assets.** Capture what the product *does* (screens, flows,
   states, data entities). Never copy HTML/CSS/JS, logos, copy, icons, photos,
   or trade dress.
2. **Public + own account only.** Public pages, help docs, store listings, and
   sessions you are allowed to use. No login scraping, cookie theft, or ToS
   bypass.
3. **Evidence over invention.** Complaint themes need linked quotes or counts
   from real reviews; inventing reviews is a fail.
4. **Clean-room rebuild if building.** Own stack, own tokens, own words. Rebrand
   before any deploy that could confuse users with another product.

## Do

- Write `recon.md`: screens, primary flows, key components, inferred data model,
  and out-of-scope network effects (marketplaces, social graphs you do not own).
- Keep `features.csv` (or equivalent table): feature, must-have?, status
  (present / partial / missing), evidence link. Diff against the target to
  score parity and gaps.
- Mine complaints from allowed public sources (store reviews, community posts
  you may read). Group into themes with counts and at least one linked quote
  each; feed durable themes into MemPalace, not into invented backlog items.
- Prefer host browser + approved connectors (`gh`, web fetch, X) over new
  scrapers.

## Don't

- Reverse-engineer binaries, bypass DRM, or scrape authenticated sessions you
  do not own.
- Clone a competitor's UI chrome, wording, or assets into a shippable surface.
- Treat DevTools or live browser MCP as a substitute for product automation regression tests.
- Add third-party scraper/RE packs or full clone-skill trees to this overlay.

## Numeric targets

| Target | Value | Check |
| --- | --- | --- |
| Screen inventory | every primary nav destination named | recon.md |
| Flow coverage | happy path + one failure/empty state per must-have flow | recon.md |
| Feature matrix | must-haves marked; no silent "assumed done" | features.csv |
| Complaint themes | ≥1 linked quote or count per theme | evidence list |
| Card size | ≤120 lines | `wc -l` |

## Rubric

- **1 (fail):** Copied assets/copy; invented reviews; ToS-breaking scrape.
- **3 (competent):** Screens + flows + matrix with evidence links; gaps named.
- **5 (expert):** Clean-room scope clear; complaint themes drive a measurable
  fix plan; out-of-scope networks called out.

## Verify

- [ ] No third-party code, logo, or marketing copy in the deliverable tree
- [ ] `features.csv` (or table) lists must-haves and gap status
- [ ] Each complaint theme cites a real source
- [ ] Inventory / craft index still needles `app-recon` / `features.csv` /
      `complaint themes` / `clean-room`

## Out of scope

Architect/build/deploy clone pipelines; binary RE; unauthorized access;
default-on Chrome DevTools MCP (optional debug aid only, telemetry off when
enabled).
