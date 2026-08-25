---
name: work-item
description: >-
  Use when opening or rewriting a work item on any git-based SCM.
  Source-control agnostic; GitHub, GitLab, and Azure Boards are adapters only.
license: MIT
---

# Work item

Load this skill when the deliverable is **open or rewrite a work item**. Do not
vendor Spec Kit or create `.specify/`. Keep GitHub PR delivery on
`work-github-playbook.md`.

Contract and adapters: [work-item.md](../../references/work-item.md).

## One item, one problem

One work item owns one actionable problem. Trackers list children; they do not
absorb them. Preserve existing acceptance when rewriting.

## Required Spec Kit sections

Every body must include:

- User Scenarios & Testing
- Edge Cases
- Functional Requirements
- Success Criteria
- Assumptions or Out of scope

Keep adopter template headings (for example bug or feature fields) so existing
validators still match.

When rewriting an existing item for Spec Kit hygiene, specialize User Story,
`FR-*`, and `SC-*` from that issue's own acceptance (see
`scripts.agents.issue_filing.rewrite_body_spec_kit`). Keep the shared heading
checklist; never paste the generic stamp `Deliver the stated acceptance` onto
unrelated tickets. Trackers may keep a campaign-level story.

## Taxonomy

Exactly one primary type, exactly one lifecycle, at least one subsystem or
module. Missing taxonomy fails closed with a named reason. Ready needs a proof
plan; blocked needs at least one https dependency URL.

## Adapter selection

Detect the live CLI from the adopter environment. Do not assume GitHub.
Command maps live in the adapters reference named from the contract. Live
create/edit remains whatever CLI the adopter already runs; dry-run and fake
runners prove the other maps.
