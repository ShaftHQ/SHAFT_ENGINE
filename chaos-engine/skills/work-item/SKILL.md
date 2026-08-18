---
name: work-item
description: >-
  Portable work-item contract for opening or rewriting tickets on any SCM.
  Source-control agnostic; GitHub, GitLab, and Azure Boards are adapters only.
license: MIT
---

# Work item

Load this skill when the deliverable is **open or rewrite a work item**. Do not
vendor Spec Kit or create `.specify/`. Keep GitHub PR delivery on the
[GitHub playbook](../../references/work-github-playbook.md).

Contract: [work-item.md](../../references/work-item.md).
Adapters: [work-item-adapters.md](../../references/work-item-adapters.md).

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

Keep adopter template headings (for example SHAFT bug/feature fields) so
existing validators still match.

## Taxonomy

Exactly one primary type, exactly one lifecycle, at least one subsystem or
module. Missing taxonomy fails closed with a named reason. Ready needs a proof
plan; blocked needs at least one https dependency URL.

## Adapter selection

Detect the live CLI from the adopter environment. Do not assume GitHub.
Command maps live in the adapters reference. Live create/edit remains whatever
CLI the adopter already runs; dry-run and fake runners prove the other maps.
