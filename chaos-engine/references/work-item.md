# Work-item contract

Portable, source-control agnostic ticket contract. Adapters live in
[work-item-adapters.md](work-item-adapters.md). GitHub PR merge and watch stay
in `work-github-playbook.md` and `work-github-planning.md`.

## Required Spec Kit sections

Every opened or rewritten work item body must contain these section names:

| Section | Purpose |
| --- | --- |
| User Scenarios & Testing | Given/When/Then acceptance scenarios and independent tests |
| Edge Cases | Boundaries, fail-closed paths, and non-goals that can bite |
| Functional Requirements | Numbered `FR-*` statements |
| Success Criteria | Numbered `SC-*` measurable outcomes |
| Assumptions or Out of scope | At least one of these headings |

Adopter templates may keep their own headings (Describe the Bug, Problem
Statement, and so on). Spec Kit sections are additive.

Hygiene rewrites must specialize User Story / `FR-*` / `SC-*` from the issue's
own acceptance via `rewrite_body_spec_kit`. Do not stamp
`Deliver the stated acceptance` (or identical FR/SC boilerplate) onto unrelated
issues. Trackers may keep a campaign-level story. Closed issues need no rewrite.

## Taxonomy rules

- Exactly one primary type.
- Exactly one lifecycle label.
- At least one proven subsystem or module label.
- Multiple modules require explicit `cross-cutting`.
- Ready requires a proof plan.
- Blocked requires https dependency links that adapters accept.
- Missing taxonomy fails closed with a named reason.

## Dependency URLs

Blocked dependency links must be https. Accepted shapes:

- GitHub issue URLs on `github.com` ending at `issues/N`
- GitLab issue URLs on `gitlab.com` (or self-hosted) ending at `issues/N`
- Azure Boards work-item URLs on `dev.azure.com` or `visualstudio.com` ending at a numeric work-item id

Reject `javascript:`, non-https schemes, and unknown hosts/shapes.

## Filing behavior

Validate before create. Confirmation digests bind repository and full content.
Idempotency markers prevent duplicate creates on retry. One work item owns one
problem; related items link, they do not merge into a receipt.
