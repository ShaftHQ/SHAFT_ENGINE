---
name: code-review
description: Use this skill only for reviewing a proposed diff, branch, commit, or pull request without implementing unrelated changes.
---

# Code Review

## Purpose
Find actionable correctness, security, compatibility, isolation, performance, and validation problems in a scoped change, with evidence and severity appropriate to a published test framework.

## When to Use
Use when asked to review code or assess a diff/PR. Do not silently modify code unless explicitly requested after the review.

## Required Docs to Read
1. Read only `docs/ai/context.md` first.
2. Read `docs/ai/code-review.md`.
3. Read `docs/ai/testing-policy.md` for validation claims.
4. Read `docs/ai/security-policy.md` only when the diff touches sensitive boundaries.
5. Read architecture/coding standards and the task-specific skill only when needed to evaluate the changed module.

Read only the required docs for this task. Inspect only impacted modules and nearby related files. Do not scan the full repository unless explicitly required for the task or safety. Keep changes minimal. Report if more context is needed.

## Workflow
1. Identify the diff base/head, stated intent, and files changed.
2. Read the changed code plus direct callers, matching tests, properties, resources, and lifecycle hooks.
3. Validate behavior against existing contracts and public API compatibility.
4. Check cleanup, concurrency, property scope, errors, reporting, and sensitive-data handling.
5. Check API/SQL/shell/path/HTML inputs where relevant.
6. Evaluate tests: reproduction, negative paths, Allure population, and environment coverage.
7. Run focused checks only when needed to confirm a suspected issue or author claim.
8. Rank findings: blockers first; avoid generic style commentary.
9. Use the exact response headings from `docs/ai/code-review.md`.

## Validation Checklist
- [ ] Every blocker has concrete file/line evidence and impact.
- [ ] Public API/fluent compatibility was checked.
- [ ] Security and per-test/thread isolation were checked where relevant.
- [ ] Resource cleanup and error paths were checked.
- [ ] Test claims and untested environments were assessed.
- [ ] Findings do not depend on invented tenant/auth/application architecture.
- [ ] No full repo scan was performed without a stated reason.

## Forbidden Actions
- Approving based only on green CI without reading affected behavior.
- Reporting speculative issues as facts without tracing the code path.
- Focusing on formatting while missing correctness/security risks.
- Exposing secrets discovered during review.
- Broadly rewriting the patch during a review-only task.

## Final Response Format
## Summary
## Blockers
## Suggestions
## Questions
## Required Tests
## Risk Level
