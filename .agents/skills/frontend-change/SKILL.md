---
name: frontend-change
description: Use this skill only for the embedded realtime dashboard or repository HTML/CSS/JavaScript test fixtures; this repository has no standalone frontend application.
---

# Frontend / Dashboard Change

## Purpose
Safely update the self-contained realtime report dashboard or deterministic HTML fixtures without introducing a new frontend toolchain, external runtime dependency, unsafe rendering, or production Java behavior changes.

## When to Use
Use for `src/main/resources/realtime/dashboard.html` or HTML/CSS/JavaScript fixtures under test resources. Do not use for Selenium action APIs merely because they test a UI; those are backend/framework changes.

## Required Docs to Read
1. Read only `docs/ai/context.md` first.
2. Read `docs/ai/coding-standards.md` and `docs/ai/testing-policy.md`.
3. Read `docs/ai/security-policy.md` for rendered test names, logs, attachments, network data, or HTML injection risk.
4. Read `docs/ai/architecture.md` only if changing dashboard data flow, serving, packaging, or runtime protocol.

Read only the required docs for this task. Inspect only impacted modules and nearby related files. Do not scan the full repository unless explicitly required for the task or safety. Keep changes minimal. Report if more context is needed.

## Workflow
1. Identify whether the file is the packaged realtime dashboard or a test fixture.
2. For dashboard work, inspect the nearby runtime/server/reporter class and realtime reporter tests that define its data contract.
3. Preserve existing theme variables, status vocabulary, filters, responsive behavior, connection state, and dependency-free packaging.
4. Render untrusted execution text safely; prefer text content/DOM construction over HTML injection.
5. Implement the smallest HTML/CSS/JS change without external libraries, CDNs, trackers, or build steps.
6. Add/update focused realtime reporter tests or fixture-consuming Selenium tests.
7. Run relevant unit tests and compile/package checks when packaged resources or Java integration are affected.
8. Manually inspect the rendered result or take a screenshot when the change is perceptible and an environment is available.
9. Check accessibility basics: semantics, labels, focus, contrast, keyboard access, and reduced surprise in status colors.

## Validation Checklist
- [ ] No standalone frontend/toolchain was introduced.
- [ ] Existing dashboard data contract and reconnect behavior remain compatible.
- [ ] Untrusted values are not inserted as unsafe HTML.
- [ ] Light/dark themes and responsive layout remain usable.
- [ ] No external runtime resource or telemetry was added.
- [ ] Relevant realtime/fixture tests pass.
- [ ] `mvn clean install -DskipTests -Dgpg.skip` passes if packaged runtime resources changed.
- [ ] Visual validation/screenshot is recorded, or the limitation is stated.

## Forbidden Actions
- Adding npm dependencies, a bundler, remote scripts/fonts, or trackers without explicit approval.
- Exposing report payloads, credentials, cookies, headers, or environment details.
- Changing Java runtime behavior or report protocols as an incidental UI cleanup.
- Replacing deterministic local fixtures with external websites.
- Broad redesigns unrelated to the requested change.

## Final Response Format
- **Changed files:** dashboard/fixtures/tests.
- **What changed:** visible behavior and data-contract impact.
- **Tests run:** exact automated/manual checks and screenshot status.
- **Tests not run:** browser/device/accessibility scenarios not covered.
- **Risks:** compatibility, rendering, injection, accessibility, packaging.
- **Rollback notes:** asset revert and any cache/repackage considerations.
