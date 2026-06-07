---
name: release-check
description: Use this skill only for release-readiness assessment, version/configuration validation, or post-release verification; never publish unless explicitly authorized.
---

# Release Check

## Purpose
Assess whether a SHAFT release is ready and whether published artifacts, documentation, CI evidence, compatibility, and rollback guidance are complete—without exposing credentials or deploying by default.

## When to Use
Use for release candidates, Maven Central/GitHub release readiness, version alignment, release notes, or post-release smoke validation.

## Required Docs to Read
1. Read only `docs/ai/context.md` first.
2. Read `docs/ai/release-policy.md` and `docs/ai/testing-policy.md`.
3. Read `docs/ai/security-policy.md` for signing, credentials, dependency/security, integrations, or report artifacts.
4. Read `docs/ai/architecture.md` only to select impacted module smoke tests/deployment assets.
5. Inspect only relevant release workflows, version properties, changed modules, and release notes.

Read only the required docs for this task. Inspect only impacted modules and nearby related files. Do not scan the full repository unless explicitly required for the task or safety. Stop gathering context once the acceptance criteria and validation path are clear. Keep changes minimal. Report if more context is needed.

## Workflow
1. Establish candidate commit/version, change range, release owner, and requested action (assessment versus authorized publication).
2. Assign risk by changed modules and public/security/configuration impact.
3. Check `pom.xml` and release-related internal version alignment.
4. Review changed public APIs/properties for compatibility, JavaDocs, migration, and deprecation notes.
5. Verify the two blocking checks from `docs/ai/release-policy.md`: the clean build and the user-guide browser smoke test with populated Allure results. Record all broader matrices as scheduled/non-blocking.
6. Confirm no secrets/generated reports/local binaries are included.
7. Review release workflow triggers, signing/repository assumptions, release body, and rollback notes without printing credential values.
8. For post-release checks, verify tag/release/artifacts, clean-consumer resolution, JavaDocs, sample sync, and high-risk smoke paths.
9. Produce a go/no-go assessment with blockers, waivers, owners, and exact remaining commands.

## Validation Checklist
- [ ] Candidate version and commit are unambiguous.
- [ ] Version metadata is aligned.
- [ ] Public compatibility/migration notes are complete.
- [ ] The clean build passed.
- [ ] The user-guide browser smoke test passed with populated Allure results.
- [ ] JavaDocs, quality/security checks, and broader test matrices are scheduled or recorded as non-blocking.
- [ ] Release notes and rollback notes are actionable.
- [ ] No credentialed deploy was run without explicit authorization.
- [ ] Post-release artifact checks are defined or completed.

## Forbidden Actions
- Running `mvn deploy`, publishing JavaDocs, creating a release, or using signing credentials without explicit authorization.
- Printing or inspecting secret values beyond what is required to confirm presence.
- Overwriting/reusing an immutable published version.
- Marking release ready with empty/invalid Allure results.
- Hiding unscheduled or unreported cloud/mobile/database/browser follow-up paths.

## Final Response Format
- **Changed files:** release documentation/config corrections, if any.
- **What changed / assessment:** candidate, scope, and go/no-go status.
- **Tests run:** exact commands/checks and evidence.
- **Tests not run:** outstanding matrix items, owner, and reason.
- **Risks:** level, blockers, waivers, monitoring gaps.
- **Rollback notes:** downgrade/corrective-release plan and downstream guidance.
