# Change-verification playbook

## Ten practices

1. Establish the requested behavior, affected files, current owner, and smallest proof of done before reviewing generated or edited code. [`ISTQB-CTFL`]
2. Run `shaft_coding_partner_plan` to find reuse matches, the recommended target and insertion anchor, missing code, and focused verification command. [`SHAFT-MCP`]
3. Produce a preview-only `shaft_coding_partner_diff` and read the entire diff for target, scope, imports, locators, actions, assertions, secrets, and cleanup. [`SHAFT-MCP`, `SELENIUM-PRACTICES`]
4. Run `test_code_guardrails_check` on the proposed Java and clear every error; warnings require an explicit review decision. [`SHAFT-MCP`]
5. Apply only reviewed hunks within the user's granted editing authority; MCP previews never write files and do not grant broader mutation, deploy, or publish authority. [`SHAFT-MCP`]
6. Use the smallest nonredundant proof first: compile or test-compile, then the single affected test, then broader package/verify only when blast radius requires it. [`ISTQB-CTFL`, `GOOGLE-SRE-TESTING`]
7. Use `verify_run_focused` for guarded headless Maven verification when its allowlisted command fits; otherwise run the equivalent approved local headless command. [`SHAFT-MCP`]
8. Read actual test-result counts, traces, attachments, and bounded output; a successful transport or build banner alone is not a behavioral verdict. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]
9. On failure, preserve exact evidence and route consistent failure to `shaft-failure-analysis` or inconsistent outcomes to `shaft-flaky-test-analysis` before changing shared code. [`ALLURE-STABILITY`, `ISTQB-CTFL`]
10. Report applied files, exact checks, pass/fail/skip evidence, unresolved warnings, and any intentionally unrun external validation. [`ISTQB-TM`, `SHAFT-REPORTING`]

## Valid examples

- Preview one missing `LoginPage` method, guardrail-check it, apply the reviewed hunk, run test-compile, then run the login test.
- Reject a generated diff containing `Thread.sleep`, raw `findElement`, or an absolute XPath before any file mutation.
- Run `verify_run_focused` with the plan's allowed Maven test command and inspect the returned bounded failure evidence.
- Preserve a passing compile but report an unrun cloud-device test as unverified rather than claiming end-to-end completion.

## Boundary

- This gate verifies an already chosen behavior and design; route new test intent, locator choice, page ownership, or root-cause diagnosis to its specialist before changing the diff.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
