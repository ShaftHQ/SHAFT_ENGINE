# Defect-reporting playbook

## Ten practices

1. Confirm the observed behavior conflicts with an approved requirement, oracle, standard, or stakeholder expectation. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Write a concise title naming the affected feature, condition, and observable failure. [`ISTQB-TM`]
3. Record build, commit, environment, platform, browser or device, configuration, account role, and data preconditions. [`ISTQB-TM`, `ALLURE-RESULTS`]
4. Provide the shortest reliable reproduction steps, including frequency and whether clean-state reproduction succeeds. [`ISTQB-TM`]
5. State expected and actual behavior separately in observable business or user terms. [`ISTQB-TM`]
6. Attach bounded evidence: result, failed step, logs, trace, screenshot, request or response, and relevant timestamps. [`SHAFT-REPORTING`, `ALLURE-RESULTS`]
7. Assign severity from impact and keep priority as an explicit product decision; do not conflate them. [`ISTQB-TM`]
8. Describe affected users, data, platforms, versions, frequency, workaround, regression status, and risk. [`ISTQB-TM`]
9. Link requirements, test cases, runs, related failures, and existing defects; search for duplicates before filing. [`ISTQB-CTFL`, `ISTQB-TM`]
10. Redact secrets and personal data, track workflow state and ownership, and record fix verification before closure. [`OWASP-WSTG`, `ISTQB-TM`, `SHAFT-REPORTING`]

## Valid examples

- Report checkout accepting an expired card with build, browser, seeded user, minimal steps, response evidence, and payment risk.
- Report an API returning 500 instead of the contracted 400 for one malformed boundary payload.
- Report a keyboard trap in a modal with the violated WCAG criterion, focus sequence, video or trace, and user impact.

## Boundary case

- If evidence identifies a test, data, or environment defect rather than product behavior, file it in the owning engineering workflow and do not mislabel it as a product defect.

## Output

Return title, context, preconditions, minimal steps, expected and actual result,
evidence, reproducibility, severity rationale, impact, links, and verification.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
