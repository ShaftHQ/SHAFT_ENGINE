# Requirements-analysis playbook

## Ten practices

1. State the stakeholder, business outcome, user, and decision the requirement must support. [`ISTQB-CTFL`, `ISTQB-TM`]
2. Rewrite vague outcomes as observable, measurable acceptance criteria without changing intent. [`ISTQB-CTFL`]
3. Trace every criterion to its source and give each gap, assumption, and question a stable identifier. [`ISTQB-CTFL`]
4. Separate contradictions, omissions, ambiguities, infeasible constraints, and unverifiable claims. [`ISTQB-CTFL`]
5. Identify positive, negative, boundary, alternate, error, recovery, and state-transition behavior. [`ISTQB-CTFL`]
6. Elicit applicable security, performance, reliability, compatibility, usability, and accessibility expectations. [`ISTQB-CTFL`, `W3C-WCAG22`, `OWASP-WSTG`]
7. Rank gaps and criteria by business impact, likelihood, compliance exposure, and dependency risk. [`ISTQB-TM`]
8. Expose required data, roles, environments, integrations, devices, browsers, and controllable preconditions. [`ISTQB-CTFL`, `APPIUM`]
9. Define the oracle and evidence that would prove each criterion passed or failed. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
10. Baseline the reviewed requirement set and record approved changes before downstream design begins. [`ISTQB-CTFL`, `ISTQB-TM`]

## Examples

- Review a checkout story and flag the missing currency rounding, declined-payment, and idempotency rules.
- Compare an OpenAPI contract with acceptance criteria and identify undocumented 401, 409, and validation-body behavior.
- Analyze a mobile permission flow across first launch, denial, permanent denial, and settings recovery.

## Boundary case

- If only a URL and “test everything” are provided, return explicit discovery questions and risk assumptions; do not invent product requirements or start authoring tests.

## Output

Return traced criteria, a categorized gap register, ranked risks, open questions,
and the evidence needed for acceptance.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
