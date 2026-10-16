# Stakeholder-reporting playbook

## Ten practices

1. Identify the audience, business objective, release or product decision, and reporting period. [`ISTQB-TM`]
2. Lead with the verified outcome and decision status in plain language, not test-framework detail. [`ISTQB-TM`]
3. Explain scope and coverage through critical journeys, product risks, platforms, and excluded areas. [`ISTQB-TM`]
4. Translate failures and defects into user, revenue, operational, legal, security, accessibility, and reputation impact. [`ISTQB-TM`, `W3C-WCAG22`, `OWASP-WSTG`]
5. Separate known facts, trends, estimates, assumptions, and unknowns; state evidence confidence. [`ISTQB-TM`]
6. Show meaningful change from the prior baseline: new risk, resolved risk, stability, coverage, or execution health. [`ALLURE-STABILITY`, `ISTQB-TM`]
7. Highlight critical unresolved defects, affected journeys, frequency, workaround, owner, and target decision date. [`ISTQB-TM`]
8. Give a go, conditional-go, no-go, or no-recommendation position with explicit thresholds and rationale. [`ISTQB-TM`]
9. Assign owners and dates to release conditions, mitigations, retests, and accepted residual risks. [`ISTQB-TM`]
10. Keep the main report concise and link a technical appendix for counts, environments, defects, and artifacts. [`ISTQB-TM`, `SHAFT-REPORTING`]

## Examples

- Give executives a conditional-go recommendation because checkout passes but refund recovery has one high-impact unresolved defect.
- Give a product owner a sprint-quality update by critical journey, changed risk, blocked coverage, and next decision.
- Give compliance stakeholders an accessibility status by WCAG level, impacted users, verified exceptions, remediation owner, and retest date.

## Boundary case

- If evidence cannot prove execution or coverage, issue no release recommendation and state the blocked decision; never translate “no reported failures” into green.

## Output

Return decision summary, scope, verified outcomes, business risks, trend,
critical exceptions, recommendation, conditions, owners, and technical links.

Report journey progress from the [journey and coverage map](../../shaft-test-case-design/references/journey-coverage-map.md).

## Execution class

- A delivery set is finished when each result passed, failed with a named id, or was blocked with a named id. An all-green filter must not build that report. Scenario status Done in the journey coverage map still needs a passing run. That sentence stays. An unclassified skip is not finished.
- The delivery report lists each test name, with platform and language distinguishable. A pass count alone is not the report. A count that does not match the visible names fails the check.
