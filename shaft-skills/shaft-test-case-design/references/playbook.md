# Test-case-design playbook

## Ten practices

1. Trace each case to one or more requirements, risks, acceptance criteria, or exploratory charters. [`ISTQB-CTFL`]
2. Choose a fitting technique: equivalence partitions, boundaries, decisions, states, use cases, pairwise, or experience-based exploration. [`ISTQB-CTFL`]
3. State preconditions, actor, permissions, environment, dependencies, and data without hiding setup in prose. [`ISTQB-CTFL`]
4. Express steps as observable user or service actions, keeping implementation details out until automation authoring. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
5. Give every case an unambiguous expected result and evidence oracle at the point it matters. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
6. Cover valid, invalid, boundary, error, alternate, recovery, and state-dependent behavior according to risk. [`ISTQB-CTFL`]
7. Keep cases independent or declare ordering, shared state, and cleanup explicitly. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
8. Assign priority, test level, type, execution cadence, and automation suitability from value and cost. [`ISTQB-TM`, `SELENIUM-PRACTICES`]
9. Use stable identifiers and versioned trace links so results and defects remain attributable. [`ISTQB-CTFL`, `ALLURE-RESULTS`]
10. Review for duplicates, missing partitions, unreachable setup, weak oracles, and unnecessary end-to-end scope. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]

## Examples

- Derive boundary cases for an age field that accepts integers from 18 through 120.
- Use a decision table for discounts depending on membership, coupon validity, and minimum basket value.
- Use state transitions for a payment moving through authorized, captured, refunded, and disputed states.

## Boundary case

- If the user asks for Java implementation, finish the reviewed case design and route implementation to `shaft-automated-test-authoring`; do not mix page-object or tool syntax into this deliverable.

## Output

Return concise case IDs, trace links, technique, priority, setup, data, steps,
expected evidence, cleanup, and automation suitability.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
