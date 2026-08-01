# Database-testing playbook

## Ten practices

1. Query the database only when persisted state or database behavior is the required oracle; prefer public service behavior when it proves the same risk. [`ISTQB-CTFL`]
2. Use `SHAFT.DB` and the project's configured connection pattern with least-privilege test credentials; never target production or embed secrets. [`SHAFT-GUIDE`]
3. Create unique, minimal fixtures in an isolated schema, tenant, or transaction so parallel tests cannot collide. [`GOOGLE-SRE-TESTING`, `ISTQB-CTFL`]
4. Keep queries explicit and deterministic, request only needed rows and columns, and impose stable ordering whenever order is asserted. [`ISTQB-CTFL`]
5. Keep untrusted values out of SQL text; use controlled fixtures and the safest parameter mechanism supported by the owning data layer. [`OWASP-WSTG`]
6. Assert business-relevant row count, column values, constraints, or state transitions through SHAFT validations, not raw dumps. [`SHAFT-GUIDE`, `ISTQB-CTFL`]
7. Verify writes through a fresh read and test rollback, constraint, duplicate, and concurrency behavior when those risks are in scope. [`ISTQB-CTFL`, `GOOGLE-SRE-TESTING`]
8. Register cleanup before mutation, make it idempotent, and preserve failure evidence without leaving shared state behind. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
9. Avoid sleep-based replication guesses; use a bounded documented consistency condition or test against the authoritative store. [`GOOGLE-SRE-TESTING`]
10. Run the focused test against approved disposable infrastructure and distinguish assertion failure, connection failure, and environment drift. [`ISTQB-CTFL`, `ISTQB-TM`]

## Valid examples

- Insert one uniquely keyed row, assert `getRowCount()` and `getColumn("label")`, then delete the row in cleanup.
- Call the public API, query the resulting order state through `SHAFT.DB`, and assert only the stable persisted fields.
- Verify an update with `executeUpdateQuery(...)`, reselect the row, assert the changed value, and restore or remove the fixture.
- Test a uniqueness constraint with two isolated attempts and assert the specified rejection without exposing connection credentials.

## Boundary

- Do not add direct database checks for convenience when the requirement is an external contract; route service behavior to `shaft-api-testing` and environment provisioning to `shaft-test-environment`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
