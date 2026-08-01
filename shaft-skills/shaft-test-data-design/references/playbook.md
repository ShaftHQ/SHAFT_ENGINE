# Test-data-design playbook

## Ten practices

1. Map every dataset to the partitions, boundaries, states, roles, and risks it must exercise. [`ISTQB-CTFL`]
2. Include representative, minimum, maximum, invalid, empty, duplicate, expired, and malformed values where relevant. [`ISTQB-CTFL`]
3. Prefer synthetic or irreversibly anonymized data; minimize production-derived records and documented exceptions. [`ISTQB-CTFL`, `OWASP-WSTG`]
4. Keep credentials, tokens, personal data, and regulated values out of source, logs, screenshots, and reports. [`OWASP-WSTG`, `SHAFT-REPORTING`]
5. Build deterministic seeds or factories and record the random seed whenever randomness adds coverage. [`GOOGLE-SRE-TESTING`]
6. Isolate records per test, worker, tenant, and environment to prevent order and concurrency coupling. [`SELENIUM-PRACTICES`, `GOOGLE-SRE-TESTING`]
7. Make setup and cleanup repeatable, idempotent, observable, and safe after partial failure. [`GOOGLE-SRE-TESTING`]
8. Generate collision-resistant identities while preserving business-valid formats and referential integrity. [`ISTQB-CTFL`]
9. Version fixtures with schemas and refresh them when contracts, migrations, clocks, locales, or reference data change. [`ISTQB-CTFL`, `GOOGLE-SRE-TESTING`]
10. Record ownership, source, creation method, retention, reset, and disposal rules for each data family. [`ISTQB-TM`, `OWASP-WSTG`]

## Valid examples

- Design valid, missing-field, boundary-length, duplicate, and malicious API request bodies from a contract.
- Allocate unique customer and order records per parallel worker in a multi-tenant checkout suite.
- Build locale- and timezone-specific dates around daylight-saving, month-end, and expiry boundaries.

## Boundary case

- Never copy or mutate live customer data without explicit authority and protection controls; return a synthetic-data plan or a blocked dependency instead.

## Output

Return coverage-to-data mapping, factories or seed rules, isolation keys,
privacy classification, setup/reset/cleanup, ownership, and retention.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
