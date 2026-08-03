# API-testing playbook

## Ten practices

1. Trace each service test to a contract, risk, or workflow and select the lowest test level that proves it. [`ISTQB-CTFL`]
2. Construct `SHAFT.API` with a configured base URL and use the project's reusable request/authentication setup; never hard-code secrets or environment endpoints. [`SHAFT-GUIDE`]
3. Build requests through SHAFT request methods and builders, making method, path, parameters, headers, cookies, payload, and content type explicit. [`SHAFT-GUIDE`]
4. Assert the contract at the right layers: status, headers, schema or shape, stable body values, side effects, and useful latency bounds. [`ISTQB-CTFL`, `SHAFT-GUIDE`]
5. Cover valid, invalid, boundary, authorization, missing-resource, conflict, retry, and idempotency behavior according to risk. [`ISTQB-CTFL`, `OWASP-WSTG`]
6. Generate unique test data, isolate accounts/tenants, and clean created resources through supported APIs or fixtures. [`ISTQB-CTFL`, `GOOGLE-SRE-TESTING`]
7. Replace sleeps with bounded polling only when the contract is asynchronous, recording the terminal condition and timeout. [`GOOGLE-SRE-TESTING`]
8. Reuse an API client/helper only when multiple tests share stable request behavior; keep scenario-specific oracles in the test. [`SELENIUM-PRACTICES`, `ISTQB-CTFL`]
9. Redact authorization, cookies, personal data, and sensitive payload leaves from logs, traces, reports, and generated examples. [`SHAFT-REPORTING`, `OWASP-WSTG`]
10. Run guardrails, a focused compile/test, and any approved contract/environment check; distinguish product failure from unavailable dependency. [`SHAFT-MCP`, `ISTQB-CTFL`]

## Valid examples

- Execute the request, then assert the response: `api.get("/posts/1").setTargetStatusCode(200);` followed by `api.assertThatResponse().extractedJsonValue("$.id").isNotNull();`.
- After the request completes, assert stable response fields with `api.assertThatResponse().extractedJsonValue("$.id").isNotNull()`.
- Test a create/read/delete workflow with unique data and guaranteed deletion in cleanup.
- Verify an unauthorized request returns the specified status and safe error shape without logging the credential.
- Validate an eventually consistent job with bounded polling for the documented terminal state.

## Boundary

- Use API capture only to discover traffic or draft code; the final test still needs reviewed contracts, deterministic data, explicit oracles, and focused verification.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
