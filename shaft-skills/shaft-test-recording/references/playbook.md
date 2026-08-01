# Test-recording playbook

## Ten practices

1. Obtain approval for the live browser, device, emulator, Inspector, proxy, or visible interaction and state the exact flow, URL/app, backend, data, and privacy boundary. [`SHAFT-MCP`, `ISTQB-CTFL`]
2. Use a direct persistent MCP client for interactive recording, or start a persistent `shaft-cli session`; never spread one recording across one-shot processes. [`SHAFT-MCP`]
3. For managed desktop web capture, call `capture_start` without driver setup; for mobile, call `driver_initialize` with `mobile_native` or `mobile_web` before `capture_start`. [`SHAFT-MCP`, `APPIUM`]
4. Record from the first meaningful precondition through the final observable outcome, keeping setup, recovery, and cleanup actions attributable. [`ISTQB-CTFL`]
5. Add `capture_checkpoint` markers for assertions, page transitions, recovery, and named flow boundaries rather than relying on inferred intent. [`SHAFT-MCP`, `SHAFT-REPORTING`]
6. Use `capture_set_mode` to separate record from inspect work, and verify chosen locators against current DOM or accessibility evidence. [`SHAFT-MCP`, `W3C-ACT`]
7. Never capture real secrets or personal data; use dedicated accounts, placeholders, redaction, and generated required-data notes. [`SHAFT-REPORTING`, `OWASP-WSTG`]
8. Read `capture_status` during the flow and resolve readiness warnings, missing checks, pending signals, or wrong-engine state before stopping. [`SHAFT-MCP`]
9. Stop with `capture_stop`, preserve the returned recording path and evidence manifest, and close the owned driver/session after artifacts are durable. [`SHAFT-MCP`, `ALLURE-RESULTS`]
10. For API traffic use `capture_api_start`, `capture_api_transactions`, and `capture_api_stop`; review classified leaves before any generated assertion. [`SHAFT-MCP`, `OWASP-WSTG`]

## Valid examples

- Record a managed desktop checkout with `capture_start`, element actions, two checkpoints, `capture_status`, and `capture_stop`.
- Initialize `mobile_native`, record an accessibility-ID sign-in flow, checkpoint the home view, stop, and quit the driver.
- Wrap an approved Appium Inspector flow with `mobile_inspector_record_start`, `mobile_inspector_record_status`, and `mobile_inspector_record_stop`.
- Capture service traffic with `capture_api_start`, review redacted `capture_api_transactions`, then stop the API recording.

## Boundary

- Recording produces evidence, not production-ready source; route persisted artifacts to `shaft-recording-codegen` and all proposed edits to `shaft-change-verification`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
