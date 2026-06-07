# Code Review Guide

Review the requested diff and its directly affected callers, tests, properties, resources, and lifecycle hooks. Do not scan the full repository unless scope or safety requires it. Report findings by severity with file/line evidence and focus on defects, regressions, security, and missing validation rather than style preferences.

## Correctness Checklist
- Public facade and fluent return behavior remain compatible.
- Null/empty/error paths, cleanup, retries, and listener ordering are correct.
- Driver, process, stream, socket, file, and JDBC resources are released.
- Configuration defaults and override precedence preserve expected behavior.
- Concurrency/global state cannot leak across parallel tests.

## Security and Data Isolation Checklist
- No secrets or sensitive payloads appear in source, logs, reports, screenshots, or fixtures.
- External authentication/authorization behavior is not bypassed.
- Inputs used in HTTP, SQL, shell, paths, or HTML are safely handled.
- Per-test/thread state is isolated and cleared.
- Not found in current codebase scan: product tenants or framework-owned permissions; flag any new assumptions for confirmation.

## Performance Checklist
- No unbounded polling, retries, logging, attachment growth, file reads, or image processing.
- Waits/timeouts remain configurable.
- Large responses/reports are not copied or parsed repeatedly without need.
- Parallel execution is not serialized by accidental global locks/state.

## Database and API Checklist
- JDBC resources close; externally influenced SQL is parameterized.
- No application schema change is hidden in a framework patch.
- API headers/cookies/session state remain scoped and sensitive values are redacted.
- HTTP status/error/reporting behavior is tested.
- Public request overloads remain unambiguous and backward-compatible.

## Frontend Checklist
- Dashboard changes are self-contained, accessible, theme-compatible, and safe against HTML/script injection.
- Realtime data contract and connection/reconnect behavior remain compatible.
- HTML fixture changes are deterministic and do not rely on external resources.

## Tests and Release Risk Checklist
- Regression/feature tests cover success, failure, cleanup, and concurrency where relevant.
- The narrowest relevant tests and mandatory Java compile were run.
- Allure results were populated before pass/fail claims.
- Environment-dependent suites not run are identified.
- Public JavaDocs/examples/properties/release notes are updated as needed.

Use exactly this review response structure:

## Summary
Briefly describe scope and overall assessment.

## Blockers
List correctness, security, compatibility, data-loss, or missing-critical-test issues. Use `None` when empty.

## Suggestions
List non-blocking improvements with concrete rationale. Use `None` when empty.

## Questions
List assumptions requiring author/team confirmation. Use `None` when empty.

## Required Tests
List exact commands/scenarios needed before approval.

## Risk Level
State `Low`, `Medium`, `High`, or `Critical` and explain why.
