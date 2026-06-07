# Security Policy for AI Changes

## Security Boundary
SHAFT is a test automation library running inside consumer test processes. It handles high-risk material such as target-system credentials, authorization headers, cookies, API bodies, database connection details, screenshots, videos, DOM content, test data, cloud-provider credentials, Jira/Xray configuration, and encryption keys.

## Authentication Rules
- Not found in current codebase scan: application-owned login, user sessions, identity provider, or inbound authentication middleware.
- Authentication to target APIs and external integrations is caller-configured. Keep it in the typed properties/request APIs; never hardcode real values.
- Do not add fallback credentials, sample production tokens, or authentication bypasses.
- Preserve header/cookie session behavior without exposing values in logs or attachments.

## Authorization and Admin Rules
- Not found in current codebase scan: framework-owned roles, permission guards, admin endpoints, or access-control lists.
- External systems such as BrowserStack, LambdaTest, Jira/Xray, cloud KMS, databases, and report services enforce their own permissions.
- Use least-privilege credentials and do not broaden scopes in code or workflow examples.
- No repository-specific role/scope matrix is required for optional integrations; use provider-documented least privilege and consumer-owned credentials.

## Data Isolation / Tenant Rules
- Not found in current codebase scan: tenants, organizations, customer records, tenant IDs, or tenant-scoped tables.
- Do not introduce tenant/customer assumptions into generic framework behavior.
- Execution isolation is mandatory: mutable properties, drivers, headers/cookies, report buffers, temporary files, database connections, and decrypted data must not leak between tests or threads.
- Use `ThreadLocalPropertiesManager` for per-thread overrides and clear via `Properties.clearForCurrentThread()` at lifecycle boundaries.
- Use fresh driver sessions and deterministic cleanup.

## Sensitive Data Handling
- Treat credentials, private keys, API tokens, authorization headers, cookies, database URLs, test identities, screenshots, videos, DOM snapshots, API payloads, query results, and encrypted/decrypted fixtures as sensitive.
- Do not commit secrets. Use placeholders in docs/tests and repository/CI secret stores at runtime.
- Do not include values found in local environment files, credential fixtures, git config, Maven settings, or workflow secrets in output.
- Test fixtures named like credentials must contain only synthetic/non-production data. No additional fixture-specific scanner is required beyond existing repository secret/security controls.
- Minimize retention of decrypted test data and delete temporary plaintext through existing lifecycle behavior.

## Logging Restrictions
- Never log or attach authorization headers, passwords, tokens, private keys, cloud credentials, full connection strings, or sensitive payloads.
- Redact before SHAFT/Allure/Log4j/ReportPortal output; remember CI artifacts can be retained and downloaded.
- Avoid recording environment dumps or all system properties.
- Do not use `System.out.println`; use established reporting APIs with safe messages.
- Exception messages and stack traces may contain URLs or payload fragments; sanitize when crossing report boundaries.

## Secrets Handling
- Keep secrets in GitHub Actions secrets, Maven settings/server credentials, environment injection, or consumer-owned property files excluded from source control.
- Never weaken `.gitignore` protections or print secret environment variables.
- Encryption keys must remain outside source control; Google Tink/KMS support must preserve authenticated encryption and lifecycle cleanup.
- Release GPG and Maven Central credentials are maintainer-only and must not be used in normal validation.

## API Security
- Validate/sanitize externally influenced URLs, headers, file paths, and bodies according to the existing API contract.
- Prevent sensitive request/response material from automatic report attachments.
- Preserve TLS verification and secure defaults; do not add trust-all behavior unless explicitly scoped as an opt-in test feature with warnings and tests.
- Avoid command injection when API values flow into CLI helpers and path traversal when they flow into files.
- Do not infer that a successful HTTP response means authorization is correct; test expected status and content.

## Database and CLI Security
- Prefer prepared statements for externally influenced SQL and avoid logging result data by default.
- Do not embed database credentials or environment-specific hosts.
- Validate shell arguments and avoid string-concatenated commands for untrusted input.
- Restrict file operations to intended paths; guard recursive delete/encrypt/decrypt behavior.

## Supply Chain and Release Security
- Dependency changes require explicit scope, vulnerability review, compatibility testing, and lock/version review; ordinary tasks must not change dependencies.
- CodeQL, dependency review, Dependabot, Codacy/Codecov, and release signing are existing controls.
- Security vulnerabilities must follow `SECURITY.md` private-reporting guidance, not public issue disclosure.

## Common AI Mistakes to Avoid
- Copying credentials or keys from fixtures/config into tests, docs, logs, or responses.
- Adding a universal token, default password, trust-all TLS, or authorization bypass to make a test pass.
- Logging whole REST requests/responses or database rows without redaction.
- Sharing static mutable sessions across parallel tests.
- Assuming target-system authentication/authorization is owned by SHAFT.
- Disabling security scans, signing, or dependency checks to unblock CI.
- Treating screenshots/video/Allure artifacts as non-sensitive.
