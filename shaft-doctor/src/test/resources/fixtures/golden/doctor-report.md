# SHAFT Doctor Report

- Evidence schema: `1.0`
- Diagnosis schema: `1.0`
- Bundle: `bundle-8d2c46c24c57991fcfcc`
- Primary cause: `LOCATOR`
- Confidence: `HIGH`
- Evidence items: 2
- Omitted by policy or limits: 0

## Diagnosis

Locator did not resolve an element.

The first matching rule in stable precedence order was locator-not-found; later matches are reported as contributing causes.

## Findings

| Type | Severity | Category | Finding | Evidence |
|---|---|---|---|---|
| INFERENCE | ERROR | LOCATOR | Locator did not resolve an element: Rule locator-not-found matched sanitized failure text in 1 cited evidence item(s). | `e-9ec0926f96f896af0007` |
| OBSERVATION | ERROR | UNKNOWN | Failed or broken test attempt: example.GoldenTest.locatorFailure: NoSuchElementException: unable to locate element | `e-9ec0926f96f896af0007` |

## Remediation

1. **Locator did not resolve an element**: Inspect the cited locator against the failing page state and update it only after confirming the intended element.

## Missing Evidence

No additional evidence requirement was identified.

## Evidence Index

| ID | Category | Source | Bytes | Redacted | Truncated | Checksum |
|---|---|---|---:|---|---|---|
| `e-1ff53f9d5c86d501149d` | EXCEPTION_CHAIN | `root-1/locator-result.json` | 175 | true | false | `ece4db2b2508b74f453121aa791b474e45f3ae32fb8f8e55bc6fc15691a6efb0` |
| `e-9ec0926f96f896af0007` | ALLURE_RESULT | `root-1/locator-result.json` | 614 | true | false | `e7b30977cbb8c562970188a9657f5a59d26610ced02c91b71f9c04bf2aec0e87` |

## Privacy

Applied redaction rules: none. Sensitive field names removed: none. Screenshot and page-snapshot evidence is retained only when explicitly enabled.
