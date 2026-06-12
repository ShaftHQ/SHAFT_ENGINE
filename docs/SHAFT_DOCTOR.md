# SHAFT Doctor

`io.github.shafthq:shaft-doctor` collects explicitly selected local test
evidence into a portable, redacted bundle and applies ordered deterministic
rules. It does not require `shaft-ai`, a provider credential, or network
access. The complete baseline works with `pilot.ai.enabled=false`.

## Outputs

Each analysis writes:

- `doctor-evidence.json`: versioned `EvidenceBundle` with checksums,
  provenance, relative paths, size-limit decisions, and a redaction summary;
- `doctor-report.json`: the bundle plus versioned `Diagnosis`, cited
  `Finding` records, confidence, uncertainty, and `Remediation` actions;
- `doctor-report.md`: a portable human-readable report and evidence index;
- `artifacts/`: approved binary evidence such as screenshots.

Reports contain no original absolute machine paths. Evidence IDs and bundle
IDs are content-derived, and JSON formatting uses LF line endings so repeated
analysis of identical inputs is byte stable.

## CLI

The executable MCP JAR exposes Doctor as a local command:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results \
  --input target/shaft-logs \
  --allowed-root "$PWD" \
  --output-dir target/shaft-doctor \
  --minimum-results 1
```

Every readable input must resolve under an explicit allowed root. Symlink
targets are resolved before collection. The output directory must also be
inside a declared root. Add repeated `--history` options to correlate recurring
signatures from older `doctor-evidence.json` files.

Screenshots and page snapshots are excluded by default. Retain them only with
explicit approval:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results \
  --allowed-root "$PWD" \
  --output-dir target/shaft-doctor \
  --include-screenshots \
  --include-page-snapshots
```

Use `--max-item-bytes` and `--max-bundle-bytes` to lower the conservative
retention limits. Use `--minimum-results` when the expected run size is known;
an empty, malformed, truncated, or unexpectedly small Allure run is reported
as incomplete and is never interpreted as successful.

## MCP

`doctor_analyze` accepts explicit input paths, historical bundle paths,
allowed roots, an output directory, screenshot/page-snapshot approvals, and
the minimum expected Allure result count. The tool calls the same
`DoctorAnalyzer` used by the CLI and performs no outbound request.

## Evidence

The collector recognizes populated Allure `*-result.json`, normalized exception
chains, SHAFT logs/action history, environment metadata, dependency/build
metadata, configuration summaries, screenshots, and page snapshots. Text and
structured JSON are redacted before retention or hashing. Password fields,
authorization and cookie headers, tokens, private keys, common credential
fields, and configured sensitive names are replaced without retaining their
original values.

Allure attempts are grouped by history ID and ordered by their recorded start
time. Non-final failed, broken, and skipped attempts remain visible even when
the final attempt passes. Historical bundles are optional and can be copied
with their relative `artifacts/` directory for offline analysis.

## Diagnosis

The ordered rule engine classifies primary and contributing causes as:

- `PRODUCT`
- `TEST`
- `LOCATOR`
- `DATA`
- `TIMING_SYNCHRONIZATION`
- `ENVIRONMENT_CONFIGURATION`
- `INFRASTRUCTURE`
- `UNKNOWN`

Rules cover locator-not-found, duplicate, stale, hidden/covered/interactable,
frame/window context, assertion and test-data mismatches, timeout symptoms,
driver/browser startup, Grid/Appium/network/filesystem/resource failures,
setup/cleanup failures, parallel shared-state symptoms, retry-hidden failures,
and recurring historical signatures. Every inference cites evidence IDs and
is kept separate from observations. Unknown and contradictory cases remain
unknown and list the missing evidence needed to narrow them.

## Sharing

Review `doctor-evidence.json`, `doctor-report.json`, and any approved
`artifacts/` before sharing. Screenshots and page source can contain personal
or confidential data even after deterministic redaction and therefore remain
opt-in. Doctor never uploads evidence automatically.

## Validation

Run from the repository root:

```bash
mvn -pl shaft-doctor,shaft-mcp -am test
mvn -pl shaft-doctor -am javadoc:javadoc
```
