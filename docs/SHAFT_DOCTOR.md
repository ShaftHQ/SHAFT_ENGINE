# SHAFT Doctor

`io.github.shafthq:shaft-doctor` collects explicitly selected local test
evidence into a portable, redacted bundle and applies ordered deterministic
rules. It does not require `shaft-ai`, a provider credential, or network
access. The complete baseline works with `pilot.ai.enabled=false`.

Optional provider analysis is advisory only. It is disabled unless explicitly
requested, receives only minimized already-redacted evidence, and never
replaces the deterministic diagnosis, findings, confidence, or remediation.

## Outputs

Each analysis writes:

- `doctor-evidence.json`: versioned `EvidenceBundle` with checksums,
  provenance, relative paths, size-limit decisions, and a redaction summary;
- `doctor-report.json`: the bundle plus versioned `Diagnosis`, cited
  `Finding` records, confidence, uncertainty, and `Remediation` actions, plus
  a separately identified advisory when provider analysis is requested;
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

## Optional provider advisory

Add `shaft-ai` when invoking `DoctorAnalyzer.analyzeWithAi(...)` directly. The
executable `SHAFT_MCP` JAR already packages the OpenAI, Anthropic, Gemini, and
Ollama adapters. CLI provider analysis also requires `--ai`; Pilot properties
must independently enable the provider, processing location, model, and every
submitted evidence category.

Local Ollama example:

```bash
java \
  -Dpilot.ai.enabled=true \
  -Dpilot.ai.provider=ollama \
  -Dpilot.ai.consent.local=true \
  -Dpilot.ai.allowedEvidenceCategories=TEXT,LOG,CONFIGURATION \
  -Dpilot.ai.ollama.model=<local-model> \
  -jar shaft-mcp/target/SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results \
  --allowed-root "$PWD" \
  --output-dir target/shaft-doctor \
  --ai
```

Ollama defaults to `http://127.0.0.1:11434/api/chat`. Changing the endpoint does
not weaken consent, redaction, minimization, schema validation, or evidence-ID
checks.

For OpenAI, Anthropic, or Gemini, select the provider and model, approve remote
processing, and approve the same evidence categories. Credentials remain in
the provider-specific environment variable documented in
[SHAFT Pilot AI](SHAFT_PILOT_AI.md); they are never Doctor arguments or report
fields.

Doctor submits the deterministic diagnosis, its explicit uncertainty, and only
the textual evidence cited by deterministic findings. Unknown cases may submit
the smallest available textual evidence set. Provider output must match the
versioned `shaft-doctor-advisory-1.0` schema and may contain observations,
hypotheses with confidence, missing evidence, recommended actions, and
limitations. References outside the submitted evidence-ID allowlist reject the
entire advisory. Uncited claims and hypotheses that contradict the
deterministic primary cause are visibly marked.

Timeout, rate limit, invalid credentials, unavailable provider, malformed JSON,
schema violation, oversized output, invented evidence, and budget exhaustion
produce an explicit fallback advisory while retaining the complete
deterministic report. Reports contain provider/model/configuration identifiers,
duration, usage when available, cache state, and a safe fallback reason. They
never contain credentials, raw provider responses, or hidden reasoning.

Use `--ai-cache` to explicitly cache successful safe structured advisories
under the output directory. Cache keys include the evidence bundle checksum,
deterministic diagnosis checksum, and a non-secret provider/configuration
checksum. Failures and raw evidence are never cached.

## MCP

`doctor_analyze` accepts explicit input paths, historical bundle paths,
allowed roots, an output directory, screenshot/page-snapshot approvals, and
the minimum expected Allure result count. The tool calls the same
`DoctorAnalyzer` used by the CLI. It remains deterministic when Pilot AI is
disabled; when the MCP server is explicitly started with an enabled provider,
the same separate advisory and fallback rules apply.

ChatGPT, Codex, Claude, Gemini, and GitHub Copilot can invoke
`doctor_analyze` as external MCP clients. Their model authentication stays in
the client and is not ingested by SHAFT. Copilot is MCP interoperability, not a
generic Copilot API-key adapter. Credential-free representative invocations
are in `docs/examples/shaft-pilot/mcp/doctor-analyze-invocations.json`.

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
mvn -pl shaft-doctor,shaft-ai,shaft-mcp -am test
mvn -pl shaft-doctor -am javadoc:javadoc
```
