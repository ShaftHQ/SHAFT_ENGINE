# SHAFT Pilot

SHAFT Pilot combines deterministic browser Capture, TestNG generation, failure
diagnosis, reviewed repair proposals, and MCP interoperability.
AI is optional and disabled by default.
Capture, generation, Doctor, and MCP remain usable without a provider account,
API key, model, or network call.

## Modules

| Module | Purpose |
| --- | --- |
| `shaft-pilot-core` | Approval, redaction, budget, schema, audit, and deterministic fallback contracts. |
| `shaft-capture` | Managed Chrome/Edge recording and deterministic TestNG generation. |
| `shaft-doctor` | Portable evidence, deterministic diagnosis, and isolated reviewed repair proposals. |
| `shaft-ai` | Optional direct OpenAI, Anthropic, Gemini, and Ollama adapters. |
| `SHAFT_MCP` | Executable stdio and Streamable HTTP server plus Capture and Doctor CLI. |

Library consumers should import `shaft-bom` and add only the modules they use:

```xml
<dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>io.github.shafthq</groupId>
      <artifactId>shaft-bom</artifactId>
      <version>${shaft.version}</version>
      <type>pom</type>
      <scope>import</scope>
    </dependency>
  </dependencies>
</dependencyManagement>

<dependencies>
  <dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>shaft-capture</artifactId>
  </dependency>
  <dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>shaft-doctor</artifactId>
  </dependency>
</dependencies>
```

Add `shaft-ai` only for direct provider calls. External ChatGPT, Codex,
Claude, Gemini, and GitHub Copilot clients use SHAFT through MCP and keep their
own authentication.

## Install the executable

Download `io.github.shafthq:SHAFT_MCP:<version>` from Maven Central, or build it
from the monorepo:

```bash
mvn -pl shaft-mcp -am package -DskipTests -Dgpg.skip
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar
```

The default process is an MCP stdio server. The same JAR dispatches `capture`
and `doctor` subcommands.

## Capture example

Start a privacy-filtered recording. This example is headless for CI or scripted
use; omit `--headless` when a person will drive the browser locally:

```bash
java -jar SHAFT_MCP-<version>.jar capture start \
  --url https://example.test \
  --browser chrome \
  --output recordings/example.json \
  --headless
java -jar SHAFT_MCP-<version>.jar capture status
```

Drive the visible browser or the automation controlling the headless session,
optionally add a checkpoint, then stop and generate:

```bash
java -jar SHAFT_MCP-<version>.jar capture checkpoint \
  --description "Checkout confirmation is visible" --kind ASSERTION
java -jar SHAFT_MCP-<version>.jar capture stop
java -jar SHAFT_MCP-<version>.jar capture generate \
  --session recordings/example.json \
  --output-dir generated-tests \
  --package generated.capture \
  --class-name CheckoutJourneyTest \
  --replay
```

Generation writes Java source, externalized ordinary test data, and
`target/shaft-capture/generation-report.json`. Passwords, configured sensitive
fields, credential-shaped values, private paths, and unsafe locator evidence
are excluded or redacted. Replay succeeds only when the generated project
compiles, passes, and produces populated Allure result JSON.

## Doctor example

Analyze only explicitly allowed evidence:

```bash
java -jar SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results \
  --allowed-root "$PWD" \
  --output-dir target/shaft-doctor
```

Outputs include `doctor-evidence.json`, `doctor-report.json`, and
`doctor-report.md`. The deterministic rules classify product, test, locator,
data, timing/synchronization, environment/configuration, infrastructure, and
unknown failures. Empty and retry-only evidence remains visible instead of
being converted into a false success.

## Reviewed repair example

Create a complete reviewed input based on
`examples/shaft-pilot/doctor/repair-input.json`, then run:

```bash
java -jar SHAFT_MCP-<version>.jar doctor propose-fix \
  --repository "$PWD" \
  --base-sha <40-character-commit> \
  --diagnosis target/shaft-doctor/doctor-report.json \
  --evidence-bundle target/shaft-doctor/doctor-evidence.json \
  --issue <issue-or-session> \
  --allowed-path src/test/java/example/CheckoutTest.java \
  --repair-input docs/examples/shaft-pilot/doctor/repair-input.json \
  --output-dir target/shaft-doctor/repairs
```

Doctor creates and validates a temporary isolated worktree. It does not modify
the current branch or write to GitHub. After reviewing the diff and validation
result, publish only a draft pull request with the exact returned token:

```bash
java -jar SHAFT_MCP-<version>.jar doctor publish-draft-pr \
  --manifest target/shaft-doctor/repairs/repair-proposal-<id>.json \
  --approval-token <exact-token> \
  --approve
```

The MCP equivalents are `doctor_propose_fix` and
`doctor_publish_draft_pr`. Publication cannot merge, bypass branch protection,
or proceed without separate explicit approval.

## MCP clients

For local clients, configure `java -jar /absolute/path/SHAFT_MCP-<version>.jar`
as a stdio server named `shaft-mcp`. Ready-to-edit examples are under
`docs/examples/shaft-pilot/mcp/` for Codex, Claude Desktop and Claude Code,
Gemini CLI, and VS Code with GitHub Copilot.

Start Streamable HTTP for clients that need a reachable HTTPS endpoint:

```bash
java -jar SHAFT_MCP-<version>.jar --spring.profiles.active=http
```

The endpoint is `/mcp` and the default port is `8081`. ChatGPT developer
mode/apps and cloud agents cannot launch a local JAR, so deploy the container
and expose `/mcp` over HTTPS. GitHub Copilot is an MCP client integration; it is
not a direct provider API-key adapter.

External client capabilities were verified against official documentation on
June 12, 2026:

| Client | Supported SHAFT connection | Current limitation or control |
| --- | --- | --- |
| ChatGPT apps/developer mode | Public HTTPS Streamable HTTP `/mcp`. See [Connect from ChatGPT](https://developers.openai.com/apps-sdk/deploy/connect-chatgpt). | Hosted ChatGPT cannot start the local JAR; workspace plan and administrator controls apply. |
| Codex CLI and IDE extension | Local stdio or Streamable HTTP, configured by CLI or shared `config.toml`. See [Codex MCP](https://developers.openai.com/codex/mcp). | Tool approval remains a Codex client policy. |
| Claude Code and Desktop | Local stdio; Claude Code also supports HTTP and can import Desktop configuration. See [Claude Code MCP](https://docs.anthropic.com/en/docs/claude-code/mcp). | The Claude Messages API uses its separate remote [MCP connector](https://docs.anthropic.com/en/docs/agents-and-tools/mcp-connector). |
| Gemini CLI | Local stdio, SSE, or Streamable HTTP through `settings.json`. See [Gemini CLI MCP](https://github.com/google-gemini/gemini-cli/blob/main/docs/tools/mcp-server.md). | Server trust and tool confirmation are controlled by Gemini CLI. |
| GitHub Copilot | MCP-capable IDEs and CLI can use local or remote servers; cloud agent uses repository MCP configuration. See [GitHub Copilot MCP](https://docs.github.com/en/copilot/concepts/context/mcp). | Cloud agent/code review currently consume tools only, and organization policy may restrict MCP. |

## Optional providers

Safe defaults:

```properties
pilot.ai.enabled=false
pilot.ai.provider=none
pilot.ai.consent.local=false
pilot.ai.consent.remote=false
pilot.ai.allowedEvidenceCategories=
```

Provider examples are under `docs/examples/shaft-pilot/providers/`. OpenAI,
Anthropic, and Gemini read credentials only from their configured environment
variable names. Ollama needs no credential but still requires local consent.
Remote consent, local consent, and every evidence category are denied unless
explicitly enabled.

Provider output is advisory. It cannot replace deterministic diagnosis, apply
a Capture enrichment without review, or publish a repair without a second
approval. Timeout, rate limit, authentication, malformed JSON, schema failure,
budget exhaustion, and provider unavailability all preserve deterministic
output.

## Privacy and security

- Review recordings, externalized data, Doctor bundles, and repair manifests
  before sharing them.
- Screenshots and page snapshots are excluded from Doctor unless explicitly
  requested.
- Remote provider sharing requires explicit consent after minimization and
  redaction.
- Credentials remain in environment variables or provider-native client
  authentication and must not be stored in SHAFT properties, examples, logs,
  generated tests, or Allure attachments.
- Pilot audit events contain provider/model identifiers, redaction counts,
  duration, and status, but not prompts, evidence, credentials, or raw model
  responses.

## Troubleshooting

| Symptom | Resolution |
| --- | --- |
| MCP process exits or prints non-protocol output | Use Java 25, run the packaged JAR directly, and keep stdio logs on stderr. |
| HTTP client cannot connect | Start with `--spring.profiles.active=http`, expose port `8081`, and use `/mcp`. |
| Capture cannot start | Confirm Chrome or Edge is installed, no prior Capture session owns the runtime directory, and use `--headless` in CI. |
| Generated test does not replay | Read `generation-report.json`; fix missing external data or an unsupported/ambiguous locator before regenerating. |
| Doctor reports incomplete evidence | Provide populated `*-result.json` Allure files and set the correct `--allowed-root`. |
| Provider is unavailable | Keep the deterministic result; verify enablement, consent, model, endpoint, budget, and environment variable name before retrying. |
| Repair proposal is rejected | Use an exact base SHA, repository-relative allowlist, complete file content, tokenized Maven commands, and a clean isolated worktree. |

See [SHAFT Capture](SHAFT_CAPTURE.md), [SHAFT Doctor](SHAFT_DOCTOR.md),
[SHAFT Pilot AI](SHAFT_PILOT_AI.md), and [SHAFT MCP](SHAFT_MCP.md) for the
complete contracts.
