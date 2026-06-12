# SHAFT Pilot AI foundation

SHAFT Pilot separates provider-neutral contracts from optional direct model
access:

- `shaft-pilot-core` contains immutable request/response records, approval and
  redaction policies, budgets, safe audit metadata, provider capabilities,
  `ServiceLoader` discovery, and deterministic fallback.
- `shaft-ai` contains direct HTTP adapters for OpenAI, Anthropic, Google Gemini,
  and local Ollama. It uses JDK `HttpClient`; no provider SDK type is exposed.
- `shaft-engine` exposes only thread-local `SHAFT.Properties.pilot`
  configuration. It has no dependency on either Pilot module.

`shaft-capture` and `shaft-doctor` depend on `shaft-pilot-core`. Add
`shaft-ai` only when direct provider calls are required. Capture creation,
validation, migration, and redaction remain deterministic with
`pilot.ai.enabled=false`.

SHAFT Doctor uses these contracts for an optional separately rendered advisory.
It submits only minimized evidence from an already-redacted `EvidenceBundle`
plus the deterministic diagnosis. Provider output cannot replace the baseline,
must cite only submitted evidence IDs, and falls back safely for provider,
budget, timeout, or validation failures. See [SHAFT Doctor](SHAFT_DOCTOR.md).

## Safe defaults

AI is off unless both settings are changed:

```properties
pilot.ai.enabled=false
pilot.ai.provider=none
```

Provider models default to blank, remote and local consent default to `false`,
and no evidence category is approved by default. Disabled or unavailable AI
returns the request's `deterministicFallback` and makes no network call.

Credentials are read only from environment variables:

| Provider | Default variable |
| --- | --- |
| OpenAI | `OPENAI_API_KEY` |
| Anthropic | `ANTHROPIC_API_KEY` |
| Gemini | `GEMINI_API_KEY` |
| Ollama | none |

The environment variable names are configurable, but credential values are
never SHAFT properties. Provider endpoints and models are configured through
`SHAFT.Properties.pilot`; no model is selected implicitly.

## Approval and evidence

Every request carries an `ApprovalPolicy`, and the effective SHAFT properties
must independently approve the same processing location and all submitted
`EvidenceCategory` values. Remote calls require
`pilot.ai.consent.remote=true`. Ollama requires
`pilot.ai.consent.local=true`; a loopback endpoint is not trusted
automatically.

Before provider execution, `AiExecutionService`:

1. checks provider capabilities and availability;
2. enforces request size, token, cost, timeout, concurrency, and circuit-breaker
   limits;
3. rejects unapproved evidence categories;
4. redacts authorization and cookie headers, password/secret assignments,
   common tokens, configured DOM selectors, attributes, and patterns;
5. validates provider JSON against the requested schema;
6. returns deterministic fallback for every failure.

Audit events contain only request ID, purpose, provider/model identifiers,
redaction counts, duration, and status. Prompts, evidence, credentials, raw
responses, and original secret values are excluded.

## Example

```java
SHAFT.Properties.pilot.set()
        .enabled(true)
        .provider("ollama")
        .localConsent(true)
        .allowedEvidenceCategories("TEXT")
        .ollamaModel("your-local-model");

var schema = JsonNodeFactory.instance.objectNode()
        .put("type", "object");
schema.putObject("properties")
        .putObject("summary")
        .put("type", "string");
schema.putArray("required").add("summary");

var approval = new ApprovalPolicy(
        true,
        false,
        Set.of(EvidenceCategory.TEXT));

var request = AiRequest.builder("doctor-summary", schema)
        .text(redactedDiagnosticText)
        .approvalPolicy(approval)
        .deterministicFallback(deterministicDiagnosis)
        .build();

AiResponse response = new AiExecutionService().execute(request);
```

Call `SHAFT.Properties.clearForCurrentThread()` at lifecycle boundaries after
setting per-thread Pilot properties.

## Direct providers and external agents

Direct provider adapters are separate from MCP clients. OpenAI, Anthropic, and
Gemini API credentials apply only to `shaft-ai` direct calls. GitHub/Microsoft
Copilot is supported through SHAFT MCP and retains its own client
authentication; SHAFT does not request or define a generic Copilot model API
key.

Credential-free MCP configuration fixtures are under
`docs/examples/shaft-pilot/mcp/`. See [SHAFT MCP](SHAFT_MCP.md) for transport
and deployment details. The representative `doctor_analyze` fixture covers
ChatGPT, Codex, Claude, Gemini, and GitHub Copilot without embedding any client
or provider credential.

Provider request mappings follow the official
[OpenAI structured output](https://developers.openai.com/api/docs/guides/structured-outputs),
[Anthropic structured output](https://platform.claude.com/docs/en/build-with-claude/structured-outputs),
[Gemini structured output](https://ai.google.dev/gemini-api/docs/structured-output),
and [Ollama chat](https://docs.ollama.com/api/chat) contracts.
