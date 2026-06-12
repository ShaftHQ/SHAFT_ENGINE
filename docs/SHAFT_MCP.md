# SHAFT MCP

`io.github.shafthq:SHAFT_MCP` is the executable Model Context Protocol server
for SHAFT browser automation. It is built in the SHAFT reactor, depends on the
canonical `shaft-engine` module, and does not add any dependency to ordinary
`shaft-engine` consumers.

## Build and verify

Run from the repository root:

```bash
mvn -pl shaft-mcp -am test
mvn -pl shaft-mcp -am package -DskipTests -Dgpg.skip
python3 scripts/ci/validate_shaft_mcp_transports.py
```

The packaged server supports:

- stdio by default, with protocol messages only on stdout and logs on stderr;
- Streamable HTTP with `--spring.profiles.active=http`, at `/mcp`;
- managed recording through `capture_start`, `capture_status`, and
  `capture_stop`, with no AI provider required;
- deterministic TestNG generation through `capture_generate`, with compile,
  optional replay, and review-only AI enrichment phases;
- offline deterministic diagnosis through `doctor_analyze`, restricted to
  explicit input paths and allowed roots;
- legacy SSE endpoint properties as configuration aliases during migration.

The HTTP port defaults to `8081` and can be overridden with `PORT` or
`--server.port`.

The same JAR also provides a local SHAFT Capture CLI:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture start \
  --url https://example.test --browser chrome --output recordings/example.json
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture status
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture stop
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar capture generate \
  --session recordings/example.json --output-dir generated-tests
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results --allowed-root "$PWD" \
  --output-dir target/shaft-doctor
```

See [SHAFT Capture](SHAFT_CAPTURE.md) for checkpoint, runtime-directory,
privacy, generation, replay, enrichment approval, browser-support, and
recovery details.

See [SHAFT Doctor](SHAFT_DOCTOR.md) for evidence categories, allowlisted path
handling, deterministic rules, redaction, and offline bundle analysis.

## Authentication boundary

SHAFT MCP does not require or store OpenAI, Anthropic, Google, Microsoft, or
Ollama credentials. The MCP client authenticates its own model/provider
session. Configure only browser/Grid settings required by the SHAFT tools, such
as `REMOTE_DRIVER_ADDRESS`.

Browser, filesystem, and GitHub mutations should remain approval-gated in the
client. Start with a tool allowlist when the client supports one.

## Local stdio clients

Build the JAR, replace `<version>` with the reactor version, and use the
absolute JAR path:

```json
{
  "mcpServers": {
    "shaft-mcp": {
      "type": "stdio",
      "command": "java",
      "args": ["-jar", "/absolute/path/SHAFT_MCP-<version>.jar"]
    }
  }
}
```

Client-specific locations:

- **Codex CLI and IDE extension:** add the server with
  `codex mcp add shaft-mcp -- java -jar /absolute/path/SHAFT_MCP-<version>.jar`,
  or use `[mcp_servers.shaft-mcp]` in `~/.codex/config.toml`.
- **Claude Desktop / Claude Code:** use the `mcpServers` stdio entry in the
  client configuration.
- **Gemini CLI:** place the server under the top-level `mcpServers` object in
  Gemini `settings.json`.
- **GitHub Copilot in an MCP-capable IDE:** use the IDE's MCP server
  configuration and the same `java -jar` command. Copilot remains the MCP
  host and uses its own authentication; there is no generic Copilot model API
  key for SHAFT to request or store.
- **Generic MCP clients:** configure a stdio server command with newline
  delimited JSON-RPC over stdin/stdout.

Codex also supports direct Streamable HTTP:

```toml
[mcp_servers.shaft-mcp]
url = "https://your-shaft-mcp.example/mcp"
default_tools_approval_mode = "prompt"
```

## Remote clients

Start the server locally for testing:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar \
  --spring.profiles.active=http
```

Use the HTTPS `/mcp` URL for ChatGPT developer mode/apps, the Claude API MCP
connector, Copilot cloud agents, Codex, and other Streamable HTTP clients.
ChatGPT and cloud-agent clients require a reachable HTTPS deployment; they
cannot launch a JAR on the user's workstation.

Current client references:

- [Codex MCP](https://developers.openai.com/codex/mcp)
- [Connect an MCP server to ChatGPT](https://developers.openai.com/apps-sdk/deploy/connect-chatgpt)
- [Claude MCP](https://docs.anthropic.com/en/docs/claude-code/mcp)
- [Gemini CLI MCP](https://github.com/google-gemini/gemini-cli/blob/main/docs/tools/mcp-server.md)
- [GitHub Copilot MCP](https://docs.github.com/copilot/how-tos/copilot-on-github/customize-copilot/configure-mcp-servers)

Credential-free sample files for Codex, Claude Desktop, Gemini CLI, and VS Code
with GitHub Copilot are under `docs/examples/shaft-pilot/mcp/`.

## Distribution identity

The first monorepo release preserves:

- Maven: `io.github.shafthq:SHAFT_MCP`
- MCP server name: `shaft-mcp`
- OCI image: `ghcr.io/shafthq/shaft-mcp`
- MCP registry name: `io.github.ShaftHQ/shaft-mcp`
- Java package and existing tool names

`shaft-mcp/server.json` is a release template. The publication workflow
replaces `@project.version@` from the root reactor version before registry
validation and publication.
