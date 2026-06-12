# SHAFT MCP

SHAFT MCP exposes SHAFT browser automation through the Model Context Protocol.
The module preserves the published `io.github.shafthq:SHAFT_MCP` coordinate,
`shaft-mcp` server name, existing Java packages, and tool names. New container
releases use the canonical `ghcr.io/shafthq/shaft-engine-mcp` image. The
verified `ghcr.io/shafthq/shaft-mcp:10.2.20260612` image remains available as
the standalone-repository migration alias.

## Build

Run from the SHAFT_ENGINE repository root:

```bash
mvn -pl shaft-mcp -am test
mvn -pl shaft-mcp -am package -DskipTests -Dgpg.skip
python3 scripts/ci/validate_shaft_mcp_transports.py
```

The executable is written to:

```text
shaft-mcp/target/SHAFT_MCP-<version>.jar
```

## Transports

Stdio is the default:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar
```

Stdout is reserved for MCP JSON-RPC messages. Application logs are written to
stderr.

Streamable HTTP uses the `http` profile:

```bash
java -jar shaft-mcp/target/SHAFT_MCP-<version>.jar \
  --spring.profiles.active=http
```

The endpoint is `http://localhost:8081/mcp`. Override the port with `PORT` or
`--server.port`.

## Client configuration

A generic local stdio configuration is:

```json
{
  "mcpServers": {
    "shaft-mcp": {
      "command": "java",
      "args": ["-jar", "/absolute/path/SHAFT_MCP-<version>.jar"]
    }
  }
}
```

SHAFT MCP does not need OpenAI, Anthropic, Google, Microsoft, or Ollama
credentials. The MCP client owns model authentication. SHAFT only needs
settings required for browser execution, such as `REMOTE_DRIVER_ADDRESS` when
using Selenium Grid.

See [the SHAFT MCP guide](../docs/SHAFT_MCP.md) for Codex, ChatGPT, Claude,
Gemini, Copilot, generic client, approval, and remote deployment guidance.

## Containers and hosting

Build the default image from the repository root:

```bash
docker build -f shaft-mcp/Dockerfile -t shaft-mcp .
```

Distribution and hosting assets:

- `shaft-mcp/Dockerfile`: stdio image and GHCR publication source
- `shaft-mcp/Dockerfile.smithery.build`: Smithery HTTP image
- `shaft-mcp/Dockerfile.render`: Render HTTP image
- `shaft-mcp/Dockerfile.fly`: Fly.io HTTP image
- `smithery.yaml`: root Smithery configuration
- `render.yaml`: root Render blueprint
- `shaft-mcp/fly.toml`: Fly.io configuration
- `shaft-mcp/server.json`: MCP registry release template

See [cloud deployment](SMITHERY_DEPLOYMENT.md) for operational commands.
