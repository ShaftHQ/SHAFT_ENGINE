# SHAFT Pilot examples

These examples contain no credentials and assume the executable
`SHAFT_MCP-<version>.jar` has been downloaded from Maven Central or built from
the monorepo.

## No-AI Capture to TestNG

```bash
java -jar SHAFT_MCP-<version>.jar capture start \
  --url https://example.test --browser chrome \
  --output recordings/example.json --headless
java -jar SHAFT_MCP-<version>.jar capture status
java -jar SHAFT_MCP-<version>.jar capture stop
java -jar SHAFT_MCP-<version>.jar capture generate \
  --session recordings/example.json \
  --output-dir generated-tests --replay
```

## No-AI Doctor analysis

```bash
java -jar SHAFT_MCP-<version>.jar doctor analyze \
  --input allure-results --allowed-root "$PWD" \
  --output-dir target/shaft-doctor
```

## Optional local Ollama advisory

Load `providers/ollama.properties` as SHAFT properties, start Ollama locally,
and add `--ai` to the Doctor command. The deterministic diagnosis remains the
baseline if Ollama is unavailable or returns invalid output.

## Reviewed repair proposal

Use `doctor/repair-input.json` only after replacing its paths and content with a
reviewed change. `doctor propose-fix` creates an isolated worktree and does not
publish to GitHub. Publishing requires a second command with the exact approval
token returned by the proposal.

MCP client configurations are in `mcp/`.
