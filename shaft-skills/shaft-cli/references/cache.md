# CLI cache

Do not run `--help` for ordinary discovery. Do not paste the 122-tool catalog into prompts. Names and summaries: [shaft-mcp-tools.md](../../references/shaft-mcp-tools.md).

## Command forms

- `shaft-cli tools --cached --group <group> --compact`
- `shaft-cli tools describe <tool> [--json]`
- `shaft-cli design ingest|analyze|gherkin|examples|lexicon|coverage|lint|gap-map|readiness|handoff`
- `shaft-cli report generate|open|summary|history|flake|tags|clusters|heal|mute|unmute|mutes`
- `shaft-cli doctor analyze|suggest|local-ai-status|cause-label`
- `shaft-cli browser navigate|screenshot|dom|url`
- `shaft-cli element click|type|hover`
- `shaft-cli capture start|stop|status|code|step-delete|step-reorder`
- `shaft-cli codegen [--session <path>]`
- `shaft-cli setup catalog|profiles [--json]`
- `shaft-cli setup doctor|status|verify --profile REPORTING [--json]`
- `shaft-cli setup plan --profile REPORTING --mode MANAGED --output <absolute-plan.json>`
- `shaft-cli setup install|apply|update --plan <absolute-plan.json> --approve <sha256:digest>`
- `shaft-cli setup start|stop --profile <profile>`
- `shaft-cli setup logs --profile REPORTING`
- `shaft-cli guide search`
- `shaft-cli session start|status|stop`
- `shaft-cli call <tool> [json]`
- `shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]`

Browser, element, capture, and driver commands need `shaft-cli session start` unless the call is a deliberately disposable one-shot. `report generate` rebuilds the Allure HTML from `--results` inside this process.

## Slash aliases are not these CLI commands

| Alias | MCP tool | CLI |
| --- | --- | --- |
| `/report` | `generate_test_report` | `shaft-cli report` |
| `/heal` | `healer_run_failed_test` | `shaft-cli report heal` |
| `/start`, `/stop`, `/status` | `capture_start`, `capture_stop`, `capture_status` | `shaft-cli session` or `shaft-cli setup` |
