# CLI playbook

Use SHAFT CLI when a deterministic command, exit code, JSON result, or shell
pipeline is the deliverable. Exact syntax lives in the
[generated CLI reference](../../references/shaft-cli-commands.md); exact tool
names live in the [generated MCP catalog](../../references/shaft-mcp-tools.md).

Assistants without MCP access (shell only) use the same tools through the
installer's stable `~/.local/bin/shaft-cli` shim: `shaft-cli tools`, then
`shaft-cli call <tool> key=value`. The installer prints a paste-ready stdio
definition for MCP hosts it did not configure.

## Ten practices

1. Prefer CLI for stateless, repeatable, scriptable work; prefer direct MCP for interactive work or state that already lives in the connected client. [`SHAFT-MCP`]
2. Use curated aliases for common intent and `shaft-cli call <tool>` for parity with every exact tool name in the generated catalog. [`SHAFT-MCP`]
3. Treat generic `call` as name dispatch only: it does not infer that browser, element, driver, mobile, or capture tools need persistent state. Start a session first for any such sequence. [`SHAFT-MCP`]
4. Run `shaft-cli session start`, confirm `session status`, execute every related stateful command, then `session stop`; one-shot child processes cannot share state. [`SHAFT-MCP`]
5. On curated stateful aliases, use `--stdio-ok` only for a deliberately disposable call; generic `call` already falls back to one-shot and does not use this flag as a state-safety gate. [`SHAFT-MCP`]
6. Supply simple inputs as `key=value`; use `--args` for nested JSON, remembering that later `key=value` inputs override matching JSON keys. [`SHAFT-MCP`]
7. Request `--json` when another program will consume the result, and gate on both exit code and returned error status. [`SHAFT-MCP`, `ALLURE-RESULTS`]
8. Use the checked-in catalogs first; use `shaft-cli tools --cached` for offline discovery and live `shaft-cli tools` only when runtime drift must be inspected. [`SHAFT-MCP`]
9. Use `shaft-cli codegen --session <recording>` for deterministic codegen from a persisted recording; specify backend and output flags when defaults are ambiguous. [`SHAFT-MCP`, `SHAFT-GUIDE`]
10. Quote JSON and shell metacharacters for the active shell, keep secrets out of arguments and logs, and preserve generated evidence paths for review. [`SHAFT-REPORTING`, `ISTQB-CTFL`]

## Valid examples

- Run `shaft-cli guide search query="browser assertions" maxResults=3 --json` in a script and fail the step on a nonzero exit code.
- Run `shaft-cli call test_code_guardrails_check --args '{"language":"java","code":"..."}' --json` for a stateless machine-readable gate.
- Start a persistent session before `shaft-cli call driver_initialize engine=web`, `shaft-cli call browser_navigate targetUrl=https://example.test`, and `shaft-cli call driver_quit`.
- Generate code with `shaft-cli codegen --session recordings/checkout.json --backend webdriver` without starting MCP state.
- `shaft-cli setup start` on Linux runs the managed Android emulator under a `systemd-run --user` cgroup capped at 8 GiB (`-Dshaft.android.emulator.memoryMaxGb=<n>`, `0` disables; #6825). `-no-snapshot-save` is not a cold boot. A snapshot or renderer wedge, and a memory-cap kill that leaves Appium alive after qemu is gone, retries once with `-no-snapshot-load`. Recycle cadence is pressure-or-wedge: only when the cap killed qemu or that wedge is detected. Do not recycle per test method, and do not recycle between long suites. No host qemu time-versus-memory measurement was available when this cadence was chosen, so a timer was not invented. An emulator that is still alive while Appium is gone is not killed. One device has one Appium session and one Maven run. Stop the Appium this agent started when the suite ends, including after a failure. Do not stop an emulator the user asked to keep. The managed lease serializes processes it owns. The same rule applies to an Appium or Maven process the lease did not start: do not start a second one beside it.

## Boundary

- If the task is exploratory, already owns an MCP connection, needs direct schema-guided calls, or must keep live state without managing a CLI daemon, route to `shaft-mcp`.
- Three-stage CLI families (#5943): `shaft design` (Design), `shaft capture` + offline `shaft codegen` (Automation), `shaft report` + `shaft doctor` (Reporting). Healing belongs to Reporting (Stage 3), not record/codegen.

## CLI usage (#6909)

- Prefer `shaft-cli call` for a stateless tool so the MCP schema is not loaded.
- Ask `shaft-cli tools --cached` before a live tool listing, and do not paste the live listing into the chat.
- Generated mobile Java taps with `driver.touch().tap(locator)` and never `tapByCoordinates`.
- Prefer the checked-in catalog over dumping `shaft-cli tools` live output into the chat.
- Inside a `shaft-cli session`, mobile taps still use a locator, not viewport coordinates.
