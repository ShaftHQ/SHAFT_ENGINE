# SHAFT MCP, CLI, and skills installer

Change into the target project first. Both public one-liners install from the
current working directory and fetch the current `install-shaft-mcp` bootstrapper.

Windows:

```powershell
irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.ps1" | iex
```

macOS and Linux:

```shell
curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.sh" | bash
```

Optional: `SHAFT_MCP_REPOSITORY` (default `ShaftHQ/SHAFT_ENGINE`) and
`SHAFT_MCP_INSTALLER_REF` (default `main`). Client flags stay on
`install-shaft-mcp.ps1` / `install-shaft-mcp.sh` / `install_shaft_mcp.py`.
