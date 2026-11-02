<#
.SYNOPSIS
  Registers CursorTouch/Windows-MCP (github.com/CursorTouch/Windows-MCP) as a
  local-scope MCP server, scoped down to only the tools this capture demo
  needs.

.DESCRIPTION
  Windows-MCP's own README states it "operates with full system access and
  can perform irreversible operations." This script never registers it at
  repo/project scope (it must never land in this repo's checked-in
  .mcp.json, shared with every SHAFT_ENGINE contributor) -- only at
  --scope local, which writes to the current user's own ~/.claude.json
  project entry.

  The --tools allow-list below is deliberately restricted to UI-Interaction,
  Desktop-Capture, and Application-Management tools:
    Click, Type, Scroll, Move, Shortcut, Wait, WaitFor, Screenshot, Snapshot, App
  Snapshot (the Windows UI Automation accessibility-tree read) is included on
  purpose -- it's why Windows-MCP was chosen over vision/pixel clicking for a
  Swing-based IDE. Explicitly EXCLUDED: PowerShell (raw shell execution),
  Registry, Process (can kill arbitrary processes), FileSystem (arbitrary
  read/write/delete), Scrape, MultiSelect/MultiEdit (bulk-edit blast
  radius), Clipboard, Notification. None of those are needed for this demo
  and all are pure downside risk.

  Idempotent: if windows-mcp is already registered at local scope, this
  script reports that and exits 0 without re-running `claude mcp add`.

.NOTES
  See tools/intellij-plugin-recording/RUNBOOK.md for the full capture
  procedure this is one step of.
#>
$serverName = 'windows-mcp'
$toolAllowList = 'Click,Type,Scroll,Move,Shortcut,Wait,WaitFor,Screenshot,Snapshot,App'

# `claude mcp get`/`claude mcp add` write plain text to stderr on failure and
# exit non-zero -- that's how this script detects "not yet installed" vs.
# "add failed". PowerShell promotes stderr output to a terminating ErrorRecord
# under $ErrorActionPreference = 'Stop', so error handling here is done by
# checking $LASTEXITCODE manually with EAP left at its default ('Continue').

& claude mcp get $serverName *> $null
if ($LASTEXITCODE -eq 0) {
    Write-Host "'$serverName' is already registered at local scope. Skipping (run 'claude mcp remove $serverName -s local' first to re-register with different flags)."
    exit 0
}

Write-Host "Registering '$serverName' at local scope with tools: $toolAllowList"
& claude mcp add $serverName --scope local -- uvx windows-mcp serve --tools $toolAllowList
if ($LASTEXITCODE -ne 0) {
    Write-Error "claude mcp add failed with exit code $LASTEXITCODE"
    exit $LASTEXITCODE
}

Write-Host "Registered. Tools are discoverable only in a NEW Claude Code session (MCP servers load at session start) -- restart before using them."
