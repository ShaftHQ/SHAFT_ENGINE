# Registers the user-level Windows Scheduled Tasks that keep the local agent
# infrastructure fresh, pointing at the source-controlled scripts in this
# directory. Idempotent: re-running updates the registrations in place.
#
# Windows only. On macOS/Linux use `gbrain autopilot --install --repo <checkout>`
# instead (launchd/systemd). Full runbook: docs-site maintainers/agent-tooling.
#
# No elevation required: MINUTE/DAILY user tasks are user-level (unlike ONSTART).

$ErrorActionPreference = 'Stop'
$here = $PSScriptRoot

New-Item -ItemType Directory -Force "$env:USERPROFILE\.gbrain\autopilot" | Out-Null

schtasks /Create /TN gbrain-autosync /TR "`"$here\gbrain-autosync.cmd`"" /SC MINUTE /MO 30 /F
schtasks /Create /TN gbrain-dream   /TR "`"$here\gbrain-dream.cmd`""   /SC DAILY /ST 05:00 /F

Write-Output "Registered gbrain-autosync (every 30 min) and gbrain-dream (daily 05:00)."
Write-Output "Verify: schtasks /Query /TN gbrain-autosync; logs in $env:USERPROFILE\.gbrain\autopilot\"
