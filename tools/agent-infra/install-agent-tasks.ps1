# Registers the user-level Windows Scheduled Task that keeps the shared
# graphify repository-map cache fresh, pointing at the source-controlled
# script in this directory. Idempotent: re-running updates the registration
# in place.
#
# Windows only.
#
# No elevation required: a DAILY user task is user-level (unlike ONSTART).

$ErrorActionPreference = 'Stop'
$here = $PSScriptRoot

New-Item -ItemType Directory -Force "$env:USERPROFILE\.agent-infra\logs" | Out-Null

schtasks /Create /TN graphify-refresh /TR "`"$here\graphify-refresh.cmd`"" /SC DAILY /ST 05:00 /F

Write-Output "Registered graphify-refresh (daily 05:00)."
Write-Output "Verify: schtasks /Query /TN graphify-refresh; logs in $env:USERPROFILE\.agent-infra\logs\"
