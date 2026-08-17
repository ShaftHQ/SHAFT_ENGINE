#Requires -Version 7
<#
.SYNOPSIS
  Named stop: shaft-local-ai-stop. Forwards to stop.ps1.
#>
[CmdletBinding()]
param(
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" })
)

& (Join-Path $PSScriptRoot "stop.ps1") -Root $Root
exit $LASTEXITCODE
