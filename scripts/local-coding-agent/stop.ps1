#Requires -Version 7
<#
.SYNOPSIS
  Stop only the Ollama process this launcher started.
#>
[CmdletBinding()]
param(
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" })
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$pidFile = Join-Path $Root "ollama\launcher.pid"
if (-not (Test-Path -LiteralPath $pidFile)) {
    Write-Output "no launcher pid file; nothing to stop"
    exit 0
}

$owned = (Get-Content -LiteralPath $pidFile -ErrorAction Stop | Select-Object -First 1).Trim()
if (-not $owned) {
    Remove-Item -LiteralPath $pidFile -Force
    exit 0
}

$proc = Get-Process -Id ([int]$owned) -ErrorAction SilentlyContinue
if ($null -eq $proc) {
    Remove-Item -LiteralPath $pidFile -Force
    Write-Output "launcher pid $owned already gone"
    exit 0
}

$ollama = Join-Path $Root "ollama\ollama.exe"
if (-not $proc.Path) {
    Write-Error "pid $owned has no path; refusing to stop an unproven process"
    exit 2
}
if (-not (Test-Path -LiteralPath $ollama)) {
    Write-Error "launcher ollama.exe is missing; refusing to stop pid $owned"
    exit 2
}
if ((Resolve-Path $proc.Path).Path -ne (Resolve-Path $ollama).Path) {
    Write-Error "pid $owned is not the launcher-owned ollama.exe; refusing to stop it"
    exit 2
}

Stop-Process -Id ([int]$owned) -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 1
Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
Write-Output "stopped launcher pid $owned"
exit 0
