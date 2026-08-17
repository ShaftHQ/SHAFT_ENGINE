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
try {
    if ($proc.Path -and $ollama -and ((Resolve-Path $proc.Path).Path -ne (Resolve-Path $ollama).Path)) {
        Write-Error "pid $owned is not the launcher-owned ollama.exe; refusing to stop it"
        exit 2
    }
} catch {
    # Path compare can fail if the process has already exited.
}

Stop-Process -Id ([int]$owned) -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 1
Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
Write-Output "stopped launcher pid $owned"
exit 0
