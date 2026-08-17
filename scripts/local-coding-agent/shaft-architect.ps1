#Requires -Version 7
<#
.SYNOPSIS
  Named read-only architect: shaft-architect. Aider --dry-run, no edits or commits.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Worktree,
    [Parameter(Mandatory = $true)]
    [string] $Spec,
    [string] $Model = "qwen2.5-coder:7b",
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" }),
    [switch] $Push
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($Push) {
    Write-Error "push is forbidden"
    exit 2
}
if (-not (Test-Path -LiteralPath $Worktree)) {
    Write-Error "worktree does not exist: $Worktree"
    exit 2
}
if (-not (Test-Path -LiteralPath $Spec)) {
    Write-Error "spec does not exist: $Spec"
    exit 2
}

$env:OLLAMA_HOST = "127.0.0.1:11434"
$env:OLLAMA_MODELS = Join-Path $Root "ollama\models"
$env:OLLAMA_API_BASE = "http://127.0.0.1:11434"
$ollama = Join-Path $Root "ollama\ollama.exe"
$aider = Join-Path $Root "aider\.venv\Scripts\aider.exe"
$pidFile = Join-Path $Root "ollama\launcher.pid"

function Get-LoopbackListener {
    Get-NetTCPConnection -LocalPort 11434 -State Listen -ErrorAction SilentlyContinue |
        Where-Object { $_.LocalAddress -in @("127.0.0.1", "::1") }
}

if (-not (Get-LoopbackListener)) {
    if (-not (Test-Path -LiteralPath $ollama)) {
        Write-Error "Ollama is not installed at $ollama. See issue #5017."
        exit 2
    }
    $foreign = Get-NetTCPConnection -LocalPort 11434 -State Listen -ErrorAction SilentlyContinue
    if ($foreign) {
        Write-Error "port 11434 is already bound to a non-loopback or unknown listener"
        exit 2
    }
    New-Item -ItemType Directory -Force -Path (Split-Path $pidFile) | Out-Null
    $info = [System.Diagnostics.ProcessStartInfo]::new()
    $info.FileName = $ollama
    $info.Arguments = "serve"
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.RedirectStandardOutput = $false
    $info.RedirectStandardError = $false
    $info.Environment["OLLAMA_HOST"] = "127.0.0.1:11434"
    $info.Environment["OLLAMA_MODELS"] = Join-Path $Root "ollama\models"
    $proc = [System.Diagnostics.Process]::Start($info)
    Set-Content -LiteralPath $pidFile -Value $proc.Id -Encoding ascii
    $deadline = (Get-Date).AddSeconds(30)
    while (-not (Get-LoopbackListener)) {
        if ((Get-Date) -gt $deadline) {
            Write-Error "Ollama did not bind 127.0.0.1:11434"
            exit 2
        }
        Start-Sleep -Milliseconds 250
    }
}

$agents = Join-Path $Worktree "AGENTS.md"
$aiderArgs = @(
    "--model", "ollama_chat/$Model",
    "--edit-format", "whole",
    "--yes-always",
    "--dry-run",
    "--no-auto-commits",
    "--no-suggest-shell-commands",
    "--no-show-model-warnings",
    "--message-file", $Spec
)
if (Test-Path -LiteralPath $agents) {
    $aiderArgs += @("--read", $agents)
}

$before = ""
Push-Location -LiteralPath $Worktree
try {
    $before = (git status --porcelain)
    if (-not (Test-Path -LiteralPath $aider)) {
        Write-Error "Aider is not installed at $aider. See issue #5017."
        exit 2
    }
    & $aider @aiderArgs
    $aiderExit = $LASTEXITCODE
    $after = (git status --porcelain)
    if ("$after" -ne "$before") {
        Write-Error "architect run changed the worktree; read-only contract failed"
        exit 2
    }
} finally {
    Pop-Location
}
if ($aiderExit -ne 0) { exit $aiderExit }
exit 0
