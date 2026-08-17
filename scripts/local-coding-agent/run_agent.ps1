#Requires -Version 7
<#
.SYNOPSIS
  Dispatch the workstation local coder and write a report the orchestrator can read.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Worktree,
    [Parameter(Mandatory = $true)]
    [string] $Spec,
    [Parameter(Mandatory = $true)]
    [string[]] $Allowlist,
    [string] $TestCommand = "",
    [string] $Model = "qwen2.5-coder:7b",
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" }),
    [switch] $Push
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Resolve-RepoRoot {
    $here = $PSScriptRoot
    return (Resolve-Path (Join-Path $here "..\..")).Path
}

$repoRoot = Resolve-RepoRoot
$agentPy = Join-Path $PSScriptRoot "agent.py"
$preflightJson = $Allowlist | ConvertTo-Json -Compress
$preflightArgs = @(
    "-3", $agentPy, "preflight",
    "--worktree", $Worktree,
    "--spec", $Spec,
    "--allowlist-json", $preflightJson
)
if ($Push) { $preflightArgs += "--push" }

$preflight = & py @preflightArgs
if ($LASTEXITCODE -ne 0) {
    Write-Error $preflight
    exit $LASTEXITCODE
}

$env:OLLAMA_HOST = "127.0.0.1:11434"
$env:OLLAMA_MODELS = Join-Path $Root "ollama\models"
$env:OLLAMA_API_BASE = "http://127.0.0.1:11434"
$env:OLLAMA_ORIGINS = ""

$ollama = Join-Path $Root "ollama\ollama.exe"
$aider = Join-Path $Root "aider\.venv\Scripts\aider.exe"
$pidFile = Join-Path $Root "ollama\launcher.pid"
$startedHere = $false
$started = Get-Date

if (-not (Test-Path -LiteralPath $Worktree)) {
    Write-Error "worktree does not exist: $Worktree"
    exit 2
}
if (-not (Test-Path -LiteralPath $Spec)) {
    Write-Error "spec does not exist: $Spec"
    exit 2
}
if ($Push) {
    Write-Error "push is forbidden"
    exit 2
}

function Get-LoopbackListener {
    Get-NetTCPConnection -LocalPort 11434 -State Listen -ErrorAction SilentlyContinue |
        Where-Object { $_.LocalAddress -in @("127.0.0.1", "::1") }
}

if (-not (Get-LoopbackListener)) {
    if (-not (Test-Path -LiteralPath $ollama)) {
        Write-Error "Ollama is not installed at $ollama. See scripts/local-coding-agent/README.md"
        exit 2
    }
    $foreign = Get-NetTCPConnection -LocalPort 11434 -State Listen -ErrorAction SilentlyContinue
    if ($foreign) {
        Write-Error "port 11434 is already bound to a non-loopback or unknown listener"
        exit 2
    }
    $log = Join-Path $Root "ollama\serve.log"
    New-Item -ItemType Directory -Force -Path (Split-Path $pidFile) | Out-Null
    $info = [System.Diagnostics.ProcessStartInfo]::new()
    $info.FileName = $ollama
    $info.Arguments = "serve"
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $info.Environment["OLLAMA_HOST"] = "127.0.0.1:11434"
    $info.Environment["OLLAMA_MODELS"] = Join-Path $Root "ollama\models"
    $proc = [System.Diagnostics.Process]::Start($info)
    Set-Content -LiteralPath $pidFile -Value $proc.Id -Encoding ascii
    $startedHere = $true
    $deadline = (Get-Date).AddSeconds(30)
    while (-not (Get-LoopbackListener)) {
        if ((Get-Date) -gt $deadline) {
            Write-Error "Ollama did not bind 127.0.0.1:11434"
            exit 2
        }
        Start-Sleep -Milliseconds 250
    }
}

$runId = (Get-Date).ToUniversalTime().ToString("yyyyMMddTHHmmssZ")
$reportDir = Join-Path $Root "reports\$runId"
New-Item -ItemType Directory -Force -Path $reportDir | Out-Null
Copy-Item -LiteralPath $Spec -Destination (Join-Path $reportDir "spec.md")

$aiderArgs = @(
    "--model", "ollama_chat/$Model",
    "--edit-format", "whole",
    "--yes-always",
    "--no-gitignore",
    "--no-show-model-warnings",
    "--message-file", $Spec
)
foreach ($file in $Allowlist) {
    $aiderArgs += @("--file", $file)
}

$agentLog = Join-Path $reportDir "agent.log"
$aiderExit = 0
if (Test-Path -LiteralPath $aider) {
    Push-Location -LiteralPath $Worktree
    try {
        & $aider @aiderArgs *>&1 | Tee-Object -FilePath $agentLog
        $aiderExit = $LASTEXITCODE
    } finally {
        Pop-Location
    }
} else {
    "Aider is not installed at $aider. See scripts/local-coding-agent/README.md" | Set-Content -LiteralPath $agentLog
    $aiderExit = 2
}

Push-Location -LiteralPath $Worktree
try {
    git diff -- $Allowlist | Set-Content -LiteralPath (Join-Path $reportDir "diff.patch") -Encoding utf8
    $changed = @()
    foreach ($file in $Allowlist) {
        $status = git status --porcelain -- $file
        if ($status) { $changed += $file }
    }
    if ($changed.Count -eq 0) {
        $fromCommit = @(git show --name-only --pretty=format: HEAD)
        foreach ($file in $fromCommit) {
            $normalized = [string]$file
            if ($normalized -and ($Allowlist | Where-Object { $_ -replace '\\','/' -eq ($normalized -replace '\\','/') })) {
                $changed += $normalized
            }
        }
    }
    $commit = (git rev-parse HEAD).Trim()
} finally {
    Pop-Location
}

$testExit = 0
if ($TestCommand) {
    Push-Location -LiteralPath $Worktree
    try {
        $testLog = Join-Path $reportDir "test-output.txt"
        cmd.exe /c $TestCommand *>&1 | Tee-Object -FilePath $testLog
        $testExit = $LASTEXITCODE
        $testText = Get-Content -LiteralPath $testLog -Raw -ErrorAction SilentlyContinue
        if ($testText) {
            & py -3 $agentPy "surefire" "--file" $testLog | Out-Null
            if ($LASTEXITCODE -ne 0) {
                $testExit = 1
            }
        }
    } finally {
        Pop-Location
    }
}

$elapsed = [int]((Get-Date) - $started).TotalMilliseconds
$blockers = @()
if ($aiderExit -ne 0) { $blockers += "aider exit $aiderExit" }
if ($TestCommand -and $testExit -ne 0) { $blockers += "test exit $testExit" }

$payload = @{
    ok = ($blockers.Count -eq 0)
    model = $Model
    worktree = $Worktree
    files_allowed = @($Allowlist)
    files_changed = @($changed)
    commit = $commit
    test_command = $TestCommand
    test_exit = $testExit
    elapsed_ms = $elapsed
    loopback = "127.0.0.1:11434"
    blockers = @($blockers)
}
$reportPath = Join-Path $reportDir "report.json"
$payload | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $reportPath -Encoding utf8

& py -3 $agentPy "validate" "--report" $reportPath
if ($LASTEXITCODE -ne 0) {
    Write-Output $reportPath
    exit $LASTEXITCODE
}

Write-Output $reportPath
if ($blockers.Count -gt 0) { exit 1 }
exit 0
