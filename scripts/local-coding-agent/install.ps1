#Requires -Version 7
<#
.SYNOPSIS
  Install portable Ollama, isolated Aider, and qwen2.5-coder:7b under SHAFT_LOCAL_AI_ROOT.
#>
[CmdletBinding()]
param(
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" }),
    [string] $OllamaVersion = "0.32.14",
    [string] $AiderVersion = "0.86.2",
    [string] $Model = "qwen2.5-coder:7b"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ollamaDir = Join-Path $Root "ollama"
$modelsDir = Join-Path $ollamaDir "models"
$aiderDir = Join-Path $Root "aider"
$backupDir = Join-Path $Root "backups\ollama-$(Get-Date -Format 'yyyyMMddTHHmmss')"
$zipPath = Join-Path $ollamaDir "ollama-windows-amd64.zip"
$userOllama = Join-Path $env:USERPROFILE ".ollama"

New-Item -ItemType Directory -Force -Path $ollamaDir, $modelsDir, $aiderDir, (Join-Path $Root "reports") | Out-Null

if (Test-Path -LiteralPath $userOllama) {
    New-Item -ItemType Directory -Force -Path $backupDir | Out-Null
    Copy-Item -LiteralPath $userOllama -Destination (Join-Path $backupDir ".ollama") -Recurse -Force
    Write-Output "backed up $userOllama to $backupDir"
}

$release = gh api "repos/ollama/ollama/releases/tags/v$OllamaVersion" | ConvertFrom-Json
$asset = $release.assets | Where-Object { $_.name -eq "ollama-windows-amd64.zip" } | Select-Object -First 1
if (-not $asset) { throw "release asset ollama-windows-amd64.zip not found for v$OllamaVersion" }
$expected = [string]$asset.digest
Write-Output "expected digest $expected size $($asset.size)"

if (-not (Test-Path -LiteralPath $zipPath) -or (Get-Item -LiteralPath $zipPath).Length -ne $asset.size) {
    curl.exe -L --fail --output $zipPath $asset.browser_download_url
}

$actual = "sha256:" + ((Get-FileHash -LiteralPath $zipPath -Algorithm SHA256).Hash.ToLowerInvariant())
if (-not $expected -or -not $expected.StartsWith("sha256:")) {
    throw "release asset digest missing or not sha256: $expected"
}
if ($actual -ne $expected.ToLowerInvariant()) {
    throw "digest mismatch: $actual != $expected"
}

Expand-Archive -LiteralPath $zipPath -DestinationPath $ollamaDir -Force
$ollama = Join-Path $ollamaDir "ollama.exe"
if (-not (Test-Path -LiteralPath $ollama)) { throw "ollama.exe missing after unzip" }

$env:OLLAMA_HOST = "127.0.0.1:11434"
$env:OLLAMA_MODELS = $modelsDir
& uv python install 3.12
$venvPython = Join-Path $aiderDir ".venv\Scripts\python.exe"
if (-not (Test-Path -LiteralPath $venvPython)) {
    & uv venv (Join-Path $aiderDir ".venv") --python 3.12
}
& uv pip install --python $venvPython "aider-chat==$AiderVersion"

Write-Output "installed ollama=$(& $ollama --version) aider=$AiderVersion root=$Root"
Write-Output "next: start ollama serve on 127.0.0.1:11434 and pull $Model"
