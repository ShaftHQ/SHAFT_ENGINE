param(
    [string] $Client,
    [string] $Version = $(if ([string]::IsNullOrWhiteSpace($env:SHAFT_MCP_VERSION)) { "" } else { $env:SHAFT_MCP_VERSION.Trim() }),
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $RemainingArguments = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

Write-Host "install-shaft-mcp.ps1 is deprecated; delegating to install-shaft-agentic-tools.ps1"

function Invoke-AgenticToolsInstaller([string] $ScriptPath) {
    $forward = @{}
    if (-not [string]::IsNullOrWhiteSpace($Client)) {
        $forward["Client"] = $Client
    }
    if (-not [string]::IsNullOrWhiteSpace($Version)) {
        $forward["Version"] = $Version
    }
    if ($RemainingArguments -and $RemainingArguments.Count -gt 0) {
        & $ScriptPath @forward @RemainingArguments
    } else {
        & $ScriptPath @forward
    }
}

$local = Join-Path $PSScriptRoot "install-shaft-agentic-tools.ps1"
if ((-not [string]::IsNullOrWhiteSpace($PSScriptRoot)) -and (Test-Path -LiteralPath $local)) {
    Invoke-AgenticToolsInstaller $local
    exit $LASTEXITCODE
}

$repository = [string]$env:SHAFT_MCP_REPOSITORY
if ([string]::IsNullOrWhiteSpace($repository)) {
    $repository = "ShaftHQ/SHAFT_ENGINE"
}
$branch = [string]$env:SHAFT_MCP_INSTALLER_REF
if ([string]::IsNullOrWhiteSpace($branch)) {
    $branch = "main"
}
$url = "https://raw.githubusercontent.com/$repository/$branch/scripts/mcp/install-shaft-agentic-tools.ps1"
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("shaft-agentic-tools-shim-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $work | Out-Null
try {
    $script = Join-Path $work "install-shaft-agentic-tools.ps1"
    Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $script
    Invoke-AgenticToolsInstaller $script
    if ($LASTEXITCODE -ne 0) {
        throw "SHAFT Agentic Tools installer failed with exit $LASTEXITCODE"
    }
} finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
