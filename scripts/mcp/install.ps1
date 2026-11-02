# Install SHAFT Agentic Tools (MCP, CLI, and skills) into the current directory.
# Change into the target project first:
#   irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/scripts/mcp/install.ps1" | iex
[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repository = [string]$env:SHAFT_MCP_REPOSITORY
if ([string]::IsNullOrWhiteSpace($repository)) {
    $repository = "ShaftHQ/SHAFT_ENGINE"
}
$branch = [string]$env:SHAFT_MCP_INSTALLER_REF
if ([string]::IsNullOrWhiteSpace($branch)) {
    $branch = "main"
}
$url = "https://raw.githubusercontent.com/$repository/$branch/scripts/mcp/install-shaft-agentic-tools.ps1"
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("shaft-agentic-tools-one-liner-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $work | Out-Null
try {
    $script = Join-Path $work "install-shaft-agentic-tools.ps1"
    Write-Output "Installing SHAFT Agentic Tools into $((Get-Location).Path) from $repository@$branch"
    Invoke-WebRequest -UseBasicParsing -Uri $url -OutFile $script
    & $script
    if ($LASTEXITCODE -ne 0) {
        throw "SHAFT Agentic Tools installer failed with exit $LASTEXITCODE"
    }
}
finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
