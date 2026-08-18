# Install or upgrade ChaosEngine into the current directory.
# Run this from the target project folder:
#   $env:CHAOS_ENGINE_REPOSITORY = 'owner/repository'
#   irm "https://raw.githubusercontent.com/$env:CHAOS_ENGINE_REPOSITORY/main/chaos-engine/install.ps1" | iex
[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Get-ChaosEnginePython {
    $py = Get-Command py -ErrorAction SilentlyContinue
    if ($null -ne $py) {
        return @($py.Source, "-3")
    }
    foreach ($name in @("python3", "python")) {
        $found = Get-Command $name -ErrorAction SilentlyContinue
        if ($null -ne $found) {
            return @($found.Source)
        }
    }
    throw "Python 3 is required (py -3, python3, or python)."
}

function Read-ChaosEngineUrl([string]$Url) {
    $transient = @(408, 425, 429, 500, 502, 503, 504)
    for ($attempt = 0; $attempt -lt 4; $attempt++) {
        $delay = [Math]::Pow(2, $attempt)
        try {
            return Invoke-WebRequest -UseBasicParsing -Uri $Url -TimeoutSec 30
        }
        catch {
            $response = $_.Exception.Response
            $code = $null
            if ($null -ne $response) {
                $code = [int]$response.StatusCode
            }
            $retryAfter = $null
            if ($null -ne $response -and $null -ne $response.Headers) {
                $retryAfter = $response.Headers["Retry-After"]
            }
            $retryable = ($null -ne $code -and $transient -contains $code) -or
                ($code -eq 403 -and -not [string]::IsNullOrWhiteSpace($retryAfter))
            if (-not $retryable -or $attempt -eq 3) {
                throw
            }
            if (-not [string]::IsNullOrWhiteSpace($retryAfter)) {
                $parsed = 0.0
                if ([double]::TryParse($retryAfter, [ref]$parsed)) {
                    $delay = $parsed
                }
            }
            elseif ($code -eq 429) {
                $delay = 60
            }
            if ($delay -lt 0 -or $delay -gt 60) {
                throw
            }
            Start-Sleep -Seconds $delay
        }
    }
    throw "unable to download ChaosEngine bootstrap"
}

$repository = [string]$env:CHAOS_ENGINE_REPOSITORY
if ([string]::IsNullOrWhiteSpace($repository)) {
    throw "Set CHAOS_ENGINE_REPOSITORY to the upstream owner/repository before running this installer."
}
$branch = [string]$env:CHAOS_ENGINE_BRANCH
if ([string]::IsNullOrWhiteSpace($branch)) {
    $branch = "main"
}
$project = (Get-Location).Path
$python = Get-ChaosEnginePython
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("chaos-engine-bootstrap-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $work | Out-Null
try {
    $bootstrap = Join-Path $work "bootstrap.py"
    $url = "https://raw.githubusercontent.com/$repository/$branch/chaos-engine/bootstrap.py"
    Write-Host "Installing ChaosEngine into $project from $repository@$branch"
    $response = Read-ChaosEngineUrl $url
    [System.IO.File]::WriteAllText($bootstrap, [string]$response.Content)
    $invoke = @($python) + @(
        $bootstrap,
        "--project",
        $project,
        "--repository",
        $repository,
        "--branch",
        $branch
    )
    & $invoke[0] @($invoke[1..($invoke.Length - 1)])
    if ($LASTEXITCODE -ne 0) {
        throw "ChaosEngine bootstrap failed with exit $LASTEXITCODE"
    }
}
finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
