# Install or upgrade ChaosEngine into the current directory.
# Run this from the target project folder:
#   irm "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.ps1" | iex
[CmdletBinding()]
param(
    [switch]$ParseOnly,
    [switch]$WithMavenTools,
    [switch]$Interactive
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$script:ChaosEngineInvocationLine = [string]$MyInvocation.Line
[Console]::Error.WriteLine("  /\  CHAOSENGINE`n /  \ transparent automation")
$env:CHAOS_ENGINE_BRAND_SHOWN = "1"

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
    return $null
}

function Install-ChaosEngineUv([string]$Work) {
    $architecture = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
    if ($architecture -eq "X64") {
        $target = "x86_64-pc-windows-msvc"
        $digest = "a047d55651bc3e0ca24595b25ec4cfcb10f9dca9fb56514e661269b37d4fae68"
    }
    elseif ($architecture -eq "Arm64") {
        $target = "aarch64-pc-windows-msvc"
        $digest = "55b597ae81bc29531a7c352a1431a8a73cc2755d7a5b9ec454580cbe02e5154f"
    }
    else { throw "Unsupported ChaosEngine platform: Windows/$architecture" }
    $archive = Join-Path $Work "uv.zip"
    Invoke-WebRequest -UseBasicParsing -Uri "https://github.com/astral-sh/uv/releases/download/0.11.29/uv-$target.zip" -OutFile $archive
    $actual = (Get-FileHash -Algorithm SHA256 -LiteralPath $archive).Hash.ToLowerInvariant()
    if ($actual -ne $digest) { throw "uv download checksum verification failed" }
    Expand-Archive -LiteralPath $archive -DestinationPath $Work
    $uv = Join-Path $Work "uv.exe"
    if (-not (Test-Path -LiteralPath $uv -PathType Leaf)) { throw "uv archive is missing its executable" }
    return $uv
}

function Get-ChaosEngineHeader([object]$Headers, [string]$Name) {
    if ($null -eq $Headers) {
        return $null
    }
    $values = $null
    try {
        if ($Headers.TryGetValues($Name, [ref]$values) -and $null -ne $values) {
            return [string](@($values)[0])
        }
        return $null
    }
    catch {
        Write-Verbose "Retry-After lookup via header API failed"
    }
    try {
        $got = $Headers.GetValues($Name)
        if ($null -ne $got) {
            return [string](@($got)[0])
        }
    }
    catch {
        Write-Verbose "Retry-After lookup via header API failed"
    }
    try {
        $indexed = $Headers[$Name]
        if ($null -ne $indexed) {
            return [string]$indexed
        }
    }
    catch {
        Write-Verbose "Retry-After lookup via header API failed"
    }
    return $null
}

function ConvertFrom-ChaosEngineRawUrl([string]$Text) {
    if ([string]::IsNullOrWhiteSpace($Text)) {
        return $null
    }
    $match = [regex]::Match(
        $Text,
        'https://raw\.githubusercontent\.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)/(.+?)/install\.(ps1|sh)\b',
        [System.Text.RegularExpressions.RegexOptions]::IgnoreCase
    )
    if (-not $match.Success) {
        return $null
    }
    $repository = $match.Groups[1].Value + "/" + $match.Groups[2].Value
    if ($repository -eq "owner/repository") {
        return $null
    }
    $rest = $match.Groups[3].Value
    $parts = @($rest -split "/")
    if ($parts.Length -ge 3 -and $parts[0] -eq "refs" -and $parts[1] -in @("heads", "tags")) {
        $ref = ($parts[0..2] -join "/")
        $prefixParts = @()
        if ($parts.Length -gt 3) {
            $prefixParts = $parts[3..($parts.Length - 1)]
        }
    }
    else {
        $ref = $parts[0]
        $prefixParts = @()
        if ($parts.Length -gt 1) {
            $prefixParts = $parts[1..($parts.Length - 1)]
        }
    }
    $prefix = $prefixParts -join "/"
    $bootstrapPath = "bootstrap.py"
    if (-not [string]::IsNullOrWhiteSpace($prefix)) {
        $bootstrapPath = "$prefix/bootstrap.py"
    }
    return @{
        Repository   = $repository
        Ref          = $ref
        Prefix       = $prefix
        BootstrapUrl = "https://raw.githubusercontent.com/$repository/$ref/$bootstrapPath"
    }
}

function Get-ChaosEngineInvocationText {
    $chunks = New-Object System.Collections.Generic.List[string]
    foreach ($frame in Get-PSCallStack) {
        if ($null -ne $frame.Position -and -not [string]::IsNullOrWhiteSpace($frame.Position.Text)) {
            $chunks.Add([string]$frame.Position.Text)
        }
        if ($null -ne $frame.InvocationInfo -and -not [string]::IsNullOrWhiteSpace($frame.InvocationInfo.Line)) {
            $chunks.Add([string]$frame.InvocationInfo.Line)
        }
    }
    if (-not [string]::IsNullOrWhiteSpace($script:ChaosEngineInvocationLine)) {
        $chunks.Add($script:ChaosEngineInvocationLine)
    }
    $commandLine = [Environment]::CommandLine
    if (-not [string]::IsNullOrWhiteSpace($commandLine)) {
        $chunks.Add([string]$commandLine)
    }
    return $chunks
}

function Test-ChaosEngineSourceTree([string]$ScriptDirectory) {
    if ([string]::IsNullOrWhiteSpace($ScriptDirectory)) {
        return $false
    }
    $skill = $ScriptDirectory
    foreach ($part in "skills/chaos-engine/SKILL.md".Split("/")) {
        $skill = Join-Path $skill $part
    }
    return Test-Path -LiteralPath $skill
}

function Test-ChaosEngineRepository([string]$Repository) {
    if ([string]::IsNullOrWhiteSpace($Repository)) {
        return $false
    }
    if ($Repository -eq "owner/repository") {
        return $false
    }
    return $Repository -match '^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$'
}

function Resolve-ChaosEngineSource {
    param([string[]]$Texts)
    if ($null -eq $Texts -or $Texts.Count -eq 0) {
        $Texts = @(Get-ChaosEngineInvocationText)
    }
    foreach ($text in $Texts) {
        $parsed = ConvertFrom-ChaosEngineRawUrl $text
        if ($null -ne $parsed) {
            return $parsed
        }
    }
    $envRepository = [string]$env:CHAOS_ENGINE_REPOSITORY
    if (Test-ChaosEngineRepository $envRepository) {
        $ref = [string]$env:CHAOS_ENGINE_BRANCH
        if ([string]::IsNullOrWhiteSpace($ref)) {
            $ref = "main"
        }
        return @{
            Repository   = $envRepository
            Ref          = $ref
            Prefix       = "chaos-engine"
            BootstrapUrl = "https://raw.githubusercontent.com/$envRepository/$ref/chaos-engine/bootstrap.py"
        }
    }
    throw "Put owner/repository in the install URL (or set CHAOS_ENGINE_REPOSITORY for a local file run)."
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
            if ($null -ne $response) {
                $retryAfter = Get-ChaosEngineHeader $response.Headers "Retry-After"
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

if ($ParseOnly) {
    return
}

$interactiveRequested = $Interactive -or $env:CHAOS_ENGINE_INTERACTIVE -eq "1"

$source = Resolve-ChaosEngineSource
$repository = [string]$source.Repository
$branch = [string]$env:CHAOS_ENGINE_BRANCH
if ([string]::IsNullOrWhiteSpace($branch)) {
    $branch = [string]$source.Ref
}
if ([string]::IsNullOrWhiteSpace($branch)) {
    $branch = "main"
}
$bootstrapUrl = [string]$source.BootstrapUrl
if (-not [string]::IsNullOrWhiteSpace($env:CHAOS_ENGINE_BRANCH)) {
    $prefix = [string]$source.Prefix
    $bootstrapPath = "bootstrap.py"
    if (-not [string]::IsNullOrWhiteSpace($prefix)) {
        $bootstrapPath = "$prefix/bootstrap.py"
    }
    $bootstrapUrl = "https://raw.githubusercontent.com/$repository/$branch/$bootstrapPath"
}
$project = (Get-Location).Path
$localBootstrap = $null
if ((-not [string]::IsNullOrWhiteSpace($PSScriptRoot)) -and (Test-ChaosEngineSourceTree $PSScriptRoot)) {
    $candidate = Join-Path $PSScriptRoot "bootstrap.py"
    if (Test-Path -LiteralPath $candidate) {
        $localBootstrap = $candidate
    }
}
if ($env:CHAOS_ENGINE_RESOLVE_ONLY -eq "1") {
    $localFlag = "remote"
    if ($null -ne $localBootstrap) {
        $localFlag = "local"
    }
    Write-Output "$repository|$branch|$localFlag|$bootstrapUrl"
    return
}
$python = Get-ChaosEnginePython
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("chaos-engine-bootstrap-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $work | Out-Null
try {
    $bootstrap = Join-Path $work "bootstrap.py"
    [Console]::Error.WriteLine("Installing ChaosEngine into $project from $repository@$branch")
    if ($interactiveRequested) {
        if ([Console]::IsInputRedirected) {
            throw "interactive mode requires a usable controlling terminal"
        }
        $answer = Read-Host "Confirm Download bootstrap from $bootstrapUrl? [y/N]"
        if ($answer.ToLowerInvariant() -notin @("y", "yes")) {
            throw "ChaosEngine installation cancelled before Download bootstrap"
        }
    }
    [Console]::Error.WriteLine("Download: $bootstrapUrl")
    if ($null -ne $localBootstrap) {
        Copy-Item -LiteralPath $localBootstrap -Destination $bootstrap
    }
    else {
        $response = Read-ChaosEngineUrl $bootstrapUrl
        [System.IO.File]::WriteAllText($bootstrap, [string]$response.Content)
    }
    $arguments = @($bootstrap, "--project", $project, "--repository", $repository, "--branch", $branch)
    if ($WithMavenTools) { $arguments += "--with-maven-tools" }
    if ($interactiveRequested) { $arguments += "--interactive" }
    if ($null -eq $python) {
        $uv = Install-ChaosEngineUv $work
        $env:UV_PYTHON_INSTALL_DIR = Join-Path $work "python"
        & $uv python install 3.10 --no-progress
        if ($LASTEXITCODE -ne 0) { throw "uv-managed Python installation failed" }
        $invoke = @($uv, "run", "--no-project", "--managed-python", "--python", "3.10") + $arguments
    }
    else { $invoke = @($python) + $arguments }
    & $invoke[0] @($invoke[1..($invoke.Length - 1)])
    if ($LASTEXITCODE -ne 0) {
        throw "ChaosEngine bootstrap failed with exit $LASTEXITCODE"
    }
}
finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
