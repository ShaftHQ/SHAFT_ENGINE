# Install or upgrade ChaosEngine into the current directory.
# Run this from the target project folder:
#   irm "https://raw.githubusercontent.com/<owner>/<repository>/main/chaos-engine/install.ps1" | iex
[CmdletBinding()]
param(
    [switch]$ParseOnly,
    [switch]$ProvisionPythonShim,
    [switch]$WithMavenTools,
    [ValidateSet("native", "docker")][string]$MavenToolsMode = "native",
    [switch]$Interactive,
    [switch]$WithoutMemory,
    [switch]$WithoutMempalace,
    [switch]$WithoutGraphify,
    [switch]$WithoutPonytail,
    [switch]$WithoutCaveman,
    # Optional add-ons: -With<Name> / -Without<Name> (for example -WithDesignSkills).
    # None is installed by default; bootstrap.py validates names against the manifests.
    [Parameter(ValueFromRemainingArguments = $true)][string[]]$AddOnSwitches
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$script:ChaosEngineInvocationLine = [string]$MyInvocation.Line

function ConvertTo-ChaosEngineAddOnFlags([string[]]$tokens) {
    # -WithDesignSkills -> --with-designskills; bootstrap.py maps it to the add-on name.
    $flags = @()
    foreach ($token in @($tokens | Where-Object { -not [string]::IsNullOrWhiteSpace($_) })) {
        $match = [regex]::Match($token, '^-(Without|With)([A-Za-z0-9]+)$', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
        if (-not $match.Success) {
            throw "Unknown ChaosEngine install argument: $token. Add-ons use -With<Name> or -Without<Name> (see INSTALL.md)."
        }
        $flags += "--" + $match.Groups[1].Value.ToLowerInvariant() + "-" + $match.Groups[2].Value.ToLowerInvariant()
    }
    return $flags
}


function Get-ChaosEngineManagedJdkHome {
    # Prefer an already-provisioned managed JDK when ambient JAVA_HOME is a stub
    # (Oracle javapath) or otherwise missing bin\javac.exe (#7734).
    # Never assign $home / $HOME: PowerShell StrictMode treats $HOME as read-only.
    $roots = @()
    if (-not [string]::IsNullOrWhiteSpace($env:USERPROFILE)) {
        $roots += (Join-Path $env:USERPROFILE ".local\share\chaos-engine\java")
    }
    if (-not [string]::IsNullOrWhiteSpace($env:LOCALAPPDATA)) {
        $roots += (Join-Path $env:LOCALAPPDATA "ChaosEngine\tools\temurin")
    }
    foreach ($root in $roots) {
        if (-not (Test-Path -LiteralPath $root -PathType Container)) { continue }
        $dirs = @(Get-ChildItem -LiteralPath $root -Directory -ErrorAction SilentlyContinue | Sort-Object Name -Descending)
        foreach ($dir in $dirs) {
            $homes = @($dir.FullName)
            $homes += @(Get-ChildItem -LiteralPath $dir.FullName -Directory -ErrorAction SilentlyContinue | ForEach-Object { $_.FullName })
            foreach ($candidate in $homes) {
                $java = Join-Path $candidate "bin\java.exe"
                $javac = Join-Path $candidate "bin\javac.exe"
                if ((Test-Path -LiteralPath $java -PathType Leaf) -and (Test-Path -LiteralPath $javac -PathType Leaf)) {
                    return $candidate
                }
            }
        }
    }
    return $null
}

function Assert-ChaosEngineMavenToolsJdkPreflight {
    # When JAVA_HOME is set but not a real JDK (Oracle javapath stub, JRE-only),
    # heal to a managed JDK for this process or fail closed with a clear message.
    # Runs whenever JAVA_HOME is set — including tools auto-detected later in
    # Python (#7734). Unset JAVA_HOME still defers to Python provisioning.
    # Never assign $home / $HOME: PowerShell StrictMode treats $HOME as read-only.
    param(
        [switch]$WithMavenTools,
        [string]$MavenToolsMode = "native"
    )
    if ($MavenToolsMode -eq "docker") { return }
    $jdkHome = [string]$env:JAVA_HOME
    if ([string]::IsNullOrWhiteSpace($jdkHome)) {
        if ($WithMavenTools) {
            [Console]::Error.WriteLine("ChaosEngine: JAVA_HOME unset; tools-pack JDK preflight will validate before package.")
        }
        return
    }
    $java = Join-Path $jdkHome "bin\java.exe"
    $javac = Join-Path $jdkHome "bin\javac.exe"
    $usable = $false
    if ((Test-Path -LiteralPath $java -PathType Leaf) -and (Test-Path -LiteralPath $javac -PathType Leaf)) {
        $usable = $true
        # Oracle javapath stubs expose leaf names but are reparse points / javapath homes (#7734).
        if ($jdkHome -match '(?i)[\\/]javapath$') { $usable = $false }
        foreach ($leaf in @($java, $javac)) {
            $item = Get-Item -LiteralPath $leaf -Force -ErrorAction SilentlyContinue
            if ($null -ne $item -and ($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
                $usable = $false
                break
            }
        }
    }
    if ($usable) {
        return
    }
    $managed = Get-ChaosEngineManagedJdkHome
    if (-not [string]::IsNullOrWhiteSpace($managed)) {
        $env:JAVA_HOME = $managed
        $env:Path = (Join-Path $managed "bin") + ";" + $env:Path
        [Console]::Error.WriteLine("ChaosEngine: JAVA_HOME='$jdkHome' is not a usable JDK (missing javac or Oracle javapath/reparse stub); using managed JDK at $managed for this install.")
        return
    }
    throw "CE-INSTALL-FAILED: JAVA_HOME='$jdkHome' is not a JDK with bin\java.exe and bin\javac.exe. fix-next: `$jdk='<Adoptium-JDK-25-home>'; [Environment]::SetEnvironmentVariable('JAVA_HOME',$jdk,'User'); `$env:JAVA_HOME=$jdk; `$p=[Environment]::GetEnvironmentVariable('Path','User'); if (`$p -notlike ('*'+$jdk+'\bin*')) { [Environment]::SetEnvironmentVariable('Path', (`$p.TrimEnd(';')+';'+$jdk+'\bin'),'User') }; open a new PowerShell; verify echo `$env:JAVA_HOME; java -version; javac -version; rerun install."
}

function Get-ChaosEnginePython {
    # Python 3.11 is the supported floor; an older interpreter falls back to uv-managed Python.
    $floor = "import sys; sys.exit(sys.version_info < (3, 11))"
    $py = Get-Command py -ErrorAction SilentlyContinue
    if ($null -ne $py) {
        & $py.Source -3 -c $floor 2>$null
        if ($LASTEXITCODE -eq 0) { return @($py.Source, "-3") }
    }
    foreach ($name in @("python3", "python")) {
        $found = Get-Command $name -ErrorAction SilentlyContinue
        if ($null -ne $found) {
            & $found.Source -c $floor 2>$null
            if ($LASTEXITCODE -eq 0) { return @($found.Source) }
        }
    }
    return $null
}

function Install-ChaosEngineUv([string]$Work) {
    $architecture = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
    if ($architecture -eq "X64") {
        $target = "x86_64-pc-windows-msvc"
        $digest = "0cdc5b38fe5a8168ed6a25814f5ba43c21d313dd3187314fccb37b7cec46e021"
    }
    elseif ($architecture -eq "Arm64") {
        $target = "aarch64-pc-windows-msvc"
        $digest = "505d9e65e7692bac3ddf98c66754aa26c5050c409c184af30a881a0a72cc85e3"
    }
    else { throw "Unsupported ChaosEngine platform: Windows/$architecture" }
    $archive = Join-Path $Work "uv.zip"
    Invoke-WebRequest -UseBasicParsing -Uri "https://github.com/astral-sh/uv/releases/download/0.13.7/uv-$target.zip" -OutFile $archive
    $actual = (Get-FileHash -Algorithm SHA256 -LiteralPath $archive).Hash.ToLowerInvariant()
    if ($actual -ne $digest) { throw "uv download checksum verification failed" }
    Expand-Archive -LiteralPath $archive -DestinationPath $Work
    $uv = Join-Path $Work "uv.exe"
    if (-not (Test-Path -LiteralPath $uv -PathType Leaf)) { throw "uv archive is missing its executable" }
    return $uv
}

function Install-ChaosEnginePyShim([string]$Interpreter, [string]$ShimDir) {
    # The launcher is embedded in this script. A fresh adopter project has no
    # source checkout and no separate shim file to copy.
    if ([string]::IsNullOrWhiteSpace($Interpreter)) { throw "py shim interpreter is missing" }
    if ([string]::IsNullOrWhiteSpace($ShimDir)) { throw "py shim directory is missing" }
    New-Item -ItemType Directory -Force -Path $ShimDir | Out-Null
    $quoted = $Interpreter.Replace('"', '""')
    $shimPy = Join-Path $ShimDir "windows_py_shim.py"
    $shimQuoted = $shimPy.Replace('"', '""')
    # SHIM-PY-START
    $shimSource = @'
import os
import sys

args = sys.argv[1:]
if not args:
    sys.stderr.write("py shim missing interpreter\n")
    raise SystemExit(2)
interpreter = args[0]
user = args[1:]
if user and user[0] == "-3":
    user = user[1:]
if os.name == "nt":
    import subprocess
    raise SystemExit(subprocess.run([interpreter, *user]).returncode)
os.execv(interpreter, [interpreter, *user])
'@
    # SHIM-PY-END
    Set-Content -LiteralPath $shimPy -Value $shimSource -Encoding ascii
    # %* stays the original argument list. -3 is an argument of the shim, not of CPython.
    @"
@echo off
"$quoted" "$shimQuoted" "$quoted" %*
"@ | Set-Content -LiteralPath (Join-Path $ShimDir "py.cmd") -Encoding ascii
    @"
@echo off
"$quoted" %*
"@ | Set-Content -LiteralPath (Join-Path $ShimDir "python.cmd") -Encoding ascii
}

function Install-ChaosEngineManagedPython([string]$Uv) {
    # #7766: provision Python 3.11+ and a py shim before any lifecycle or download prompt.
    $env:UV_PYTHON_INSTALL_DIR = Join-Path $env:LOCALAPPDATA "chaos-engine\python"
    & $Uv python install 3.14 --no-progress
    if ($LASTEXITCODE -ne 0) { throw "uv-managed Python installation failed" }
    $interpreter = ((& $Uv python find 3.14) | Out-String).Trim()
    if ([string]::IsNullOrWhiteSpace($interpreter)) { throw "uv-managed Python was installed but not found" }
    $shimDir = Join-Path $env:LOCALAPPDATA "chaos-engine\bin"
    Install-ChaosEnginePyShim $interpreter $shimDir
    if ($env:Path -notlike "*${shimDir}*") { $env:Path = "$shimDir;" + $env:Path }
    $userPath = [Environment]::GetEnvironmentVariable("Path", "User")
    if ($null -eq $userPath) { $userPath = "" }
    if ($userPath -notlike "*${shimDir}*") {
        if ([string]::IsNullOrWhiteSpace($userPath)) { $joined = $shimDir }
        else { $joined = $userPath.TrimEnd(';') + ';' + $shimDir }
        [Environment]::SetEnvironmentVariable("Path", $joined, "User")
    }
    $pointerDir = Join-Path $project ".chaos-engine-state"
    New-Item -ItemType Directory -Force -Path $pointerDir | Out-Null
    Set-Content -LiteralPath (Join-Path $pointerDir "hook-python") -Value $interpreter -Encoding ascii
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
    # A ref may contain '/' (feature/x, refs/heads/feature/x). The installer lives
    # either in the nested chaos-engine/ folder or at the repository root, so the
    # ref is everything before that folder (#6495).
    $rest = $match.Groups[3].Value
    if ($rest.EndsWith("/chaos-engine", [System.StringComparison]::Ordinal)) {
        $ref = $rest.Substring(0, $rest.Length - "/chaos-engine".Length)
        $prefix = "chaos-engine"
    }
    else {
        $ref = $rest
        $prefix = ""
    }
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
    throw "Could not derive owner/repository from the install URL. Pipe the raw.githubusercontent.com/<owner>/<repository>/.../install.ps1 URL through irm|iex (see chaos-engine/INSTALL.md), or set CHAOS_ENGINE_REPOSITORY=<owner>/<repository> for a local file run."
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
    throw "Unable to download ChaosEngine bootstrap (network or URL). Check connectivity, then rerun the irm one-liner."
}

if ($ParseOnly) {
    return
}
if ($ProvisionPythonShim) {
    Install-ChaosEnginePyShim ([string]$env:CHAOS_ENGINE_SHIM_INTERPRETER) ([string]$env:CHAOS_ENGINE_SHIM_DIR)
    return
}

$interactiveRequested = $Interactive

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
Assert-ChaosEngineMavenToolsJdkPreflight -WithMavenTools:$WithMavenTools -MavenToolsMode $MavenToolsMode
$python = Get-ChaosEnginePython
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("chaos-engine-bootstrap-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $work | Out-Null
try {
    $bootstrap = Join-Path $work "bootstrap.py"
    [Console]::Error.WriteLine("Installing ChaosEngine into $project from $repository@$branch")
    # #7766: locate or provision Python 3.11+ before the download/lifecycle prompt.
    $managedUv = $null
    if ($null -eq $python) {
        $managedUv = Install-ChaosEngineUv $work
        Install-ChaosEngineManagedPython $managedUv
    }
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
    if ($MavenToolsMode -eq "docker") { $arguments += @("--maven-tools-mode", "docker") }
    if ($interactiveRequested) { $arguments += "--interactive" }
    # -Verbose is the CmdletBinding common parameter; irm|iex callers use CHAOS_ENGINE_VERBOSE=1.
    if ($PSBoundParameters.ContainsKey("Verbose")) { $arguments += "--verbose" }
    if ($WithoutMemory) { $arguments += "--without-memory" }
    if ($WithoutMempalace) { $arguments += "--without-mempalace" }
    if ($WithoutGraphify) { $arguments += "--without-graphify" }
    if ($WithoutPonytail) { $arguments += "--without-ponytail" }
    if ($WithoutCaveman) { $arguments += "--without-caveman" }
    $arguments += @(ConvertTo-ChaosEngineAddOnFlags $AddOnSwitches)
    if ($null -ne $managedUv) {
        $invoke = @($managedUv, "run", "--no-project", "--managed-python") + $arguments
    }
    else { $invoke = @($python) + $arguments }
    & $invoke[0] @($invoke[1..($invoke.Length - 1)])
    if ($LASTEXITCODE -ne 0) {
        $cause = $null
        $trace = Join-Path $project ".chaos-engine-state\install-trace.json"
        if (Test-Path -LiteralPath $trace) {
            try {
                $payload = Get-Content -LiteralPath $trace -Raw | ConvertFrom-Json
                $cause = [string]$payload.result.cause
            }
            catch {
                $cause = $null
            }
        }
        if (-not [string]::IsNullOrWhiteSpace($cause)) {
            throw "ChaosEngine bootstrap failed with exit ${LASTEXITCODE}: $cause"
        }
        throw "ChaosEngine bootstrap failed with exit $LASTEXITCODE"
    }
}
finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
