#requires -Version 5.1
[CmdletBinding()]
param(
    [string] $Client,
    [string] $Version = $env:SHAFT_MCP_VERSION,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $RemainingArguments = @()
)

function Install-ShaftMcp {
    [CmdletBinding()]
    param(
        [string] $Client,
        [string] $Version = $env:SHAFT_MCP_VERSION,
        [string[]] $Arguments = @()
    )

    Set-StrictMode -Version Latest

    $ErrorActionPreference = "Stop"
    $mavenVersion = "3.9.12"
    $dependencyGoal = "org.apache.maven.plugins:maven-dependency-plugin:3.9.0:copy"
    $artifact = "io.github.shafthq:shaft-mcp"
    $targets = @{
        "codex" = "--codex"
        "codex-app" = "--codex-app"
        "claude" = "--claude"
        "claude-desktop" = "--claude-desktop"
        "copilot" = "--copilot"
        "copilot-vscode" = "--copilot-vscode"
    }

    function Fail([string] $Message, [int] $Code = 1) {
        [Console]::Error.WriteLine("install-shaft-mcp: $Message")
        exit $Code
    }

    function Debug-Log([string] $Message) {
        if ($env:SHAFT_MCP_DEBUG -eq "1") {
            Write-Host "install-shaft-mcp debug: $Message"
        }
    }

    function Use-Client([string] $NamedClient, [string[]] $Remaining) {
        if ([string]::IsNullOrWhiteSpace($NamedClient) -and $Remaining.Count -eq 1) {
            $candidate = $Remaining[0]
            if ($candidate.StartsWith("--")) {
                $candidate = $candidate.Substring(2)
            }
            $NamedClient = $candidate
        }
        if ([string]::IsNullOrWhiteSpace($NamedClient) -or -not $targets.ContainsKey($NamedClient)) {
            Fail "Usage: install-shaft-mcp.ps1 -Client <codex|codex-app|claude|claude-desktop|copilot|copilot-vscode>" 2
        }
        return $NamedClient
    }

    function Bootstrap-Root {
        if (-not [string]::IsNullOrWhiteSpace($env:SHAFT_MCP_BOOTSTRAP_HOME)) {
            return [System.IO.Path]::GetFullPath($env:SHAFT_MCP_BOOTSTRAP_HOME)
        }
        $base = $env:LOCALAPPDATA
        if ([string]::IsNullOrWhiteSpace($base)) {
            $base = Join-Path $HOME "AppData\Local"
        }
        return (Join-Path $base "ShaftHQ\shaft-mcp\bootstrap")
    }

    function Save-Url([string] $Url, [string] $OutFile) {
        New-Item -ItemType Directory -Force -Path (Split-Path -Parent $OutFile) | Out-Null
        Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $OutFile
    }

    function Java-Feature([string] $Java) {
        $previousErrorActionPreference = $ErrorActionPreference
        $ErrorActionPreference = "Continue"
        try {
            $output = & $Java -version 2>&1 | Out-String
            $exitCode = $LASTEXITCODE
        } catch {
            return $null
        } finally {
            $ErrorActionPreference = $previousErrorActionPreference
        }
        if ($exitCode -ne 0) {
            return $null
        }
        $raw = $null
        if ($output -match 'version "([^"]+)"') {
            $raw = $Matches[1]
        } elseif ($output -match 'openjdk\s+([0-9][^\s]*)') {
            $raw = $Matches[1]
        }
        if ([string]::IsNullOrWhiteSpace($raw)) {
            return $null
        }
        if ($raw.StartsWith("1.")) {
            return [int]($raw.Split(".")[1])
        }
        return [int]($raw -replace "[._-].*$", "")
    }

    function Java-25([string] $Java) {
        return (Java-Feature $Java) -eq 25
    }

    function Java-Command-Path([string] $Name) {
        $command = Get-Command $Name -ErrorAction SilentlyContinue
        if ($null -eq $command) {
            return $null
        }
        return $command.Source
    }

    function Cached-Java([string] $Root) {
        if (-not (Test-Path -LiteralPath $Root)) {
            return $null
        }
        foreach ($candidate in Get-ChildItem -LiteralPath $Root -Filter "java.exe" -Recurse -ErrorAction SilentlyContinue) {
            if ($candidate.FullName -notmatch "\\bin\\java\.exe$") {
                continue
            }
            $feature = Java-Feature $candidate.FullName
            Debug-Log "Java candidate $($candidate.FullName) has feature version $feature."
            if ($feature -eq 25) {
                return $candidate.FullName
            }
        }
        return $null
    }

    function Architecture {
        switch (([System.Runtime.InteropServices.RuntimeInformation]::ProcessArchitecture).ToString()) {
            "X64" {
                return "x64"
            }
            "Arm64" {
                return "aarch64"
            }
            default {
                Fail "Unsupported Windows architecture: $([System.Runtime.InteropServices.RuntimeInformation]::ProcessArchitecture)" 3
            }
        }
    }

    function Download-Java25([string] $Root) {
        $arch = Architecture
        $javaRoot = Join-Path $Root "tools\jdk\temurin-25-windows-$arch"
        $cached = Cached-Java $javaRoot
        if ($null -ne $cached) {
            return $cached
        }
        $archive = Join-Path $Root "downloads\temurin-jdk-25-windows-$arch.zip"
        $url = "https://api.adoptium.net/v3/binary/latest/25/ga/windows/$arch/jdk/hotspot/normal/eclipse"
        Write-Host "Downloading Java 25 for Windows $arch..."
        Remove-Item -LiteralPath $javaRoot -Recurse -Force -ErrorAction SilentlyContinue
        New-Item -ItemType Directory -Force -Path $javaRoot | Out-Null
        Save-Url $url $archive
        Expand-Archive -Path $archive -DestinationPath $javaRoot -Force
        $java = Cached-Java $javaRoot
        if ($null -eq $java) {
            Fail "Downloaded Java archive did not contain bin\java.exe." 3
        }
        return $java
    }

    function Java-Home([string] $Java) {
        $javaPath = (Resolve-Path -LiteralPath $Java).Path
        return (Resolve-Path -LiteralPath (Join-Path (Split-Path -Parent $javaPath) "..")).Path
    }

    function Get-Java25([string] $Root) {
        $force = $env:SHAFT_MCP_FORCE_BOOTSTRAP_JAVA -eq "1"
        if (-not $force -and -not [string]::IsNullOrWhiteSpace($env:JAVA_HOME)) {
            $javaHomeCandidate = Join-Path $env:JAVA_HOME "bin\java.exe"
            Debug-Log "Checking JAVA_HOME Java candidate $javaHomeCandidate."
            if ((Test-Path -LiteralPath $javaHomeCandidate) -and (Java-25 $javaHomeCandidate)) {
                return (Resolve-Path -LiteralPath $javaHomeCandidate).Path
            }
        }
        if (-not $force) {
            $pathJava = Java-Command-Path "java.exe"
            if ($null -eq $pathJava) {
                $pathJava = Java-Command-Path "java"
            }
            Debug-Log "Checking PATH Java candidate $pathJava."
            if ($null -ne $pathJava -and (Java-25 $pathJava)) {
                return $pathJava
            }
        }
        $cached = Cached-Java (Join-Path $Root "tools\jdk")
        if ($null -ne $cached -and -not $force) {
            return $cached
        }
        return Download-Java25 $Root
    }

    function Maven-OK([string] $Maven) {
        $previousErrorActionPreference = $ErrorActionPreference
        $ErrorActionPreference = "Continue"
        try {
            $output = & $Maven --version 2>&1 | Out-String
            $exitCode = $LASTEXITCODE
        } catch {
            return $false
        } finally {
            $ErrorActionPreference = $previousErrorActionPreference
        }
        if ($exitCode -ne 0 -or $output -notmatch "Apache Maven\s+(\d+)\.(\d+)") {
            return $false
        }
        $major = [int]$Matches[1]
        $minor = [int]$Matches[2]
        return $major -gt 3 -or ($major -eq 3 -and $minor -ge 9)
    }

    function File-Sha512([string] $Path) {
        $stream = [System.IO.File]::OpenRead($Path)
        try {
            $sha512 = [System.Security.Cryptography.SHA512]::Create()
            try {
                $hash = $sha512.ComputeHash($stream)
                return -join ($hash | ForEach-Object { $_.ToString("x2") })
            } finally {
                $sha512.Dispose()
            }
        } finally {
            $stream.Dispose()
        }
    }

    function Verify-Sha512([string] $Archive, [string] $Checksum) {
        $expected = ((Get-Content -Raw -Path $Checksum).Trim() -split "\s+")[0].ToLowerInvariant()
        $actual = File-Sha512 $Archive
        if ($actual -ne $expected) {
            Fail "Checksum verification failed for $Archive." 3
        }
    }

    function Download-Maven([string] $Root) {
        $mavenHome = Join-Path $Root "tools\maven\apache-maven-$mavenVersion"
        $maven = Join-Path $mavenHome "bin\mvn.cmd"
        if ((Test-Path -LiteralPath $maven) -and (Maven-OK $maven)) {
            return $maven
        }
        $archive = Join-Path $Root "downloads\apache-maven-$mavenVersion-bin.zip"
        $checksum = "$archive.sha512"
        $url = "https://archive.apache.org/dist/maven/maven-3/$mavenVersion/binaries/apache-maven-$mavenVersion-bin.zip"
        Write-Host "Downloading Apache Maven $mavenVersion..."
        Save-Url $url $archive
        Save-Url "$url.sha512" $checksum
        Verify-Sha512 $archive $checksum
        Remove-Item -LiteralPath $mavenHome -Recurse -Force -ErrorAction SilentlyContinue
        New-Item -ItemType Directory -Force -Path (Split-Path -Parent $mavenHome) | Out-Null
        Expand-Archive -Path $archive -DestinationPath (Split-Path -Parent $mavenHome) -Force
        if (-not (Maven-OK $maven)) {
            Fail "Downloaded Maven $mavenVersion is not executable." 3
        }
        return $maven
    }

    function Get-Maven([string] $Root) {
        if ($env:SHAFT_MCP_FORCE_BOOTSTRAP_MAVEN -ne "1") {
            foreach ($name in @("mvn.cmd", "mvn")) {
                $candidate = Java-Command-Path $name
                if ($null -ne $candidate -and (Maven-OK $candidate)) {
                    return $candidate
                }
            }
        }
        return Download-Maven $Root
    }

    $Client = Use-Client $Client $Arguments
    if ([string]::IsNullOrWhiteSpace($Version)) {
        $Version = "LATEST"
    }

    $root = Bootstrap-Root
    New-Item -ItemType Directory -Force -Path $root | Out-Null

    $java = Get-Java25 $root
    $javaHome = Java-Home $java
    $env:JAVA_HOME = $javaHome
    $env:PATH = "$(Split-Path -Parent $java);$env:PATH"

    $maven = Get-Maven $root
    $env:PATH = "$(Split-Path -Parent $maven);$env:PATH"

    $jarDirectory = Join-Path ([System.IO.Path]::GetTempPath()) "shaft-mcp-bootstrap"
    $workDirectory = Join-Path $root "work"
    New-Item -ItemType Directory -Force -Path $jarDirectory, $workDirectory | Out-Null

    $mavenArguments = @(
        "--batch-mode",
        "--no-transfer-progress",
        "-U",
        "-N",
        $dependencyGoal,
        "-Dartifact=${artifact}:$Version",
        "-DoutputDirectory=$jarDirectory",
        "-Dmdep.stripVersion=true",
        "-Dmdep.overWriteReleases=true"
    )

    Write-Host "Resolving ${artifact}:$Version..."
    Push-Location $workDirectory
    try {
        & $maven @mavenArguments
        if ($LASTEXITCODE -ne 0) {
            exit $LASTEXITCODE
        }
    } finally {
        Pop-Location
    }

    $jar = Join-Path $jarDirectory "shaft-mcp.jar"
    if (-not (Test-Path -LiteralPath $jar)) {
        Fail "Maven did not copy shaft-mcp.jar into $jarDirectory." 4
    }

    Write-Host "Configuring shaft-mcp for $Client..."
    & $java -jar $jar install $targets[$Client]
    exit $LASTEXITCODE
}

if ($PSCommandPath) {
    try {
        Install-ShaftMcp -Client $Client -Version $Version -Arguments $RemainingArguments
    } catch {
        [Console]::Error.WriteLine("install-shaft-mcp: $($_.Exception.Message)")
        exit 1
    }
}
