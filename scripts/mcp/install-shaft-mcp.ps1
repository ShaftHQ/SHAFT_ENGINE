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
    $ProgressPreference = "SilentlyContinue"

    $pythonRelease = "20260610"
    $pythonVersion = "3.13.14"
    $pythonAssets = @{
        "x86_64-pc-windows-msvc" = @{
            Url = "https://github.com/astral-sh/python-build-standalone/releases/download/20260610/cpython-3.13.14%2B20260610-x86_64-pc-windows-msvc-install_only.tar.gz"
            Sha256 = "9a77f87ec431f16e79fc7e90d9115edf187d18b64100b6f6c27189f419fd79be"
        }
        "aarch64-pc-windows-msvc" = @{
            Url = "https://github.com/astral-sh/python-build-standalone/releases/download/20260610/cpython-3.13.14%2B20260610-aarch64-pc-windows-msvc-install_only.tar.gz"
            Sha256 = "7146b88d4b8fcd2f7154f516dee051b0891536bf48a1c2fe625a540c7bc06be6"
        }
    }

    function Fail([string] $Message, [int] $Code = 1) {
        [Console]::Error.WriteLine("install-shaft-mcp: $Message")
        exit $Code
    }

    function Show-Banner {
        Write-Host @"
  ____  _   _    _    _____ _____
 / ___|| | | |  / \  |  ___|_   _|
 \___ \| |_| | / _ \ | |_    | |
  ___) |  _  |/ ___ \|  _|   | |
 |____/|_| |_/_/   \_\_|     |_|
              MCP installer
"@
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

    function File-Sha256([string] $Path) {
        $stream = [System.IO.File]::OpenRead($Path)
        try {
            $sha256 = [System.Security.Cryptography.SHA256]::Create()
            try {
                $hash = $sha256.ComputeHash($stream)
                return -join ($hash | ForEach-Object { $_.ToString("x2") })
            } finally {
                $sha256.Dispose()
            }
        } finally {
            $stream.Dispose()
        }
    }

    function Save-Url([string] $Url, [string] $OutFile) {
        New-Item -ItemType Directory -Force -Path (Split-Path -Parent $OutFile) | Out-Null
        $lastError = $null
        for ($attempt = 1; $attempt -le 3; $attempt++) {
            try {
                $curl = Get-Command "curl.exe" -ErrorAction SilentlyContinue
                if ($null -ne $curl) {
                    & $curl.Source -fL --retry 3 --progress-bar -o $OutFile $Url
                    if ($LASTEXITCODE -eq 0) {
                        return
                    }
                    throw "curl.exe exited with code $LASTEXITCODE"
                }
                Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $OutFile
                return
            } catch {
                $lastError = $_.Exception
                Start-Sleep -Seconds $attempt
            }
        }
        throw $lastError
    }

    function Test-PythonCommand([string] $Executable, [string[]] $Prefix = @()) {
        try {
            & $Executable @Prefix -c "import sys; raise SystemExit(0 if sys.version_info >= (3, 9) else 1)" 2>$null | Out-Null
            return $LASTEXITCODE -eq 0
        } catch {
            return $false
        }
    }

    function Find-Python {
        foreach ($name in @("python", "python3")) {
            $command = Get-Command $name -ErrorAction SilentlyContinue
            if ($null -ne $command -and (Test-PythonCommand $command.Source)) {
                return [pscustomobject] @{ Command = $command.Source; Prefix = @() }
            }
        }
        $launcher = Get-Command "py" -ErrorAction SilentlyContinue
        if ($null -ne $launcher -and (Test-PythonCommand $launcher.Source @("-3"))) {
            return [pscustomobject] @{ Command = $launcher.Source; Prefix = @("-3") }
        }
        return $null
    }

    function Python-Target {
        switch (([System.Runtime.InteropServices.RuntimeInformation]::ProcessArchitecture).ToString()) {
            "X64" { return "x86_64-pc-windows-msvc" }
            "Arm64" { return "aarch64-pc-windows-msvc" }
            default { Fail "Unsupported Windows architecture: $([System.Runtime.InteropServices.RuntimeInformation]::ProcessArchitecture)" 3 }
        }
    }

    function Find-PortablePython([string] $InstallRoot) {
        foreach ($name in @("python.exe", "python3.exe")) {
            foreach ($candidate in Get-ChildItem -LiteralPath $InstallRoot -Filter $name -Recurse -ErrorAction SilentlyContinue) {
                if (Test-PythonCommand $candidate.FullName) {
                    return $candidate.FullName
                }
            }
        }
        return $null
    }

    function Install-PortablePython([string] $Root) {
        $target = Python-Target
        $asset = $pythonAssets[$target]
        $installRoot = Join-Path $Root "tools\python\$pythonVersion-$pythonRelease-$target"
        $cached = if (Test-Path -LiteralPath $installRoot) { Find-PortablePython $installRoot } else { $null }
        if ($null -ne $cached) {
            return [pscustomobject] @{ Command = $cached; Prefix = @() }
        }

        $tar = Get-Command "tar" -ErrorAction SilentlyContinue
        if ($null -eq $tar) {
            Fail "Python is unavailable and tar is required to extract the portable Python runtime." 3
        }

        $download = Join-Path $Root "downloads\python-$pythonVersion-$pythonRelease-$target.tar.gz"
        Write-Host "Downloading portable Python $pythonVersion for $target..."
        Save-Url $asset.Url $download
        $actual = File-Sha256 $download
        if ($actual -ne $asset.Sha256) {
            Fail "Checksum verification failed for the portable Python runtime." 3
        }

        $temporary = Join-Path $Root "tools\python\.extract-$PID"
        Remove-Item -LiteralPath $temporary -Recurse -Force -ErrorAction SilentlyContinue
        Remove-Item -LiteralPath $installRoot -Recurse -Force -ErrorAction SilentlyContinue
        New-Item -ItemType Directory -Force -Path $temporary | Out-Null
        & $tar.Source -xzf $download -C $temporary
        if ($LASTEXITCODE -ne 0) {
            Fail "Could not extract the portable Python runtime." 3
        }
        New-Item -ItemType Directory -Force -Path (Split-Path -Parent $installRoot) | Out-Null
        Move-Item -LiteralPath $temporary -Destination $installRoot -Force
        $python = Find-PortablePython $installRoot
        if ($null -eq $python) {
            Fail "Portable Python archive did not contain a usable Python executable." 3
        }
        return [pscustomobject] @{ Command = $python; Prefix = @() }
    }

    function Resolve-PythonInstallerScript([string] $Root) {
        if (-not [string]::IsNullOrWhiteSpace($PSScriptRoot)) {
            $local = Join-Path $PSScriptRoot "install_shaft_mcp.py"
            if (Test-Path -LiteralPath $local) {
                return (Resolve-Path -LiteralPath $local).Path
            }
        }
        $ref = if ([string]::IsNullOrWhiteSpace($env:SHAFT_MCP_INSTALLER_REF)) { "main" } else { $env:SHAFT_MCP_INSTALLER_REF }
        $url = if ([string]::IsNullOrWhiteSpace($env:SHAFT_MCP_INSTALLER_PYTHON_URL)) {
            "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/$ref/scripts/mcp/install_shaft_mcp.py"
        } else {
            $env:SHAFT_MCP_INSTALLER_PYTHON_URL
        }
        $target = Join-Path $Root "scripts\install_shaft_mcp.py"
        Save-Url $url $target
        return $target
    }

    Show-Banner
    $root = Bootstrap-Root
    New-Item -ItemType Directory -Force -Path $root | Out-Null
    $python = Find-Python
    if ($null -eq $python) {
        $python = Install-PortablePython $root
    }
    $installer = Resolve-PythonInstallerScript $root

    $installerArguments = @($installer)
    if (-not [string]::IsNullOrWhiteSpace($Client)) {
        $installerArguments += @("--client", $Client)
    }
    if (-not [string]::IsNullOrWhiteSpace($Version)) {
        $installerArguments += @("--version", $Version)
    }
    if ($null -ne $Arguments -and $Arguments.Count -gt 0) {
        $installerArguments += $Arguments
    }

    $pythonArguments = @()
    if ($null -ne $python.Prefix -and $python.Prefix.Count -gt 0) {
        $pythonArguments += $python.Prefix
    }
    $pythonArguments += $installerArguments
    & $python.Command @pythonArguments
    exit $LASTEXITCODE
}

if ($PSCommandPath) {
    Install-ShaftMcp -Client $Client -Version $Version -Arguments $RemainingArguments
}
