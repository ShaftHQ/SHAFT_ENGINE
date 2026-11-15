# ChaosEngine moved to https://github.com/ShaftHQ/chaosengine-harness.
# This shim keeps the old one-liner working. The harness installer runs in a
# child process so this process command line is not read as a source URL.
[Console]::Error.WriteLine("ChaosEngine moved to ShaftHQ/chaosengine-harness; forwarding.")
$ErrorActionPreference = "Stop"
$temp = Join-Path ([System.IO.Path]::GetTempPath()) ("chaosengine-install-" + [guid]::NewGuid().ToString("n") + ".ps1")
try {
    Invoke-WebRequest -UseBasicParsing -Uri "https://raw.githubusercontent.com/ShaftHQ/chaosengine-harness/main/install.ps1" -OutFile $temp
    $exe = (Get-Process -Id $PID).Path
    & $exe -NoProfile -File $temp @args
    exit $LASTEXITCODE
} finally {
    if (Test-Path -LiteralPath $temp) {
        Remove-Item -LiteralPath $temp -Force -ErrorAction SilentlyContinue
    }
}
