# Install a pinned WinAppDriver MSI without calling the GitHub Releases API.
# Appium's windows-driver release lookup lists
# https://api.github.com/repos/microsoft/winappdriver/releases unauthenticated and
# 403s under shared Actions IP rate limits (#5697).
param(
    [string]$Version = "1.2.1",
    [string]$MsiUrl = "",
    [int]$ReadyTimeoutSeconds = 60
)

$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($MsiUrl)) {
    $MsiUrl = "https://github.com/microsoft/WinAppDriver/releases/download/v$Version/WindowsApplicationDriver_$Version.msi"
}

if ($MsiUrl -match "api\.github\.com/repos/microsoft/winappdriver") {
    throw "Refusing WinAppDriver install URL that hits the GitHub Releases API: $MsiUrl"
}

$winAppDriverPath = Join-Path ${env:ProgramFiles(x86)} "Windows Application Driver\WinAppDriver.exe"
if (Test-Path $winAppDriverPath) {
    Write-Host "WinAppDriver already present at $winAppDriverPath"
    exit 0
}

$msiPath = Join-Path $env:RUNNER_TEMP "WindowsApplicationDriver_$Version.msi"
if (-not $env:RUNNER_TEMP) {
    $msiPath = Join-Path ([System.IO.Path]::GetTempPath()) "WindowsApplicationDriver_$Version.msi"
}

Write-Host "Downloading pinned WinAppDriver from $MsiUrl"
Invoke-WebRequest -Uri $MsiUrl -OutFile $msiPath

Write-Host "Installing $msiPath"
$install = Start-Process -FilePath "msiexec.exe" `
    -ArgumentList @("/i", $msiPath, "/quiet", "/norestart") `
    -Wait -PassThru
if ($install.ExitCode -ne 0 -and $install.ExitCode -ne 3010) {
    throw "msiexec failed with exit code $($install.ExitCode) while installing WinAppDriver."
}

$installed = $false
for ($i = 0; $i -lt $ReadyTimeoutSeconds; $i++) {
    if (Test-Path $winAppDriverPath) {
        $installed = $true
        break
    }
    Start-Sleep -Seconds 1
}
if (-not $installed) {
    throw "WinAppDriver did not finish installing at $winAppDriverPath after ${ReadyTimeoutSeconds}s."
}

Write-Host "WinAppDriver ready at $winAppDriverPath"
