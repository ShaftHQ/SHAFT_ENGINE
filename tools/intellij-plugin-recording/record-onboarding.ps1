$ErrorActionPreference = 'Stop'
Set-Location (Resolve-Path "$PSScriptRoot\..\..");
$gradle = if (Test-Path '.\gradlew.bat') { '.\gradlew.bat' } elseif (Test-Path '.\gradlew') { '.\gradlew' } else { 'gradle' }
if ($env:OS -eq 'Windows_NT') {
    if ([string]::IsNullOrWhiteSpace($env:JAVA_HOME)) {
        Write-Error "JAVA_HOME is not set. Set JAVA_HOME to your installed JDK path and re-run this script."
        exit 1
    }
    if (-not (Test-Path -LiteralPath $env:JAVA_HOME -PathType Container)) {
        Write-Error "JAVA_HOME path is invalid: '$($env:JAVA_HOME)'. Set JAVA_HOME to a valid JDK directory."
        exit 1
    }
    $packages = Join-Path $env:JAVA_HOME 'Packages'
    try {
        if (-not (Test-Path -LiteralPath $packages)) {
            New-Item -ItemType Directory -Path $packages -Force -ErrorAction Stop | Out-Null
        }
    } catch {
        Write-Error "Failed to create JAVA_HOME\\Packages at '$packages'. Ensure write permissions are available."
        Write-Error $_.Exception.Message
        exit 1
    }
    try {
        $probePath = Join-Path $packages 'shaft-intellij-preflight-probe'
        Set-Content -Path $probePath -Value '' -ErrorAction Stop
        Remove-Item -Path $probePath -ErrorAction Stop
    } catch {
        Write-Error "JAVA_HOME\\Packages is not writable: '$packages'. Run this shell as a user with write access to JAVA_HOME."
        Write-Error $_.Exception.Message
        exit 1
    }
}
& $gradle -p shaft-intellij check buildPlugin verifyPlugin
