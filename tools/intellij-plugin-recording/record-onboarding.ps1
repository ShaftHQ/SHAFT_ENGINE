$ErrorActionPreference = 'Stop'
Set-Location (Resolve-Path "$PSScriptRoot\..\..")
$gradle = if (Test-Path ".\gradlew.bat") { ".\gradlew.bat" } elseif (Test-Path ".\gradlew") { ".\gradlew" } else { "gradle" }
if ($env:OS -eq 'Windows_NT' -and $env:JAVA_HOME -and (Test-Path -LiteralPath $env:JAVA_HOME)) {
    $packages = Join-Path $env:JAVA_HOME 'Packages'
    if (-not (Test-Path -LiteralPath $packages)) {
        New-Item -ItemType Directory -Path $packages -Force | Out-Null
    }
}
& $gradle -p shaft-intellij check buildPlugin verifyPlugin
