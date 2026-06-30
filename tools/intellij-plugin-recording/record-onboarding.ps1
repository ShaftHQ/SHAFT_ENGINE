$ErrorActionPreference = 'Stop'
Set-Location (Resolve-Path "$PSScriptRoot\..\..")
$gradle = if (Test-Path ".\gradlew.bat") { ".\gradlew.bat" } elseif (Test-Path ".\gradlew") { ".\gradlew" } else { "gradle" }
& $gradle -p shaft-intellij check buildPlugin verifyPlugin
