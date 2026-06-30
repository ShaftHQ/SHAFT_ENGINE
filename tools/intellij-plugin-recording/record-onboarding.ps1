$ErrorActionPreference = 'Stop'
Set-Location (Resolve-Path "$PSScriptRoot\..\..")
& .\gradlew -p shaft-intellij check buildPlugin verifyPlugin
