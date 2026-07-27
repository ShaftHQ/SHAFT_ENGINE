<#
.SYNOPSIS
  Resets the SHAFT IntelliJ plugin capture demo's Selenium+TestNG project to
  a pristine copy of demo-project-template/.

.DESCRIPTION
  Run this before every recording take (including re-recording attempts) so
  codegen output, .idea/, and target/ left over from a prior take never leak
  into the next one. Deletes -TargetPath entirely, then re-copies the
  checked-in template.

  See tools/intellij-plugin-recording/video-capture-demo.md for the full
  capture procedure this script is a step of.

.PARAMETER TargetPath
  Working directory the demo project is (re)created at. Defaults to
  %USERPROFILE%\shaft-demo-workspace\selenium-testng-demo -- deliberately
  outside the SHAFT_ENGINE repo so opening it in IntelliJ never touches this
  repo's VCS state.
#>
param(
    [string]$TargetPath = (Join-Path $env:USERPROFILE 'shaft-demo-workspace\selenium-testng-demo')
)
$ErrorActionPreference = 'Stop'
$templatePath = Join-Path $PSScriptRoot 'demo-project-template'

if (-not (Test-Path -LiteralPath $templatePath)) {
    Write-Error "Template not found at '$templatePath'."
    exit 1
}

if (Test-Path -LiteralPath $TargetPath) {
    Remove-Item -LiteralPath $TargetPath -Recurse -Force
}
New-Item -ItemType Directory -Path $TargetPath -Force | Out-Null
Copy-Item -Path (Join-Path $templatePath '*') -Destination $TargetPath -Recurse -Force

Write-Host "Demo project reset at: $TargetPath"
