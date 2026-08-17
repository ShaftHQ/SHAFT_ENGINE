#Requires -Version 7
<#
.SYNOPSIS
  Named workstation coder: shaft-java-agent. Forwards to run_agent.ps1.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Worktree,
    [Parameter(Mandatory = $true)]
    [string] $Spec,
    [Parameter(Mandatory = $true)]
    [string[]] $Allowlist,
    [string] $TestCommand = "",
    [string] $Model = "qwen2.5-coder:7b",
    [string] $Root = $(if ($env:SHAFT_LOCAL_AI_ROOT) { $env:SHAFT_LOCAL_AI_ROOT } else { "D:\AI" }),
    [switch] $Push
)

$forward = @{
    Worktree = $Worktree
    Spec = $Spec
    Allowlist = $Allowlist
    TestCommand = $TestCommand
    Model = $Model
    Root = $Root
}
if ($Push) { $forward.Push = $true }
& (Join-Path $PSScriptRoot "run_agent.ps1") @forward
exit $LASTEXITCODE
