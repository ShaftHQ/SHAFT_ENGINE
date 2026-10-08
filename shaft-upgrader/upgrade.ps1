#Requires -Version 5.1
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
function Test-Python([string]$Executable, [string[]]$Prefix) {
  try {
    & $Executable @Prefix -c "import sys; raise SystemExit(0 if sys.version_info >= (3, 9) else 1)" 2>$null | Out-Null
    return ($LASTEXITCODE -eq 0)
  } catch {
    return $false
  }
}
function Find-Python {
  foreach ($name in @("py", "python3", "python")) {
    $cmd = Get-Command $name -ErrorAction SilentlyContinue
    if ($null -eq $cmd) { continue }
    $prefix = @()
    if ($name -eq "py") { $prefix = @("-3") }
    # Skips the Windows Store alias stub (exit 9009) and Python older than 3.9.
    if (Test-Python $cmd.Source $prefix) { return @{ Path = $cmd.Source; Prefix = $prefix } }
  }
  throw "Python 3.9 or newer is required to run the SHAFT Engine project upgrader."
}
$Python = Find-Python
$Prefix = $Python.Prefix
& $Python.Path @Prefix "$ScriptDir\upgrade_to_modular_shaft.py" @args
exit $LASTEXITCODE
