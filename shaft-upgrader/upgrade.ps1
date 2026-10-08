#Requires -Version 5.1
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
function Find-Python {
  foreach ($name in @("py", "python3", "python")) {
    $cmd = Get-Command $name -ErrorAction SilentlyContinue
    if ($null -ne $cmd) { return $cmd.Source }
  }
  throw "python3 is required to run the SHAFT Engine project upgrader."
}
$Python = Find-Python
if ($Python -like "*py.exe" -or (Split-Path -Leaf $Python) -eq "py") {
  & $Python -3 "$ScriptDir\upgrade_to_modular_shaft.py" @args
} else {
  & $Python "$ScriptDir\upgrade_to_modular_shaft.py" @args
}
exit $LASTEXITCODE
