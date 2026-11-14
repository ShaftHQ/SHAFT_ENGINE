# ChaosEngine moved to https://github.com/ShaftHQ/chaosengine-harness.
# This shim keeps the old SHAFT_ENGINE one-liner working: it forwards every
# argument to the installer on chaosengine-harness main.
[Console]::Error.WriteLine("ChaosEngine moved to ShaftHQ/chaosengine-harness; forwarding.")
& ([scriptblock]::Create((Invoke-RestMethod "https://raw.githubusercontent.com/ShaftHQ/chaosengine-harness/main/install.ps1"))) @args
