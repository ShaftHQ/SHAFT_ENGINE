[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string] $OutputPath,

    [int] $FrameRate = 15,

    [int] $MaximumDurationSeconds = 900,

    [string] $StopFile
)

$ErrorActionPreference = 'Stop'

if ($PSVersionTable.PSVersion.Major -lt 7) {
    throw 'capture-desktop.ps1 requires PowerShell 7 or newer. Run it with pwsh.'
}

if ($FrameRate -lt 1 -or $FrameRate -gt 60) {
    throw 'FrameRate must be between 1 and 60.'
}
if ($MaximumDurationSeconds -lt 1) {
    throw 'MaximumDurationSeconds must be positive.'
}

$ffmpeg = Get-Command ffmpeg -ErrorAction Stop
$resolvedOutput = [System.IO.Path]::GetFullPath($OutputPath)
$outputDirectory = Split-Path -Parent $resolvedOutput
if (-not (Test-Path -LiteralPath $outputDirectory -PathType Container)) {
    New-Item -ItemType Directory -Path $outputDirectory -Force | Out-Null
}
if (Test-Path -LiteralPath $resolvedOutput) {
    throw "Refusing to overwrite existing capture: $resolvedOutput"
}

$resolvedStopFile = if ([string]::IsNullOrWhiteSpace($StopFile)) {
    "$resolvedOutput.stop"
} else {
    [System.IO.Path]::GetFullPath($StopFile)
}
if ([System.StringComparer]::OrdinalIgnoreCase.Equals($resolvedOutput, $resolvedStopFile)) {
    throw 'StopFile must be different from OutputPath.'
}
if (Test-Path -LiteralPath $resolvedStopFile) {
    throw "Refusing to remove existing stop marker: $resolvedStopFile"
}

$arguments = @(
    '-hide_banner',
    '-loglevel', 'warning',
    '-f', 'gdigrab',
    '-framerate', $FrameRate,
    '-i', 'desktop',
    '-t', $MaximumDurationSeconds,
    '-c:v', 'libx264',
    '-preset', 'ultrafast',
    '-pix_fmt', 'yuv420p',
    '-y', $resolvedOutput
)

$startInfo = [System.Diagnostics.ProcessStartInfo]::new()
$startInfo.FileName = $ffmpeg.Source
$startInfo.UseShellExecute = $false
$startInfo.CreateNoWindow = $true
$startInfo.RedirectStandardInput = $true
$startInfo.RedirectStandardError = $false
foreach ($argument in $arguments) {
    $startInfo.ArgumentList.Add([string] $argument)
}

$process = [System.Diagnostics.Process]::new()
$process.StartInfo = $startInfo
if (-not $process.Start()) {
    throw 'ffmpeg did not start.'
}

Write-Output "Recording PID $($process.Id) to $resolvedOutput"
Write-Output "Create $resolvedStopFile to stop and finalize the capture."

try {
    while (-not $process.HasExited) {
        if (Test-Path -LiteralPath $resolvedStopFile) {
            $process.StandardInput.WriteLine('q')
            $process.StandardInput.Flush()
            break
        }
        Start-Sleep -Milliseconds 250
    }
    $process.WaitForExit()
    if ($process.ExitCode -ne 0) {
        throw "ffmpeg exited with code $($process.ExitCode)."
    }
} finally {
    if (Test-Path -LiteralPath $resolvedStopFile) {
        Remove-Item -LiteralPath $resolvedStopFile -Force
    }
    $process.Dispose()
}

if (-not (Test-Path -LiteralPath $resolvedOutput -PathType Leaf)) {
    throw "Capture file was not created: $resolvedOutput"
}

Get-Item -LiteralPath $resolvedOutput
