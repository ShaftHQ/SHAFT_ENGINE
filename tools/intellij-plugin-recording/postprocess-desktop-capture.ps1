[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string] $InputPath,

    [Parameter(Mandatory)]
    [string] $OutputPath
)

$ErrorActionPreference = 'Stop'

if ($PSVersionTable.PSVersion.Major -lt 7) {
    throw 'postprocess-desktop-capture.ps1 requires PowerShell 7 or newer. Run it with pwsh.'
}

$ffmpeg = Get-Command ffmpeg -ErrorAction Stop
$ffprobe = Get-Command ffprobe -ErrorAction Stop
$resolvedInput = (Resolve-Path -LiteralPath $InputPath).Path
$resolvedOutput = [System.IO.Path]::GetFullPath($OutputPath)

if ($resolvedInput -eq $resolvedOutput) {
    throw 'InputPath and OutputPath must be different; the raw capture is preserved.'
}
if (Test-Path -LiteralPath $resolvedOutput) {
    throw "Refusing to overwrite existing processed capture: $resolvedOutput"
}
$outputDirectory = Split-Path -Parent $resolvedOutput
if (-not (Test-Path -LiteralPath $outputDirectory -PathType Container)) {
    New-Item -ItemType Directory -Path $outputDirectory -Force | Out-Null
}

$filter = 'mpdecimate=hi=768:lo=320:frac=0.33,setpts=N/FRAME_RATE/TB'
& $ffmpeg.Source -hide_banner -loglevel warning -i $resolvedInput -vf $filter -an -c:v libx264 -preset medium -crf 20 -pix_fmt yuv420p -movflags +faststart -n $resolvedOutput
if ($LASTEXITCODE -ne 0) {
    throw "ffmpeg post-processing failed with exit code $LASTEXITCODE."
}

$probeFormat = 'duration,nb_streams,size'
$raw = & $ffprobe.Source -v error -show_entries "format=$probeFormat" -of json $resolvedInput | ConvertFrom-Json
$processed = & $ffprobe.Source -v error -show_entries "format=$probeFormat" -of json $resolvedOutput | ConvertFrom-Json

[pscustomobject]@{
    RawPath = $resolvedInput
    RawDurationSeconds = [double] $raw.format.duration
    RawBytes = [long] $raw.format.size
    ProcessedPath = $resolvedOutput
    ProcessedDurationSeconds = [double] $processed.format.duration
    ProcessedBytes = [long] $processed.format.size
}
