<#
.SYNOPSIS
Launches or stops SK-Ana through the batch launcher.

.EXAMPLE
./run_app.ps1

.EXAMPLE
./run_app.ps1 -Stop

.EXAMPLE
./run_app.ps1 stop
#>
[CmdletBinding()]
param(
    [switch] $Stop,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $LauncherArgs
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$batchLauncher = Join-Path $scriptDir "run_app.bat"

if (-not (Test-Path -LiteralPath $batchLauncher)) {
    throw "Could not find run_app.bat next to run_app.ps1: $batchLauncher"
}

if ($Stop) {
    $LauncherArgs = @("stop") + $LauncherArgs
}

& $batchLauncher @LauncherArgs
exit $LASTEXITCODE
