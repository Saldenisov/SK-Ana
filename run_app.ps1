[CmdletBinding()]
param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $LauncherArgs
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$batchLauncher = Join-Path $scriptDir "run_app.bat"

if (-not (Test-Path -LiteralPath $batchLauncher)) {
    throw "Could not find run_app.bat next to run_app.ps1: $batchLauncher"
}

& $batchLauncher @LauncherArgs
exit $LASTEXITCODE
