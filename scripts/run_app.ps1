[CmdletBinding()]
param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $LauncherArgs
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$rootLauncher = Join-Path (Split-Path -Parent $scriptDir) "run_app.ps1"

if (-not (Test-Path -LiteralPath $rootLauncher)) {
    throw "Could not find top-level run_app.ps1: $rootLauncher"
}

& $rootLauncher @LauncherArgs
exit $LASTEXITCODE
