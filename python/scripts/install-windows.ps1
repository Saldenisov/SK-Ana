[CmdletBinding()]
param(
    [switch]$SetupOnly,
    [switch]$Dev,
    [switch]$DryRun,
    [string]$CondaExe,
    [string]$EnvironmentPrefix,
    [ValidateRange(1, 65535)][int]$Port = 8501
)

$ErrorActionPreference = 'Stop'
$pythonRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..')).Path
$appPath = Join-Path $pythonRoot 'skana\app.py'
$installRoot = Join-Path $env:LOCALAPPDATA 'SKAna\miniconda3'
if (-not $EnvironmentPrefix) {
    $EnvironmentPrefix = Join-Path $env:LOCALAPPDATA 'SKAna\envs\skana-python'
}
$installerName = 'Miniconda3-py312_26.5.3-2-Windows-x86_64.exe'
$installerUrl = "https://repo.anaconda.com/miniconda/$installerName"
$installerSha256 = '75e829b26bd7b33b1dce118639b8f39e561a6ebaa3b593b633d7445dd1a2d65a'

if ([Runtime.InteropServices.RuntimeInformation]::OSArchitecture -ne 'X64') {
    throw 'This installer supports Windows x86_64. Use an existing compatible Conda installation for other architectures.'
}

if ($DryRun) {
    [pscustomobject]@{
        Platform = 'Windows-x86_64'
        MinicondaUrl = $installerUrl
        MinicondaSha256 = $installerSha256
        EnvironmentPrefix = $EnvironmentPrefix
        Python = '3.12'
        Package = $(if ($Dev) { "${pythonRoot}[dev]" } else { $pythonRoot })
        SetupOnly = [bool]$SetupOnly
    } | Format-List
    return
}

function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Program failed with exit code $LASTEXITCODE."
    }
}

if ($CondaExe) {
    $CondaExe = (Resolve-Path -LiteralPath $CondaExe).Path
} elseif ($env:CONDA_EXE -and (Test-Path -LiteralPath $env:CONDA_EXE)) {
    $CondaExe = $env:CONDA_EXE
} elseif (Get-Command conda.exe -ErrorAction SilentlyContinue) {
    $CondaExe = (Get-Command conda.exe).Source
} elseif (Test-Path -LiteralPath (Join-Path $installRoot 'Scripts\conda.exe')) {
    $CondaExe = Join-Path $installRoot 'Scripts\conda.exe'
} else {
    if (Test-Path -LiteralPath $installRoot) {
        throw "Incomplete Miniconda installation at $installRoot. Resolve it before retrying."
    }
    New-Item -ItemType Directory -Path (Split-Path -Parent $installRoot) -Force | Out-Null
    $installerPath = Join-Path ([IO.Path]::GetTempPath()) ("skana-" + [guid]::NewGuid().ToString('N') + '.exe')
    try {
        Write-Host "Downloading Miniconda from $installerUrl"
        Invoke-WebRequest -Uri $installerUrl -OutFile $installerPath
        $actualHash = (Get-FileHash -LiteralPath $installerPath -Algorithm SHA256).Hash.ToLowerInvariant()
        if ($actualHash -ne $installerSha256) {
            throw "Miniconda SHA-256 mismatch: $actualHash"
        }
        $arguments = @('/InstallationType=JustMe', '/AddToPath=0', '/RegisterPython=0', '/S', "/D=$installRoot")
        $process = Start-Process -FilePath $installerPath -ArgumentList $arguments -Wait -PassThru -WindowStyle Hidden
        if ($process.ExitCode -ne 0) {
            throw "Miniconda installer failed with exit code $($process.ExitCode)."
        }
    } finally {
        if (Test-Path -LiteralPath $installerPath) {
            Remove-Item -LiteralPath $installerPath -Force
        }
    }
    $CondaExe = Join-Path $installRoot 'Scripts\conda.exe'
}

if (-not (Test-Path -LiteralPath $CondaExe)) {
    throw "Conda executable not found: $CondaExe"
}
if (-not (Test-Path -LiteralPath (Join-Path $EnvironmentPrefix 'conda-meta\history'))) {
    Invoke-Checked -Program $CondaExe -Arguments @('create', '--yes', '--prefix', $EnvironmentPrefix,
        '--override-channels', '--channel', 'conda-forge', 'python=3.12', 'pip')
}
$package = if ($Dev) { "${pythonRoot}[dev]" } else { $pythonRoot }
Invoke-Checked -Program $CondaExe -Arguments @('run', '--no-capture-output', '--prefix', $EnvironmentPrefix,
    'python', '-m', 'pip', 'install', '--disable-pip-version-check', '-e', $package)
Invoke-Checked -Program $CondaExe -Arguments @('run', '--no-capture-output', '--prefix', $EnvironmentPrefix,
    'python', '-m', 'pip', 'check')

Write-Host "SK-Ana Python is installed in $EnvironmentPrefix"
if (-not $SetupOnly) {
    Write-Host "Open http://127.0.0.1:$Port/"
    Invoke-Checked -Program $CondaExe -Arguments @('run', '--no-capture-output', '--prefix', $EnvironmentPrefix,
        'python', '-m', 'streamlit', 'run', $appPath,
        '--server.address', '127.0.0.1', '--server.port', "$Port", '--server.headless', 'true')
}
