# Launch Time Warp IDE on Windows
# Requires Python on PATH

$ErrorActionPreference = 'Stop'

# Prefer the Python entry under platforms/python
$repoRoot = Split-Path -Parent $PSScriptRoot
$pythonEntry = Join-Path $repoRoot 'platforms/python/time_warp_ide.py'

if (-not (Get-Command python -ErrorAction SilentlyContinue)) {
    Write-Error 'Python not found on PATH. Please install Python 3.10+.'
}

Write-Host "Launching Time Warp..."
python "$pythonEntry" @args
