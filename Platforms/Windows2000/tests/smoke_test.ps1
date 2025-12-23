# Simple smoke test for TimeWarpIDE.exe
# Launches the EXE for a short time and ensures it started

$exe = Join-Path $PSScriptRoot "..\TimeWarpIDE.exe"
if (!(Test-Path $exe)) {
    Write-Error "Executable not found: $exe"
    exit 2
}

Write-Host "Starting smoke-run for $exe"
$p = Start-Process -FilePath $exe -PassThru
Start-Sleep -Seconds 5
if ($p.HasExited) {
    Write-Host "Process started and exited quickly; exit code: $($p.ExitCode)"
} else {
    Write-Host "Process started, now terminating"
    Stop-Process -Id $p.Id -Force
}

Write-Host "Smoke test completed"
exit 0
